// The answers of block classes' requests, which core keeps for the page, as
// the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// - Core keeps an answer by its key: the registry id, the request's name, its
//   args, the values of the board options it sent and the revisions it read.
//   Every block of the class shares it. An answer never goes stale for its
//   key, so nothing asks R for it again.
// - A request merges with an identical one in flight. Once no caller that
//   merged with it still waits, it is given up, and R gets a cancel.
// - Core drops an answer once the revisions it read are superseded, but keeps
//   a block's last answer of a request until the block's next answer of it
//   arrives, so that it is there while the next one loads, however long the
//   module takes to ask again.
// - Core keeps nothing of a volatile request, and neither keeps nor merges an
//   action, which R runs once per call, or a request that sends a file.
// - Core keeps answers for one opening of a board. Once another board opens,
//   it drops them and gives up the requests on their way to R, as a page that
//   starts with that board holds none.

import type { BlockClassRequest, ProgressNote, Reply } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Json } from "../types/json.ts";
import { asError, isObject, report } from "../util/index.ts";

// A request of a block class, as it goes to R but for its id, which the
// protocol's client gives it.
export type ClassRequest = Omit<BlockClassRequest, "id" | "body">;

export interface Ask {
  readonly request: ClassRequest;
  // The file it sends, which the client announces in the request's `body`.
  readonly body?: Blob;
  // How core shares it: "keep" for an answer that core keeps, "merge" for a
  // volatile request, whose answer only merges with an identical request in
  // flight, and "once" for an action or a request that sends a file.
  readonly share: "keep" | "merge" | "once";
}

export interface Answer {
  readonly reply: Reply;
  readonly body: Blob | null;
}

export interface Answers {
  // Resolves with R's reply to the request and the body it announces, from
  // what core keeps where it can, or rejects with what the caller's signal
  // aborted with, or with why the request got no reply.
  ask(
    ask: Ask,
    options?: { signal?: AbortSignal; progress?: (note: ProgressNote) => void },
  ): Promise<Answer>;
  // Drops every answer and gives up every request on its way to R, as
  // another board opens.
  clear(): void;
  // How many answers core keeps, for tests.
  size(): number;
}

// A request on its way to R, with the callers that wait on it.
interface Flight {
  readonly done: Promise<Answer>;
  readonly controller: AbortController;
  readonly progress: Set<{ fn: (note: ProgressNote) => void }>;
  callers: number;
}

interface Kept {
  readonly answer: Answer;
  readonly request: ClassRequest;
}

export function createAnswers(scheduler: Pick<Scheduler, "request" | "status" | "subscribe">): Answers {
  const kept = new Map<string, Kept>();
  const flights = new Map<string, Flight>();
  // The key of each block's last answer of each request, by the block and the
  // request.
  const last = new Map<string, { block: string; key: string }>();

  // A status that changes may be a revision that moved on, which supersedes
  // what an answer read.
  scheduler.subscribe(() => {
    sweep();
  });

  // Whether R still holds what the answer read, at the revisions it read.
  function current(request: ClassRequest): boolean {
    return request.reads.every(
      (read) => read.block === undefined || scheduler.status(read.block)?.revision === read.revision,
    );
  }

  // Drops the answers whose reads are superseded, but for the last answer of
  // a request of each block still on the board.
  function sweep(): void {
    const held = new Set<string>();
    for (const [asking, { block, key }] of last) {
      if (scheduler.status(block) === undefined) last.delete(asking);
      else held.add(key);
    }
    for (const [key, { request }] of kept) {
      if (!held.has(key) && !current(request)) kept.delete(key);
    }
  }

  function start(ask: Ask, key: string | undefined): Flight {
    const controller = new AbortController();
    const progress = new Set<{ fn: (note: ProgressNote) => void }>();

    const done = scheduler
      .request(ask.request, {
        ...(ask.body === undefined ? {} : { body: ask.body }),
        signal: controller.signal,
        progress(note) {
          for (const caller of [...progress]) {
            try {
              caller.fn(note);
            } catch (error) {
              report(error);
            }
          }
        },
      })
      // The answer is kept before the request leaves what is in flight, so
      // that an identical request finds one or the other.
      .then((answer) => {
        if (key !== undefined && ask.share === "keep" && answer.reply.status === "ok") {
          kept.set(key, { answer, request: ask.request });
          last.set(askingOf(ask.request), { block: ask.request.block, key });
          sweep();
        }
        return answer;
      })
      .finally(() => {
        if (key !== undefined && flights.get(key) === flight) flights.delete(key);
      });
    // What nobody waits on any more is given up, and its failure is no news.
    done.catch(() => undefined);

    const flight: Flight = { done, controller, progress, callers: 0 };
    if (key !== undefined) flights.set(key, flight);
    return flight;
  }

  function join(
    flight: Flight,
    key: string | undefined,
    signal: AbortSignal | undefined,
    progress: ((note: ProgressNote) => void) | undefined,
  ): Promise<Answer> {
    flight.callers += 1;
    const listener = progress === undefined ? undefined : { fn: progress };
    if (listener !== undefined) flight.progress.add(listener);

    return new Promise<Answer>((resolve, reject) => {
      let left = false;
      const leave = (): void => {
        left = true;
        flight.callers -= 1;
        if (listener !== undefined) flight.progress.delete(listener);
        signal?.removeEventListener("abort", abort);
      };
      // A caller who gives up rejects at once. The request is given up once no
      // caller waits on it, and a new caller then starts another.
      const abort = (): void => {
        leave();
        reject(asError(signal?.reason));
        if (flight.callers === 0) {
          if (key !== undefined && flights.get(key) === flight) flights.delete(key);
          flight.controller.abort();
        }
      };
      signal?.addEventListener("abort", abort, { once: true });
      flight.done.then(
        (answer) => {
          if (left) return;
          leave();
          resolve(answer);
        },
        (error: unknown) => {
          if (left) return;
          leave();
          reject(asError(error));
        },
      );
    });
  }

  return {
    ask(ask, { signal, progress } = {}) {
      if (signal?.aborted === true) return Promise.reject(asError(signal.reason));
      const key = ask.share === "once" ? undefined : keyOf(ask.request);
      const answer = key === undefined ? undefined : kept.get(key)?.answer;
      if (key !== undefined && answer !== undefined) {
        last.set(askingOf(ask.request), { block: ask.request.block, key });
        sweep();
        return Promise.resolve(answer);
      }
      const flight = (key === undefined ? undefined : flights.get(key)) ?? start(ask, key);
      return join(flight, key, signal, progress);
    },
    clear() {
      kept.clear();
      last.clear();
      const given = [...flights.values()];
      flights.clear();
      for (const flight of given) flight.controller.abort();
    },
    size: () => kept.size,
  };
}

// The block that asks and the request it asks, as one key.
function askingOf(request: ClassRequest): string {
  return JSON.stringify([request.block, request.name]);
}

// What makes two requests identical: the class, the name, the args, the values
// of the board options sent and the revisions read, but not the block that
// asks, since every block of the class shares the answer.
function keyOf(request: ClassRequest): string {
  return JSON.stringify(
    sorted([
      request.registry_id,
      request.name,
      request.args,
      request.options ?? {},
      request.reads as unknown as Json,
    ]),
  );
}

// The value with every object's keys in order, so that two values that differ
// only in their keys' order give one key.
function sorted(x: Json): Json {
  if (Array.isArray(x)) return (x as readonly Json[]).map(sorted);
  if (isObject(x)) {
    const object = x as Readonly<Record<string, Json>>;
    return Object.fromEntries(
      Object.keys(object)
        .sort()
        .map((name) => [name, sorted(object[name] ?? null)]),
    );
  }
  return x;
}
