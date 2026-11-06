// The answers of requests that core keeps for one opening of a board, those of
// block classes, output classes and renderers, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// - Core keeps an answer by its key. For a block class's request, that is the
//   registry id, the request's name, its args, the values of the board options
//   it sent and the revisions it read, so that every block of the class shares
//   it. For an output class's or a renderer's request, it is the renderer, the
//   output's block and revision, the request's name, its args and the board
//   option values and parameters that the output was asked with. An answer
//   never goes stale for its key, so nothing asks R for it again.
// - A request merges with an identical one in flight. Once no caller that
//   merged with it still waits, it is given up, and R gets a cancel.
// - Core drops an answer once the revisions it read are superseded. No host
//   method hands out such an answer, and a module keeps the last answer it
//   got on screen while it asks again.
// - Core keeps nothing of a volatile request, nor of a reply that R marks
//   volatile, and neither keeps nor merges an action, which R runs once per
//   call, or a request that sends a file.
// - Core keeps answers for the board it makes the mounts for. As another board
//   opens, the signal that ends the mounts drops them and gives up the
//   requests on their way to R, as a page that starts with that board holds
//   none.

import type {
  BlockClassRequest,
  BlockRevision,
  OutputClassRequest,
  ProgressNote,
  RendererRequest,
  Reply,
} from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Json } from "../types/json.ts";
import { asError, isObject, report } from "../util/index.ts";

// A request of a block class, as it goes to R but for its id, which the
// protocol's client gives it.
export type ClassRequest = Omit<BlockClassRequest, "id" | "body">;

// A request of an output class or of a renderer, as it goes to R but for its
// id.
export type OutputAsk = Omit<OutputClassRequest, "id" | "body"> | Omit<RendererRequest, "id" | "body">;

export interface Ask {
  readonly request: ClassRequest | OutputAsk;
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
  readonly request: Ask["request"];
}

export function createAnswers(
  scheduler: Pick<Scheduler, "request" | "status" | "subscribe">,
  signal?: AbortSignal,
): Answers {
  const kept = new Map<string, Kept>();
  const flights = new Map<string, Flight>();

  signal?.addEventListener(
    "abort",
    () => {
      kept.clear();
      const given = [...flights.values()];
      flights.clear();
      for (const flight of given) flight.controller.abort();
    },
    { once: true },
  );

  // A status that changes may be a revision that moved on, which supersedes
  // what an answer read.
  scheduler.subscribe(() => {
    sweep();
  });

  // Whether R still holds what the answer read, at the revisions it read.
  function current(request: Ask["request"]): boolean {
    return readsOf(request).every(
      (read) => read.block === undefined || scheduler.status(read.block)?.revision === read.revision,
    );
  }

  // Drops the answers whose reads are superseded.
  function sweep(): void {
    for (const [key, { request }] of kept) {
      if (!current(request)) kept.delete(key);
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
        if (
          key !== undefined &&
          ask.share === "keep" &&
          answer.reply.status === "ok" &&
          !("volatile" in answer.reply && answer.reply.volatile === true) &&
          current(ask.request)
        ) {
          kept.set(key, { answer, request: ask.request });
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
      if (answer !== undefined) return Promise.resolve(answer);
      const flight = (key === undefined ? undefined : flights.get(key)) ?? start(ask, key);
      return join(flight, key, signal, progress);
    },
    size: () => kept.size,
  };
}


// What makes two requests identical. For a block class's request, that is the
// class, the name, the args, the values of the board options sent and the
// revisions read, but not the block that asks, since every block of the class
// shares the answer. For an output class's or a renderer's request, it is the
// renderer, the output it belongs to, the name, the args and the board option
// values and parameters that the output was asked with.
function keyOf(request: Ask["request"]): string {
  if (request.type === "block_class") {
    return JSON.stringify(
      sorted([
        request.type,
        request.registry_id,
        request.name,
        request.args,
        request.options ?? {},
        request.reads as unknown as Json,
      ]),
    );
  }
  return JSON.stringify(
    sorted([
      request.type,
      request.type === "renderer" ? request.renderer : null,
      request.output as unknown as Json,
      request.name,
      request.args,
      request.options,
      request.params,
    ]),
  );
}

// The results that a request reads: a block class's, the results it names,
// and an output class's or a renderer's, the result its output was made from.
function readsOf(request: Ask["request"]): readonly (BlockRevision | { block?: never })[] {
  return request.type === "block_class" ? request.reads : [request.output];
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
