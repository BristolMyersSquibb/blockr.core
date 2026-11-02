// The transport that core's store, scheduler and CoreApi send their requests
// through, as inst/protocol/README.md gives the protocol. It gives each
// request its id, sends nothing but a session's hello until R has answered
// it, matches R's replies and progress notes to the requests waiting on them,
// and ignores a reply to a request it is not waiting on. It runs over a
// channel, which the Shiny bridge gives on the page (srcts/bridge/shiny.ts).

import type { BlockId } from "../types/board.ts";
import type {
  Capability,
  CancelRequest,
  ClientMessage,
  ForgetRequest,
  HelloRequest,
  Progress,
  Reply,
  Request,
} from "./messages.ts";

type DistributiveOmit<T, K extends PropertyKey> = T extends unknown
  ? Omit<T, K>
  : never;

// A request as its caller hands it to the transport, which gives it its id and
// announces its body. The transport sends each session's hello itself, and a
// request's cancel when its signal aborts.
export type Outgoing = DistributiveOmit<
  Exclude<Request, HelloRequest | CancelRequest | ForgetRequest>,
  "id" | "body"
>;

// What the client names in a session's hello.
export type Hello = Omit<HelloRequest, "id" | "kind">;

export type ProgressNote = Progress["progress"];

export interface RequestOptions {
  // The request's binary body, such as the file an upload action receives,
  // which the transport announces in the request's `body` by its type and, for
  // a File, its name.
  body?: Blob;
  // Hears each progress note that R sends for the request.
  progress?: (note: ProgressNote) => void;
  // Gives the request up: one not sent yet ends cancelled, as R would have
  // ended it, and one that R holds is cancelled in R where both sides named
  // `cancel`, and ends with whatever R replies to it.
  signal?: AbortSignal;
}

// R's reply to a request, with the binary body that it announces, a File where
// the reply names one, or null.
export interface Answer {
  reply: Reply;
  body: Blob | null;
}

export interface Transport {
  // Sends a request and resolves with R's reply to it, whatever its status.
  // Rejects with a SessionEndedError where the session that it went to ended
  // before R replied, and with the channel's error where the channel could not
  // send it, such as where R refused its body.
  request(request: Outgoing, options?: RequestOptions): Promise<Answer>;
  // Has R forget blocks that left the board, which gets no reply.
  forget(blocks: BlockId[]): void;
}

export interface TransportOptions {
  // What to name in each session's hello, such as the packages of a board that
  // the client holds.
  hello(): Hello;
  // Hears R's reply to each session's hello, the first session's included,
  // once the transport has sent what it held for the session.
  session(answer: Answer): void;
}

// What a transport sends its messages over and hears R's on, such as the Shiny
// bridge (srcts/bridge/shiny.ts).
export interface Channel {
  // Sends a message once the bodies of its requests, given by request id, have
  // gone as the channel carries them, and rejects where it could not send
  // them, sending nothing.
  send(message: ClientMessage, bodies: ReadonlyMap<string, Blob>): Promise<void>;
  // Hears R from now on.
  listen(listener: ChannelListener): void;
}

export interface ChannelListener {
  // R started a session, to which the client sends its hello first.
  started(): void;
  // The session ended, so that R replies to nothing that was sent to it.
  ended(): void;
  // A reply or a progress note from R, in the order R sent them, a reply that
  // announces a body with a promise of it.
  received(item: Reply | Progress, body: Promise<Blob> | null): void;
}

// The session that a request went to ended before R replied, so that it may
// or may not have run.
export class SessionEndedError extends Error {
  constructor(id: string) {
    super(`The session ended before R replied to request ${id}.`);
    this.name = "SessionEndedError";
  }
}

type Settled = { answer: Answer } | { error: Error };

// A request that a caller waits on.
interface Call {
  id: string;
  body: Blob | undefined;
  progress: ((note: ProgressNote) => void) | undefined;
  settle(result: Settled): void;
  // Whether the channel has sent it, and whether its signal aborted before.
  sent: boolean;
  aborted: boolean;
}

// A message's entry: a request with its call, or a cancel or a forget.
interface Outbound {
  message: Request;
  call?: Call;
}

export function createTransport(
  channel: Channel,
  options: TransportOptions,
): Transport {
  let next = 0;
  // "none" while no session runs, "started" once R started one, and "ready"
  // once R answered its hello.
  let state: "none" | "started" | "ready" = "none";
  // The capabilities that both sides named in the session's hello.
  let shared = new Set<Capability>();

  const waiting = new Map<string, Call>();
  let held: Outbound[] = [];

  function mint(): string {
    next += 1;
    return String(next);
  }

  function dispatch(outbound: Outbound[]): void {
    // Requests without a body travel together, while one with a body goes in a
    // message of its own once its body has, so that a body that cannot be sent
    // fails its request alone, and requests after it may reach R first.
    const plain = outbound.filter((entry) => entry.call?.body === undefined);

    if (plain.length > 0) {
      transmit(plain, new Map());
    }

    for (const entry of outbound) {
      if (entry.call?.body !== undefined) {
        transmit([entry], new Map([[entry.call.id, entry.call.body]]));
      }
    }
  }

  function transmit(outbound: Outbound[], bodies: Map<string, Blob>): void {
    const calls = outbound.flatMap((entry) => entry.call ?? []);

    for (const call of calls) {
      waiting.set(call.id, call);
    }

    channel
      .send(
        outbound.map((entry) => entry.message),
        bodies,
      )
      .then(
        () => {
          for (const call of calls) {
            call.sent = true;
            if (call.aborted) cancel(call);
          }
        },
        (error: unknown) => {
          for (const call of calls) {
            if (waiting.get(call.id) === call) {
              waiting.delete(call.id);
              call.settle({ error: asError(error) });
            }
          }
        },
      );
  }

  function cancel(call: Call): void {
    if (waiting.get(call.id) !== call || !shared.has("cancel")) return;
    quietly(channel.send([{ kind: "cancel", request: call.id }], new Map()));
  }

  function abort(entry: Outbound, call: Call): void {
    const index = held.indexOf(entry);

    if (index >= 0) {
      held.splice(index, 1);
      call.settle({ answer: cancelled(call.id) });
    } else if (call.sent) {
      cancel(call);
    } else {
      call.aborted = true;
    }
  }

  function queue(entry: Outbound): void {
    if (state === "ready") {
      dispatch([entry]);
    } else {
      held.push(entry);
    }
  }

  function end(): void {
    state = "none";
    shared = new Set();

    const lost = [...waiting.values()];
    waiting.clear();

    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }

  function hello(): void {
    const named = options.hello();
    const id = mint();

    const call: Call = {
      id,
      body: undefined,
      progress: undefined,
      sent: false,
      aborted: false,
      settle(result) {
        // A session may end before R answers its hello, which the next
        // session's hello then starts afresh.
        if (!("answer" in result)) {
          if (!(result.error instanceof SessionEndedError)) report(result.error);
          return;
        }

        const { reply } = result.answer;

        if (reply.status === "ok" && "capabilities" in reply) {
          const theirs = new Set(reply.capabilities);
          shared = new Set(named.capabilities.filter((c) => theirs.has(c)));
          state = "ready";

          const outbound = held;
          held = [];
          dispatch(outbound);
        }

        guard(() => {
          options.session(result.answer);
        });
      },
    };

    transmit([{ message: { ...named, id, kind: "hello" }, call }], new Map());
  }

  channel.listen({
    started() {
      end();
      state = "started";
      hello();
    },
    ended() {
      end();
    },
    received(item, body) {
      const call = waiting.get(item.id);

      // A reply or a note for a request that nothing waits on, such as one to
      // a hello that Shiny replayed after a reconnect.
      if (call === undefined) return;

      if (!("status" in item)) {
        const note = item.progress;
        guard(() => {
          call.progress?.(note);
        });
        return;
      }

      waiting.delete(item.id);

      if (body === null) {
        call.settle({ answer: { reply: item, body: null } });
      } else {
        body.then(
          (blob) => {
            call.settle({ answer: { reply: item, body: blob } });
          },
          (error: unknown) => {
            call.settle({ error: asError(error) });
          },
        );
      }
    },
  });

  return {
    request(request, { body, progress, signal } = {}) {
      const id = mint();
      const message = {
        ...request,
        id,
        ...(body === undefined ? {} : { body: announce(body) }),
      } as Request;

      return new Promise<Answer>((resolve, reject) => {
        const onAbort = (): void => {
          abort(entry, call);
        };

        const call: Call = {
          id,
          body,
          progress,
          sent: false,
          aborted: false,
          settle(result) {
            signal?.removeEventListener("abort", onAbort);
            if ("answer" in result) {
              resolve(result.answer);
            } else {
              reject(result.error);
            }
          },
        };

        const entry: Outbound = { message, call };

        if (signal?.aborted === true) {
          call.settle({ answer: cancelled(id) });
          return;
        }

        signal?.addEventListener("abort", onAbort, { once: true });
        queue(entry);
      });
    },
    forget(blocks) {
      queue({ message: { kind: "forget", blocks } });
    },
  };
}

// A body's announcement, by its MIME type and, for a File, its name.
function announce(body: Blob): { type: string; name?: string } {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}

function cancelled(id: string): Answer {
  return { reply: { id, status: "cancelled" }, body: null };
}

// Calls a caller's function, reporting what it throws without stopping the
// transport, which goes on with the rest of R's message.
function guard(fn: () => void): void {
  try {
    fn();
  } catch (error: unknown) {
    report(error);
  }
}

function asError(error: unknown): Error {
  return error instanceof Error ? error : new Error(String(error));
}

function report(error: unknown): void {
  queueMicrotask(() => {
    throw error;
  });
}

// Sends a message that nothing waits on, such as a cancel, which goes nowhere
// once its session has ended.
function quietly(sent: Promise<void>): void {
  sent.catch(() => undefined);
}
