// The client of the wire protocol, through which core's store, scheduler and
// CoreApi send their requests to R, as inst/protocol/README.md gives the
// protocol. It gives each request its id, sends nothing but a session's hello
// until R has answered it, matches R's replies and progress notes to the
// requests waiting on them, and ignores a reply to a request it is not waiting
// on. It runs over a transport, which carries the messages: the Shiny
// transport on the page (srcts/transport/shiny.ts).

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

export type ProgressNote = Progress["progress"];

export interface RequestOptions {
  // The request's binary body, such as the file an upload action receives,
  // which the client announces in the request's `body` by its type and, for a
  // File, its name.
  body?: Blob;
  // Hears each progress note that R sends for the request.
  progress?: (note: ProgressNote) => void;
  // Gives the request up: one not sent yet ends cancelled, as R would have
  // ended it, and one that R holds is cancelled in R where both sides named
  // `cancel`, and ends with whatever R replies to it.
  signal?: AbortSignal;
}

export interface Client {
  // Sends a request, which the client gives its id and the announcement of its
  // body, and resolves with R's reply to it, whatever its status, and the
  // binary body that the reply announces, a File where the reply names one, or
  // null. The client sends each session's hello itself, and a request's cancel
  // when its signal aborts. Rejects with a SessionEndedError where the session
  // that the request went to ended before R replied, and with the transport's
  // error where the transport could not send it, such as where R refused its
  // body.
  request(
    request: DistributiveOmit<
      Exclude<Request, HelloRequest | CancelRequest | ForgetRequest>,
      "id" | "body"
    >,
    options?: RequestOptions,
  ): Promise<{ reply: Reply; body: Blob | null }>;
  // Has R forget blocks that left the board, which gets no reply.
  forget(blocks: BlockId[]): void;
}

export interface ClientOptions {
  // What to name in each session's hello, such as the packages of a board that
  // the client holds.
  hello(): Omit<HelloRequest, "id" | "kind">;
  // Hears R's reply to each session's hello, the first session's included,
  // once the client has sent what it held for the session.
  session(reply: Reply): void;
}

// What a client sends its messages over and hears R's on, which carries them,
// such as the Shiny transport (srcts/transport/shiny.ts).
export interface Transport {
  // Sends a message once the bodies of its requests, given by request id, have
  // gone as the transport carries them, and rejects where it could not send
  // them, sending nothing.
  send(message: ClientMessage, bodies: ReadonlyMap<string, Blob>): Promise<void>;
  // Hears R from now on.
  listen(listener: TransportListener): void;
}

export interface TransportListener {
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

// A request that a caller waits on.
interface Call {
  id: string;
  body: Blob | undefined;
  progress: ((note: ProgressNote) => void) | undefined;
  settle(result: { reply: Reply; body: Blob | null } | { error: Error }): void;
  // Whether the transport has sent it, and whether its signal aborted before.
  sent: boolean;
  aborted: boolean;
}

// A message's entry: a request with its call, or a cancel or a forget.
interface Outbound {
  message: Request;
  call?: Call;
}

export function createClient(
  transport: Transport,
  options: ClientOptions,
): Client {
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

    transport
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
    quietly(transport.send([{ kind: "cancel", request: call.id }], new Map()));
  }

  function abort(entry: Outbound, call: Call): void {
    const index = held.indexOf(entry);

    if (index >= 0) {
      held.splice(index, 1);
      call.settle(cancelled(call.id));
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
        if ("error" in result) {
          if (!(result.error instanceof SessionEndedError)) report(result.error);
          return;
        }

        const { reply } = result;

        if (reply.status === "ok" && "capabilities" in reply) {
          const theirs = new Set(reply.capabilities);
          shared = new Set(named.capabilities.filter((c) => theirs.has(c)));
          state = "ready";

          const outbound = held;
          held = [];
          dispatch(outbound);
        }

        guard(() => {
          options.session(reply);
        });
      },
    };

    transmit([{ message: { ...named, id, kind: "hello" }, call }], new Map());
  }

  transport.listen({
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
        call.settle({ reply: item, body: null });
      } else {
        body.then(
          (blob) => {
            call.settle({ reply: item, body: blob });
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

      return new Promise((resolve, reject) => {
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
            if ("error" in result) {
              reject(result.error);
            } else {
              resolve(result);
            }
          },
        };

        const entry: Outbound = { message, call };

        if (signal?.aborted === true) {
          call.settle(cancelled(id));
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

function cancelled(id: string): { reply: Reply; body: null } {
  return { reply: { id, status: "cancelled" }, body: null };
}

// Calls a caller's function, reporting what it throws without stopping the
// client, which goes on with the rest of R's message.
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
