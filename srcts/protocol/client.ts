// The client of the wire protocol, through which core's store, scheduler and
// CoreApi send their requests to R, as inst/protocol/README.md gives the
// protocol. It gives each request its id, sends nothing but a session's hello
// until R has answered it, matches R's replies and progress notes to the
// requests waiting on them, and ignores a reply to a request it is not waiting
// on. It runs over a transport, which carries the messages: the Shiny
// transport on the page (srcts/transport/shiny.ts).
// Where R answers a hello at another major of the protocol, in the first
// session or a later one, the client stops for good:
// 1. It sends R nothing of what it held for the session, and rejects each
//    request it held with why it stopped.
// 2. It hands R's reply to its session(), so that core says what to do.
// 3. It sends R nothing from then on, not even the hello of a later session,
//    and rejects each request that it is asked to send.
//
// The client records a span of each evaluate, request and output that it is
// asked to send, as Tracing has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing),
// from the call of request() until the request settles, as with R's reply.
// The span goes under the span that is active as request() is called, unless
// the caller names another parent, as the scheduler names none for an
// evaluation, which links to the span of the edit that caused it instead.
// While the span records, the request carries its context in `traceparent`,
// so that R's span of the request is a child of it.
// The span holds the block's id, its revision and its class's registry id,
// where the request names them, and a request's type and name, with what it
// belongs to, never a block's state or data. It holds R's status of the reply
// once R replies, and is marked failed where R replies error or no reply
// comes. The client records no span of a hello, nor of the other kinds, of
// which R records none.

import { context, SpanKind, SpanStatusCode } from "@opentelemetry/api";
import type { Attributes, Context, Link, Span } from "@opentelemetry/api";
import type { BlockId } from "../types/board.ts";
import { log } from "../log/index.ts";
import { failSpan, traceparent, tracer } from "../trace/index.ts";
import { asError } from "../util/index.ts";
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

// Omits the keys `K` from each type of a union, where `Omit` would keep only
// the keys that the types of the union share.
type DistributiveOmit<T, K extends PropertyKey> = T extends unknown ? Omit<T, K> : never;

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
  // What the request's span goes under, the span that is active as request()
  // is called unless given, the spans that it links to, and attributes of the
  // span beside those that the client takes from the request.
  span?: RequestSpan;
}

export interface RequestSpan {
  readonly parent?: Context;
  readonly links?: readonly Link[];
  readonly attributes?: Attributes;
}

// A request as a caller gives it to request(), without the id, the body's
// announcement and the traceparent, which the client gives it.
export type Asked = DistributiveOmit<
  Exclude<Request, HelloRequest | CancelRequest | ForgetRequest>,
  "id" | "body" | "traceparent"
>;

export interface Client {
  // Sends a request, which the client gives its id and the announcement of its
  // body, and resolves with R's reply to it, whatever its status, and the
  // binary body that the reply announces, a File where the reply names one, or
  // null. The client sends each session's hello itself, and a request's cancel
  // when its signal aborts. Rejects with a SessionEndedError where the session
  // that the request went to ended before R replied, with the transport's
  // error where the transport could not send it, such as where R refused its
  // body, and with why the client stopped where R speaks another major of the
  // protocol, as for a request that the client held.
  request(request: Asked, options?: RequestOptions): Promise<{ reply: Reply; body: Blob | null }>;
  // Has R forget blocks that left the board, which gets no reply.
  forget(blocks: BlockId[]): void;
  // The capabilities that both sides named in the hello of the current
  // session. It holds none while no session is ready.
  capabilities(): ReadonlySet<Capability>;
  // Sends another hello in the session, which R takes as a fresh start of the
  // session, as core starts again for a board that opens:
  // 1. The client gives up every request that waits on R or that it holds,
  //    each of which rejects with a SessionEndedError, since it may or may not
  //    have run.
  // 2. It sends the hello once every message whose body it is sending has
  //    gone, so that R hears such a message before the hello, which drops it.
  // 3. It holds what it is asked to send until R has answered the hello, and
  //    hands the reply to its session(), as for a session that starts.
  // Where no session runs, the client sends the next session's hello as that
  // session starts, as always. Once the client has stopped, it sends none.
  hello(): void;
}

export interface ClientOptions {
  // What to name in each session's hello: the protocol version and the
  // capabilities that the client speaks.
  hello(): Omit<HelloRequest, "id" | "kind">;
  // Hears R's reply to each session's hello, the first session's included,
  // once the client has sent what it held for the session. Where R speaks
  // another major of the protocol, it hears the reply once the client has
  // stopped instead.
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
  // The id of the session that runs, by which telemetry names it, or undefined
  // while none runs or where the transport names none.
  sessionId?(): string | undefined;
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

// Whether two versions of the protocol have the same major, so that each side
// reads the other's messages.
export function sameMajor(a: string, b: string): boolean {
  return a.split(".")[0] === b.split(".")[0];
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
  // Why the client sends R nothing, once R has answered a hello at another
  // major of the protocol.
  let stopped: Error | undefined;
  // The capabilities that both sides named in the session's hello.
  let shared = new Set<Capability>();

  const waiting = new Map<string, Call>();
  let held: Outbound[] = [];
  // The messages with bodies that the transport is sending, and a count of
  // the sessions that ended, so that a hello waiting for those messages goes
  // only where its session is still the one that runs.
  const sending = new Set<Promise<void>>();
  let ends = 0;

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

    const sent = transport
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

    if (bodies.size > 0) {
      sending.add(sent);
      void sent.finally(() => sending.delete(sent));
    }
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
    if (stopped !== undefined) {
      entry.call?.settle({ error: stopped });
    } else if (state === "ready") {
      dispatch([entry]);
    } else {
      held.push(entry);
    }
  }

  function end(): void {
    state = "none";
    shared = new Set();
    ends += 1;

    const lost = [...waiting.values()];
    waiting.clear();

    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }

  // Stops the client for good, where R speaks another major of the protocol
  // than the client: it ends the session for the client, and rejects what it
  // held for the session, none of which it sent.
  function stop(theirs: string, ours: string): void {
    stopped = new Error(
      `R speaks version ${theirs} of the wire protocol, where the client speaks ${ours}, so the client sends R nothing.`,
    );
    end();
    const lost = held;
    held = [];
    for (const { call } of lost) {
      call?.settle({ error: stopped });
    }
  }

  function sendHello(): void {
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
          if (!(result.error instanceof SessionEndedError)) log.error(asError(result.error));
          return;
        }

        const { reply } = result;

        if (reply.status === "ok" && "capabilities" in reply) {
          if (sameMajor(reply.protocol, named.protocol)) {
            const theirs = new Set(reply.capabilities);
            shared = new Set(named.capabilities.filter((c) => theirs.has(c)));
            state = "ready";

            const outbound = held;
            held = [];
            dispatch(outbound);
          } else {
            stop(reply.protocol, named.protocol);
          }
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
      if (stopped !== undefined) return;
      end();
      state = "started";
      sendHello();
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
    request(request, { body, progress, signal, span: spanned } = {}) {
      const id = mint();
      const span = requestSpan(request, spanned);
      const parent = span === undefined ? undefined : traceparent(span);
      const message = {
        ...request,
        id,
        ...(parent === undefined ? {} : { traceparent: parent }),
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
            if (span !== undefined) endRequestSpan(span, result);
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
    capabilities: () => shared,
    hello() {
      if (stopped !== undefined) return;
      const runs = state !== "none";
      const lost = held;
      held = [];
      end();
      for (const { call } of lost) {
        call?.settle({ error: new SessionEndedError(call.id) });
      }
      if (!runs) return;

      state = "started";
      if (sending.size === 0) {
        sendHello();
        return;
      }
      const at = ends;
      void Promise.allSettled([...sending]).then(() => {
        if (ends === at) sendHello();
      });
    },
  };
}

// The span of a request that the client is asked to send, as it starts, or
// undefined for a kind of which R records no span.
function requestSpan(request: Asked, given: RequestSpan = {}): Span | undefined {
  const named = spanName(request);
  if (named === undefined) return undefined;
  return tracer().startSpan(
    named,
    {
      kind: SpanKind.CLIENT,
      attributes: { ...requestAttributes(request), ...given.attributes },
      links: [...(given.links ?? [])],
    },
    given.parent ?? context.active(),
  );
}

// The name of a request's span, as R names its own span of the request:
// `evaluate`, `output`, or `request` with the request's name, as
// "request columns", or undefined for a kind of which R records none.
function spanName(request: Pick<Asked, "kind"> & { readonly name?: string }): string | undefined {
  switch (request.kind) {
    case "evaluate":
    case "output":
      return request.kind;
    case "request":
      return `request ${request.name ?? ""}`;
    default:
      return undefined;
  }
}

// The attributes of a request's span that the request gives: the block's id,
// its revision and its class's registry id where the request names them, and
// a request's type, its name and what it belongs to.
function requestAttributes(request: Asked): Attributes {
  switch (request.kind) {
    case "evaluate":
      return {
        "blockr.block.id": request.block,
        "blockr.block.revision": request.revision,
        "blockr.block.registry_id": request.recipe.registry_id,
      };
    case "output":
      return { "blockr.block.id": request.block, "blockr.block.revision": request.revision };
    case "request": {
      const named = { "blockr.request.type": request.type, "blockr.request.name": request.name };
      switch (request.type) {
        case "block_class":
          return { ...named, "blockr.block.id": request.block, "blockr.block.registry_id": request.registry_id };
        case "option":
          return { ...named, "blockr.option.id": request.option };
        case "plugin":
          return { ...named, "blockr.plugin.class_id": request.plugin };
        case "output_class":
          return { ...named, "blockr.block.id": request.output.block, "blockr.block.revision": request.output.revision };
        case "renderer":
          return {
            ...named,
            "blockr.block.id": request.output.block,
            "blockr.block.revision": request.output.revision,
            "blockr.renderer.id": request.renderer,
          };
      }
      break;
    }
    default:
      break;
  }
  return {};
}

// Ends a request's span as the request settles: with R's status of the reply,
// marked failed where R replied error, or where no reply came.
function endRequestSpan(span: Span, result: { reply: Reply; body: Blob | null } | { error: Error }): void {
  if ("error" in result) {
    failSpan(span, result.error);
  } else {
    span.setAttribute("blockr.reply.status", result.reply.status);
    if (result.reply.status === "error") {
      const conditions = "conditions" in result.reply ? result.reply.conditions : [];
      const message = conditions.find((condition) => condition.severity === "error")?.message;
      span.setStatus({ code: SpanStatusCode.ERROR, ...(message === undefined ? {} : { message }) });
    }
  }
  span.end();
}

// A body's announcement, by its MIME type and, for a File, its name.
function announce(body: Blob): { type: string; name?: string } {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}

function cancelled(id: string): { reply: Reply; body: null } {
  return { reply: { id, status: "cancelled" }, body: null };
}

// Calls a caller's function, logging what it throws without stopping the
// client, which goes on with the rest of R's message.
function guard(fn: () => void): void {
  try {
    fn();
  } catch (error: unknown) {
    log.error(asError(error));
  }
}

// Sends a message that nothing waits on, such as a cancel, which goes nowhere
// once its session has ended.
function quietly(sent: Promise<void>): void {
  sent.catch(() => undefined);
}
