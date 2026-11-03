// R, played over the protocol's own client and a transport that keeps what
// the client sends, for the tests of what sends through the client: the
// scheduler and what builds on it.

import { createClient } from "../protocol/client.ts";
import type { Transport, TransportListener } from "../protocol/client.ts";
import type {
  Capability,
  EvaluateReply,
  EvaluateRequest,
  Progress,
  Reply,
  Request,
} from "../protocol/messages.ts";
import type { BlockId } from "../types/board.ts";
import { hello } from "./store.ts";

export function playR() {
  const sent: Request[] = [];
  const replied = new Set<string>();
  // The kinds of request whose messages the transport cannot send.
  const refuse = new Set<Request["kind"]>();
  let listener: TransportListener | undefined;

  const hears = (): TransportListener => {
    if (listener === undefined) throw new Error("Nothing listens yet.");
    return listener;
  };

  const transport: Transport = {
    send(message) {
      if (message.some((request) => refuse.has(request.kind))) {
        return Promise.reject(new Error("The transport is closed."));
      }
      sent.push(...message);
      return Promise.resolve();
    },
    listen(l) {
      listener = l;
    },
  };

  const client = createClient(transport, {
    hello: () => ({ protocol: "1.0", capabilities: ["cancel", "interrupt", "progress"] }),
    session: () => undefined,
  });

  // Replies as R does, with the binary body that the reply announces, if any.
  function reply(item: Reply, body?: Blob): void {
    replied.add(item.id);
    hears().received(item, body === undefined ? null : Promise.resolve(body));
  }

  function evaluations(): EvaluateRequest[] {
    return sent.filter((request): request is EvaluateRequest => request.kind === "evaluate");
  }

  return {
    client,
    sent,
    refuse,
    reply,
    evaluations,
    // The evaluations that R has not replied to.
    pending(): EvaluateRequest[] {
      return evaluations().filter((request) => !replied.has(request.id));
    },
    // Starts a session and answers its hello, naming R's capabilities.
    connect(capabilities: Capability[] = ["cancel", "progress"]): void {
      hears().started();
      const request = sent.filter((item) => item.kind === "hello").at(-1);
      if (request === undefined) throw new Error("The client sent no hello.");
      reply({ ...hello, id: request.id, capabilities });
    },
    end(): void {
      hears().ended();
    },
    // Replies to an evaluation as R does.
    evaluated(
      request: EvaluateRequest,
      status: EvaluateReply["status"] = "ok",
      fields: Partial<EvaluateReply> = {},
    ): void {
      reply({
        id: request.id,
        status,
        block: request.block,
        revision: request.revision,
        summary: status === "ok" ? "<data.frame> [150 × 5]" : null,
        conditions: [],
        omitted: { warning: 0, message: 0 },
        stdout: "",
        stderr: "",
        ...fields,
      });
    },
    // Sends a progress note for a request, as R does while it runs.
    note(id: string, progress: Progress["progress"]): void {
      hears().received({ id, progress }, null);
    },
    cancels(): string[] {
      return sent.flatMap((item) => (item.kind === "cancel" ? [item.request] : []));
    },
    forgets(): BlockId[][] {
      return sent.flatMap((item) => (item.kind === "forget" ? [item.blocks] : []));
    },
    // The requests of a kind, an output's or another request's, in order.
    asked(kind: "output" | "request"): Request[] {
      return sent.filter((item) => item.kind === kind);
    },
  };
}

export type PlayedR = ReturnType<typeof playR>;
