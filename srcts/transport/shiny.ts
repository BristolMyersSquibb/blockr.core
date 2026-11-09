// The client's half of the Shiny transport, which carries the messages of the
// wire protocol over a Shiny session for the client of the protocol
// (srcts/protocol/client.ts), and whose other half is R/shiny-transport.R. It
// sends each message as its JSON text with Shiny.setInputValue("blockr", ...),
// at the priority "event", so that Shiny sends every message, and hears R's
// through Shiny.addCustomMessageHandler("blockr", ...). Shiny only carries the
// messages, so leaving it later replaces these two files and nothing else.
//
// R starts each session with a message of its own, an object rather than an
// array, which gives the URL that the client posts request bodies to, in
// `body_url`, and the largest body that Shiny takes there, in
// `max_request_size`, so that the transport has to listen before Shiny
// connects, as core does when the page starts. A session ends when Shiny's websocket closes, which Shiny's
// shiny:disconnected event on the document says.
//
// R carries a reply's body in its message, as base64 in the body's `data`, or
// in a binary frame of its own, which holds the length of the JSON of a message
// of the reply alone in four bytes, little-endian, that JSON and then the
// body's bytes, or from the URL in the body's `url`, which the client fetches,
// as R's blockr options `max_message_body` and `message_body_encoding` say,
// and which the client deletes once it has read the body. The transport hands
// the client the reply without the fields that carry the body.

import type {
  Body,
  ClientMessage,
  Progress,
  Reply,
  Transport,
} from "../protocol/index.ts";

// What the transport uses of Shiny's JS API.
export interface ShinyApi {
  setInputValue(
    name: string,
    value: string,
    options: { priority: "event" },
  ): void;
  addCustomMessageHandler(
    type: string,
    handler: (message: unknown) => void,
  ): void;
}

export interface ShinyTransportOptions {
  // Shiny's JS API, window.Shiny by default.
  shiny?: ShinyApi;
  // What fetches and posts bodies, window.fetch by default.
  fetch?: typeof fetch;
  // Calls fn each time Shiny's websocket closes, by default as Shiny's
  // shiny:disconnected event says, through the jQuery that Shiny puts on the
  // page.
  onDisconnected?: (fn: () => void) => void;
}

type JQueryLike = (element: Document) => {
  on(event: string, handler: () => void): unknown;
};

// An item of R's message, a reply that announces a body with a promise of it.
type Received = [Reply | Progress, Promise<Blob> | null];

// The fields that R adds to a reply's body to carry its bytes.
interface Carried {
  data?: string;
  url?: string;
}

const name = "blockr";

export function shinyTransport(options: ShinyTransportOptions = {}): Transport {
  const page = window as Window & { Shiny?: ShinyApi; jQuery?: JQueryLike };

  const shiny = options.shiny ?? page.Shiny;
  if (shiny === undefined) {
    throw new Error("The Shiny transport needs Shiny's JS API, window.Shiny.");
  }

  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);

  // What R gives the session for request bodies, and a count of the sessions
  // started and ended, so that a message whose body outlived its session goes
  // nowhere.
  let start: SessionStart | null = null;
  let session = 0;

  return {
    async send(message: ClientMessage, bodies: ReadonlyMap<string, Blob>) {
      const at = session;

      for (const [id, body] of bodies) {
        await post(fetcher, start, id, body);
      }

      if (at !== session) {
        throw new Error("The session ended before its message was sent.");
      }

      shiny.setInputValue(name, JSON.stringify(message), {
        priority: "event",
      });
    },
    listen(listener) {
      shiny.addCustomMessageHandler(name, (message: unknown) => {
        if (message instanceof ArrayBuffer) {
          for (const [item, body] of framed(message)) {
            listener.received(item, body);
          }
        } else if (Array.isArray(message)) {
          for (const item of message as unknown[]) {
            // An item that is no object R never sends, which the transport
            // skips.
            if (typeof item !== "object" || item === null) continue;
            const [carried, body] = received(item as Reply | Progress, fetcher);
            listener.received(carried, body);
          }
        } else if (isSessionStart(message)) {
          session += 1;
          start = message;
          listener.started();
        }
      });

      onDisconnected(() => {
        session += 1;
        start = null;
        listener.ended();
      });
    },
  };
}

function disconnected(
  jquery: JQueryLike | undefined,
): (fn: () => void) => void {
  if (jquery === undefined) {
    throw new Error("The Shiny transport needs the jQuery that Shiny puts on the page.");
  }

  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}

// R's message that starts a session, with the limit to a request's size where
// Shiny's option shiny.maxRequestSize sets one.
interface SessionStart {
  body_url: string;
  max_request_size?: number;
}

function isSessionStart(message: unknown): message is SessionStart {
  return (
    typeof message === "object" &&
    message !== null &&
    typeof (message as { body_url?: unknown }).body_url === "string"
  );
}

// The items of a message that R sent as a binary frame, the reply that
// announces a body with the frame's bytes.
function framed(frame: ArrayBuffer): Received[] {
  const length = new DataView(frame).getUint32(0, true);
  const json = new TextDecoder().decode(new Uint8Array(frame, 4, length));
  const bytes = frame.slice(4 + length);

  return (JSON.parse(json) as unknown[]).flatMap((item): Received[] => {
    if (typeof item !== "object" || item === null) return [];

    const announced: unknown =
      "status" in item && "body" in item ? item.body : null;
    if (typeof announced !== "object" || announced === null) {
      return [[item as Reply | Progress, null]];
    }

    const body = Promise.resolve(blob([bytes], announced as Body));
    return [[item as Reply, body]];
  });
}

// A reply without the fields that carry its body, with a promise of the body.
function received(item: Reply | Progress, fetcher: typeof fetch): Received {
  // A body that R writes as null, from a NULL in R, is none.
  const announced: unknown = "status" in item && "body" in item ? item.body : null;
  if (typeof announced !== "object" || announced === null) return [item, null];

  const { data, url, ...body } = announced as Body & Carried;
  const reply = { ...item, body } as Reply;

  let bytes: Promise<Blob>;

  if (data !== undefined) {
    // Decoded in the promise, so that a body that is no base64 fails its
    // reply alone.
    bytes = new Promise((resolve) => {
      resolve(blob([fromBase64(data)], body));
    });
  } else if (url !== undefined) {
    bytes = fetchBody(fetcher, url, body);
  } else {
    bytes = Promise.reject(
      new Error(`R sent no bytes of the body of its reply to ${reply.id}.`),
    );
  }

  // Marks a body as handled, so that one that no request waits on fails
  // quietly.
  bytes.catch(() => undefined);

  return [reply, bytes];
}

async function fetchBody(
  fetcher: typeof fetch,
  url: string,
  body: Body,
): Promise<Blob> {
  const response = await fetcher(url);

  if (!response.ok) {
    throw new Error(
      `R served no body at ${url}: ${String(response.status)} ${await response.text()}`,
    );
  }

  const bytes = await response.blob();

  // Lets R drop the body, which it serves until then, so that a request for it
  // that the browser sends again after a broken connection is still answered.
  // Its response is read, as every response is, so that the browser cancels
  // none.
  fetcher(url, { method: "DELETE" })
    .then((response) => response.text())
    .catch(() => undefined);

  return blob([bytes], body);
}

async function post(
  fetcher: typeof fetch,
  start: SessionStart | null,
  id: string,
  body: Blob,
): Promise<void> {
  if (start === null) {
    throw new Error("No session runs that a body could be posted to.");
  }

  // As Shiny refuses it, by its size, which the browser may not hear of if
  // Shiny ends the connection while the browser still sends the body.
  const max = start.max_request_size;
  if (max !== undefined && body.size > max) {
    throw new Error(
      `The body of request ${id}, of ${String(body.size)} bytes, is larger than the ${String(max)} bytes that R takes.`,
    );
  }

  const url = `${start.body_url}${start.body_url.includes("?") ? "&" : "?"}id=${encodeURIComponent(id)}`;

  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type,
    },
  });

  const text = await response.text();

  if (!response.ok) {
    throw new Error(`R refused the body of request ${id}: ${text}`);
  }
}

// A body's bytes typed as R announced them, a File where R names one.
function blob(parts: BlobPart[], body: Body): Blob {
  return body.name === undefined
    ? new Blob(parts, { type: body.type })
    : new File(parts, body.name, { type: body.type });
}

function fromBase64(data: string): Uint8Array<ArrayBuffer> {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);

  for (let i = 0; i < text.length; i += 1) {
    bytes[i] = text.charCodeAt(i);
  }

  return bytes;
}
