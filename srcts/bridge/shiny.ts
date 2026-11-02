// The client's side of the Shiny bridge, the channel that core's transport runs
// over on the page, whose other half is R/shiny-bridge.R. It sends each message
// as its JSON text with Shiny.setInputValue("blockr", ...), at the priority
// "event", so that Shiny sends every message, and hears R's through
// Shiny.addCustomMessageHandler("blockr", ...). Shiny only carries the
// messages, so leaving it later replaces these two files and nothing else.
//
// R starts each session with a message of the bridge's own, an object rather
// than an array, which gives the URL that the client posts request bodies to,
// so that the bridge has to listen before Shiny connects, as core does when the
// page starts. A session ends when Shiny's websocket closes, which Shiny's
// shiny:disconnected event on the document says.
//
// R carries a reply's body in its message, as base64 in the body's `data`, or
// in a binary frame of its own, which holds the length of the JSON of a message
// of the reply alone in four bytes, little-endian, that JSON and then the
// body's bytes, or from the URL in the body's `url`, which the client fetches,
// as R's blockr options `max_message_body` and `message_body_encoding` say. The
// bridge hands the transport the reply without the fields that carry the body.

import type {
  Body,
  ClientMessage,
  Progress,
  Reply,
} from "../protocol/messages.ts";
import type { Channel } from "../protocol/transport.ts";

// What the bridge uses of Shiny's client.
export interface ShinyClient {
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

export interface ShinyChannelOptions {
  // Shiny's client, window.Shiny by default.
  shiny?: ShinyClient;
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

export function shinyChannel(options: ShinyChannelOptions = {}): Channel {
  const page = window as Window & { Shiny?: ShinyClient; jQuery?: JQueryLike };

  const shiny = options.shiny ?? page.Shiny;
  if (shiny === undefined) {
    throw new Error("The Shiny bridge needs Shiny's client, window.Shiny.");
  }

  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);

  // The URL that R gives the session for request bodies, and a count of the
  // sessions started and ended, so that a message whose body outlived its
  // session goes nowhere.
  let upload: string | null = null;
  let session = 0;

  return {
    async send(message: ClientMessage, bodies: ReadonlyMap<string, Blob>) {
      const at = session;

      for (const [id, body] of bodies) {
        await post(fetcher, upload, id, body);
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
            // An item that is no object R never sends, which the bridge skips.
            if (typeof item !== "object" || item === null) continue;
            const [carried, body] = received(item as Reply | Progress, fetcher);
            listener.received(carried, body);
          }
        } else if (isSessionStart(message)) {
          session += 1;
          upload = message.upload;
          listener.started();
        }
      });

      onDisconnected(() => {
        session += 1;
        upload = null;
        listener.ended();
      });
    },
  };
}

function disconnected(
  jquery: JQueryLike | undefined,
): (fn: () => void) => void {
  if (jquery === undefined) {
    throw new Error("The Shiny bridge needs the jQuery that Shiny puts on the page.");
  }

  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}

function isSessionStart(message: unknown): message is { upload: string } {
  return (
    typeof message === "object" &&
    message !== null &&
    typeof (message as { upload?: unknown }).upload === "string"
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

  return blob([await response.blob()], body);
}

async function post(
  fetcher: typeof fetch,
  upload: string | null,
  id: string,
  body: Blob,
): Promise<void> {
  if (upload === null) {
    throw new Error("No session runs that a body could be posted to.");
  }

  const url = `${upload}${upload.includes("?") ? "&" : "?"}id=${encodeURIComponent(id)}`;

  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type,
    },
  });

  if (!response.ok) {
    throw new Error(
      `R refused the body of request ${id}: ${await response.text()}`,
    );
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
