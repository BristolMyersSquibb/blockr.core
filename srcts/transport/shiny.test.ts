import { afterEach, describe, expect, test, vi } from "vitest";
import type {
  EvaluateRequest,
  Progress,
  Reply,
  TransportListener,
} from "../protocol/index.ts";
import { shinyTransport } from "./shiny.ts";
import type { ShinyApi } from "./shiny.ts";

const bodyUrl = "session/0a1b/dataobj/blockr-bodies?w=&nonce=c2d3";

interface Posted {
  url: string;
  init: RequestInit | undefined;
}

// Shiny's JS API, R's half of the transport and the browser's fetch, as a test
// plays them.
function page(respond: (url: string, init?: RequestInit) => Promise<Response> = () =>
  Promise.resolve(new Response(""))) {
  const inputs: { name: string; value: string; options: unknown }[] = [];
  const handlers = new Map<string, (message: unknown) => void>();
  const fetched: Posted[] = [];
  let disconnect: () => void = () => undefined;

  const shiny: ShinyApi = {
    setInputValue(name, value, options) {
      inputs.push({ name, value, options });
    },
    addCustomMessageHandler(type, handler) {
      handlers.set(type, handler);
    },
  };

  const fetch = vi.fn((input: string | URL | Request, init?: RequestInit) => {
    const url =
      typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
    fetched.push({ url, init });
    return respond(url, init);
  });

  const transport = shinyTransport({
    shiny,
    fetch,
    onDisconnected: (fn) => {
      disconnect = fn;
    },
  });

  const received: [Reply | Progress, Promise<Blob> | null][] = [];
  const started = vi.fn();
  const ended = vi.fn();
  const listener: TransportListener = {
    started,
    ended,
    received: (item, body) => received.push([item, body]),
  };

  transport.listen(listener);

  return {
    transport,
    inputs,
    fetched,
    received,
    started,
    ended,
    // The body that came with the i-th item R sent.
    body(i: number): Promise<Blob> {
      const body = received[i]?.[1];
      if (body === undefined || body === null) throw new Error("There is no body.");
      return body;
    },
    // Delivers a message from R, as Shiny hands it to the handler.
    deliver(message: unknown) {
      const handler = handlers.get("blockr");
      if (handler === undefined) throw new Error("No handler for blockr.");
      handler(message);
    },
    disconnect() {
      disconnect();
    },
  };
}

async function text(blob: Blob | null | undefined): Promise<string> {
  if (blob === null || blob === undefined) throw new Error("There is no body.");
  return new TextDecoder().decode(await blob.arrayBuffer());
}

const evaluate: EvaluateRequest = {
  id: "17",
  kind: "evaluate",
  block: "a",
  revision: 2,
  recipe: { registry_id: "blockr.core-dataset_block", state: {}, inputs: [] },
};

describe("the Shiny transport", () => {
  test("sends a message as its JSON text, at the priority event", async () => {
    const p = page();
    p.deliver({ body_url: bodyUrl });

    await p.transport.send([evaluate], new Map());

    expect(p.inputs).toEqual([
      { name: "blockr", value: JSON.stringify([evaluate]), options: { priority: "event" } },
    ]);
  });

  test("starts a session as R says and ends it when Shiny's websocket closes", () => {
    const p = page();

    p.deliver({ body_url: bodyUrl });
    expect(p.started).toHaveBeenCalledTimes(1);

    p.disconnect();
    expect(p.ended).toHaveBeenCalledTimes(1);
  });

  test("ignores what is neither a message nor the start of a session", () => {
    const p = page();

    p.deliver("[]");
    p.deliver({ blockr: [] });
    p.deliver(null);

    expect(p.started).not.toHaveBeenCalled();
    expect(p.received).toEqual([]);
  });

  test("hands on R's replies and notes in order, without the properties that carry a body", async () => {
    const p = page();

    p.deliver([
      { id: "28", progress: { value: 0.5 } },
      { id: "28", status: "ok", body: { type: "image/png", data: btoa("\u0089PNG") } },
      { id: "29", status: "ok", body: { type: "text/csv", name: "a.csv", data: btoa("a,b") } },
      { id: "30", status: "ok", value: [1] },
    ]);

    expect(p.received.map(([item]) => item)).toEqual([
      { id: "28", progress: { value: 0.5 } },
      { id: "28", status: "ok", body: { type: "image/png" } },
      { id: "29", status: "ok", body: { type: "text/csv", name: "a.csv" } },
      { id: "30", status: "ok", value: [1] },
    ]);

    const png = await p.body(1);
    const csv = await p.body(2);

    expect(png.type).toBe("image/png");
    expect(png).not.toBeInstanceOf(File);
    expect([...new Uint8Array(await png.arrayBuffer())]).toEqual([0x89, 0x50, 0x4e, 0x47]);

    expect(csv).toBeInstanceOf(File);
    expect((csv as File).name).toBe("a.csv");
    expect(csv.type).toBe("text/csv");
    expect(await text(csv)).toBe("a,b");

    expect(p.received[0]?.[1]).toBeNull();
    expect(p.received[3]?.[1]).toBeNull();
  });

  test("hands on a reply whose body is null as one without, and skips what is no object", async () => {
    const p = page();

    p.deliver([1, null, { id: "34", status: "ok", body: null }, { id: "35", status: "ok", body: { type: "image/png", data: "%%" } }]);

    expect(p.received.map(([item]) => item)).toEqual([
      { id: "34", status: "ok", body: null },
      { id: "35", status: "ok", body: { type: "image/png" } },
    ]);
    expect(p.received[0]?.[1]).toBeNull();
    await expect(p.body(1)).rejects.toThrow();
  });

  test("takes a body that R sends in a binary frame beside its reply", async () => {
    const p = page();

    const frame = (items: unknown[], bytes: number[]): ArrayBuffer => {
      const json = new TextEncoder().encode(JSON.stringify(items));
      const out = new Uint8Array(4 + json.length + bytes.length);
      new DataView(out.buffer).setUint32(0, json.length, true);
      out.set(json, 4);
      out.set(bytes, 4 + json.length);
      return out.buffer;
    };

    p.deliver(frame([{ id: "36", status: "ok", body: { type: "image/png" } }], [0x89, 0x50]));
    p.deliver(
      frame([{ id: "37", status: "ok", body: { type: "text/csv", name: "a.csv" } }], [0x61]),
    );

    expect(p.received.map(([item]) => item)).toEqual([
      { id: "36", status: "ok", body: { type: "image/png" } },
      { id: "37", status: "ok", body: { type: "text/csv", name: "a.csv" } },
    ]);

    const png = await p.body(0);
    expect(png.type).toBe("image/png");
    expect(png).not.toBeInstanceOf(File);
    expect([...new Uint8Array(await png.arrayBuffer())]).toEqual([0x89, 0x50]);

    const csv = await p.body(1);
    expect((csv as File).name).toBe("a.csv");
    expect(await text(csv)).toBe("a");
  });

  test("fetches a body that R serves from a URL", async () => {
    const url = "session/0a1b/dataobj/blockr-body-31?w=&nonce=e4f5";
    const p = page(() => Promise.resolve(new Response("pptx bytes")));

    p.deliver([
      {
        id: "31",
        status: "ok",
        body: { type: "application/vnd.ms-powerpoint", name: "r.pptx", url },
      },
    ]);

    expect(p.received[0]?.[0]).toEqual({
      id: "31",
      status: "ok",
      body: { type: "application/vnd.ms-powerpoint", name: "r.pptx" },
    });

    const file = await p.body(0);
    // Once the client has read the body, it deletes it, for R to drop it.
    expect(p.fetched.map((f) => [f.url, f.init?.method ?? "GET"])).toEqual([
      [url, "GET"],
      [url, "DELETE"],
    ]);
    expect(file).toBeInstanceOf(File);
    expect(file.type).toBe("application/vnd.ms-powerpoint");
    expect(await text(file)).toBe("pptx bytes");
  });

  test("fails a body that R does not serve, or sent no bytes of", async () => {
    const p = page(() =>
      Promise.resolve(new Response("The body was fetched already.", { status: 404 })),
    );

    p.deliver([
      { id: "32", status: "ok", body: { type: "image/png", url: "session/x/dataobj/y" } },
      { id: "33", status: "ok", body: { type: "image/png" } },
    ]);

    await expect(p.body(0)).rejects.toThrow("The body was fetched already.");
    // Nothing is deleted that never came.
    expect(p.fetched.map((f) => f.init?.method ?? "GET")).toEqual(["GET"]);
    await expect(p.body(1)).rejects.toThrow("R sent no bytes of the body of its reply to 33.");
  });

  test("posts a request's body to the URL R gave the session before it sends the request", async () => {
    const p = page();
    p.deliver({ body_url: bodyUrl });
    const csv = new File(["a,b\n"], "s.csv", { type: "text/csv" });
    const upl = { ...evaluate, id: "25", body: { type: "text/csv", name: "s.csv" } };

    await p.transport.send([upl], new Map([["25", csv]]));

    expect(p.fetched).toHaveLength(1);
    expect(p.fetched[0]?.url).toBe(`${bodyUrl}&id=25`);
    expect(p.fetched[0]?.init).toMatchObject({
      method: "POST",
      body: csv,
      headers: { "Content-Type": "text/csv" },
    });
    expect(p.inputs.map((i) => JSON.parse(i.value) as unknown)).toEqual([[upl]]);
  });

  test("sends nothing where R refused the body", async () => {
    const p = page(() =>
      Promise.resolve(new Response("Maximum upload size exceeded", { status: 413 })),
    );
    p.deliver({ body_url: bodyUrl });

    await expect(
      p.transport.send([{ ...evaluate, id: "26" }], new Map([["26", new Blob(["x"])]])),
    ).rejects.toThrow("R refused the body of request 26: Maximum upload size exceeded");
    expect(p.inputs).toEqual([]);
  });

  test("refuses a body larger than Shiny takes, posting nothing", async () => {
    const p = page();
    p.deliver({ body_url: bodyUrl, max_request_size: 2 });

    await expect(
      p.transport.send([{ ...evaluate, id: "41" }], new Map([["41", new Blob(["xyz"])]])),
    ).rejects.toThrow("The body of request 41, of 3 bytes, is larger than the 2 bytes that R takes.");
    expect(p.fetched).toEqual([]);
    expect(p.inputs).toEqual([]);

    await p.transport.send([{ ...evaluate, id: "42" }], new Map([["42", new Blob(["xy"])]]));
    expect(p.fetched).toHaveLength(1);
  });

  test("sends nothing whose session ended while its body went", async () => {
    let respond: (response: Response) => void = () => undefined;
    const p = page(
      () =>
        new Promise((resolve) => {
          respond = resolve;
        }),
    );
    p.deliver({ body_url: bodyUrl });

    const sent = p.transport.send([{ ...evaluate, id: "27" }], new Map([["27", new Blob(["x"])]]));
    p.disconnect();
    respond(new Response(""));

    await expect(sent).rejects.toThrow("The session ended before its message was sent.");
    expect(p.inputs).toEqual([]);
  });

  test("posts nothing while no session runs", async () => {
    const p = page();

    await expect(
      p.transport.send([{ ...evaluate, id: "28" }], new Map([["28", new Blob(["x"])]])),
    ).rejects.toThrow("No session runs that a body could be posted to.");
    expect(p.fetched).toEqual([]);
  });
});

describe("the Shiny transport on the page", () => {
  const page = window as Window & { Shiny?: unknown; jQuery?: unknown };

  afterEach(() => {
    delete page.Shiny;
    delete page.jQuery;
  });

  test("uses window.Shiny and the jQuery that Shiny puts on the page", () => {
    const handlers = new Map<string, () => void>();
    const addCustomMessageHandler = vi.fn();
    const shiny: ShinyApi = { setInputValue: vi.fn(), addCustomMessageHandler };

    page.Shiny = shiny;
    page.jQuery = (element: Document) => {
      expect(element).toBe(document);
      return {
        on: (event: string, handler: () => void) => handlers.set(event, handler),
      };
    };

    const ended = vi.fn();
    shinyTransport().listen({ started: vi.fn(), ended, received: vi.fn() });

    expect(addCustomMessageHandler).toHaveBeenCalledWith("blockr", expect.any(Function));

    handlers.get("shiny:disconnected")?.();
    expect(ended).toHaveBeenCalledTimes(1);
  });

  test("needs Shiny and its jQuery", () => {
    expect(() => shinyTransport()).toThrow("window.Shiny");

    page.Shiny = { setInputValue: vi.fn(), addCustomMessageHandler: vi.fn() };
    expect(() => shinyTransport()).toThrow("jQuery");
  });
});
