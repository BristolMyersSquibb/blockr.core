// @vitest-environment node

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, test, vi } from "vitest";
import type {
  ClientMessage,
  Fixture,
  HelloRequest,
  Progress,
  Reply,
  Request,
} from "./messages.ts";
import { createClient, SessionEndedError } from "./client.ts";
import type {
  Client,
  ClientOptions,
  Transport,
  TransportListener,
} from "./client.ts";

const fixtures = join(
  resolve(dirname(fileURLToPath(import.meta.url)), "../.."),
  "inst",
  "protocol",
  "fixtures",
);

function fixture(name: string): Fixture {
  return JSON.parse(
    readFileSync(join(fixtures, `${name}.json`), "utf8"),
  ) as Fixture;
}

// R's reply to the hello of the hello fixture, which every test but those of
// the handshake starts its session with.
const helloReply = fixture("hello").r[0]?.[0] as Reply;

interface Sent {
  message: ClientMessage;
  bodies: Map<string, Blob>;
}

// A transport that keeps what the client sends and plays R's part as a test
// says, refusing the bodies whose ids it is told to.
function transport() {
  const sent: Sent[] = [];
  const refuse = new Set<string>();
  let listener: TransportListener | undefined;

  const hears = (): TransportListener => {
    if (listener === undefined) throw new Error("Nothing listens yet.");
    return listener;
  };

  const fake: Transport = {
    send(message, bodies) {
      for (const id of bodies.keys()) {
        if (refuse.has(id)) {
          return Promise.reject(new Error("Maximum upload size exceeded"));
        }
      }
      sent.push({ message, bodies: new Map(bodies) });
      return Promise.resolve();
    },
    listen(l) {
      listener = l;
    },
  };

  return {
    fake,
    sent,
    refuse,
    start() {
      hears().started();
    },
    end() {
      hears().ended();
    },
    receive(item: Reply | Progress, body: Promise<Blob> | null = null) {
      hears().received(item, body);
    },
    // The messages sent, without their bodies.
    messages(): ClientMessage[] {
      return sent.map((s) => s.message);
    },
  };
}

function setup(
  hello: ReturnType<ClientOptions["hello"]> = {
    protocol: "1.0",
    capabilities: ["cancel", "progress"],
  },
) {
  const tr = transport();
  const sessions: Reply[] = [];
  const hellos = vi.fn(() => hello);
  const options: ClientOptions = {
    hello: hellos,
    session: (reply) => sessions.push(reply),
  };
  const client = createClient(tr.fake, options);

  return { ...tr, client, sessions, hellos };
}

// The id of the last request of a kind that the client sent.
function idOf(sent: Sent[], kind: Request["kind"]): string {
  const all = sent.flatMap((s) => s.message).filter((m) => m.kind === kind);
  const id = all.at(-1)?.id;
  if (id === undefined) throw new Error(`No ${kind} was sent.`);
  return id;
}

// Starts a session and answers its hello as R does.
function ready(t: ReturnType<typeof setup>, reply: Reply = helloReply): void {
  t.start();
  t.receive({ ...reply, id: idOf(t.sent, "hello") });
}

// A copy of an object without some of its fields.
function without(value: object, ...keys: string[]): Record<string, unknown> {
  return Object.fromEntries(
    Object.entries(value).filter(([key]) => !keys.includes(key)),
  );
}

function only<T>(items: T[]): T {
  const [item] = items;
  if (item === undefined) throw new Error("There is none.");
  return item;
}

// Lets the client's promise callbacks run.
async function settle(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}

const evaluate: Parameters<Client["request"]>[0] = {
  kind: "evaluate",
  block: "a",
  revision: 2,
  recipe: {
    registry_id: "blockr.core-dataset_block",
    state: { dataset: "iris", package: "datasets" },
    inputs: [],
  },
};

const echo: Parameters<Client["request"]>[0] = {
  kind: "request",
  type: "plugin",
  plugin: "blockr.test-echo_plugin",
  name: "echo",
  args: {},
  reads: [],
};

describe("the handshake", () => {
  test("sends a session's hello first and nothing else until R answers it", async () => {
    const t = setup();

    const pending = t.client.request(evaluate);
    t.client.forget(["z"]);
    expect(t.sent).toEqual([]);

    t.start();
    expect(t.messages()).toEqual([
      [
        {
          id: idOf(t.sent, "hello"),
          kind: "hello",
          protocol: "1.0",
          capabilities: ["cancel", "progress"],
        },
      ],
    ]);

    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    await settle();

    // What was held goes in one message, in the order it came.
    expect(t.messages()[1]).toEqual([
      { ...evaluate, id: idOf(t.sent, "evaluate") },
      { kind: "forget", blocks: ["z"] },
    ]);
    expect(t.sessions).toEqual([
      { ...helloReply, id: idOf(t.sent, "hello") },
    ]);

    t.receive({ ...okReply, id: idOf(t.sent, "evaluate") });
    expect((await pending).reply.status).toBe("ok");
  });

  test("holds everything while R has refused the hello", async () => {
    const t = setup();

    void t.client.request(evaluate);
    t.start();
    t.receive({
      id: idOf(t.sent, "hello"),
      status: "error",
      conditions: [],
    });
    await settle();

    expect(t.sent).toHaveLength(1);
    expect(t.sessions[0]?.status).toBe("error");
  });

  test("gives every request an id of its own", () => {
    const t = setup();
    ready(t);

    for (let i = 0; i < 3; i++) void t.client.request(echo);

    const ids = t.sent.flatMap((s) => s.message).map((m) => m.id);
    expect(new Set(ids).size).toBe(ids.length);
  });

  test("gives the capabilities that both sides named while a session is ready, and none otherwise", () => {
    const t = setup({ protocol: "1.0", capabilities: ["cancel", "interrupt", "progress"] });
    expect([...t.client.capabilities()]).toEqual([]);

    t.start();
    expect([...t.client.capabilities()]).toEqual([]);
    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    expect([...t.client.capabilities()]).toEqual(["cancel", "progress"]);

    t.end();
    expect([...t.client.capabilities()]).toEqual([]);
  });
});

const okReply = {
  id: "",
  status: "ok",
  block: "a",
  revision: 2,
  summary: "<data.frame> [150 × 5]",
  conditions: [],
  stdout: "",
  stderr: "",
} satisfies Reply;

describe("replies", () => {
  test("go to the requests they answer, in any order", async () => {
    const t = setup();
    ready(t);

    const a = t.client.request(echo);
    const b = t.client.request(evaluate);
    const [ida, idb] = t.sent.flatMap((s) => s.message).slice(1).map((m) => m.id);

    t.receive({ id: idb ?? "", status: "ok", value: ["b"] });
    t.receive({ id: ida ?? "", status: "ok", value: ["a"] });

    expect(await a).toEqual({ reply: { id: ida, status: "ok", value: ["a"] }, body: null });
    expect(await b).toEqual({ reply: { id: idb, status: "ok", value: ["b"] }, body: null });
  });

  test("that nothing waits on are ignored", async () => {
    const t = setup();
    ready(t);

    const a = t.client.request(echo);
    const id = idOf(t.sent, "request");

    t.receive({ id: "unknown", status: "ok", value: [] });
    t.receive({ id: "unknown", progress: { value: 0.5 } });
    t.receive({ id, status: "ok", value: [1] });
    // A second reply to an answered request.
    t.receive({ id, status: "ok", value: [2] });

    expect((await a).reply).toEqual({ id, status: "ok", value: [1] });
  });

  test("come with their bodies", async () => {
    const t = setup();
    ready(t);

    const a = t.client.request(echo);
    const b = t.client.request(echo);
    const [ida, idb] = t.sent.flatMap((s) => s.message).slice(1).map((m) => m.id);
    const png = new Blob(["png"], { type: "image/png" });

    t.receive(
      { id: ida ?? "", status: "ok", body: { type: "image/png" } },
      Promise.resolve(png),
    );
    t.receive(
      { id: idb ?? "", status: "ok", body: { type: "image/png" } },
      Promise.reject(new Error("R served no body")),
    );

    expect((await a).body).toBe(png);
    await expect(b).rejects.toThrow("R served no body");
  });

  test("hand progress notes to the request they belong to", async () => {
    const t = setup();
    ready(t);

    const notes: unknown[] = [];
    const a = t.client.request(echo, { progress: (note) => notes.push(note) });
    const id = idOf(t.sent, "request");

    t.receive({ id, progress: { value: 0.3, message: "Rendering" } });
    t.receive({ id, progress: { value: 0.8 } });
    t.receive({ id, status: "ok", value: [] });

    await a;
    expect(notes).toEqual([{ value: 0.3, message: "Rendering" }, { value: 0.8 }]);
  });
});

describe("bodies", () => {
  test("are announced by their type and a File's name, each request in a message of its own", () => {
    const t = setup();
    ready(t);

    const csv = new File(["a,b\n1,2\n"], "sales.csv", { type: "text/csv" });
    const bytes = new Blob(["?"]);

    void t.client.request(echo, { body: csv });
    void t.client.request(echo);
    void t.client.request(echo, { body: bytes });

    const [first, plain, second] = t.sent.slice(1);

    expect(plain?.message.map((m) => m.kind)).toEqual(["request"]);
    expect(plain?.bodies.size).toBe(0);

    const [withCsv] = first?.message ?? [];
    expect(withCsv).toMatchObject({ body: { type: "text/csv", name: "sales.csv" } });
    expect(first?.bodies).toEqual(new Map([[withCsv?.id, csv]]));

    const [withBytes] = second?.message ?? [];
    expect(withBytes).toMatchObject({ body: { type: "application/octet-stream" } });
    expect(withBytes).not.toHaveProperty("body.name");
  });

  test("that the transport cannot send fail their request alone", async () => {
    const t = setup();
    ready(t);
    // The next id the client mints is the upload's.
    t.refuse.add(String(Number(idOf(t.sent, "hello")) + 1));

    const upload = t.client.request(echo, { body: new Blob(["big"]) });
    const other = t.client.request(echo);
    const id = idOf(t.sent, "request");
    t.receive({ id, status: "ok", value: [] });

    await expect(upload).rejects.toThrow("Maximum upload size exceeded");
    expect((await other).reply.status).toBe("ok");
  });
});

describe("a session that ends", () => {
  test("rejects what waits on it and keeps what was not sent", async () => {
    const t = setup();
    ready(t);

    const lost = t.client.request(echo);
    const lostId = idOf(t.sent, "request");
    const lostHello = idOf(t.sent, "hello");
    t.end();

    await expect(lost).rejects.toBeInstanceOf(SessionEndedError);

    const later = t.client.request(evaluate);
    expect(t.sent).toHaveLength(2);

    // The next session starts with a hello built anew.
    t.start();
    expect(t.hellos).toHaveBeenCalledTimes(2);
    expect(t.sent).toHaveLength(3);

    // A reply to a request of the session before, such as to a hello that
    // Shiny replayed, is ignored.
    t.receive({ ...helloReply, id: lostHello });
    t.receive({ id: lostId, status: "ok", value: [] });
    expect(t.sent).toHaveLength(3);

    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    await settle();
    expect(t.sessions).toHaveLength(2);
    expect(t.messages()[3]).toEqual([
      { ...evaluate, id: idOf(t.sent, "evaluate") },
    ]);

    t.receive({ ...okReply, id: idOf(t.sent, "evaluate") });
    expect((await later).reply.status).toBe("ok");
  });

  test("before R answered its hello starts afresh with the next", async () => {
    const t = setup();
    t.start();
    t.end();
    t.start();
    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    await settle();

    expect(t.sessions).toHaveLength(1);
  });
});

describe("another hello in the session", () => {
  test("gives up what waits on R and what the client holds, and holds what comes until R answers it", async () => {
    const t = setup();
    ready(t);
    const waiting = t.client.request(echo);
    const waitingId = idOf(t.sent, "request");
    const firstHello = idOf(t.sent, "hello");

    t.client.hello();
    const held = t.client.request(evaluate);

    await expect(waiting).rejects.toBeInstanceOf(SessionEndedError);
    expect(t.messages().at(-1)).toEqual([
      { id: idOf(t.sent, "hello"), kind: "hello", protocol: "1.0", capabilities: ["cancel", "progress"] },
    ]);
    expect(idOf(t.sent, "hello")).not.toBe(firstHello);
    expect([...t.client.capabilities()]).toEqual([]);

    // What R replies to a request before the hello, such as cancelled to one
    // that the hello drops, is ignored.
    t.receive({ id: waitingId, status: "cancelled" });
    expect(t.sent).toHaveLength(3);

    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    await settle();
    expect(t.sessions).toHaveLength(2);
    expect(t.messages().at(-1)).toEqual([{ ...evaluate, id: idOf(t.sent, "evaluate") }]);

    t.receive({ ...okReply, id: idOf(t.sent, "evaluate") });
    expect((await held).reply.status).toBe("ok");
  });

  test("gives up what the client holds for a session whose hello R has not answered", async () => {
    const t = setup();
    t.start();
    const held = t.client.request(echo);
    t.client.forget(["z"]);

    t.client.hello();
    await expect(held).rejects.toBeInstanceOf(SessionEndedError);

    t.receive({ ...helloReply, id: idOf(t.sent, "hello") });
    await settle();
    expect(t.sessions).toHaveLength(1);
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"], ["hello"]]);
  });

  test("goes once the message whose body the transport is sending has gone, so that R hears it first", async () => {
    const t = setup();
    ready(t);
    let sendBody: () => void = () => undefined;
    const send = t.fake.send.bind(t.fake);
    t.fake.send = (message, bodies) =>
      bodies.size === 0
        ? send(message, bodies)
        : new Promise((resolve) => {
            sendBody = () => {
              resolve(send(message, bodies));
            };
          });

    const upload = t.client.request(echo, { body: new Blob(["x"]) });
    t.client.hello();
    await expect(upload).rejects.toBeInstanceOf(SessionEndedError);
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"]]);

    sendBody();
    await settle();
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"], ["request"], ["hello"]]);
  });

  test("goes nowhere where its session ends first, and the next session sends its own", async () => {
    const t = setup();
    ready(t);
    let sendBody: () => void = () => undefined;
    const send = t.fake.send.bind(t.fake);
    t.fake.send = (message, bodies) =>
      bodies.size === 0
        ? send(message, bodies)
        : new Promise((resolve) => {
            sendBody = () => {
              resolve(send(message, bodies));
            };
          });

    void t.client.request(echo, { body: new Blob(["x"]) }).catch(() => undefined);
    t.client.hello();
    t.end();
    sendBody();
    await settle();
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"], ["request"]]);

    t.start();
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"], ["request"], ["hello"]]);
  });

  test("sends nothing where no session runs, as the next session sends its hello as it starts", async () => {
    const t = setup();
    ready(t);
    t.end();
    const held = t.client.request(echo);

    t.client.hello();
    await expect(held).rejects.toBeInstanceOf(SessionEndedError);
    expect(t.sent).toHaveLength(1);

    t.start();
    expect(t.messages().map((message) => message.map((m) => m.kind))).toEqual([["hello"], ["hello"]]);
  });
});

describe("a request given up", () => {
  test("before it was sent ends cancelled, sending nothing", async () => {
    const t = setup();
    const ac = new AbortController();

    const held = t.client.request(echo, { signal: ac.signal });
    ac.abort();

    const aborted = new AbortController();
    aborted.abort();
    const never = t.client.request(echo, { signal: aborted.signal });

    expect((await held).reply.status).toBe("cancelled");
    expect((await never).reply.status).toBe("cancelled");

    ready(t);
    await settle();
    expect(t.sent).toHaveLength(1);
  });

  test("that R holds is cancelled where both sides named cancel", async () => {
    const t = setup();
    ready(t);
    const ac = new AbortController();

    const pending = t.client.request(evaluate, { signal: ac.signal });
    const id = idOf(t.sent, "evaluate");
    await settle();
    ac.abort();

    expect(t.messages().at(-1)).toEqual([{ kind: "cancel", request: id }]);

    t.receive({ id, status: "cancelled" });
    expect((await pending).reply).toEqual({ id, status: "cancelled" });
  });

  test("goes on where R did not name cancel", async () => {
    const t = setup();
    ready(t, { ...helloReply, capabilities: ["progress"] } as Reply);
    const ac = new AbortController();

    const pending = t.client.request(evaluate, { signal: ac.signal });
    const id = idOf(t.sent, "evaluate");
    await settle();
    ac.abort();

    expect(t.sent).toHaveLength(2);

    t.receive({ ...okReply, id });
    expect((await pending).reply.status).toBe("ok");
  });

  test("while its body goes is cancelled once it is sent", async () => {
    const t = setup();
    ready(t);
    let sendBody: () => void = () => undefined;
    const send = t.fake.send.bind(t.fake);
    t.fake.send = (message, bodies) =>
      bodies.size === 0
        ? send(message, bodies)
        : new Promise((resolve) => {
            sendBody = () => {
              resolve(send(message, bodies));
            };
          });
    const ac = new AbortController();

    void t.client.request(echo, { body: new Blob(["x"]), signal: ac.signal });
    ac.abort();
    expect(t.sent).toHaveLength(1);

    sendBody();
    await settle();

    const id = idOf(t.sent, "request");
    expect(t.messages().at(-1)).toEqual([{ kind: "cancel", request: id }]);
  });
});

// The fixtures of the wire protocol, played through the client: each
// fixture's message is what the client sends for the requests it is handed,
// and R's messages in it end those requests, with the client's ids in place of
// the fixture's.
describe("the protocol's fixtures", () => {
  const names = readdirSync(fixtures)
    .filter((file) => file.endsWith(".json"))
    .map((file) => file.slice(0, -".json".length));

  // Swaps ids in a message, the fixture's for the client's or back.
  function swap<T>(value: T, ids: Map<string, string>): T {
    return JSON.parse(
      JSON.stringify(value, (key, v: unknown) =>
        (key === "id" || key === "request") && typeof v === "string"
          ? (ids.get(v) ?? v)
          : v,
      ),
    ) as T;
  }

  // What a caller hands the client for a request of a fixture.
  function outgoing(
    request: Request,
  ): [Parameters<Client["request"]>[0], Blob | undefined] {
    const body = "body" in request ? request.body : undefined;
    const blob =
      body === undefined
        ? undefined
        : body.name === undefined
          ? new Blob(["bytes"], { type: body.type })
          : new File(["bytes"], body.name, { type: body.type });
    return [
      without(request, "id", "body") as Parameters<Client["request"]>[0],
      blob,
    ];
  }

  // Plays R's messages, handing a reply that announces a body its bytes.
  function play(t: ReturnType<typeof setup>, messages: (Reply | Progress)[][]) {
    for (const item of messages.flat()) {
      const body = "body" in item ? item.body : undefined;
      t.receive(
        item,
        body === undefined
          ? null
          : Promise.resolve(new Blob(["bytes"], { type: body.type })),
      );
    }
  }

  test.each(names.filter((n) => n.startsWith("hello")))("%s", async (name) => {
    const fx = fixture(name);
    const request = only(fx.client) as HelloRequest;
    const t = setup(
      without(request, "id", "kind") as ReturnType<ClientOptions["hello"]>,
    );

    t.start();
    const ids = new Map([[request.id, idOf(t.sent, "hello")]]);
    expect(t.messages()).toEqual([swap(fx.client, ids)]);

    play(t, swap(fx.r, ids));
    await settle();
    expect(t.sessions).toEqual(swap(fx.r[0], ids));
  });

  const plain = names.filter(
    (n) => !n.startsWith("hello") && !["cancel", "forget"].includes(n),
  );

  test.each(plain)("%s", async (name) => {
    const fx = fixture(name);
    const t = setup();
    ready(t);
    const before = t.sent.length;

    const notes: unknown[] = [];
    const pendings = fx.client.map((request) => {
      const [out, body] = outgoing(request);
      return t.client.request(out, {
        body,
        progress: (note) => notes.push(note),
      });
    });

    // Requests that came together while the session was ready each went at
    // once, so that a fixture of several is compared message by message.
    const sent = t.messages().slice(before).flat();
    const ids = new Map(
      fx.client.map((request, i) => [request.id ?? "", sent[i]?.id ?? ""]),
    );
    expect(sent).toEqual(swap(fx.client, ids));

    play(t, swap(fx.r, ids));

    const replies = fx.r.flat().filter((item): item is Reply => "status" in item);
    for (const [i, request] of pendings.entries()) {
      const { reply, body } = await request;
      expect(swap(reply, new Map([...ids].map(([a, b]) => [b, a])))).toEqual(
        replies[i],
      );
      expect(body === null).toBe(!("body" in reply));
    }

    expect(notes).toEqual(
      fx.r.flat().flatMap((item) => ("progress" in item ? [item.progress] : [])),
    );
  });

  test("several-requests, as one message while R has not answered the hello", () => {
    const fx = fixture("several-requests");
    const t = setup();

    for (const request of fx.client) void t.client.request(outgoing(request)[0]);
    ready(t);

    const [message] = t.messages().slice(1);
    const ids = new Map(
      fx.client.map((request, i) => [request.id ?? "", message?.[i]?.id ?? ""]),
    );
    expect(message).toEqual(swap(fx.client, ids));
  });

  test("cancel", async () => {
    const fx = fixture("cancel");
    const t = setup();
    ready(t);
    const ac = new AbortController();

    const pending = t.client.request(evaluate, { signal: ac.signal });
    await settle();
    ac.abort();

    const ids = new Map([["17", idOf(t.sent, "evaluate")]]);
    expect(t.messages().at(-1)).toEqual(swap(fx.client, ids));

    play(t, swap(fx.r, ids));
    expect((await pending).reply).toEqual(swap(fx.r[0]?.[0], ids));
  });

  test("forget", async () => {
    const fx = fixture("forget");
    const output = only(fixture("output").client);
    const t = setup();
    ready(t);

    const pending = t.client.request(outgoing(output)[0]);
    t.client.forget(["d", "e"]);

    expect(t.messages().at(-1)).toEqual(fx.client);

    const ids = new Map([["34", idOf(t.sent, "output")]]);
    play(t, swap(fx.r, ids));
    expect((await pending).reply).toEqual(swap(fx.r[0]?.[0], ids));
  });
});
