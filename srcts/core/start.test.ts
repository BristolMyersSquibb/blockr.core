// Starting core in the order of the startup modules, and switching the page to
// the front-end of each board that opens, with R played over a transport whose
// replies the test sends, on the board of the fixture hello, which core's basic
// front-end, blockr.core-basic, draws.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { EvaluateRequest, HelloReply, Reply, Request, Transport, TransportListener } from "../protocol/index.ts";
import { coreOptions } from "../options/index.ts";
import { reports } from "../testing/reports.ts";
import { hello } from "../testing/store.ts";
import type { CoreApi } from "../types/core-api.ts";
import { pageFrontEnds, start } from "./index.ts";
import type { Changed, StartOptions } from "./index.ts";

const BASIC = "blockr.core/basic.js?v=0f3d8e21";
const DOCK = "blockr.dock/dock.js?v=1";

// R, played over a transport: the test starts sessions and sends replies.
// Once the test has answered the blockr options that the scheduler reads, R
// answers each later request for blockr options itself, with no values, as
// the one for the default of dark_mode, which core's board class requires.
function playedR() {
  const sent: Request[] = [];
  const replied = new Set<string>();
  let listener: TransportListener | undefined;
  let answering = false;
  const heard = (): TransportListener => {
    if (listener === undefined) throw new Error("Core listens to nothing yet.");
    return listener;
  };
  const transport: Transport = {
    send(message) {
      sent.push(...message);
      for (const request of message) {
        if (!answering || request.kind !== "blockr_options") continue;
        void Promise.resolve().then(() => {
          heard().received({ id: request.id, status: "ok", values: {} }, null);
        });
      }
      return Promise.resolve();
    },
    listen(given) {
      listener = given;
    },
  };
  const last = (kind: Request["kind"]): Request => {
    const found = sent.filter((request) => request.kind === kind).at(-1);
    if (found === undefined) throw new Error(`Core sent no ${kind}.`);
    return found;
  };
  return {
    transport,
    sent,
    kinds: () => sent.map((request) => request.kind),
    // Starts a session, and answers its hello with the reply given.
    session(reply: HelloReply = hello): void {
      heard().started();
      const asked = last("hello");
      heard().received({ ...reply, id: asked.id ?? "" }, null);
    },
    // Answers the latest blockr options request.
    options(values: Readonly<Record<string, number | string | boolean | null>> = {}): void {
      const asked = last("blockr_options");
      answering = true;
      heard().received({ id: asked.id ?? "", status: "ok", values }, null);
    },
    reply(item: Reply): void {
      replied.add(item.id);
      heard().received(item, null);
    },
    evaluations: () => sent.filter((request): request is EvaluateRequest => request.kind === "evaluate"),
    // Answers every evaluation that waits on R with ok, until none waits.
    async evaluateAll(): Promise<void> {
      for (let i = 0; i < 20; i++) {
        await tick(100);
        const pending = sent.filter((request): request is EvaluateRequest => request.kind === "evaluate" && !replied.has(request.id));
        if (pending.length === 0) return;
        for (const request of pending) {
          replied.add(request.id);
          heard().received(
            {
              id: request.id,
              status: "ok",
              block: request.block,
              revision: request.revision,
              summary: null,
              conditions: [],
              omitted: { warning: 0, message: 0 },
              stdout: "",
              stderr: "",
            },
            null,
          );
        }
      }
    },
  };
}

// A module that keeps when its function ran, and that resolves once the test
// lets it, where it waits.
function gate() {
  let open = (): void => undefined;
  const opened = new Promise<void>((resolve) => {
    open = resolve;
  });
  return { opened, open };
}

interface Setup {
  r: ReturnType<typeof playedR>;
  el: HTMLElement;
  imported: string[];
  started: ReturnType<typeof start>;
}

function setup(modules: Readonly<Record<string, unknown>>, given: Partial<StartOptions> = {}): Setup {
  const r = playedR();
  const el = document.createElement("div");
  const imported: string[] = [];
  const started = start({
    transport: r.transport,
    loader: {
      importer(url) {
        imported.push(url);
        return Object.hasOwn(modules, url)
          ? Promise.resolve(modules[url])
          : Promise.reject(new TypeError(`Failed to fetch dynamically imported module: ${url}`));
      },
      stylesheet: () => Promise.resolve(),
    },
    options: coreOptions,
    frontEnds: pageFrontEnds(el),
    measure: { info: () => null, watch: () => () => undefined },
    ...given,
  });
  return { r, el, imported, started };
}

// A front-end that demands every block from within render(), records each
// core it renders with, and returns its cleanup once the test lets it.
function frontEnd(wait?: Promise<void>) {
  const renders: { core: CoreApi; signal: AbortSignal; el: HTMLElement }[] = [];
  let cleanups = 0;
  return {
    exports: {
      api: 1,
      default: {
        async render({ el, core, signal }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }) {
          renders.push({ core, signal, el });
          core.scheduler.demand("front-end", (board) => Object.keys(board.blocks));
          el.textContent = core.store.board().name;
          await wait;
          return () => {
            cleanups++;
          };
        },
      },
    },
    renders,
    cleanups: () => cleanups,
  };
}

function startup(name: string, events: string[], fn: () => unknown = () => undefined) {
  return {
    api: 1,
    default: {
      async initialize({ core }: { core: CoreApi }) {
        events.push(`${name} starts, board open: ${String(core.store.hasBoard())}`);
        const result = await fn();
        events.push(`${name} done`);
        return result;
      },
    },
  };
}

function withStartup(...packages: string[]): HelloReply {
  return {
    ...hello,
    packages: { ...hello.packages, ...Object.fromEntries(packages.map((pkg) => [pkg, "1.0.0"])) },
    startup_modules: packages.map((pkg) => ({ package: pkg, module: `${pkg}/startup.js?v=1` })),
  };
}

async function tick(ms = 0): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("starting the page", () => {
  test("asks for the blockr options the scheduler reads in one request, as soon as R answers hello", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();

    expect(s.r.kinds()).toEqual(["hello", "blockr_options"]);
    expect(s.r.sent[1]).toMatchObject({ names: ["evaluation_queue", "background_evaluation_delay", "gate_visibility"] });

    s.r.options({ evaluation_queue: 0 });
    const core = await s.started.ready;
    expect(core.store.board().name).toBe("Iris review");
    expect(s.el.textContent).toBe("Iris review");
  });

  test("initializes the startup modules one at a time in R's order before it opens the board and draws it", async () => {
    const events: string[] = [];
    const first = gate();
    const basic = frontEnd();
    const s = setup({
      "blockr.ui/startup.js?v=1": startup("blockr.ui", events, () => first.opened),
      "blockr.dock/startup.js?v=1": startup("blockr.dock", events),
      [BASIC]: basic.exports,
    });
    s.r.session(withStartup("blockr.ui", "blockr.dock"));
    await tick();
    s.r.options();
    await tick();

    expect(events).toEqual(["blockr.ui starts, board open: false"]);
    expect(s.imported).toEqual(["blockr.ui/startup.js?v=1"]);

    first.open();
    await s.started.ready;
    expect(events).toEqual([
      "blockr.ui starts, board open: false",
      "blockr.ui done",
      "blockr.dock starts, board open: false",
      "blockr.dock done",
    ]);
    // The front-end's module is imported only once every startup module has
    // initialized.
    expect(s.imported).toEqual(["blockr.ui/startup.js?v=1", "blockr.dock/startup.js?v=1", BASIC]);
    expect(basic.renders).toHaveLength(1);
  });

  test("sends R nothing to evaluate before the front-end's render() has resolved", async () => {
    const drawn = gate();
    const basic = frontEnd(drawn.opened);
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();
    s.r.options();
    await tick(100);

    expect(basic.renders).toHaveLength(1);
    expect(s.r.evaluations()).toHaveLength(0);

    drawn.open();
    await tick(100);
    // What the front-end demanded within render() goes first.
    expect(s.r.evaluations().length).toBeGreaterThan(0);
  });

  test("opens the board that a startup module names in place of R's", async () => {
    const named = { ...hello.board, board: { ...hello.board.board, id: "named_board", name: "From the URL" } };
    const basic = frontEnd();
    const s = setup({
      "blockr.session/startup.js?v=1": startup("blockr.session", [], () => JSON.stringify(named)),
      [BASIC]: basic.exports,
    });
    s.r.session(withStartup("blockr.session"));
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(core.store.board().name).toBe("From the URL");
  });

  test.each([
    ["the board is locked", { lock: { open: ["b"] } }, ["blockr.session"], "The board is locked, so core opens the board that R sent rather than the one that blockr.session named."],
    ["two startup modules name one", {}, ["blockr.session", "blockr.other"], 'The startup modules of "blockr.session" and "blockr.other" each named a board to open, so core opens the board that R sent.'],
  ])("keeps the board R sent where %s, and names the packages", async (_, changes, packages, message) => {
    const named = JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Named" } });
    const modules = Object.fromEntries(packages.map((pkg) => [`${pkg}/startup.js?v=1`, startup(pkg, [], () => named)]));
    const s = setup({ ...modules, [BASIC]: frontEnd().exports });
    const reported = reports();
    s.r.session({ ...withStartup(...packages), ...changes });
    await tick();
    s.r.options();
    const core = await s.started.ready;
    reported.restore();

    expect(core.store.board().name).toBe("Iris review");
    expect(reported.errors().map((error) => (error as Error).message)).toContain(message);
  });

  test("names a startup module that fails and carries on with the rest", async () => {
    const events: string[] = [];
    const s = setup({
      "blockr.bad/startup.js?v=1": startup("blockr.bad", events, () => {
        throw new Error("boom");
      }),
      "blockr.good/startup.js?v=1": startup("blockr.good", events),
      [BASIC]: frontEnd().exports,
    });
    const reported = reports();
    s.r.session(withStartup("blockr.bad", "blockr.missing", "blockr.good"));
    await tick();
    s.r.options();
    const core = await s.started.ready;
    reported.restore();

    expect(events).toEqual(["blockr.bad starts, board open: false", "blockr.good starts, board open: false", "blockr.good done"]);
    expect(core.store.hasBoard()).toBe(true);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      "The module blockr.bad/startup.js of blockr.bad failed in initialize(): boom",
      "The module blockr.missing/startup.js of blockr.missing failed to load: Failed to fetch dynamically imported module: blockr.missing/startup.js?v=1",
    ]);
  });

  test("refuses to start where R speaks another major of the protocol", async () => {
    let failed: Error | undefined;
    const s = setup({}, { fail: (error) => (failed = error) });
    s.r.session({ ...hello, protocol: "2.0" });
    await expect(s.started.ready).rejects.toThrow("R speaks version 2.0 of the wire protocol, where this page speaks 1.0.");
    expect(failed?.message).toContain("Reload the page");
    expect(s.r.kinds()).toEqual(["hello"]);
  });
});

describe("a board opened later", () => {
  // The reply to hello, in which blockr.dock names its front-end for its board
  // class dock_board, which its startup module defines.
  const docked: HelloReply = {
    ...withStartup("blockr.dock"),
    modules: { ...hello.modules, "blockr.dock-dock": { module: DOCK } },
    board_classes: { ...hello.board_classes, dock_board: { package: "blockr.dock", front_end: "blockr.dock-dock", version: 1 } },
  };
  const dockStartup = {
    api: 1,
    default: {
      initialize({ core }: { core: CoreApi }) {
        core.store.register({ class: "dock_board", parent: "board", properties: {} });
      },
    },
  };
  const dockBoard = JSON.stringify({ ...hello.board, board: { ...hello.board.board, id: "dock_one", class: ["dock_board", "board"], name: "Docked" } });

  test("switches the page to the front-end of its class, ending the one on the page first", async () => {
    const basic = frontEnd();
    const dock = frontEnd();
    const s = setup({ "blockr.dock/startup.js?v=1": dockStartup, [BASIC]: basic.exports, [DOCK]: dock.exports });
    s.r.session(docked);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    await core.store.open(dockBoard);

    expect(basic.renders[0]?.signal.aborted).toBe(true);
    expect(basic.cleanups()).toBe(1);
    expect(dock.renders).toHaveLength(1);
    expect(s.el.textContent).toBe("Docked");
  });

  test("starts the front-end again where it is the page's already, and holds the scheduler until render() has resolved", async () => {
    const drawn = gate();
    // What render() waits on, nothing as the page starts.
    const hold: { wait?: Promise<void> } = {};
    const renders: AbortSignal[] = [];
    const exports = {
      api: 1,
      default: {
        async render({ core, signal }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }) {
          renders.push(signal);
          core.scheduler.demand("front-end", (board) => Object.keys(board.blocks));
          await hold.wait;
        },
      },
    };
    const s = setup({ [BASIC]: exports });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;
    await s.r.evaluateAll();
    const before = s.r.evaluations().length;

    hold.wait = drawn.opened;
    const opening = core.store.open(JSON.stringify(hello.board));
    await tick(100);
    expect(renders).toHaveLength(2);
    expect(renders[0]?.aborted).toBe(true);
    expect(renders[1]?.aborted).toBe(false);
    expect(s.r.evaluations()).toHaveLength(before);

    drawn.open();
    await opening;
    await tick(100);
    expect(s.r.evaluations().length).toBeGreaterThan(before);
  });

  test("that fails to open leaves the board the store holds drawn by its front-end", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;

    // A board of a class that requires a board option no package registers
    // fails once the front-end is imported.
    const reported = reports();
    core.store.register({ class: "strict_board", parent: "board", options: ["nowhere"], properties: {} });
    const strict = JSON.stringify({ ...hello.board, board: { ...hello.board.board, class: ["strict_board", "board"] } });
    await expect(core.store.open(strict)).rejects.toThrow("no loaded package defines");
    reported.restore();

    expect(basic.renders).toHaveLength(2);
    expect(basic.renders[1]?.signal.aborted).toBe(false);
    expect(s.el.textContent).toBe("Iris review");
  });

  test("releases what the front-end held of the board before: its demands and its mounts", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;
    const scoped = basic.renders[0]?.core;
    const heard: string[] = [];
    scoped?.store.subscribe(() => {
      heard.push("store");
    });

    await core.store.open(JSON.stringify(hello.board));
    core.store.apply({ name: "Renamed" });

    // The first front-end hears nothing of the board that replaced its own.
    expect(heard).toEqual([]);
  });
});

describe("a later session", () => {
  test("gives every block a new revision, and asks for a reload where R runs a package at another version", async () => {
    const changes: (readonly Changed[])[] = [];
    const s = setup({ [BASIC]: frontEnd().exports }, { reload: (changed) => changes.push(changed) });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;
    const revision = core.scheduler.status("a")?.revision ?? 0;

    s.r.session();
    await tick();
    expect(core.scheduler.status("a")?.revision).toBeGreaterThan(revision);
    expect(changes).toEqual([]);

    s.r.session({ ...hello, packages: { "blockr.core": "0.2.1" } });
    await tick();
    expect(changes).toEqual([[{ package: "blockr.core", loaded: "0.2.0", now: "0.2.1" }]]);
  });
});
