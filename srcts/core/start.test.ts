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
function playedR() {
  const sent: Request[] = [];
  const replied = new Set<string>();
  let listener: TransportListener | undefined;
  const heard = (): TransportListener => {
    if (listener === undefined) throw new Error("Core listens to nothing yet.");
    return listener;
  };
  const transport: Transport = {
    send(message) {
      sent.push(...message);
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
    // Answers the latest hello with the reply given, as R answers the hello
    // with which core starts again in the session.
    answer(reply: HelloReply = hello): void {
      const asked = last("hello");
      heard().received({ ...reply, id: asked.id ?? "" }, null);
    },
    // Answers the latest blockr options request.
    options(values: Readonly<Record<string, number | string | boolean | null>> = {}): void {
      const asked = last("blockr_options");
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
  const named = (name: string): string => JSON.stringify({ ...hello.board, board: { ...hello.board.board, name } });

  // Starts the page on the reply given, whose blockr options R holds none of.
  async function started(modules: Readonly<Record<string, unknown>>, reply: HelloReply = hello, given: Partial<StartOptions> = {}) {
    const s = setup(modules, given);
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    return { ...s, core };
  }

  // Opens the board through the store's open(), and answers the hello with
  // which core starts again.
  async function reopen(s: Setup, core: CoreApi, text: string, reply: HelloReply = hello): Promise<unknown> {
    const opening = core.store.open(text);
    await tick();
    s.r.answer(reply);
    return opening;
  }

  test("is checked by the store on the page first, so that a board it refuses leaves the page as it was, with the reason", async () => {
    const basic = frontEnd();
    const s = await started({ [BASIC]: basic.exports });
    const sent = s.r.sent.length;

    await expect(s.core.store.open("{")).rejects.toThrow("The store refused to open the board\n- The text holds no JSON");
    s.core.store.register({ class: "strict_board", parent: "board", options: ["nowhere"], properties: {} });
    const strict = JSON.stringify({ ...hello.board, board: { ...hello.board.board, class: ["strict_board", "board"] } });
    await expect(s.core.store.open(strict)).rejects.toThrow('requires the board option "nowhere", which no loaded package defines');

    expect(s.r.sent.slice(sent)).toEqual([]);
    expect(basic.renders).toHaveLength(1);
    expect(basic.renders[0]?.signal.aborted).toBe(false);
    expect(s.el.textContent).toBe("Iris review");
    expect(s.started.core()).toBe(s.core);
    s.core.store.apply({ name: "Still open" });
    expect(s.core.store.board().name).toBe("Still open");
  });

  test("ends everything core ran for the board with one abort, and starts core again as the page starts, once R has answered another hello", async () => {
    const events: string[] = [];
    const initialized: { core: CoreApi; signal: AbortSignal }[] = [];
    const session = {
      api: 1,
      default: {
        initialize({ core, signal }: { core: CoreApi; signal: AbortSignal }) {
          events.push(`blockr.session starts, board open: ${String(core.store.hasBoard())}`);
          initialized.push({ core, signal });
          signal.addEventListener("abort", () => events.push("blockr.session ends"));
        },
      },
    };
    const drawn = gate();
    const hold: { wait?: Promise<void> } = {};
    const renders: { core: CoreApi; signal: AbortSignal }[] = [];
    const front = {
      api: 1,
      default: {
        async render({ el, core, signal }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }) {
          renders.push({ core, signal });
          signal.addEventListener("abort", () => events.push("the front-end ends"));
          core.scheduler.demand("front-end", (board) => Object.keys(board.blocks));
          el.textContent = core.store.board().name;
          await hold.wait;
          return () => events.push("the front-end cleans up");
        },
      },
    };
    const s = await started({ "blockr.session/startup.js?v=1": session, [BASIC]: front }, withStartup("blockr.session"));
    await s.r.evaluateAll();
    const top = Math.max(...Object.keys(s.core.store.board().blocks).map((id) => s.core.scheduler.status(id)?.revision ?? 0));
    const sent = s.r.sent.length;

    hold.wait = drawn.opened;
    const opening = s.core.store.open(named("Opened"));
    await tick();

    // One abort ended what core ran for the board, and core waits for R to
    // answer the hello with which it starts again.
    expect([...events].sort()).toEqual(
      ["blockr.session starts, board open: false", "blockr.session ends", "the front-end ends", "the front-end cleans up"].sort(),
    );
    expect(s.r.sent.slice(sent).map((request) => request.kind)).toEqual(["hello"]);
    expect(s.started.core()).toBeUndefined();
    expect(s.el.textContent).toBe("Loading the board…");
    expect(() => {
      s.core.store.apply({ name: "Late" });
    }).toThrow("Another board opened in a new store, so this store changes its board no more.");

    s.r.answer(withStartup("blockr.session"));
    await tick(100);
    const next = s.started.core();
    expect(next).toBeDefined();
    expect(next).not.toBe(s.core);
    expect(next?.store).not.toBe(s.core.store);
    expect(next?.store.board().name).toBe("Opened");
    expect(next?.store.canUndo()).toBe(false);
    expect(initialized.map((entry) => entry.core)).toEqual([s.core, next]);
    expect(initialized[1]?.signal.aborted).toBe(false);
    expect(renders.map((entry) => entry.core)).toEqual([s.core, next]);
    expect(s.el.textContent).toBe("Opened");
    // The scheduler holds its evaluations until render() has resolved.
    expect(s.r.sent.slice(sent).map((request) => request.kind)).toEqual(["hello"]);

    drawn.open();
    expect(await opening).toEqual([]);
    await tick(100);
    const evaluated = s.r.evaluations().filter((request) => s.r.sent.indexOf(request) >= sent);
    expect(evaluated.length).toBeGreaterThan(0);
    // Every revision of the board that opened is above any the page gave, and
    // R forgets nothing, since the hello dropped every result.
    for (const request of evaluated) expect(request.revision).toBeGreaterThan(top);
    expect(s.r.kinds()).not.toContain("forget");
  });

  test("finds nothing of the board before: no demand, subscription or mount that its front-end or a startup module made", async () => {
    const heard: string[] = [];
    const mounted: AbortSignal[] = [];
    let initialized = 0;
    const session = {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          const at = ++initialized;
          core.store.subscribe(() => heard.push(`startup module ${String(at)} hears the store`));
          core.scheduler.subscribe(() => heard.push(`startup module ${String(at)} hears the scheduler`));
          // As the page starts, it demands c, which nothing demands later.
          if (at === 1) core.scheduler.demand("blockr.session", ["c"]);
        },
      },
    };
    let renders = 0;
    const front = {
      api: 1,
      default: {
        render({ el, core }: { el: HTMLElement; core: CoreApi }) {
          const at = ++renders;
          core.store.subscribe(() => heard.push(`front-end ${String(at)} hears the store`));
          core.scheduler.subscribe(() => heard.push(`front-end ${String(at)} hears the scheduler`));
          core.scheduler.demand("front-end", at === 1 ? ["b"] : ["a"]);
          const card = document.createElement("div");
          el.append(card);
          core.mountBlock(card, "b");
        },
      },
    };
    const head = {
      api: 1,
      default: {
        render({ signal }: { signal: AbortSignal }) {
          mounted.push(signal);
        },
      },
    };
    const s = await started(
      { "blockr.session/startup.js?v=1": session, [BASIC]: front, "blockr.core/head-block.js?v=9a41f2c7": head },
      withStartup("blockr.session"),
    );
    await s.r.evaluateAll();
    expect(new Set(s.r.evaluations().map((request) => request.block))).toEqual(new Set(["a", "b", "c"]));
    expect(mounted).toHaveLength(1);

    await reopen(s, s.core, named("Opened"), withStartup("blockr.session"));
    const sent = s.r.sent.length;
    await s.r.evaluateAll();
    const next = s.started.core();
    heard.length = 0;
    next?.store.apply({ blocks: { mod: { a: { name: "Renamed" } } } });
    next?.scheduler.invalidate("a");
    await tick(100);

    // What the first front-end and the first startup module held heard
    // nothing, and demanded nothing of the board that opened.
    expect([...new Set(heard)].sort()).toEqual([
      "front-end 2 hears the scheduler",
      "front-end 2 hears the store",
      "startup module 2 hears the scheduler",
      "startup module 2 hears the store",
    ]);
    expect(new Set(s.r.evaluations().filter((request) => s.r.sent.indexOf(request) >= sent).map((request) => request.block))).toEqual(
      new Set(["a"]),
    );
    expect(mounted).toHaveLength(2);
    expect(mounted[0]?.aborted).toBe(true);
    expect(mounted[1]?.aborted).toBe(false);
    // R answered the hello with which core started again, and the page sent
    // two hellos in one session.
    expect(s.r.kinds().filter((kind) => kind === "hello")).toHaveLength(2);
  });

  test("switches the page to the front-end of its class", async () => {
    const basic = frontEnd();
    const dock = frontEnd();
    const s = await started({ "blockr.dock/startup.js?v=1": dockStartup, [BASIC]: basic.exports, [DOCK]: dock.exports }, docked);

    await reopen(s, s.core, dockBoard, docked);

    expect(basic.renders[0]?.signal.aborted).toBe(true);
    expect(basic.cleanups()).toBe(1);
    expect(dock.renders).toHaveLength(1);
    expect(s.el.textContent).toBe("Docked");

    // A board of core's class opened in a dock app is drawn by core's basic
    // front-end, which starts again.
    await reopen(s, s.started.core() ?? s.core, named("Basic again"), docked);
    expect(dock.renders[0]?.signal.aborted).toBe(true);
    expect(basic.renders).toHaveLength(2);
    expect(s.el.textContent).toBe("Basic again");
  });

  test("opens no board through the store of a board before, and opens them one at a time", async () => {
    const basic = frontEnd();
    const s = await started({ [BASIC]: basic.exports });

    const first = s.core.store.open(named("First"));
    const second = s.core.store.open(named("Second"));
    await tick();
    s.r.answer();
    await first;
    await expect(second).rejects.toThrow("Another board opened since, so this store opens no board.");
    expect(s.started.core()?.store.board().name).toBe("First");
    expect(s.r.kinds().filter((kind) => kind === "hello")).toHaveLength(2);
  });

  test("that fails to open after all opens the board before in the new store, and open() rejects with the reason", async () => {
    // A startup module that registers the board option its class requires as
    // the page starts only, so that the board checks as the page holds it but
    // fails to open as core starts again.
    let initialized = 0;
    const strict = {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          core.store.register({ class: "strict_board", parent: "board", options: ["zoom"], properties: {} });
          if (++initialized === 1) {
            core.options.register({ id: "zoom", label: "Zoom", category: "View", schema: { type: "number" }, default: 1, ui: { render: () => undefined } });
          }
        },
      },
    };
    const basic = frontEnd();
    const s = await started({ "blockr.strict/startup.js?v=1": strict, [BASIC]: basic.exports }, withStartup("blockr.strict"));
    s.core.store.apply({ name: "Kept" });
    const board = JSON.stringify({ ...hello.board, board: { ...hello.board.board, class: ["strict_board", "board"] } });

    const reported = reports();
    const opening = reopen(s, s.core, board, withStartup("blockr.strict"));
    await expect(opening).rejects.toThrow('requires the board option "zoom", which no loaded package defines');
    reported.restore();

    expect(s.started.core()?.store.board().name).toBe("Kept");
    expect(basic.renders).toHaveLength(2);
    expect(basic.renders[1]?.signal.aborted).toBe(false);
    expect(s.el.textContent).toBe("Kept");
  });

  test("shows why where R refuses the hello with which core starts again", async () => {
    let failed: Error | undefined;
    const s = await started({ [BASIC]: frontEnd().exports }, hello, { fail: (error) => (failed = error) });

    const opening = s.core.store.open(named("Opened"));
    await tick();
    s.r.answer({ ...hello, protocol: "2.0" });
    await expect(opening).rejects.toThrow("R speaks version 2.0 of the wire protocol");
    expect(failed?.message).toContain("Reload the page");
    expect(s.started.core()).toBeUndefined();
  });

  test("ignores a board that a startup module names as it opens, since only one that runs as the page starts names one", async () => {
    const session = {
      api: 1,
      default: {
        initialize: () => named("From the URL"),
      },
    };
    const s = await started({ "blockr.session/startup.js?v=1": session, [BASIC]: frontEnd().exports }, withStartup("blockr.session"));
    expect(s.core.store.board().name).toBe("From the URL");

    const reported = reports();
    await reopen(s, s.core, named("Opened"), withStartup("blockr.session"));
    reported.restore();

    expect(s.started.core()?.store.board().name).toBe("Opened");
    expect(reported.errors().map((error) => (error as Error).message)).toContain(
      "The startup module of blockr.session named a board to open as another board opened, which core ignores, since only a startup module that runs as the page starts names one.",
    );
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
