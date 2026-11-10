// Starting core in the order of the startup modules, and starting it again for
// each board that opens, with R played over a transport whose replies the test
// sends, on the board of the fixture hello, which core's basic frontend,
// blockr.core-basic, draws, as it draws every board that holds no frontend,
// and on boards whose plugins a test's package defines, a frontend among them.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { EvaluateRequest, HelloReply, Reply, Request, Transport, TransportListener } from "../protocol/index.ts";
import { coreOptions } from "../options/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { reports } from "../testing/reports.ts";
import { hello, reply, withPlugins } from "../testing/store.ts";
import { startPage } from "../page/index.ts";
import type { JsonBoard, Plugin, PluginId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { pageFrontEnds, start } from "./index.ts";
import type { Changed, StartOptions } from "./index.ts";

const BASIC = "blockr.core/basic.js?v=0f3d8e21";

// R, played over a transport: the test starts sessions and sends replies.
// Once the test has answered the blockr options that the scheduler reads, R
// answers each later request for blockr options itself, with no values, as
// the one for the default of dark_mode, which core's basic frontend requires.
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
    // Starts a session, whose hello the test answers with answer().
    start(): void {
      heard().started();
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

// A frontend that demands every block from within render(), records each
// core it renders with, and returns its cleanup once the test lets it.
function frontEnd(wait?: Promise<void>) {
  const renders: { core: CoreApi; signal: AbortSignal; el: HTMLElement; plugin: string | undefined }[] = [];
  let cleanups = 0;
  return {
    exports: {
      api: 1,
      default: {
        async render({ el, core, signal, plugin }: { el: HTMLElement; core: CoreApi; signal: AbortSignal; plugin?: string }) {
          renders.push({ core, signal, el, plugin });
          core.scheduler.demand("frontend", (board) => Object.keys(board.blocks));
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

// The board of the fixture hello with values of the wrong shape, as an app
// script may give them: a head block that shows -1 rows, and the board option
// n_rows at 0.
function unfit(): JsonBoard {
  const { board } = hello.board;
  const b = board.blocks.b;
  if (b === undefined) throw new Error("The fixture's board has no block b.");
  return {
    ...hello.board,
    board: {
      ...board,
      blocks: { ...board.blocks, b: { ...b, state: { ...b.state, n: -1 } } },
      options: { ...board.options, n_rows: { value: 0, version: 1 } },
    },
  };
}

// Why the store refuses the board of unfit().
const unfitness = [
  "The store refused to open the board",
  'The state of the block "b" fails its schema at /n: -1 is less than 0.',
  'The value of the board option "n_rows" fails its schema: 0 is less than 1.',
].join("\n- ");

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
  test("asks for the blockr options the scheduler reads and rand_id in one request, as soon as R answers hello", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();

    expect(s.r.kinds()).toEqual(["hello", "blockr_options"]);
    expect(s.r.sent[1]).toMatchObject({
      names: ["evaluation_queue", "background_evaluation_delay", "gate_visibility", "rand_id"],
    });

    s.r.options({ evaluation_queue: 0 });
    const core = await s.started.ready;
    expect(core.store.board().name).toBe("Iris review");
    expect(s.el.textContent).toBe("Iris review");
  });

  test("starts with the blockr options that environment variables set, which R sends as strings", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();
    s.r.options({ evaluation_queue: "3", background_evaluation_delay: "-1", gate_visibility: "TRUE", rand_id: "uuid" });

    const core = await s.started.ready;
    expect(core.store.board().name).toBe("Iris review");
  });

  test("does not start where the scheduler refuses a blockr option, naming what R sent", async () => {
    const basic = frontEnd();
    const failed: string[] = [];
    const s = setup(
      { [BASIC]: basic.exports },
      {
        fail(error) {
          failed.push(error.message);
        },
      },
    );
    s.r.session();
    await tick();
    s.r.options({ evaluation_queue: "-2" });

    await expect(s.started.ready).rejects.toThrow('The blockr option evaluation_queue is "-2", where it takes a whole number of 0 or more, or -1.');
    expect(failed).toHaveLength(1);
  });

  test("does not start, with R's message, where R cannot give a blockr option that core reads as it starts", async () => {
    const basic = frontEnd();
    const failed: string[] = [];
    const s = setup(
      { [BASIC]: basic.exports },
      {
        fail(error) {
          failed.push(error.message);
        },
      },
    );
    s.r.session();
    await tick();

    s.r.reply({
      id: s.r.sent[1]?.id ?? "",
      status: "error",
      conditions: [
        {
          severity: "error",
          class: ["conflicing_blockr_option", "blockr_error", "rlang_error", "error", "condition"],
          message: "Conflicting options set for evaluation_queue: check environment variable BLOCKR_EVALUATION_QUEUE and option blockr.evaluation_queue.",
        },
      ],
    });

    await expect(s.started.ready).rejects.toThrow("Conflicting options set for evaluation_queue");
    expect(failed).toEqual([
      'R sent no values of the blockr options "evaluation_queue", "background_evaluation_delay", "gate_visibility" and "rand_id": R replied error. Conflicting options set for evaluation_queue: check environment variable BLOCKR_EVALUATION_QUEUE and option blockr.evaluation_queue.',
    ]);
    expect(s.r.kinds()).toEqual(["hello", "blockr_options"]);
  });

  test("does not start where the blockr option rand_id names no shape", async () => {
    const basic = frontEnd();
    const failed: string[] = [];
    const s = setup(
      { [BASIC]: basic.exports },
      {
        fail(error) {
          failed.push(error.message);
        },
      },
    );
    s.r.session();
    await tick();
    s.r.options({ rand_id: "counter" });

    await expect(s.started.ready).rejects.toThrow('The blockr option rand_id is "counter"');
    expect(failed).toHaveLength(1);
  });

  test("has core's commands mint ids in the shape that the blockr option rand_id names", async () => {
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports });
    s.r.session();
    await tick();
    s.r.options({ rand_id: "uuid" });
    const core = await s.started.ready;
    const before = Object.keys(core.store.board().blocks);

    await core.commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block" });

    const minted = Object.keys(core.store.board().blocks).filter((id) => !before.includes(id));
    expect(minted).toHaveLength(1);
    expect(minted[0]).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/);
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
    // The frontend's module is imported only once every startup module has
    // initialized.
    expect(s.imported).toEqual(["blockr.ui/startup.js?v=1", "blockr.dock/startup.js?v=1", BASIC]);
    expect(basic.renders).toHaveLength(1);
  });

  test("sends R nothing to evaluate before the frontend's render() has resolved", async () => {
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
    // What the frontend demanded within render() goes first.
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

  test("has the board that a startup module names hold the plugins of R's, each with the state that the named board gives it", async () => {
    // A package whose plugin class the board that R sent holds two plugins of,
    // and the board that its startup module names holds one of them and one
    // of its own.
    const notes = {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          core.plugins.register({
            id: "blockr.notes-notes_plugin",
            state: { schema: { type: "object" }, default: { text: "" } },
          });
          return JSON.stringify({
            ...hello.board,
            board: {
              ...hello.board.board,
              name: "From the URL",
              plugins: {
                todo: { class: "blockr.notes-notes_plugin", name: "To do", state: { text: "Call back" } },
                later: { class: "blockr.notes-notes_plugin", name: "Later", state: { text: "Soon" } },
              },
            },
          });
        },
      },
    };
    const reply = withPlugins(
      {
        todo: { class: "blockr.notes-notes_plugin", name: "To do", state: { text: "Buy milk" } },
        done: { class: "blockr.notes-notes_plugin", name: "Done", state: { text: "Paid" } },
      },
      withStartup("blockr.notes"),
    );
    const s = setup({ "blockr.notes/startup.js?v=1": notes, [BASIC]: frontEnd().exports });
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(core.store.board().name).toBe("From the URL");
    expect(core.store.board().plugins).toEqual({
      todo: { class: "blockr.notes-notes_plugin", name: "To do", state: { text: "Call back" } },
      done: { class: "blockr.notes-notes_plugin", name: "Done", state: { text: "" } },
    });
  });

  test.each([
    ["the load lock is on", { lock: { board: false, load: true } }, ["blockr.session"], "The load lock is on, so core opens the board that R sent rather than the one that blockr.session named."],
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

  test("leaves the page with the errors alone where the store refuses the opening board, for the deployment to fix", async () => {
    const r = playedR();
    const el = document.createElement("div");
    const basic = frontEnd();
    const reported = reports();
    const started = startPage({
      el,
      transport: r.transport,
      loader: {
        importer: (url) =>
          url === BASIC ? Promise.resolve(basic.exports) : Promise.reject(new TypeError(`No module at ${url}.`)),
        stylesheet: () => Promise.resolve(),
      },
      measure: { info: () => null, watch: () => () => undefined },
    });
    r.session({ ...hello, board: unfit() });
    await tick();
    r.options();
    await expect(started.ready).rejects.toThrow(unfitness);
    reported.restore();

    expect(el.children).toHaveLength(1);
    expect(el.firstElementChild?.getAttribute("role")).toBe("alert");
    expect(el.textContent).toBe(unfitness);
    expect(basic.renders).toHaveLength(0);
    expect(r.evaluations()).toEqual([]);
  });
});

describe("a board opened later", () => {
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
    // R's board holds a plugin of a class that a package registers once the
    // page has started.
    const s = await started(
      { [BASIC]: basic.exports },
      withPlugins({ strict: { class: "test.pkg-strict_plugin", name: "Strict", state: { zoom: 1 } } }),
    );
    const sent = s.r.sent.length;

    await expect(s.core.store.open("{")).rejects.toThrow("The store refused to open the board\n- The text holds no JSON");
    s.core.plugins.register({
      id: "test.pkg-strict_plugin",
      state: { schema: { type: "object", required: ["zoom"] }, default: { zoom: 1 } },
    });
    const strict = JSON.stringify({
      ...hello.board,
      board: { ...hello.board.board, plugins: { strict: { class: "test.pkg-strict_plugin", name: "Strict", state: {} } } },
    });
    await expect(s.core.store.open(strict)).rejects.toThrow('The state of the plugin "strict"');
    await expect(s.core.store.open(JSON.stringify(unfit()))).rejects.toThrow(unfitness);

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
          signal.addEventListener("abort", () => events.push("the frontend ends"));
          core.scheduler.demand("frontend", (board) => Object.keys(board.blocks));
          el.textContent = core.store.board().name;
          await hold.wait;
          return () => events.push("the frontend cleans up");
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
      ["blockr.session starts, board open: false", "blockr.session ends", "the frontend ends", "the frontend cleans up"].sort(),
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

  test("finds nothing of the board before: no demand, subscription or mount that its frontend or a startup module made", async () => {
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
          core.store.subscribe(() => heard.push(`frontend ${String(at)} hears the store`));
          core.scheduler.subscribe(() => heard.push(`frontend ${String(at)} hears the scheduler`));
          core.scheduler.demand("frontend", at === 1 ? ["b"] : ["a"]);
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

    // What the first frontend and the first startup module held heard
    // nothing, and demanded nothing of the board that opened.
    expect([...new Set(heard)].sort()).toEqual([
      "frontend 2 hears the scheduler",
      "frontend 2 hears the store",
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
    // A startup module that registers its plugin class with a schema that a
    // zoom of any kind fits as the page starts, and with one that only a
    // number fits later, so that the board checks as the page holds it but
    // fails to open as core starts again.
    let initialized = 0;
    const strict = {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          const zoom: JsonSchema = ++initialized === 1 ? {} : { type: "number" };
          core.plugins.register({
            id: "blockr.strict-zoom_plugin",
            state: { schema: { type: "object", properties: { zoom } }, default: { zoom: 1 } },
          });
        },
      },
    };
    const basic = frontEnd();
    const reply = withPlugins(
      { zoom: { class: "blockr.strict-zoom_plugin", name: "Zoom", state: { zoom: 1 } } },
      withStartup("blockr.strict"),
    );
    const s = await started({ "blockr.strict/startup.js?v=1": strict, [BASIC]: basic.exports }, reply);
    s.core.store.apply({ name: "Kept" });
    const board = JSON.stringify({
      ...hello.board,
      board: { ...hello.board.board, plugins: { zoom: { class: "blockr.strict-zoom_plugin", name: "Zoom", state: { zoom: "big" } } } },
    });

    const reported = reports();
    const opening = reopen(s, s.core, board, reply);
    await expect(opening).rejects.toThrow('The state of the plugin "zoom"');
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

describe("board options", () => {
  const STARTUP = "blockr.test/startup.js?v=1";

  // A startup module that registers the board option given, as the package
  // that ships it does.
  function registering(option: Option) {
    return {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          core.options.register(option);
        },
      },
    };
  }

  // Starts the page on the board of the fixture hello, with a package that
  // registers the board option given.
  async function started(option: Option) {
    const s = setup({ [STARTUP]: registering(option), [BASIC]: frontEnd().exports });
    s.r.session(withStartup("blockr.test"));
    await tick();
    s.r.options();
    const core = await s.started.ready;
    await tick();
    return { ...s, core };
  }

  // Opens a board that holds the values given, and answers the hello with
  // which core starts again.
  async function reopen(s: Setup, core: CoreApi, values: Readonly<Record<string, Json>>): Promise<CoreApi> {
    const options = Object.fromEntries(Object.entries(values).map(([id, value]) => [id, { value, version: 1 }]));
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, options } }));
    await tick();
    s.r.answer(withStartup("blockr.test"));
    await opening;
    await tick();
    const next = s.started.core();
    if (next === undefined) throw new Error("No board is open.");
    return next;
  }

  test("have their onChange called as each board opens, with the board's value, which core adds with the default where the board lacks it", async () => {
    const heard: Json[] = [];
    let calls = 0;
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: () => {
        calls++;
        return Promise.resolve(1);
      },
      ui: { render: () => undefined },
      onChange(value) {
        heard.push(value);
      },
    };
    const s = await started(zoom);
    // Though nothing on the page requires zoom.
    expect(heard).toEqual([1]);
    expect(s.core.store.board().options.zoom).toBe(1);

    // A delta that removes the value has core add the default again, which it
    // resolves again, since it keeps none.
    s.core.store.apply({ options: { set: { zoom: 2 } } });
    s.core.store.apply({ options: { unset: ["zoom"] } });
    await tick();
    expect(heard).toEqual([1, 2, 1]);
    expect(s.core.store.board().options.zoom).toBe(1);
    expect(calls).toBe(2);

    // Each board that opens later has it called again.
    const next = await reopen(s, s.core, { zoom: 3 });
    expect(heard).toEqual([1, 2, 1, 3]);
    await reopen(s, next, {});
    expect(heard).toEqual([1, 2, 1, 3, 1]);
    expect(calls).toBe(3);
  });

  test("give their values as the board holds them through value()", async () => {
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: { render: () => undefined },
    };
    const s = await started(zoom);
    // The board of the fixture hello holds n_rows, while nothing on the page
    // requires zoom, so the board holds no value of it.
    expect(s.core.options.value("n_rows")).toBe(50);
    expect(s.core.options.value("zoom")).toBeUndefined();

    s.core.store.apply({ options: { set: { zoom: 3 } } });
    expect(s.core.options.value("zoom")).toBe(3);
  });
});

describe("the frontend and the plugins", () => {
  const STARTUP = "blockr.test/startup.js?v=1";
  const FRONT = "blockr.test/front.js?v=1";

  // A startup module that registers the plugin classes and the board options
  // given, as the package that ships them does.
  function registering(classes: readonly PluginClass[], options: readonly Option[] = []) {
    return {
      api: 1,
      default: {
        initialize({ core }: { core: CoreApi }) {
          for (const option of options) core.options.register(option);
          for (const def of classes) core.plugins.register(def);
        },
      },
    };
  }

  // R's reply to hello with the startup module of blockr.test, which offers
  // the module of a frontend too, and an opening board that holds the plugins
  // given, and the board options given in place of the fixture's.
  function replied(plugins: Readonly<Record<PluginId, Plugin>>, options?: JsonBoard["board"]["options"]): HelloReply {
    const base = withPlugins(plugins, withStartup("blockr.test"));
    return {
      ...base,
      modules: { ...base.modules, "blockr.test-front": { module: FRONT } },
      board: options === undefined ? base.board : { ...base.board, board: { ...base.board.board, options } },
    };
  }

  // A frontend that records which plugin it draws and the board's values of
  // the board options given as its render() runs.
  function recording(...ids: string[]) {
    const seen: unknown[] = [];
    return {
      seen,
      exports: {
        api: 1,
        default: {
          render({ core, plugin }: { core: CoreApi; plugin?: string }) {
            seen.push([plugin, ...ids.map((id) => core.store.board().options[id])]);
          },
        },
      },
    };
  }

  const zoom: Option = {
    id: "zoom",
    label: "Zoom",
    category: "View options",
    schema: { type: "number" },
    default: 1,
    ui: { render: () => undefined },
  };
  const grid: Plugin = { class: "blockr.test-grid_plugin", name: "Grid", state: {} };
  const plugin = (name: string): Plugin => ({ class: `blockr.test-${name}_plugin`, name, state: {} });

  async function started(modules: Readonly<Record<string, unknown>>, reply: HelloReply, given: Partial<StartOptions> = {}) {
    const s = setup(modules, given);
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    await tick();
    return { ...s, core };
  }

  test("draws a board with the frontend that it holds, whose render() gets the plugin's id once the board options of its class are on the board", async () => {
    const front = recording("zoom");
    const basic = frontEnd();
    const frontend: PluginClass = { id: "blockr.test-grid_plugin", frontend: "blockr.test-front", options: ["zoom"] };
    const s = await started(
      { [STARTUP]: registering([frontend], [zoom]), [FRONT]: front.exports, [BASIC]: basic.exports },
      replied({ grid }, {}),
    );

    expect(front.seen).toEqual([["grid", 1]]);
    expect(basic.renders).toHaveLength(0);
    expect(s.imported).toEqual([STARTUP, FRONT]);
    // Core's basic frontend draws nothing, so the board takes none of the
    // board options that it requires, but for dark_mode, whose onChange core
    // calls as every board opens.
    expect(s.core.store.board().options).toEqual({ zoom: 1, dark_mode: "light" });
  });

  test("draws a board that holds no frontend with core's basic frontend, which gets no plugin's id, once its board options are on the board", async () => {
    const basic = recording("show_conditions", "dark_mode");
    const empty = { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } };
    await started({ [BASIC]: basic.exports }, empty);

    expect(basic.seen).toEqual([[undefined, ["warning", "error"], "light"]]);
  });

  test("shows why in the page's element in place of the frontend where a board option that it requires does not go onto the board", async () => {
    const front = frontEnd();
    const frontend: PluginClass = { id: "blockr.test-grid_plugin", frontend: "blockr.test-front", options: ["zoom", "nowhere"] };
    const reported = reports();
    const s = await started(
      { [STARTUP]: registering([frontend], [zoom]), [FRONT]: front.exports },
      replied({ grid }, {}),
    );
    reported.restore();

    const why = 'The board option "nowhere", which the plugin class "blockr.test-grid_plugin" requires, is defined by no package loaded.';
    expect(front.renders).toHaveLength(0);
    expect(s.el.firstElementChild?.getAttribute("role")).toBe("alert");
    expect(s.el.textContent).toBe(why);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([why]);

    // So it does for core's basic frontend, where no package defines a board
    // option that it requires.
    const basic = frontEnd();
    const again = reports();
    const t = await started(
      { [BASIC]: basic.exports },
      { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } },
      { options: (core) => coreOptions(core).filter((option) => option.id !== "show_conditions") },
    );
    again.restore();
    expect(basic.renders).toHaveLength(0);
    expect(t.el.textContent).toBe(
      'The board option "show_conditions", which core\'s basic frontend requires, is defined by no package loaded.',
    );
  });

  test("calls each plugin class's start once the frontend's render() has returned, in the order of the board's plugins, and ends each as the next board opens", async () => {
    const events: string[] = [];
    const drawn = gate();
    const starting = (name: string): PluginClass => ({
      id: `blockr.test-${name}_plugin`,
      start({ core, plugin: id, signal }) {
        events.push(`${id} starts on ${core.store.board().name}`);
        signal.addEventListener("abort", () => events.push(`${id} aborts`));
        return () => {
          events.push(`${id} cleans up`);
        };
      },
    });
    const basic = {
      api: 1,
      default: {
        async render() {
          events.push("the frontend renders");
          await drawn.opened;
          events.push("the frontend has rendered");
        },
      },
    };
    const classes = [starting("dag"), starting("notes"), { id: "blockr.test-quiet_plugin" }];
    const plugins = { notes: plugin("notes"), dag: plugin("dag"), quiet: plugin("quiet") };
    const s = setup({ [STARTUP]: registering(classes), [BASIC]: basic });
    s.r.session(replied(plugins));
    await tick();
    s.r.options();
    await tick();
    expect(events).toEqual(["the frontend renders"]);

    drawn.open();
    const core = await s.started.ready;
    expect(events).toEqual([
      "the frontend renders",
      "the frontend has rendered",
      "notes starts on Iris review",
      "dag starts on Iris review",
    ]);

    events.length = 0;
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Next" } }));
    await tick();
    s.r.answer(replied(plugins));
    await opening;
    expect(events).toEqual([
      "notes aborts",
      "notes cleans up",
      "dag aborts",
      "dag cleans up",
      "the frontend renders",
      "the frontend has rendered",
      "notes starts on Next",
      "dag starts on Next",
    ]);
  });

  test("calls no start of a plugin whose class requires a board option that does not go onto the board, nor goes on where a start throws", async () => {
    const events: string[] = [];
    const classes: PluginClass[] = [
      {
        id: "blockr.test-needy_plugin",
        options: ["zoom", "nowhere"],
        start: () => {
          events.push("needy starts");
        },
      },
      {
        id: "blockr.test-broken_plugin",
        start: () => {
          throw new Error("boom");
        },
      },
      {
        id: "blockr.test-fine_plugin",
        options: ["zoom"],
        start: ({ core }) => {
          events.push(`fine starts with zoom at ${JSON.stringify(core.options.value("zoom"))}`);
        },
      },
    ];
    const reported = reports();
    const s = await started(
      { [STARTUP]: registering(classes, [zoom]), [BASIC]: frontEnd().exports },
      replied({ needy: plugin("needy"), broken: plugin("broken"), fine: plugin("fine") }),
    );
    reported.restore();

    expect(events).toEqual(["fine starts with zoom at 1"]);
    expect(s.core.store.board().options.zoom).toBe(1);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      'The board option "nowhere", which the plugin class "blockr.test-needy_plugin" requires, is defined by no package loaded.',
      'The plugin "needy" does not start, since board options that its class "blockr.test-needy_plugin" requires did not go onto the board.',
      'The start of the plugin class "blockr.test-broken_plugin" failed for the plugin "broken": boom',
    ]);
  });

  test("keeps the board options of every plugin's class on the board while the board is open, those of a class without start or ui included", async () => {
    const storing: PluginClass = {
      id: "blockr.test-store_plugin",
      options: ["zoom"],
      storage: { save: () => Promise.resolve(), open: () => Promise.resolve(null) },
    };
    const s = await started(
      { [STARTUP]: registering([storing], [zoom]), [BASIC]: frontEnd().exports },
      replied({ store: plugin("store") }),
    );
    expect(s.core.store.board().options.zoom).toBe(1);

    s.core.store.apply({ options: { unset: ["zoom"] } });
    await tick();
    expect(s.core.store.board().options.zoom).toBe(1);
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

  test("where R speaks another major of the protocol stops the page, sending R nothing more and saying what to do", async () => {
    let failed: Error | undefined;
    const basic = frontEnd();
    const s = setup({ [BASIC]: basic.exports }, { fail: (error) => (failed = error) });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;
    await s.r.evaluateAll();

    // The scheduler sends an evaluation while the new session starts, which
    // the client holds until R has answered the session's hello.
    s.r.start();
    const sent = s.r.sent.length;
    core.scheduler.invalidate("a");
    await tick(100);
    s.r.answer({ ...hello, protocol: "2.0" });
    await tick(100);

    expect(s.r.sent.slice(sent)).toEqual([]);
    expect(failed?.message).toBe(
      "R speaks version 2.0 of the wire protocol, where this page speaks 1.0. Reload the page, which then loads the client that R serves.",
    );
    expect(basic.renders).toHaveLength(1);
    expect(basic.renders[0]?.signal.aborted).toBe(true);
    expect(basic.cleanups()).toBe(1);

    // R hears nothing more, not even the hello of a later session.
    core.scheduler.invalidate("a");
    s.r.start();
    await tick(100);
    expect(s.r.sent.slice(sent)).toEqual([]);
  });

  test("where R speaks another major of the protocol as the first board opens says why once", async () => {
    const failures: string[] = [];
    const s = setup({ [BASIC]: frontEnd().exports }, { fail: (error) => failures.push(error.message) });
    s.r.session();
    await tick();
    // The session ends before R has answered the blockr options that core
    // asks for as the first board opens.
    s.r.start();
    s.r.answer({ ...hello, protocol: "2.0" });
    const why =
      "R speaks version 2.0 of the wire protocol, where this page speaks 1.0. Reload the page, which then loads the client that R serves.";
    await expect(s.started.ready).rejects.toThrow(why);
    await tick(100);

    expect(failures).toEqual([why]);
    expect(s.r.kinds()).toEqual(["hello", "blockr_options", "hello"]);
  });

  test("that names a package at another version loads no module of that package from its reply, nor one the page has not loaded", async () => {
    const events: string[] = [];
    const shared = { name: "shared at 1.0.0" };
    const first: HelloReply = {
      ...withStartup("blockr.test"),
      modules: {
        ...hello.modules,
        "blockr.test-shared": { module: "blockr.test/shared.js?v=1" },
        "blockr.test-other": { module: "blockr.test/other.js?v=1" },
      },
    };
    const later: HelloReply = {
      ...first,
      packages: { ...first.packages, "blockr.test": "1.1.0" },
      startup_modules: [{ package: "blockr.test", module: "blockr.test/startup.js?v=2" }],
      modules: {
        ...hello.modules,
        "blockr.test-shared": { module: "blockr.test/shared.js?v=2" },
        "blockr.test-other": { module: "blockr.test/other.js?v=2" },
      },
    };
    const changes: (readonly Changed[])[] = [];
    const s = setup(
      {
        "blockr.test/startup.js?v=1": startup("1.0.0", events),
        "blockr.test/startup.js?v=2": startup("1.1.0", events),
        "blockr.test/shared.js?v=1": shared,
        "blockr.test/shared.js?v=2": { name: "shared at 1.1.0" },
        "blockr.test/other.js?v=1": {},
        "blockr.test/other.js?v=2": {},
        [BASIC]: frontEnd().exports,
      },
      { reload: (changed) => changes.push(changed) },
    );
    s.r.session(first);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    expect(await core.module("blockr.test-shared")).toBe(shared);

    s.r.session(later);
    await tick();
    const changed = [{ package: "blockr.test", loaded: "1.0.0", now: "1.1.0" }];
    expect(changes).toEqual([changed]);
    expect(await core.module("blockr.test-shared")).toBe(shared);
    await expect(core.module("blockr.test-other")).rejects.toThrow(
      "The module blockr.test/other.js of blockr.test is not loaded: R now runs blockr.test 1.1.0, where the page loaded 1.0.0. Reload the page to load it.",
    );

    // A board that opens starts core again with the reply before, so that it
    // initializes the startup module that the page loaded.
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Opened" } }));
    await tick();
    s.r.answer(later);
    await opening;
    const next = s.started.core();
    expect(next?.store.board().name).toBe("Opened");
    expect(await next?.module("blockr.test-shared")).toBe(shared);
    expect(changes).toEqual([changed, changed]);
    expect(events).toEqual(["1.0.0 starts, board open: false", "1.0.0 done", "1.0.0 starts, board open: false", "1.0.0 done"]);
    expect(s.imported.filter((url) => url.endsWith("?v=2"))).toEqual([]);
  });
});

describe("a block that serve() serves alone", () => {
  // The reply to the hello of an app that serves the head block alone, with
  // R binding its input data, whose startup module of blockr.session names
  // another board to open.
  const helloBlock = reply("hello-block") as HelloReply;
  const named = { ...helloBlock.board, board: { ...helloBlock.board.board, id: "named_board", name: "From the URL" } };
  const withSession: HelloReply = {
    ...helloBlock,
    packages: { ...helloBlock.packages, "blockr.session": "1.0.0" },
    startup_modules: [{ package: "blockr.session", module: "blockr.session/startup.js?v=1" }],
  };

  test("opens R's board, adds no board options of core's basic frontend, and draws the block in its card before the scheduler sends", async () => {
    const drawn: { core: CoreApi; sent: number }[] = [];
    const s = setup(
      { "blockr.session/startup.js?v=1": startup("blockr.session", [], () => JSON.stringify(named)) },
      {
        alone: true,
        frontEnds: {
          prepare: () => Promise.reject(new Error("Core prepares no frontend.")),
          render: () => Promise.reject(new Error("Core renders no frontend.")),
          card(core) {
            drawn.push({ core, sent: s.r.evaluations().length });
            core.scheduler.demand("blockr.core", Object.keys(core.store.board().blocks));
          },
        },
      },
    );
    s.r.session(withSession);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(core.store.board().name).toBe(helloBlock.board.board.name);
    expect(drawn).toEqual([{ core, sent: 0 }]);
    expect(s.imported).toEqual(["blockr.session/startup.js?v=1"]);

    // The scheduler sends what the card demands, naming the input that R binds
    // by the input alone.
    await tick(100);
    expect(s.r.evaluations().map((request) => request.recipe.inputs)).toEqual([[{ input: "data" }]]);

    // The board takes dark_mode, which switches the page's theme, as every
    // board does, but not show_conditions, which only the basic frontend
    // requires.
    expect(Object.keys(core.store.board().options).sort()).toEqual(["dark_mode", "n_rows"]);
  });
});
