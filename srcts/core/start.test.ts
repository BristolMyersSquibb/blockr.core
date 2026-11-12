// Starting core on a page, and starting it again for each board that opens,
// with R played over a transport whose replies the test sends, on the board of
// the fixture hello, which core's basic frontend draws, since the app of the
// fixture runs it, and in apps that run plugins that a test's package defines,
// a frontend among them, each class and each board option from the module that
// R's reply names for it.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
// The module of core's storage plugin, which the build bundles on its own.
// eslint-disable-next-line no-restricted-imports
import fileStorage from "../modules/file-storage/index.ts";
import type { EvaluateRequest, HelloReply, Reply, Request, Transport, TransportListener } from "../protocol/index.ts";
import { coreOptions } from "../options/index.ts";
import type { Option, PluginClass, Storage } from "../store/index.ts";
import { classModule, optionModule } from "../testing/plugins.ts";
import { reports } from "../testing/reports.ts";
import { hello, optionUrl, pluginUrl, reply, withOptions, withoutPlugins, withPlugins } from "../testing/store.ts";
import type { GivenPlugin } from "../testing/store.ts";
import { startPage } from "../page/index.ts";
import type { JsonBoard, PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, PluginModule } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { own } from "../util/index.ts";
import { pageFrontEnds, start } from "./index.ts";
import type { StartOptions } from "./index.ts";

// The URL at which the fixture hello has R serve the module of the class of
// core's basic frontend, which each test gives as frontEnd() makes it.
const BASIC = "blockr.core/basic.js?v=0f3d8e21";

// The classes of core's own plugins, which the app of the fixture hello runs:
// its basic frontend, its storage plugin and its Edit board plugin.
const BASIC_FRONTEND = "blockr.core-basic_frontend_plugin";
const FILE_STORAGE = "blockr.core-file_storage_plugin";
const EDIT_BOARD = "blockr.core-edit_board_plugin";

// The modules of core's own plugin classes and board options at the URLs that
// the fixture hello names, which every test serves but for core's basic
// frontend: its storage plugin, its Edit board plugin with a ui that draws
// nothing, and its board options.
const CORE: Readonly<Record<string, unknown>> = {
  "blockr.core/file-storage.js?v=8e1f0b2d": { api: 1, default: fileStorage },
  "blockr.core/edit-board.js?v=5c1e7a39": classModule({ ui: { render: () => undefined } }),
  ...Object.fromEntries(
    Object.entries(hello.options).map(([id, entry]) => [entry.module, { api: 1, default: own(coreOptions, id) }]),
  ),
};

// The class of core's basic frontend with the frontend given, as its module
// exports it, with the board options that the class requires.
function basicModule(frontend: FrontendModule) {
  return classModule({ options: ["show_conditions", "dark_mode"], frontend });
}

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
        const given = Object.hasOwn(modules, url) ? modules : CORE;
        return Object.hasOwn(given, url)
          ? Promise.resolve(given[url])
          : Promise.reject(new TypeError(`Failed to fetch dynamically imported module: ${url}`));
      },
      stylesheet: () => Promise.resolve(),
    },
    frontEnds: pageFrontEnds(el),
    measure: { info: () => null, watch: () => () => undefined },
    ...given,
  });
  return { r, el, imported, started };
}

// A frontend that demands every block from within render(), records each
// core it renders with, and returns its cleanup once the test lets it, with the
// module of the class of core's basic frontend that draws with it.
function frontEnd(wait?: Promise<void>) {
  const renders: { core: CoreApi; signal: AbortSignal; el: HTMLElement; plugin: string }[] = [];
  let cleanups = 0;
  const frontend: PluginModule = {
    async render({ el, core, signal, plugin }) {
      renders.push({ core, signal, el, plugin });
      core.scheduler.demand("frontend", (board) => Object.keys(board.blocks));
      el.textContent = core.store.board().name;
      await wait;
      return () => {
        cleanups++;
      };
    },
  };
  return { frontend, exports: basicModule(frontend), renders, cleanups: () => cleanups };
}

// The class of a storage plugin of blockr.session, whose load() names the
// board given to open as the page starts, as the board that the page's URL
// names, and records the CoreApi and the plugin that it gets.
const SESSION = "blockr.session-session_plugin";

function session(load: Storage["load"]) {
  const loads: { core: CoreApi; plugin: string; open: boolean }[] = [];
  const storage: Storage = {
    controls: () => [],
    ...(load === undefined
      ? {}
      : {
          load(args) {
            loads.push({ core: args.core, plugin: args.plugin, open: args.core.store.hasBoard() });
            return load(args);
          },
        }),
  };
  return { loads, exports: classModule({ storage }) };
}

// R's reply to hello, that of the fixture hello unless given, whose app saves
// through blockr.session's storage plugin in place of core's.
function withSession(replied: HelloReply = hello): HelloReply {
  return withPlugins({ [SESSION]: { name: "Session" } }, withoutPlugins(replied, FILE_STORAGE));
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

  test("imports the module of each of the app's plugin classes and board options before it opens the board, once for the page", async () => {
    const held = gate();
    const basic = frontEnd();
    const rows = hello.options.n_rows?.module ?? "";
    const s = setup({ [BASIC]: basic.exports, [rows]: held.opened.then(() => CORE[rows]) });
    s.r.session();
    await tick();
    s.r.options();
    await tick(100);

    // Core waits for every module that R names, at once, before it opens the
    // board, and draws nothing until then.
    const listed = [...Object.values(hello.plugins), ...Object.values(hello.options)].map((entry) => entry.module);
    expect(new Set(s.imported)).toEqual(new Set(listed));
    expect(s.started.core()).toBeUndefined();
    expect(basic.renders).toHaveLength(0);

    held.open();
    const core = await s.started.ready;
    expect(basic.renders).toHaveLength(1);
    // Each registered under the id that R gives it.
    expect(Object.keys(core.plugins.appPlugins())).toEqual([BASIC_FRONTEND, FILE_STORAGE, EDIT_BOARD]);
    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode"]);

    // A board that opens later registers them again, from what the page
    // imported once.
    const imported = s.imported.length;
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Next" } }));
    await tick();
    s.r.answer();
    await opening;
    expect(s.imported).toHaveLength(imported);
    expect(Object.keys(s.started.core()?.options.registered() ?? {})).toEqual(["n_rows", "show_conditions", "dark_mode"]);
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

  test("opens the board that the app's storage plugin names in place of R's, with the app's plugins", async () => {
    // A package with three plugin classes, of which the app runs two. The
    // board that R sent holds the state of both, and the board that the
    // storage plugin names holds the state of one of them and of the class
    // that the app does not run.
    const classes = ["blockr.notes-todo_plugin", "blockr.notes-done_plugin", "blockr.notes-later_plugin"];
    const notes = classModule({ state: { schema: { type: "object" }, default: { text: "" } } });
    const named = JSON.stringify({
      ...hello.board,
      board: {
        ...hello.board.board,
        name: "From the URL",
        plugins: {
          "blockr.notes-todo_plugin": { text: "Call back" },
          "blockr.notes-later_plugin": { text: "Soon" },
        },
      },
    });
    const storage = session(() => Promise.resolve(named));
    const reply = withPlugins(
      {
        "blockr.notes-todo_plugin": { name: "To do", state: { text: "Buy milk" } },
        "blockr.notes-done_plugin": { name: "Done", state: { text: "Paid" } },
      },
      withSession(),
    );
    const s = setup({
      [BASIC]: frontEnd().exports,
      [pluginUrl(SESSION)]: storage.exports,
      ...Object.fromEntries(classes.map((id) => [pluginUrl(id), notes])),
    });
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    // Core called load() once, with the CoreApi of the board, before the
    // store held a board.
    expect(storage.loads).toEqual([{ core, plugin: SESSION, open: false }]);
    expect(core.store.board().name).toBe("From the URL");
    expect(core.store.board().plugins).toEqual({
      "blockr.notes-todo_plugin": { text: "Call back" },
      "blockr.notes-done_plugin": { text: "" },
    });
    expect(Object.keys(core.plugins.appPlugins())).toEqual([
      BASIC_FRONTEND,
      EDIT_BOARD,
      SESSION,
      "blockr.notes-todo_plugin",
      "blockr.notes-done_plugin",
    ]);
  });

  test.each([
    ["load() names none", () => Promise.resolve(undefined), undefined],
    [
      "load() rejects",
      () => Promise.reject(new Error("boom")),
      'The load() of the storage plugin "blockr.session-session_plugin" failed, so core opens the board that R sent: boom',
    ],
    [
      "load() names a board that holds no JSON",
      () => Promise.resolve("{"),
      'The storage plugin "blockr.session-session_plugin" named a board that holds no JSON, so core opens the board that R sent:',
    ],
    [
      "load() resolves with no text",
      () => Promise.resolve(3 as unknown as string),
      'The load() of the storage plugin "blockr.session-session_plugin" resolved with something other than the text of a JSON board, so core opens the board that R sent.',
    ],
  ])("keeps the board R sent where %s, naming why in a notification that concerns the storage plugin's package", async (_, load, message) => {
    const storage = session(load);
    const s = setup({ [BASIC]: frontEnd().exports, [pluginUrl(SESSION)]: storage.exports });
    const reported = reports();
    s.r.session(withSession());
    await tick();
    s.r.options();
    const core = await s.started.ready;
    reported.restore();

    expect(core.store.board().name).toBe("Iris review");
    expect(storage.loads).toHaveLength(1);
    const messages = reported.errors().map((error) => (error as Error).message);
    const named = core.notifications.get().filter((n) => n.class[0] === "board_open_failed");
    if (message === undefined) {
      expect(messages).toEqual([]);
      expect(named).toEqual([]);
    } else {
      expect(messages.some((said) => said.startsWith(message))).toBe(true);
      expect(named.map((n) => [n.severity, n.class, n.origin, n.package, n.message.startsWith(message)])).toEqual([
        ["error", ["board_open_failed", "blockr_error", "error", "condition"], "core", "blockr.session", true],
      ]);
    }
  });

  test("keeps the board R sent where the board that load() names fails to open, naming why in a notification", async () => {
    const storage = session(() => Promise.resolve(JSON.stringify({ format: 1 })));
    const s = setup({ [BASIC]: frontEnd().exports, [pluginUrl(SESSION)]: storage.exports });
    const reported = reports();
    s.r.session(withSession());
    await tick();
    s.r.options();
    const core = await s.started.ready;
    reported.restore();

    expect(core.store.board().name).toBe("Iris review");
    expect(reported.errors()).toHaveLength(1);
    const named = core.notifications.get().filter((n) => n.class[0] === "board_open_failed");
    expect(named.map((n) => n.package)).toEqual(["blockr.session"]);
    expect(named[0]?.message).toContain(
      'The board that the storage plugin "blockr.session-session_plugin" named did not open, so core opens the board that R sent: ',
    );
  });

  test("asks the storage plugin for no board under the load lock, which fixes the app to the board that R sent", async () => {
    const storage = session(() => Promise.resolve(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Named" } })));
    const s = setup({ [BASIC]: frontEnd().exports, [pluginUrl(SESSION)]: storage.exports });
    s.r.session({ ...withSession(), lock: { board: false, load: true } });
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(storage.loads).toEqual([]);
    expect(core.store.board().name).toBe("Iris review");
  });

  test("names a module that fails to load, or whose default export the store refuses, and carries on with the rest", async () => {
    const reply = withOptions(
      ["zoom", "gap"],
      withPlugins({
        "blockr.bad-bad_plugin": { name: "Bad" },
        "blockr.missing-missing_plugin": { name: "Missing" },
        "blockr.good-good_plugin": { name: "Good" },
      }),
    );
    const zoom: Omit<Option, "id"> = { label: "Zoom", category: "View", schema: { type: "number" }, default: 1, ui: { render: () => undefined } };
    const s = setup({
      [BASIC]: frontEnd().exports,
      [pluginUrl("blockr.bad-bad_plugin")]: classModule({ start: "now" } as unknown as Omit<PluginClass, "id">),
      [pluginUrl("blockr.good-good_plugin")]: classModule(),
      [optionUrl("zoom")]: optionModule(zoom),
      [optionUrl("gap")]: { api: 1 },
    });
    const reported = reports();
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    reported.restore();

    expect(core.store.hasBoard()).toBe(true);
    expect(Object.keys(core.plugins.appPlugins())).toEqual([BASIC_FRONTEND, FILE_STORAGE, EDIT_BOARD, "blockr.good-good_plugin"]);
    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode", "zoom"]);
    expect(reported.errors().map((error) => (error as Error).message).sort()).toEqual([
      "The module blockr.missing/blockr.missing-missing_plugin.js of blockr.missing failed to load: Failed to fetch dynamically imported module: blockr.missing/blockr.missing-missing_plugin.js",
      "The module blockr.test/gap.js of blockr.test has no default export, an object.",
      'The plugin class "blockr.bad-bad_plugin" has a start that is no function.',
    ]);
    // Each raises a notification that concerns its plugin class or board
    // option, with the module's package, which the frontend hears as it draws.
    const failed = core.notifications.get().filter((n) => n.class[0] === "module_failed");
    expect(failed.map((n) => [n.plugin ?? n.option, n.package, n.origin, n.message]).sort()).toEqual([
      [
        "blockr.bad-bad_plugin",
        "blockr.bad",
        "core",
        'The plugin class "blockr.bad-bad_plugin" has a start that is no function.',
      ],
      [
        "blockr.missing-missing_plugin",
        "blockr.missing",
        "core",
        "The module blockr.missing/blockr.missing-missing_plugin.js of blockr.missing failed to load: Failed to fetch dynamically imported module: blockr.missing/blockr.missing-missing_plugin.js",
      ],
      ["gap", "blockr.test", "core", "The module blockr.test/gap.js of blockr.test has no default export, an object."],
    ]);
  });

  test("raises the notification that names what the store dropped before the frontend draws, which reads it as it draws", async () => {
    const seen: (readonly string[])[] = [];
    const front = basicModule({
      render({ core }) {
        seen.push(core.notifications.get().map((n) => n.class[0] ?? ""));
      },
    });
    const filter = { registry_id: "blockr.dplyr-filter_block", version: 1, state: {}, name: "Filter", locked: null };
    const dropping: HelloReply = {
      ...hello,
      board: { ...hello.board, board: { ...hello.board.board, blocks: { ...hello.board.board.blocks, x: filter } } },
    };
    const s = setup({ [BASIC]: front });
    s.r.session(dropping);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(seen).toEqual([["board_objects_dropped"]]);
    expect(core.notifications.get().map(({ severity, class: cls, message, origin }) => ({ severity, class: cls, message, origin }))).toEqual([
      {
        severity: "warning",
        class: ["board_objects_dropped", "blockr_warning", "warning", "condition"],
        message: 'The board opened without:\n- block "x" (blockr.dplyr-filter_block)',
        origin: "core",
      },
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
          url === BASIC
            ? Promise.resolve(basic.exports)
            : Object.hasOwn(CORE, url)
              ? Promise.resolve(CORE[url])
              : Promise.reject(new TypeError(`No module at ${url}.`)),
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
    // The app runs a plugin of a class whose state must hold a zoom, as R's
    // board does.
    const s = await started(
      {
        [BASIC]: basic.exports,
        [pluginUrl("test.pkg-strict_plugin")]: classModule({
          state: { schema: { type: "object", required: ["zoom"] }, default: { zoom: 1 } },
        }),
      },
      withPlugins({ "test.pkg-strict_plugin": { name: "Strict", state: { zoom: 1 } } }),
    );
    const sent = s.r.sent.length;

    await expect(s.core.store.open("{")).rejects.toThrow("The store refused to open the board\n- The text holds no JSON");
    const strict = JSON.stringify({
      ...hello.board,
      board: { ...hello.board.board, plugins: { "test.pkg-strict_plugin": {} } },
    });
    await expect(s.core.store.open(strict)).rejects.toThrow('The state of the plugin "test.pkg-strict_plugin"');
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
    const runs: { core: CoreApi; signal: AbortSignal }[] = [];
    const running = classModule({
      start({ core, signal }) {
        events.push("blockr.notes starts");
        runs.push({ core, signal });
        signal.addEventListener("abort", () => events.push("blockr.notes ends"));
        return undefined;
      },
    });
    const drawn = gate();
    const hold: { wait?: Promise<void> } = {};
    const renders: { core: CoreApi; signal: AbortSignal }[] = [];
    const front = basicModule({
      async render({ el, core, signal }) {
        renders.push({ core, signal });
        signal.addEventListener("abort", () => events.push("the frontend ends"));
        core.scheduler.demand("frontend", (board) => Object.keys(board.blocks));
        el.textContent = core.store.board().name;
        await hold.wait;
        return () => events.push("the frontend cleans up");
      },
    });
    const notes = withPlugins({ "blockr.notes-notes_plugin": { name: "Notes" } });
    const s = await started({ [pluginUrl("blockr.notes-notes_plugin")]: running, [BASIC]: front }, notes);
    await s.r.evaluateAll();
    const top = Math.max(...Object.keys(s.core.store.board().blocks).map((id) => s.core.scheduler.status(id)?.revision ?? 0));
    const sent = s.r.sent.length;

    hold.wait = drawn.opened;
    const opening = s.core.store.open(named("Opened"));
    await tick();

    // One abort ended what core ran for the board, and core waits for R to
    // answer the hello with which it starts again.
    expect([...events].sort()).toEqual(
      ["blockr.notes starts", "blockr.notes ends", "the frontend ends", "the frontend cleans up"].sort(),
    );
    expect(s.r.sent.slice(sent).map((request) => request.kind)).toEqual(["hello"]);
    expect(s.started.core()).toBeUndefined();
    expect(s.el.textContent).toBe("Loading the board…");
    expect(() => {
      s.core.store.apply({ name: "Late" });
    }).toThrow("Another board opened in a new store, so this store changes its board no more.");

    s.r.answer(notes);
    await tick(100);
    const next = s.started.core();
    expect(next).toBeDefined();
    expect(next).not.toBe(s.core);
    expect(next?.store).not.toBe(s.core.store);
    expect(next?.store.board().name).toBe("Opened");
    expect(next?.store.canUndo()).toBe(false);
    expect(renders.map((entry) => entry.core)).toEqual([s.core, next]);
    expect(s.el.textContent).toBe("Opened");
    // The scheduler holds its evaluations until render() has resolved.
    expect(s.r.sent.slice(sent).map((request) => request.kind)).toEqual(["hello"]);

    drawn.open();
    expect(await opening).toEqual([]);
    // The plugin starts again once the frontend has rendered.
    expect(runs.map((entry) => entry.core)).toEqual([s.core, next]);
    expect(runs[1]?.signal.aborted).toBe(false);
    await tick(100);
    const evaluated = s.r.evaluations().filter((request) => s.r.sent.indexOf(request) >= sent);
    expect(evaluated.length).toBeGreaterThan(0);
    // Every revision of the board that opened is above any the page gave, and
    // R forgets nothing, since the hello dropped every result.
    for (const request of evaluated) expect(request.revision).toBeGreaterThan(top);
    expect(s.r.kinds()).not.toContain("forget");
  });

  test("finds nothing of the board before: no demand, subscription or mount that its frontend or a plugin made", async () => {
    const heard: string[] = [];
    const mounted: AbortSignal[] = [];
    let starts = 0;
    const running = classModule({
      start({ core }) {
        const at = ++starts;
        core.store.subscribe(() => heard.push(`plugin ${String(at)} hears the store`));
        core.scheduler.subscribe(() => heard.push(`plugin ${String(at)} hears the scheduler`));
        // As the page starts, it demands c, which nothing demands later.
        if (at === 1) core.scheduler.demand("blockr.notes", ["c"]);
        return undefined;
      },
    });
    let renders = 0;
    const front = basicModule({
      render({ el, core }) {
        const at = ++renders;
        core.store.subscribe(() => heard.push(`frontend ${String(at)} hears the store`));
        core.scheduler.subscribe(() => heard.push(`frontend ${String(at)} hears the scheduler`));
        core.scheduler.demand("frontend", at === 1 ? ["b"] : ["a"]);
        const card = document.createElement("div");
        el.append(card);
        core.mountBlock(card, "b");
        return undefined;
      },
    });
    const head = {
      api: 1,
      default: {
        render({ signal }: { signal: AbortSignal }) {
          mounted.push(signal);
        },
      },
    };
    const notes = withPlugins({ "blockr.notes-notes_plugin": { name: "Notes" } });
    const s = await started(
      { [pluginUrl("blockr.notes-notes_plugin")]: running, [BASIC]: front, "blockr.core/head-block.js?v=9a41f2c7": head },
      notes,
    );
    await s.r.evaluateAll();
    expect(new Set(s.r.evaluations().map((request) => request.block))).toEqual(new Set(["a", "b", "c"]));
    expect(mounted).toHaveLength(1);

    await reopen(s, s.core, named("Opened"), notes);
    const sent = s.r.sent.length;
    await s.r.evaluateAll();
    const next = s.started.core();
    heard.length = 0;
    next?.store.apply({ blocks: { mod: { a: { name: "Renamed" } } } });
    next?.scheduler.invalidate("a");
    await tick(100);

    // What the first frontend and the first plugin held heard nothing, and
    // demanded nothing of the board that opened.
    expect([...new Set(heard)].sort()).toEqual([
      "frontend 2 hears the scheduler",
      "frontend 2 hears the store",
      "plugin 2 hears the scheduler",
      "plugin 2 hears the store",
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
    // A plugin class whose module the first session serves with a schema that
    // a zoom of any kind fits, and the next, as after R restarts with the
    // package rebuilt, with one that only a number fits, so that the board
    // checks as the page holds it but fails to open as core starts again.
    const lax = classModule({
      state: { schema: { type: "object", properties: { zoom: {} } }, default: { zoom: 1 } },
    });
    const strict = classModule({
      state: { schema: { type: "object", properties: { zoom: { type: "number" } as JsonSchema } }, default: { zoom: 1 } },
    });
    const basic = frontEnd();
    const reply = withPlugins({ "blockr.strict-zoom_plugin": { name: "Zoom", state: { zoom: 1 } } });
    const entry = reply.plugins["blockr.strict-zoom_plugin"];
    if (entry === undefined) throw new Error("The reply names no zoom plugin.");
    const rebuilt: HelloReply = {
      ...reply,
      plugins: { ...reply.plugins, "blockr.strict-zoom_plugin": { ...entry, module: `${entry.module}?v=2` } },
    };
    const s = await started({ [entry.module]: lax, [`${entry.module}?v=2`]: strict, [BASIC]: basic.exports }, reply);
    s.core.store.apply({ name: "Kept" });
    const board = JSON.stringify({
      ...hello.board,
      board: { ...hello.board.board, plugins: { "blockr.strict-zoom_plugin": { zoom: "big" } } },
    });

    const reported = reports();
    const opening = reopen(s, s.core, board, rebuilt);
    await expect(opening).rejects.toThrow('The state of the plugin "blockr.strict-zoom_plugin"');
    reported.restore();

    expect(s.started.core()?.store.board().name).toBe("Kept");
    expect(basic.renders).toHaveLength(2);
    expect(basic.renders[1]?.signal.aborted).toBe(false);
    expect(s.el.textContent).toBe("Kept");
    // The opening has ended the code that called open(), so core tells the
    // user itself, in a notification that concerns the board.
    const failed = s.started.core()?.notifications.get().filter((n) => n.class[0] === "board_open_failed") ?? [];
    expect(failed.map((n) => [n.origin, n.package])).toEqual([["core", undefined]]);
    expect(failed[0]?.message).toMatch(
      /^The board did not open, so core opened the board before it again: .*The state of the plugin "blockr\.strict-zoom_plugin"/s,
    );
  });

  test("migrates a block's state by the migrate that its class's UI module exports, which core imports before the board opens", async () => {
    // The head block's class at version 2 of its state, whose UI module
    // converts the n of version 1, a string, into a number.
    const entry = hello.block_registry["blockr.core-head_block"];
    if (entry === undefined) throw new Error("The fixture has no head block.");
    // The board of the head block's blocks at the version given.
    const at = (version: number, n?: string): JsonBoard["board"]["blocks"] =>
      Object.fromEntries(
        Object.entries(hello.board.board.blocks).map(([id, block]) => [
          id,
          block.registry_id === "blockr.core-head_block"
            ? { ...block, version, state: n === undefined ? block.state : { ...block.state, n } }
            : block,
        ]),
      );
    const versioned: HelloReply = {
      ...hello,
      block_registry: { ...hello.block_registry, "blockr.core-head_block": { ...entry, version: 2 } },
      board: { ...hello.board, board: { ...hello.board.board, blocks: at(2) } },
    };
    const head = {
      api: 1,
      default: { render: () => undefined },
      migrate: (state: Record<string, Json>, version: number) => ({ ...state, n: version === 1 ? Number(state.n) : state.n }),
    };
    const s = await started({ [BASIC]: frontEnd().exports, [entry.module]: head }, versioned);
    const imported = s.imported.length;

    // R's board records the head block at version 2, so the store migrates
    // nothing, and core imports no UI module for it.
    expect(s.imported.filter((url) => url === entry.module)).toEqual([]);

    const old = { ...hello.board, board: { ...hello.board.board, name: "Older", blocks: at(1, "3") } };
    await reopen(s, s.core, JSON.stringify(old), versioned);
    const next = s.started.core();
    const opened = Object.values(next?.store.board().blocks ?? {}).find((block) => block.registry_id === "blockr.core-head_block");

    expect(next?.store.board().name).toBe("Older");
    expect(opened?.state.n).toBe(3);
    // Once, for the check on the page and the opening alike.
    expect(s.imported.slice(imported)).toEqual([entry.module]);
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

  test("asks the storage plugin for no board as it opens, since only the page's start names one", async () => {
    const storage = session(() => Promise.resolve(named("From the URL")));
    const s = await started({ [pluginUrl(SESSION)]: storage.exports, [BASIC]: frontEnd().exports }, withSession());
    expect(s.core.store.board().name).toBe("From the URL");

    await reopen(s, s.core, named("Opened"), withSession());

    expect(s.started.core()?.store.board().name).toBe("Opened");
    expect(storage.loads).toHaveLength(1);
  });
});

describe("board options", () => {
  // Starts the page on the board of the fixture hello, with a package that
  // lists the board option given.
  async function started({ id, ...option }: Option) {
    const s = setup({ [optionUrl(id)]: optionModule(option), [BASIC]: frontEnd().exports });
    s.r.session(withOptions([id]));
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
    s.r.answer(withOptions(["zoom"]));
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

  test("whose onChange throws raise a notification that concerns the board option, with its package, until its next call returns", async () => {
    let failing = true;
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: { render: () => undefined },
      onChange(value) {
        if (failing) throw new Error(`No zoom of ${JSON.stringify(value)}.`);
      },
    };
    const reported = reports();
    const s = await started(zoom);
    const changes = (): readonly unknown[] =>
      s.core.notifications
        .get()
        .filter((n) => n.class[0] === "on_change_failed")
        .map((n) => [n.option, n.package, n.message]);
    expect(changes()).toEqual([["zoom", "blockr.test", 'The onChange of the board option "zoom" failed: No zoom of 1.']]);

    s.core.store.apply({ options: { set: { zoom: 2 } } });
    expect(changes()).toEqual([["zoom", "blockr.test", 'The onChange of the board option "zoom" failed: No zoom of 2.']]);

    failing = false;
    s.core.store.apply({ options: { set: { zoom: 3 } } });
    expect(changes()).toEqual([]);
    reported.restore();
    expect(reported.errors().map(String)).toEqual(["Error: No zoom of 1.", "Error: No zoom of 2."]);
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
  // R's reply to hello for an app whose blockr.test lists the board option
  // zoom, and whose app runs the plugins given beside core's, but for those of
  // core's classes given in `instead`, and an opening board with the board
  // options given in place of the fixture's.
  function replied(
    plugins: Readonly<Record<PluginClassId, GivenPlugin>>,
    options?: JsonBoard["board"]["options"],
    instead: readonly PluginClassId[] = [],
  ): HelloReply {
    const base = withOptions(["zoom"], withPlugins(plugins, withoutPlugins(hello, ...instead)));
    return {
      ...base,
      board: options === undefined ? base.board : { ...base.board, board: { ...base.board.board, options } },
    };
  }

  // A frontend that records which plugin it draws and the board's values of
  // the board options given as its render() runs.
  function recording(...ids: string[]) {
    const seen: unknown[] = [];
    const frontend: PluginModule = {
      render({ core, plugin }) {
        seen.push([plugin, ...ids.map((id) => core.store.board().options[id])]);
        return undefined;
      },
    };
    return { seen, frontend };
  }

  const zoom = optionModule({
    label: "Zoom",
    category: "View options",
    schema: { type: "number" },
    default: 1,
    ui: { render: () => undefined },
  });
  const ZOOM = optionUrl("zoom");

  // The modules of blockr.test's plugin classes given, by the URLs that the
  // reply of replied() names, with that of its board option zoom.
  function listed(classes: readonly PluginClass[]): Readonly<Record<string, unknown>> {
    return { [ZOOM]: zoom, ...Object.fromEntries(classes.map(({ id, ...def }) => [pluginUrl(id), classModule(def)])) };
  }
  const GRID = "blockr.test-grid_plugin";
  // The app's plugin of blockr.test's class of the name given, as the reply
  // names it.
  const plugin = (name: string): Readonly<Record<PluginClassId, GivenPlugin>> => ({ [`blockr.test-${name}_plugin`]: { name } });

  async function started(modules: Readonly<Record<string, unknown>>, reply: HelloReply, given: Partial<StartOptions> = {}) {
    const s = setup(modules, given);
    s.r.session(reply);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    await tick();
    return { ...s, core };
  }

  test("draws every board with the app's frontend, whose render() gets the id of its class once the board options of its class are on the board", async () => {
    const front = recording("zoom");
    const basic = frontEnd();
    const frontend: PluginClass = { id: GRID, frontend: front.frontend, options: ["zoom"] };
    // An app whose frontend takes the place of core's basic frontend.
    const reply = replied({ [GRID]: { name: "Grid" } }, {}, [BASIC_FRONTEND]);
    const s = await started({ ...listed([frontend]), [BASIC]: basic.exports }, reply);

    expect(front.seen).toEqual([[GRID, 1]]);
    expect(basic.renders).toHaveLength(0);
    // Core imports the module of each class that runs, and none of core's
    // basic frontend, which does not.
    expect(s.imported).toContain(pluginUrl(GRID));
    expect(s.imported).not.toContain(BASIC);
    // The app runs no basic frontend, so the board takes none of the board
    // options that it requires, but for dark_mode, whose onChange core calls
    // as every board opens.
    expect(s.core.store.board().options).toEqual({ zoom: 1, dark_mode: "light" });
    // The frontend's state stays out of the board, since its class keeps none.
    expect(s.core.store.board().plugins).toEqual({});

    // A board that opens later, whatever plugins' state it holds, is drawn
    // with the same frontend.
    const opening = s.core.store.open(
      JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Next", plugins: { [BASIC_FRONTEND]: {} } } }),
    );
    await tick();
    s.r.answer(reply);
    await opening;
    expect(front.seen).toEqual([
      [GRID, 1],
      [GRID, 1],
    ]);
    expect(basic.renders).toHaveLength(0);
  });

  test("draws the board of a core app with core's basic frontend, which gets the id of its class, once its board options are on the board", async () => {
    const basic = recording("show_conditions", "dark_mode");
    const empty = { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } };
    await started({ [BASIC]: basicModule(basic.frontend) }, empty);

    expect(basic.seen).toEqual([[BASIC_FRONTEND, ["message", "warning", "error"], "light"]]);
  });

  test("calls the frontend's render() with what R's reply to hello holds in frontend, in args, for every board that opens", async () => {
    // A frontend that keeps the args that each render() gets.
    const keeping = () => {
      const seen: Readonly<Record<string, Json>>[] = [];
      const frontend: FrontendModule = {
        render({ args }) {
          seen.push(args);
          return undefined;
        },
      };
      return { seen, frontend };
    };

    // Core's basic frontend takes no argument of serve(), so it gets none in a
    // core app.
    const basic = keeping();
    await started({ [BASIC]: basicModule(basic.frontend) }, hello);
    expect(basic.seen).toEqual([{}]);

    // An app whose frontend takes a title from serve().
    const front = keeping();
    const frontend: PluginClass = { id: GRID, frontend: front.frontend };
    const reply: HelloReply = {
      ...replied({ [GRID]: { name: "Grid" } }, undefined, [BASIC_FRONTEND]),
      frontend: { title: "Trials" },
    };
    const s = await started(listed([frontend]), reply);
    expect(front.seen).toEqual([{ title: "Trials" }]);

    // A board that opens later, whatever it holds, gets the same, from R's
    // reply to the hello with which core starts again.
    const opening = s.core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Next" } }));
    await tick();
    s.r.answer(reply);
    await opening;
    expect(front.seen).toEqual([{ title: "Trials" }, { title: "Trials" }]);
  });

  test("shows why in the page's element in place of the board where the app runs no frontend", async () => {
    const basic = frontEnd();
    const reported = reports();
    const s = await started({ [BASIC]: basic.exports }, withoutPlugins(hello, BASIC_FRONTEND));
    reported.restore();

    const why = "The app runs no frontend that draws the board, since none of the app's plugins is a frontend.";
    expect(basic.renders).toHaveLength(0);
    expect(s.el.firstElementChild?.getAttribute("role")).toBe("alert");
    expect(s.el.textContent).toBe(why);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([why]);
    expect(s.core.store.board().name).toBe("Iris review");

    // So it does where the module of the class of the app's frontend fails to
    // load.
    const again = reports();
    const t = await started(listed([]), replied({ [GRID]: { name: "Grid" } }, undefined, [BASIC_FRONTEND]));
    again.restore();
    expect(t.el.textContent).toBe(
      'The app runs no frontend that draws the board, since core registered no class of the app\'s plugin "blockr.test-grid_plugin".',
    );
  });

  test("shows why in the page's element in place of the frontend where a board option that it requires does not go onto the board", async () => {
    const front = frontEnd();
    const frontend: PluginClass = { id: GRID, frontend: front.frontend, options: ["zoom", "nowhere"] };
    const reported = reports();
    const s = await started(listed([frontend]), replied({ [GRID]: { name: "Grid" } }, {}, [BASIC_FRONTEND]));
    reported.restore();

    const why = 'The board option "nowhere", which the plugin class "blockr.test-grid_plugin" requires, is defined by no package loaded.';
    expect(front.renders).toHaveLength(0);
    expect(s.el.firstElementChild?.getAttribute("role")).toBe("alert");
    expect(s.el.textContent).toBe(why);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([why]);

    // So it does for core's basic frontend, where no package lists a board
    // option that it requires.
    const basic = frontEnd();
    const again = reports();
    const lacking = Object.fromEntries(Object.entries(hello.options).filter(([id]) => id !== "show_conditions"));
    const t = await started(
      { [BASIC]: basic.exports },
      { ...hello, options: lacking, board: { ...hello.board, board: { ...hello.board.board, options: {} } } },
    );
    again.restore();
    expect(basic.renders).toHaveLength(0);
    expect(t.el.textContent).toBe(
      'The board option "show_conditions", which the plugin class "blockr.core-basic_frontend_plugin" requires, is defined by no package loaded.',
    );
  });

  test("calls each plugin class's start once the frontend's render() has returned, in the order of the app's plugins, and ends each as the next board opens", async () => {
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
    const basic = basicModule({
      async render() {
        events.push("the frontend renders");
        await drawn.opened;
        events.push("the frontend has rendered");
        return undefined;
      },
    });
    const classes = [starting("dag"), starting("notes"), { id: "blockr.test-quiet_plugin" }];
    const plugins = { ...plugin("notes"), ...plugin("dag"), ...plugin("quiet") };
    const s = setup({ ...listed(classes), [BASIC]: basic });
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
      "blockr.test-notes_plugin starts on Iris review",
      "blockr.test-dag_plugin starts on Iris review",
    ]);

    events.length = 0;
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Next" } }));
    await tick();
    s.r.answer(replied(plugins));
    await opening;
    expect(events).toEqual([
      "blockr.test-notes_plugin aborts",
      "blockr.test-notes_plugin cleans up",
      "blockr.test-dag_plugin aborts",
      "blockr.test-dag_plugin cleans up",
      "the frontend renders",
      "the frontend has rendered",
      "blockr.test-notes_plugin starts on Next",
      "blockr.test-dag_plugin starts on Next",
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
      { ...listed(classes), [BASIC]: frontEnd().exports },
      replied({ ...plugin("needy"), ...plugin("broken"), ...plugin("fine") }),
    );
    reported.restore();

    expect(events).toEqual(["fine starts with zoom at 1"]);
    expect(s.core.store.board().options.zoom).toBe(1);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      'The board option "nowhere", which the plugin class "blockr.test-needy_plugin" requires, is defined by no package loaded.',
      'The plugin "blockr.test-needy_plugin" does not start, since board options that its class requires did not go onto the board.',
      'The start of the plugin class "blockr.test-broken_plugin" failed: boom',
    ]);
  });

  test("keeps the board options of every plugin's class on the board while the board is open, those of a class without start or ui included", async () => {
    const storing: PluginClass = {
      id: "blockr.test-store_plugin",
      options: ["zoom"],
      storage: { controls: () => [] },
    };
    const s = await started(
      { ...listed([storing]), [BASIC]: frontEnd().exports },
      replied(plugin("store"), undefined, [FILE_STORAGE]),
    );
    expect(s.core.store.board().options.zoom).toBe(1);

    s.core.store.apply({ options: { unset: ["zoom"] } });
    await tick();
    expect(s.core.store.board().options.zoom).toBe(1);
  });
});

describe("a later session", () => {
  test("gives every block a new revision, and asks for a reload through a notification while R runs a package at another version", async () => {
    const s = setup({ [BASIC]: frontEnd().exports });
    s.r.session();
    await tick();
    s.r.options();
    const core = await s.started.ready;
    const revision = core.scheduler.status("a")?.revision ?? 0;
    const reloads = (): readonly string[] =>
      core.notifications
        .get()
        .filter((n) => n.class[0] === "page_reload_needed")
        .map((n) => n.message);

    s.r.session();
    await tick();
    expect(core.scheduler.status("a")?.revision).toBeGreaterThan(revision);
    expect(reloads()).toEqual([]);

    s.r.session({ ...hello, packages: { "blockr.core": "0.2.1" } });
    await tick();
    expect(core.notifications.get().filter((n) => n.class[0] === "page_reload_needed")).toEqual([
      {
        id: expect.any(String) as string,
        severity: "warning",
        class: ["page_reload_needed", "blockr_warning", "warning", "condition"],
        message:
          "R now runs other versions of packages than this page loaded (blockr.core 0.2.0 to 0.2.1). Save the board, then reload the page.",
        origin: "core",
      },
    ]);

    // A reply that names another version again replaces it, and one that
    // names those the page loaded withdraws it.
    s.r.session({ ...hello, packages: { "blockr.core": "0.2.2" } });
    await tick();
    expect(reloads()).toEqual([
      "R now runs other versions of packages than this page loaded (blockr.core 0.2.0 to 0.2.2). Save the board, then reload the page.",
    ]);
    s.r.session();
    await tick();
    expect(reloads()).toEqual([]);
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
    const versioned = (version: string) =>
      classModule({
        start: () => {
          events.push(`${version} starts`);
          return undefined;
        },
      });
    const notes = withPlugins({ "blockr.test-notes_plugin": { name: "Notes" } });
    const first: HelloReply = {
      ...notes,
      packages: { ...notes.packages, "blockr.test": "1.0.0" },
      plugins: { ...notes.plugins, "blockr.test-notes_plugin": { name: "Notes", version: 1, module: "blockr.test/notes.js?v=1" } },
      modules: {
        ...hello.modules,
        "blockr.test-shared": { module: "blockr.test/shared.js?v=1" },
        "blockr.test-other": { module: "blockr.test/other.js?v=1" },
      },
    };
    const later: HelloReply = {
      ...first,
      packages: { ...first.packages, "blockr.test": "1.1.0" },
      plugins: { ...first.plugins, "blockr.test-notes_plugin": { name: "Notes", version: 1, module: "blockr.test/notes.js?v=2" } },
      modules: {
        ...hello.modules,
        "blockr.test-shared": { module: "blockr.test/shared.js?v=2" },
        "blockr.test-other": { module: "blockr.test/other.js?v=2" },
      },
    };
    const s = setup({
      "blockr.test/notes.js?v=1": versioned("1.0.0"),
      "blockr.test/notes.js?v=2": versioned("1.1.0"),
      "blockr.test/shared.js?v=1": shared,
      "blockr.test/shared.js?v=2": { name: "shared at 1.1.0" },
      "blockr.test/other.js?v=1": {},
      "blockr.test/other.js?v=2": {},
      [BASIC]: frontEnd().exports,
    });
    const reloads = (core: CoreApi | undefined): readonly string[] =>
      (core?.notifications.get() ?? []).filter((n) => n.class[0] === "page_reload_needed").map((n) => n.message);
    const asked = "R now runs other versions of packages than this page loaded (blockr.test 1.0.0 to 1.1.0). Save the board, then reload the page.";
    s.r.session(first);
    await tick();
    s.r.options();
    const core = await s.started.ready;
    expect(await core.module("blockr.test-shared")).toBe(shared);

    s.r.session(later);
    await tick();
    expect(reloads(core)).toEqual([asked]);
    expect(await core.module("blockr.test-shared")).toBe(shared);
    await expect(core.module("blockr.test-other")).rejects.toThrow(
      "The module blockr.test/other.js of blockr.test is not loaded: R now runs blockr.test 1.1.0, where the page loaded 1.0.0. Reload the page to load it.",
    );

    // A board that opens starts core again with the reply before, so that it
    // registers the class from the module that the page loaded.
    const opening = core.store.open(JSON.stringify({ ...hello.board, board: { ...hello.board.board, name: "Opened" } }));
    await tick();
    s.r.answer(later);
    await opening;
    const next = s.started.core();
    expect(next?.store.board().name).toBe("Opened");
    expect(await next?.module("blockr.test-shared")).toBe(shared);
    // The board that opens asks again, while R runs the other version.
    expect(reloads(next)).toEqual([asked]);
    expect(events).toEqual(["1.0.0 starts", "1.0.0 starts"]);
    expect(s.imported.filter((url) => url.endsWith("?v=2"))).toEqual([]);
  });
});

describe("a block that serve() serves alone", () => {
  // The reply to the hello of an app that serves the head block alone, with
  // R binding its input data, which runs no plugin.
  const helloBlock = reply("hello-block") as HelloReply;

  test("opens R's board, runs no plugin, and draws the block in its card before the scheduler sends", async () => {
    const drawn: { core: CoreApi; sent: number }[] = [];
    const s = setup(
      {},
      {
        alone: true,
        frontEnds: {
          prepare: () => {
            throw new Error("Core prepares no frontend.");
          },
          render: () => Promise.reject(new Error("Core renders no frontend.")),
          card(core) {
            drawn.push({ core, sent: s.r.evaluations().length });
            core.scheduler.demand("blockr.core", Object.keys(core.store.board().blocks));
          },
        },
      },
    );
    s.r.session(helloBlock);
    await tick();
    s.r.options();
    const core = await s.started.ready;

    expect(core.store.board().name).toBe(helloBlock.board.board.name);
    expect(drawn).toEqual([{ core, sent: 0 }]);
    // R names no plugin for a block served alone, so none runs, and core
    // imports the modules of core's board options alone.
    expect(new Set(s.imported)).toEqual(new Set(Object.values(helloBlock.options).map((entry) => entry.module)));
    expect(core.plugins.appPlugins()).toEqual({});
    expect(core.plugins.storage()).toBeUndefined();

    // The scheduler sends what the card demands, naming the input that R binds
    // by the input alone.
    await tick(100);
    expect(s.r.evaluations().map((request) => request.recipe.inputs)).toEqual([[{ input: "data" }]]);

    // The board takes dark_mode, which switches the page's theme, as every
    // board does, but not show_conditions, which only the basic frontend
    // requires, which does not run.
    expect(Object.keys(core.store.board().options).sort()).toEqual(["dark_mode", "n_rows"]);
  });
});
