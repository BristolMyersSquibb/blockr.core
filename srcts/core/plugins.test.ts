// The app's plugins on the CoreApi for tests: which run and of which class, as
// R's reply to hello names them, each from the module of its class, the
// storage whose controls a frontend draws, mounting a plugin's UI where a
// frontend places it, and what a frontend offers other plugins, the
// notifications they raise and which block has focus.

import { describe, expect, test } from "vitest";
// The modules of core's own plugin classes, which the build bundles on their
// own.
// eslint-disable-next-line no-restricted-imports
import basicFrontend from "../modules/basic/index.tsx";
// eslint-disable-next-line no-restricted-imports
import editBoard from "../modules/edit-board/index.tsx";
import type { HelloReply, Lock } from "../protocol/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { classModule, listedModules, pluginUi } from "../testing/plugins.ts";
import { reports } from "../testing/reports.ts";
import { withOptions, withoutPlugins, withPlugins } from "../testing/store.ts";
import type { GivenPlugin } from "../testing/store.ts";
import type { CoreApi, Notification, Raised } from "../types/core-api.ts";

// The classes of core's own plugins, which the app of the fixture hello runs.
const BASIC = "blockr.core-basic_frontend_plugin";
const FILE = "blockr.core-file_storage_plugin";
const EDIT = "blockr.core-edit_board_plugin";

const saving: PluginClass = {
  id: "blockr.test-session_plugin",
  storage: { controls: () => [] },
};
const zoom: Option = {
  id: "zoom",
  label: "Zoom",
  category: "View options",
  schema: { type: "number" },
  default: 1,
  ui: { render: () => undefined },
};

// The CoreApi for tests on the board of the fixture hello, whose app runs core's
// plugins and the plugins given too, by the ids of their classes, of the classes
// given, each the default export of its class's module, and whose reply names
// the board options given, each the default export of its module.
function stubbed(
  classes: readonly PluginClass[],
  plugins: Readonly<Record<string, GivenPlugin>>,
  options: StubCoreOptions & { readonly options?: readonly Option[] } = {},
  replied: HelloReply = HELLO,
) {
  const given = options.options ?? [];
  return stubCore({
    ...options,
    hello: withOptions(
      given.map(({ id }) => id),
      withPlugins(plugins, replied),
    ),
    modules: { ...listedModules(classes, given), ...options.modules },
  });
}

// Opens the fixture's board again, under another name, which starts core
// again, and resolves with the CoreApi of the board that opens.
async function reopen(s: Awaited<ReturnType<typeof stubCore>>): Promise<CoreApi> {
  await s.core.store.open(JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Next" } }));
  await s.settled();
  return s.core;
}

describe("the plugins that run", () => {
  test("are the app's plugins that the reply names, each of a class whose module loads, which pluginClass() gives", async () => {
    const ui = pluginUi();
    const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: ui.ui };
    // A class whose plugin draws with the ui of the notes' class.
    const report: PluginClass = { id: "blockr.test-report_plugin", ui: ui.ui };
    const reported = reports();
    const s = await stubbed([notes, report, saving], {
      [notes.id]: { name: "Notes" },
      [report.id]: { name: "Report" },
      [saving.id]: { name: "Session" },
      "blockr.gone-gone_plugin": { name: "Gone" },
    });
    reported.restore();
    const { core } = s;

    // In the order of the reply, core's own first, but for the plugin whose
    // class's module fails to load, which core names.
    expect(Object.keys(core.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT, notes.id, report.id, saving.id]);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      "The module blockr.gone/blockr.gone-gone_plugin.js of blockr.gone failed to load: The test gives no module blockr.gone/blockr.gone-gone_plugin.js.",
    ]);
    expect(core.plugins.appPlugins()[report.id]).toEqual({ name: "Report" });
    expect(core.plugins.appPlugins()[BASIC]).toEqual({ name: "Basic frontend" });
    expect(core.plugins.appPlugins()[EDIT]).toEqual({ name: "Edit board" });
    expect(core.plugins.appPlugins()).toBe(core.plugins.appPlugins());
    // The default export of the class's module, under the id that R gives it.
    expect(core.plugins.pluginClass(report.id)).toEqual(report);
    expect(core.plugins.pluginClass(report.id).ui).toBe(ui.ui);
    expect(core.plugins.pluginClass(BASIC).frontend).toBe(basicFrontend.frontend);
    // Core's Edit board plugin has a ui alone, so that the board holds nothing
    // for it.
    expect(core.plugins.pluginClass(EDIT)).toEqual({ id: EDIT, ui: editBoard.ui });
    expect(Object.keys(core.store.board().plugins)).not.toContain(EDIT);
    expect(() => core.plugins.pluginClass("blockr.gone-gone_plugin")).toThrow(
      'The app runs no plugin "blockr.gone-gone_plugin" on the open board.',
    );
    s.stop();
  });

  test("register the commands of their classes, which change their plugins' state", async () => {
    const notes: PluginClass = {
      id: "blockr.test-notes_plugin",
      state: { schema: { type: "object" }, default: { text: "" } },
      commands: [
        {
          id: "blockr.test-clear_notes",
          label: "Clear the notes",
          run: () => ({ patches: [{ op: "replace", path: "/plugins/blockr.test-notes_plugin/text", value: "" }] }),
        },
      ],
    };
    const s = await stubbed([notes], { [notes.id]: { name: "Notes", state: { text: "To do" } } });

    expect(Object.keys(s.core.commands.registered())).toContain("blockr.test-clear_notes");
    await s.core.commands.run("blockr.test-clear_notes");
    expect(s.core.store.board().plugins[notes.id]).toEqual({ text: "" });

    // As each board opens, since an opening ends the commands of the board
    // before.
    const next = await reopen(s);
    expect(Object.keys(next.commands.registered())).toContain("blockr.test-clear_notes");
    s.stop();
  });

  test("hand the commands of their classes the app's locks, the blockr options that R gives and ids in the shape that rand_id names", async () => {
    const got: { lock?: Lock; limit?: unknown } = {};
    const notes: PluginClass = {
      id: "blockr.test-notes_plugin",
      state: { schema: { type: "object" }, default: {} },
      commands: [
        {
          id: "blockr.test-add_note",
          label: "Add a note",
          enabled(_board, _args, { lock }) {
            got.lock = lock;
            return true;
          },
          prepare: (_board, _args, { blockrOption }) => blockrOption("notes_limit"),
          run(_board, limit, { mintId }) {
            got.limit = limit;
            return { patches: [{ op: "add", path: `/plugins/blockr.test-notes_plugin/${mintId(() => false)}`, value: "To do" }] };
          },
        },
      ],
    };
    const s = await stubbed(
      [notes],
      { [notes.id]: { name: "Notes", state: {} } },
      { blockrOptions: { rand_id: "letters_8", notes_limit: 3 } },
      { ...HELLO, lock: { board: true, load: false } },
    );

    // Under the board lock, the store applies a patch inside a plugin's state,
    // which the plugin's commands lock themselves.
    await s.core.commands.run("blockr.test-add_note");

    expect(got).toEqual({ lock: { board: true, load: false }, limit: 3 });
    const state = s.core.store.board().plugins[notes.id] as Readonly<Record<string, unknown>>;
    expect(Object.entries(state)).toEqual([[expect.stringMatching(/^[a-z]{8}$/), "To do"]]);
    s.stop();
  });

  test("leave out a class whose module declares another major of core's JS API, or whose class the store refuses", async () => {
    const reported = reports();
    const s = await stubCore({
      hello: withPlugins({ "blockr.test-old_plugin": { name: "Old" }, "blockr.test-bad_plugin": { name: "Bad" } }),
      modules: {
        "blockr.test-old_plugin": { api: 2, default: {} },
        "blockr.test-bad_plugin": classModule({ start: "now" } as unknown as Omit<PluginClass, "id">),
      },
    });
    reported.restore();

    expect(Object.keys(s.core.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT]);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      "The module blockr.test/blockr.test-old_plugin.js of blockr.test was built against major 2 of core's JS API, where core's is 1.",
      'The plugin class "blockr.test-bad_plugin" has a start that is no function.',
    ]);
    s.stop();
  });

  test("are the reply's plugins, whatever the opening board holds the state of", async () => {
    const dag: PluginClass = {
      id: "blockr.test-dag_plugin",
      state: { schema: { type: "object" }, default: { positions: {} } },
    };
    const kept: PluginClass = {
      id: "blockr.test-kept_plugin",
      state: { schema: { type: "object" }, default: { kept: false } },
    };
    const replied = withPlugins({ [kept.id]: { name: "Kept" } });
    // An opening board that holds the state of a plugin that the app does not
    // run, and none for the plugin that it runs.
    const hello: HelloReply = {
      ...replied,
      board: {
        ...replied.board,
        board: { ...replied.board.board, versions: { [dag.id]: 1 }, plugins: { [dag.id]: { positions: { a: { x: 1 } } } } },
      },
    };
    const s = await stubCore({ hello, modules: listedModules([dag, kept]) });

    expect(Object.keys(s.core.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT, kept.id]);
    expect(s.core.store.board().plugins).toEqual({ [kept.id]: { kept: false } });
    expect(() => s.core.plugins.pluginClass(dag.id)).toThrow(`The app runs no plugin "${dag.id}" on the open board.`);

    // The next board runs the same plugins.
    const next = await reopen(s);
    expect(Object.keys(next.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT, kept.id]);
    s.stop();
  });

  test("give a frontend the storage whose controls it draws: the app's storage plugin's, or none where the app runs none", async () => {
    // Core's own storage plugin, whose buttons save a download and open an
    // upload, where the app runs no other.
    const t = await stubCore();
    const own = t.core.plugins.storage();
    expect(own?.controls({ core: t.core }).map((control) => control.id)).toEqual(["save", "open"]);
    expect(t.core.plugins.storage()).toBe(own);
    expect(Object.keys(t.core.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT]);
    t.stop();

    // The storage plugin that takes the place of core's own.
    const s = await stubbed([saving], { [saving.id]: { name: "Session" } }, {}, withoutPlugins(HELLO, FILE));
    expect(s.core.plugins.storage()).toBe(saving.storage);
    s.stop();

    const u = await stubCore({ hello: withoutPlugins(HELLO, FILE) });
    expect(u.core.plugins.storage()).toBeUndefined();
    expect(Object.keys(u.core.plugins.appPlugins())).toEqual([BASIC, EDIT]);
    u.stop();
  });
});

describe("mounting a plugin's UI", () => {
  test("calls the ui of its class, with CoreApi, the id of the class and a signal, until the cleanup runs", async () => {
    const ui = pluginUi();
    const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: ui.ui };
    const report: PluginClass = { id: "blockr.test-report_plugin", ui: ui.ui };
    const s = await stubbed([notes, report], { [notes.id]: { name: "Notes" }, [report.id]: { name: "Report" } });
    const first = document.createElement("div");
    const second = document.createElement("div");

    // One ui draws the plugin of each class that holds it, by the id that it
    // gets.
    const end = s.core.mountPlugin(first, notes.id);
    s.core.mountPlugin(second, report.id);
    await s.settled();

    expect(ui.renders.map((render) => render.plugin)).toEqual([notes.id, report.id]);
    expect(ui.renders[0]?.core).toBe(s.core);
    expect([first.textContent, second.textContent]).toEqual(["Notes", "Report"]);

    end();
    expect(ui.renders[0]?.signal.aborted).toBe(true);
    expect(ui.cleanups()).toBe(1);
    expect(first.childNodes).toHaveLength(0);

    // The next board ends every mount of the board before.
    const next = await reopen(s);
    expect(ui.renders[1]?.signal.aborted).toBe(true);
    expect(ui.cleanups()).toBe(2);
    expect(() => s.core.mountPlugin(first, notes.id)).not.toThrow();
    expect(next).not.toBe(ui.renders[0]?.core);
    s.stop();
  });

  test("throws for a plugin that the app does not run or whose class has no ui, and shows why in the element where its ui fails", async () => {
    const broken: PluginClass = {
      id: "blockr.test-broken_plugin",
      ui: {
        render: () => {
          throw new Error("boom");
        },
      },
    };
    const s = await stubbed(
      [saving, broken],
      { [saving.id]: { name: "Session" }, [broken.id]: { name: "Broken" } },
      {},
      withoutPlugins(HELLO, FILE),
    );
    const el = (): HTMLElement => document.createElement("div");

    expect(() => s.core.mountPlugin(el(), "blockr.test-nowhere_plugin")).toThrow(
      'The app runs no plugin "blockr.test-nowhere_plugin" on the open board.',
    );
    expect(() => s.core.mountPlugin(el(), saving.id)).toThrow(
      'The class of the plugin "blockr.test-session_plugin" has no ui to mount.',
    );
    expect(() => s.core.mountPlugin(el(), BASIC)).toThrow(
      'The class of the plugin "blockr.core-basic_frontend_plugin" has no ui to mount.',
    );

    const reported = reports();
    const failing = el();
    s.core.mountPlugin(failing, broken.id);
    await s.settled();
    reported.restore();

    expect(failing.textContent).toBe('The ui of the plugin class "blockr.test-broken_plugin" failed in render(): boom');
    expect(reported.errors()).toHaveLength(1);
    s.stop();
  });

  test("adds the board options that the plugin's class requires before render(), and shows why in place of the UI where one does not go onto the board", async () => {
    const seen: unknown[] = [];
    const ui = {
      render({ core }: { core: CoreApi }) {
        seen.push(core.store.board().options.zoom);
        return undefined;
      },
    };
    const zoomed: PluginClass = { id: "blockr.test-notes_plugin", ui, options: ["zoom"] };
    const needy: PluginClass = { id: "blockr.test-needy_plugin", ui, options: ["nowhere"] };
    const reported = reports();
    const s = await stubbed([zoomed, needy], { [zoomed.id]: { name: "Notes" }, [needy.id]: { name: "Needy" } }, {
      options: [zoom],
    });
    const fine = document.createElement("div");
    const failing = document.createElement("div");
    s.core.mountPlugin(fine, zoomed.id);
    s.core.mountPlugin(failing, needy.id);
    await s.settled();
    reported.restore();

    expect(seen).toEqual([1]);
    expect(failing.textContent).toBe(
      'The board option "nowhere", which the plugin class "blockr.test-needy_plugin" requires, is defined by no package loaded.',
    );
    s.stop();
  });
});

describe("what a frontend offers other plugins", () => {
  test("holds each notification that a plugin raises until it withdraws it, from the plugin and about the plugin unless it names another board object, hands every subscriber those that hold, and holds none once the next board opens", async () => {
    const s = await stubCore();
    const { core } = s;
    const heard: (readonly Notification[])[] = [];
    core.notifications.subscribe((all) => heard.push(all));
    const off = core.notifications.subscribe((all) => heard.push(all));
    const before = core.notifications.get();

    const saved = core.notifications.notify(FILE, { severity: "message", code: "saved", message: "Saved." });
    off();
    core.notifications.notify(FILE, { severity: "error", code: "share_failed", message: "Not shared.", about: { block: "b" } });
    core.notifications.notify(FILE, { severity: "warning", code: "quota", message: "Almost full.", about: null });
    const all = core.notifications.get();
    const source = { kind: "module", object: { plugin: FILE }, package: "blockr.core" } as const;
    expect(all).toHaveLength(before.length + 3);
    expect(all.slice(before.length)).toEqual([
      {
        id: expect.any(String) as string,
        severity: "message",
        message: "Saved.",
        source: { ...source, code: "saved" },
        about: { plugin: FILE },
      },
      {
        id: expect.any(String) as string,
        severity: "error",
        message: "Not shared.",
        source: { ...source, code: "share_failed" },
        about: { block: "b" },
      },
      {
        id: expect.any(String) as string,
        severity: "warning",
        message: "Almost full.",
        source: { ...source, code: "quota" },
      },
    ]);
    expect(heard.map((notifications) => notifications.length)).toEqual([
      before.length + 1,
      before.length + 1,
      before.length + 2,
      before.length + 3,
    ]);
    expect(Object.isFrozen(all)).toBe(true);
    expect(Object.isFrozen(all.at(-1))).toBe(true);
    expect(Object.isFrozen(all.at(-1)?.source)).toBe(true);
    expect(Object.isFrozen(all.at(-2)?.about)).toBe(true);
    expect(core.notifications.get()).toBe(all);

    saved();
    saved();
    expect(core.notifications.get().map((n) => n.message).slice(before.length)).toEqual(["Not shared.", "Almost full."]);
    expect(heard).toHaveLength(5);

    await reopen(s);
    expect(heard.at(-1)).toEqual([]);
    expect(s.core.notifications.get().some((n) => n.source.kind === "module")).toBe(false);
    // The board before ends with its notifications, so a plugin raises none
    // there.
    const late = core.notifications.notify(FILE, { severity: "warning", code: "late", message: "Late." });
    late();
    expect(core.notifications.get()).toEqual([]);
    s.stop();
  });

  test("refuses a notification whose members are of the wrong kind", async () => {
    const s = await stubCore();
    const { notifications } = s.core;
    const base = { severity: "message", code: "saved", message: "Saved." } as const;
    const refused =
      (given: unknown, plugin: unknown = FILE): (() => void) =>
      () => {
        notifications.notify(plugin as string, given as Raised);
      };
    expect(refused({ ...base, severity: "fatal" })).toThrow(
      'The severity of a notification is "message", "warning" or "error", not "fatal".',
    );
    expect(refused({ ...base, message: 1 })).toThrow("The message of a notification is a string, not 1.");
    expect(refused({ ...base, code: "" })).toThrow('The code of a notification is a non-empty string, as "edit_refused", not "".');
    const part =
      "A notification concerns one board object, as { block: \"a\" }: a block, a link, a stack, a board option or a plugin, or null for the board, not";
    expect(refused({ ...base, about: { block: "b", link: "ab" } })).toThrow(`${part} {"block":"b","link":"ab"}.`);
    expect(refused({ ...base, about: { row: "b" } })).toThrow(`${part} {"row":"b"}.`);
    expect(refused({ ...base, about: { block: "" } })).toThrow(`${part} {"block":""}.`);
    expect(refused({ ...base, about: "b" })).toThrow(`${part} "b".`);
    expect(refused("Saved.")).toThrow('A notification is an object, not "Saved.".');
    expect(refused(base, "")).toThrow(
      'A plugin\'s notification names the plugin by the id of its class, a non-empty string, not "".',
    );
    s.stop();
  });

  test("holds nothing of a notification about a block, a link or a stack that the board does not hold, which the work that raises it can outlive, and reports one about a board object that it has never held, and one that a plugin that the app does not run raises", async () => {
    const s = await stubCore();
    const { notifications } = s.core;
    // The link "ab" leaves with its block.
    s.core.store.apply({ blocks: { rm: ["b"] } });
    await s.settled();
    const before = notifications.get();
    const heard: (readonly Notification[])[] = [];
    notifications.subscribe((all) => heard.push(all));

    const reported = reports();
    const base = { severity: "error", code: "share_failed", message: "Not shared." } as const;
    const raised = [
      notifications.notify(FILE, { ...base, about: { block: "b" } }),
      notifications.notify(FILE, { ...base, about: { link: "ab" } }),
      notifications.notify(FILE, { ...base, about: { block: "nowhere" } }),
      notifications.notify(FILE, { ...base, about: { stack: "nowhere" } }),
      notifications.notify(FILE, { ...base, about: { option: "nowhere" } }),
      notifications.notify(FILE, { ...base, about: { plugin: "blockr.test-session_plugin" } }),
      notifications.notify("blockr.test-session_plugin", base),
    ];
    for (const withdraw of raised) withdraw();
    const errors = reported.errors();
    reported.restore();
    expect(notifications.get()).toBe(before);
    expect(heard).toEqual([]);
    expect(errors.map((error) => (error as Error).message)).toEqual([
      `Core drops a notification that the plugin "${FILE}" raised about the block "nowhere", which the board has never held.`,
      `Core drops a notification that the plugin "${FILE}" raised about the stack "nowhere", which the board has never held.`,
      `Core drops a notification that the plugin "${FILE}" raised about the board option "nowhere", which no loaded package defines.`,
      `Core drops a notification that the plugin "${FILE}" raised about the plugin "blockr.test-session_plugin", which the app does not run.`,
      'Core drops a notification that the plugin "blockr.test-session_plugin" raised, which the app does not run.',
    ]);
    s.stop();
  });

  test("gives a block of the board focus, which goes back to none where the block leaves the board, and is none as the next board opens", async () => {
    const s = await stubCore();
    const { core } = s;
    const heard: (string | null)[] = [];
    core.focus.subscribe((block) => heard.push(block));
    expect(core.focus.get()).toBeNull();

    core.focus.set("b");
    core.focus.set("b");
    expect(core.focus.get()).toBe("b");
    expect(() => {
      core.focus.set("nowhere");
    }).toThrow('The board holds no block "nowhere" to give focus to.');

    core.store.apply({ blocks: { rm: ["b"] } });
    expect(core.focus.get()).toBeNull();
    expect(heard).toEqual(["b", null]);

    core.focus.set("a");
    const next = await reopen(s);
    expect(next.focus.get()).toBeNull();
    s.stop();
  });
});
