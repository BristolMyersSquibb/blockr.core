// The app's plugins on the CoreApi for tests: which run and of which class, as
// R's reply to hello names them, the storage whose controls a frontend draws,
// mounting a plugin's UI where a frontend places it, and what a frontend
// offers other plugins, the notes they send and which block has focus.

import { describe, expect, test } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { pluginUi, registering } from "../testing/plugins.ts";
import { reports } from "../testing/reports.ts";
import { withoutPlugins, withPlugins } from "../testing/store.ts";
import type { GivenPlugin } from "../testing/store.ts";
import type { CoreApi, Note } from "../types/core-api.ts";

// The classes of core's own plugins, which the app of the fixture hello runs.
const BASIC = "blockr.core-basic_frontend_plugin";
const FILE = "blockr.core-file_storage_plugin";
const EDIT = "blockr.core-edit_board_plugin";

const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: "blockr.test-notes" };
// A class whose plugin draws with the module of the notes' class.
const report: PluginClass = { id: "blockr.test-report_plugin", ui: "blockr.test-notes" };
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
// given, which blockr.test registers.
function stubbed(
  classes: readonly PluginClass[],
  plugins: Readonly<Record<string, GivenPlugin>>,
  options: StubCoreOptions & { readonly options?: readonly Option[] } = {},
  replied: HelloReply = HELLO,
) {
  return stubCore({
    ...options,
    hello: withPlugins(plugins, replied),
    startup: { "blockr.test": registering(classes, options.options) },
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
  test("are the app's plugins that the reply names, each of a class that a package registers, which pluginClass() gives", async () => {
    const s = await stubbed([notes, report, saving], {
      [notes.id]: { name: "Notes" },
      [report.id]: { name: "Report" },
      [saving.id]: { name: "Session" },
      "blockr.gone-gone_plugin": { name: "Gone" },
    });
    const { core } = s;

    // In the order of the reply, core's own first, but for the plugin whose
    // class no package registers.
    expect(Object.keys(core.plugins.appPlugins())).toEqual([BASIC, FILE, EDIT, notes.id, report.id, saving.id]);
    expect(core.plugins.appPlugins()[report.id]).toEqual({ name: "Report" });
    expect(core.plugins.appPlugins()[BASIC]).toEqual({ name: "Basic frontend" });
    expect(core.plugins.appPlugins()[EDIT]).toEqual({ name: "Edit board" });
    expect(core.plugins.appPlugins()).toBe(core.plugins.appPlugins());
    expect(core.plugins.pluginClass(report.id)).toBe(report);
    expect(core.plugins.pluginClass(BASIC).frontend).toBe("blockr.core-basic");
    // Core's Edit board plugin has a ui alone, so that the board holds nothing
    // for it.
    expect(core.plugins.pluginClass(EDIT)).toEqual({ id: EDIT, ui: "blockr.core-edit_board" });
    expect(Object.keys(core.store.board().plugins)).not.toContain(EDIT);
    expect(() => core.plugins.pluginClass("blockr.gone-gone_plugin")).toThrow(
      'The app runs no plugin "blockr.gone-gone_plugin" on the open board.',
    );
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
    const s = await stubCore({ hello, startup: { "blockr.test": registering([dag, kept]) } });

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
  test("mounts the module of its class, with CoreApi, the id of the class and a signal, until the cleanup runs", async () => {
    const ui = pluginUi();
    const s = await stubbed([notes, report], { [notes.id]: { name: "Notes" }, [report.id]: { name: "Report" } }, {
      modules: { "blockr.test-notes": ui.exports },
    });
    const first = document.createElement("div");
    const second = document.createElement("div");

    // One module draws the plugin of each class that names it, by the id that
    // it gets.
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

  test("throws for a plugin that the app does not run or whose class has no ui, and shows why in the element where its module cannot draw", async () => {
    const missing: PluginClass = { id: "blockr.test-missing_plugin", ui: "blockr.test-missing" };
    const older: PluginClass = { id: "blockr.test-older_plugin", ui: "blockr.test-older" };
    const broken: PluginClass = { id: "blockr.test-broken_plugin", ui: "blockr.test-broken" };
    const s = await stubbed(
      [saving, missing, older, broken],
      {
        [saving.id]: { name: "Session" },
        [missing.id]: { name: "Missing" },
        [older.id]: { name: "Older" },
        [broken.id]: { name: "Broken" },
      },
      {
        modules: {
          "blockr.test-older": { api: 2, default: { render: () => undefined } },
          "blockr.test-broken": {
            api: 1,
            default: {
              render: () => {
                throw new Error("boom");
              },
            },
          },
        },
      },
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
    const [a, b, c] = [el(), el(), el()];
    s.core.mountPlugin(a, missing.id);
    s.core.mountPlugin(b, older.id);
    s.core.mountPlugin(c, broken.id);
    await s.settled();
    reported.restore();

    expect(a.textContent).toBe('No package offers a module "blockr.test-missing".');
    expect(b.textContent).toBe(
      "The module blockr.test-older of blockr.test was built against major 2 of core's JS API, where core's is 1.",
    );
    expect(c.textContent).toBe("The module blockr.test-broken of blockr.test failed in render(): boom");
    expect(reported.errors()).toHaveLength(3);
    s.stop();
  });

  test("adds the board options that the plugin's class requires before render(), and shows why in place of the UI where one does not go onto the board", async () => {
    const zoomed: PluginClass = { ...notes, options: ["zoom"] };
    const needy: PluginClass = { id: "blockr.test-needy_plugin", ui: "blockr.test-notes", options: ["nowhere"] };
    const seen: unknown[] = [];
    const ui = {
      api: 1,
      default: {
        render({ core }: { core: CoreApi }) {
          seen.push(core.store.board().options.zoom);
        },
      },
    };
    const reported = reports();
    const s = await stubbed([zoomed, needy], { [zoomed.id]: { name: "Notes" }, [needy.id]: { name: "Needy" } }, {
      modules: { "blockr.test-notes": ui },
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
  test("hands each note that a plugin sends to every subscriber, refuses what is no note, and hands on nothing once the next board opens", async () => {
    const s = await stubCore();
    const heard: Note[] = [];
    const { core } = s;
    core.notifications.subscribe((note) => heard.push(note));
    const off = core.notifications.subscribe((note) => heard.push(note));

    core.notifications.notify({ message: "Saved.", severity: "message" });
    off();
    core.notifications.notify({ message: "Not saved.", severity: "error" });
    expect(heard).toEqual([
      { message: "Saved.", severity: "message" },
      { message: "Saved.", severity: "message" },
      { message: "Not saved.", severity: "error" },
    ]);
    expect(Object.isFrozen(heard[0])).toBe(true);

    expect(() => {
      core.notifications.notify({ message: "Loud", severity: "fatal" } as unknown as Note);
    }).toThrow('A note holds a message, as a string, and a severity, "message", "warning" or "error".');

    await reopen(s);
    core.notifications.notify({ message: "Late.", severity: "warning" });
    expect(heard).toHaveLength(3);
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
