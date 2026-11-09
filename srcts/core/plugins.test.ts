// The plugins of the open board on the CoreApi for tests: which run and of
// which class, the storage that a frontend's save and open controls call,
// mounting a plugin's UI where a frontend places it, and what a frontend
// offers other plugins, the notes they send and which block has focus.

import { describe, expect, test } from "vitest";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { reports } from "../testing/reports.ts";
import { withPlugins } from "../testing/store.ts";
import type { Plugin, PluginId } from "../types/board.ts";
import type { CoreApi, Note } from "../types/core-api.ts";

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

// The UI module of a plugin class, which writes the name of the plugin it
// draws and keeps what each render() gets.
function pluginUi() {
  const renders: { el: HTMLElement; core: CoreApi; plugin: PluginId; signal: AbortSignal }[] = [];
  let cleanups = 0;
  return {
    renders,
    cleanups: () => cleanups,
    exports: {
      api: 1,
      default: {
        render(args: { el: HTMLElement; core: CoreApi; plugin: PluginId; signal: AbortSignal }) {
          renders.push(args);
          args.el.textContent = args.core.store.board().plugins[args.plugin]?.name ?? "";
          return () => {
            cleanups++;
          };
        },
      },
    },
  };
}

const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: "blockr.test-notes" };
const saving: PluginClass = {
  id: "blockr.test-session_plugin",
  storage: { save: () => Promise.resolve(), open: () => Promise.resolve(null) },
};
const zoom: Option = {
  id: "zoom",
  label: "Zoom",
  category: "View options",
  schema: { type: "number" },
  default: 1,
  ui: { render: () => undefined },
};

const plugin = (cls: PluginClass, name: string): Plugin => ({ class: cls.id, name, state: {} });

// The CoreApi for tests on the board of the fixture hello, which holds the
// plugins given, of the classes given, which blockr.test registers.
function stubbed(
  classes: readonly PluginClass[],
  plugins: Readonly<Record<PluginId, Plugin>>,
  options: StubCoreOptions & { readonly options?: readonly Option[] } = {},
) {
  return stubCore({
    ...options,
    hello: withPlugins(plugins),
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
  test("are the open board's plugins, each of a class that a package registers, which classOf() gives", async () => {
    const s = await stubbed([notes, saving], {
      notes: plugin(notes, "Notes"),
      report: plugin(notes, "Report"),
      session: plugin(saving, "Session"),
      gone: { class: "blockr.gone-gone_plugin", name: "Gone", state: {} },
    });
    const { core } = s;

    // The store drops the plugin whose class no package registers.
    expect(Object.keys(core.plugins.running())).toEqual(["notes", "report", "session"]);
    expect(core.plugins.running()).toBe(core.plugins.running());
    expect(core.plugins.classOf("report")).toBe(notes);
    expect(() => core.plugins.classOf("gone")).toThrow('The open board runs no plugin "gone".');
    s.stop();
  });

  test("give their storage to a frontend's save and open controls, or core's own where the board holds no storage plugin", async () => {
    const s = await stubbed([saving], { session: plugin(saving, "Session") });
    expect(s.core.plugins.storage()).toBe(saving.storage);
    s.stop();

    const t = await stubCore();
    const own = t.core.plugins.storage();
    expect(Object.keys(own)).toEqual(["save", "open"]);
    expect(t.core.plugins.storage()).toBe(own);
    expect(t.core.plugins.running()).toEqual({});
    t.stop();
  });
});

describe("mounting a plugin's UI", () => {
  test("mounts the module of its class once for each plugin, with CoreApi, the plugin's id and a signal, until the cleanup runs", async () => {
    const ui = pluginUi();
    const s = await stubbed([notes], { notes: plugin(notes, "Notes"), report: plugin(notes, "Report") }, {
      modules: { "blockr.test-notes": ui.exports },
    });
    const first = document.createElement("div");
    const second = document.createElement("div");

    const end = s.core.mountPlugin(first, "notes");
    s.core.mountPlugin(second, "report");
    await s.settled();

    expect(ui.renders.map((render) => render.plugin)).toEqual(["notes", "report"]);
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
    expect(() => s.core.mountPlugin(first, "notes")).not.toThrow();
    expect(next).not.toBe(ui.renders[0]?.core);
    s.stop();
  });

  test("throws for a plugin that the board does not run or whose class has no ui, and shows why in the element where its module cannot draw", async () => {
    const missing: PluginClass = { id: "blockr.test-missing_plugin", ui: "blockr.test-missing" };
    const older: PluginClass = { id: "blockr.test-older_plugin", ui: "blockr.test-older" };
    const broken: PluginClass = { id: "blockr.test-broken_plugin", ui: "blockr.test-broken" };
    const s = await stubbed(
      [saving, missing, older, broken],
      {
        session: plugin(saving, "Session"),
        missing: plugin(missing, "Missing"),
        older: plugin(older, "Older"),
        broken: plugin(broken, "Broken"),
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
    );
    const el = (): HTMLElement => document.createElement("div");

    expect(() => s.core.mountPlugin(el(), "nowhere")).toThrow('The open board runs no plugin "nowhere".');
    expect(() => s.core.mountPlugin(el(), "session")).toThrow(
      'The class "blockr.test-session_plugin" of the plugin "session" has no ui to mount.',
    );

    const reported = reports();
    const [a, b, c] = [el(), el(), el()];
    s.core.mountPlugin(a, "missing");
    s.core.mountPlugin(b, "older");
    s.core.mountPlugin(c, "broken");
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
    const s = await stubbed([zoomed, needy], { notes: plugin(zoomed, "Notes"), needy: plugin(needy, "Needy") }, {
      modules: { "blockr.test-notes": ui },
      options: [zoom],
    });
    const fine = document.createElement("div");
    const failing = document.createElement("div");
    s.core.mountPlugin(fine, "notes");
    s.core.mountPlugin(failing, "needy");
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
