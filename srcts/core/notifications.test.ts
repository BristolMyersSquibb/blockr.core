// The notifications of the open board on the CoreApi for tests: one for each
// condition of each block's status, those that core raises where a block's UI,
// a board option's UI or a plugin fails, and their withdrawal as the block
// leaves the board (./notifications.ts). Those that a plugin raises are tested
// with the other members that reach plugins (./plugins.test.ts), and those that
// core raises as it starts the page and opens a board with the start
// (./start.test.ts).

import { describe, expect, test } from "vitest";
import type { Condition } from "../protocol/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { listedModules, pluginUi } from "../testing/plugins.ts";
import { reports } from "../testing/reports.ts";
import { withOptions, withPlugins } from "../testing/store.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";

const ERROR = ["blockr_error", "error", "condition"];

// What the notifications that hold say, without their ids, of those that
// `keep` keeps.
function held(core: CoreApi, keep: (notification: Notification) => boolean = () => true): Record<string, unknown>[] {
  return core.notifications
    .get()
    .filter(keep)
    .map((notification) => Object.fromEntries(Object.entries(notification).filter(([key]) => key !== "id")));
}

const of = (block: string) => (notification: Notification) => notification.block === block;

// The CoreApi for tests, whose R evaluates block b with the conditions that
// `conditions` gives at the time, with b demanded.
async function conditioned(conditions: () => Condition[], options: StubCoreOptions = {}) {
  const s = await stubCore({
    ...options,
    evaluate: (request) => (request.block === "b" ? { conditions: conditions() } : undefined),
  });
  s.core.scheduler.demand("test", ["b"]);
  await s.settled();
  return s;
}

describe("a block's notifications", () => {
  test("are one for each condition of its status, with the origin r, the same id while the condition holds, and one for two that share the block, severity, class and message", async () => {
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "block_eval", message: "Few rows." };
    const read: Condition = { severity: "message", class: ["simpleMessage", "message", "condition"], phase: "block_eval", message: "Read 150 rows." };
    let conditions = [few, few, read];
    const s = await conditioned(() => conditions);

    expect(held(s.core, of("b"))).toEqual([
      { severity: "warning", class: few.class, message: "Few rows.", origin: "r", phase: "block_eval", block: "b" },
      { severity: "message", class: read.class, message: "Read 150 rows.", origin: "r", phase: "block_eval", block: "b" },
    ]);
    const before = s.core.notifications.get().filter(of("b"));

    // The block's next reply drops one condition and keeps the other, whose
    // notification keeps its id.
    conditions = [read];
    s.core.scheduler.invalidate("b");
    await s.settled();
    const after = s.core.notifications.get().filter(of("b"));
    expect(after.map((n) => n.message)).toEqual(["Read 150 rows."]);
    expect(after[0]?.id).toBe(before[1]?.id);
    s.stop();
  });

  test("have the origin core for the message of a block that waits, which the scheduler writes itself", async () => {
    const s = await stubCore();
    await s.settled();
    await s.core.commands.run("blockr.core-remove_link", { id: "ab" });
    await s.settled();

    expect(held(s.core, of("b"))).toEqual([
      {
        severity: "message",
        class: ["blockr_message", "message", "condition"],
        message: 'This block is waiting for its input "data" to be linked.',
        origin: "core",
        phase: "status",
        block: "b",
      },
    ]);
    s.stop();
  });

  test("go as the block leaves the board, those that core and plugins raise included", async () => {
    const reported = reports();
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "block_eval", message: "Few rows." };
    const s = await conditioned(() => [few]);
    s.core.notifications.notify({ severity: "error", class: ["share_failed"], message: "Not shared.", origin: "blockr.test-session_plugin", block: "b" });
    s.core.notifications.notify({ severity: "message", class: ["saved"], message: "Saved.", origin: "blockr.test-session_plugin" });
    // The UI module of b's class, which the test gives none of, fails to load.
    s.core.mountBlock(document.createElement("div"), "b");
    await s.settled();
    expect(held(s.core, of("b")).map((n) => n.class)).toEqual([few.class, ["share_failed"], ["module_failed", ...ERROR]]);

    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    await s.settled();
    reported.restore();
    expect(held(s.core, of("b"))).toEqual([]);
    expect(held(s.core).map((n) => n.message)).toContain("Saved.");
    s.stop();
  });
});

describe("core's notifications", () => {
  test("of a block's UI module that cannot draw are one, however many mounts fail, which concerns the block, with the module's package", async () => {
    const reported = reports();
    const s = await stubCore();
    const first = document.createElement("div");
    const second = document.createElement("div");
    s.core.mountBlock(first, "b");
    s.core.mountBlock(second, "b");
    await s.settled();
    reported.restore();

    const module = HELLO.block_registry["blockr.core-head_block"]?.module.replace(/[?#].*$/, "") ?? "";
    expect(held(s.core, (n) => n.class[0] === "module_failed")).toEqual([
      {
        severity: "error",
        class: ["module_failed", ...ERROR],
        message: first.textContent,
        origin: "core",
        block: "b",
        package: "blockr.core",
      },
    ]);
    expect(first.textContent).toContain(`The module ${module} of blockr.core failed to load`);
    expect(second.textContent).toBe(first.textContent);
    s.stop();
  });

  test("of a board option's UI that fails in render() concern the board option, with the package that lists it", async () => {
    const reported = reports();
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: {
        render: () => {
          throw new Error("No slider.");
        },
      },
    };
    const s = await stubCore({ hello: withOptions(["zoom"]), modules: listedModules([], [zoom]) });
    const el = document.createElement("div");
    s.core.mountOption(el, "zoom");
    await s.settled();
    reported.restore();

    expect(el.textContent).toBe('The UI of the board option "zoom" failed in render(): No slider.');
    expect(held(s.core, (n) => n.class[0] === "module_failed")).toEqual([
      {
        severity: "error",
        class: ["module_failed", ...ERROR],
        message: 'The UI of the board option "zoom" failed in render(): No slider.',
        origin: "core",
        option: "zoom",
        package: "blockr.test",
      },
    ]);
    s.stop();
  });

  test("of a plugin whose start throws or whose ui fails in render() concern the plugin, with its class's package", async () => {
    const reported = reports();
    const ui = pluginUi();
    const failing: PluginClass = {
      id: "blockr.test-notes_plugin",
      start: () => {
        throw new Error("No notes.");
      },
      ui: {
        render: () => {
          throw new Error("No pad.");
        },
      },
    };
    const fine: PluginClass = { id: "blockr.test-report_plugin", ui: ui.ui };
    const s = await stubCore({
      hello: withPlugins({ [failing.id]: { name: "Notes" }, [fine.id]: { name: "Report" } }),
      modules: listedModules([failing, fine]),
    });
    s.core.mountPlugin(document.createElement("div"), failing.id);
    s.core.mountPlugin(document.createElement("div"), fine.id);
    await s.settled();
    reported.restore();

    expect(held(s.core, (n) => n.plugin !== undefined)).toEqual([
      {
        severity: "error",
        class: ["plugin_start_failed", ...ERROR],
        message: 'The start of the plugin class "blockr.test-notes_plugin" failed: No notes.',
        origin: "core",
        plugin: "blockr.test-notes_plugin",
        package: "blockr.test",
      },
      {
        severity: "error",
        class: ["module_failed", ...ERROR],
        message: 'The ui of the plugin class "blockr.test-notes_plugin" failed in render(): No pad.',
        origin: "core",
        plugin: "blockr.test-notes_plugin",
        package: "blockr.test",
      },
    ]);
    s.stop();
  });

  test("of a board option that does not go onto the board concern the board option, once for the board", async () => {
    const reported = reports();
    const needy: PluginClass = { id: "blockr.test-needy_plugin", options: ["nowhere"], ui: pluginUi().ui };
    const s = await stubCore({ hello: withPlugins({ [needy.id]: { name: "Needy" } }), modules: listedModules([needy]) });
    s.core.mountPlugin(document.createElement("div"), needy.id);
    s.core.mountPlugin(document.createElement("div"), needy.id);
    await s.settled();
    reported.restore();

    expect(held(s.core, (n) => n.option === "nowhere")).toEqual([
      {
        severity: "error",
        class: ["board_option_failed", ...ERROR],
        message: 'The board option "nowhere", which the plugin class "blockr.test-needy_plugin" requires, is defined by no package loaded.',
        origin: "core",
        option: "nowhere",
      },
    ]);
    s.stop();
  });
});
