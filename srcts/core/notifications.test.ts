// The notifications of the open board on the CoreApi for tests: one for each
// condition of each block's status, one for each condition of R's error reply
// to a request, those that core raises where a block's UI, a board option's UI
// or a plugin fails, those that a module raises through its host or an
// onChange() through its `notify`, and their withdrawal as the board object
// that they concern leaves the board (./notifications.ts). The checks of what a plugin raises
// are tested with the other members that reach plugins (./plugins.test.ts),
// and what core raises as it starts the page and opens a board with the start
// (./start.test.ts).

import { describe, expect, test } from "vitest";
import type { Condition } from "../protocol/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { codeOf, concerning, held } from "../testing/notifications.ts";
import { listedModules, pluginUi } from "../testing/plugins.ts";
import { reports } from "../testing/reports.ts";
import { withOptions, withPlugins } from "../testing/store.ts";
import type { Notification } from "../types/core-api.ts";
import type { BlockHost } from "../types/hosts.ts";

const FILE = "blockr.core-file_storage_plugin";

// The URL of the UI module of the class of block b, the head block.
const HEAD = HELLO.block_registry["blockr.core-head_block"]?.module ?? "";

const fromModules = (notification: Notification): boolean => notification.source.kind === "module";

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
  test("are one for each condition of its status, from R's evaluation in the phase that raised it, with the same id while the condition holds, and one for two that share the block, severity, source and message", async () => {
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "block_eval", message: "Few rows." };
    const read: Condition = { severity: "message", class: ["simpleMessage", "message", "condition"], phase: "block_eval", message: "Read 150 rows." };
    let conditions = [few, few, read];
    const s = await conditioned(() => conditions);

    const evaluation = { type: "evaluate", phase: "block_eval" } as const;
    expect(held(s.core, concerning("b"))).toEqual([
      { severity: "warning", message: "Few rows.", source: { kind: "r", class: few.class, request: evaluation }, about: { block: "b" } },
      { severity: "message", message: "Read 150 rows.", source: { kind: "r", class: read.class, request: evaluation }, about: { block: "b" } },
    ]);
    const before = s.core.notifications.get().filter(concerning("b"));

    // The block's next reply drops one condition and keeps the other, whose
    // notification keeps its id.
    conditions = [read];
    s.core.scheduler.invalidate("b");
    await s.settled();
    const after = s.core.notifications.get().filter(concerning("b"));
    expect(after.map((n) => n.message)).toEqual(["Read 150 rows."]);
    expect(after[0]?.id).toBe(before[1]?.id);
    s.stop();
  });

  test("are core's for the error of an evaluation that the transport could not send, which the scheduler writes itself, and none for why a block waits, which its status gives as its reason", async () => {
    const s = await stubCore({ refuse: ["evaluate"] });
    s.core.scheduler.demand("test", ["b"]);
    await s.settled();

    expect(s.core.scheduler.status("b")?.reason).toBe('This block is waiting for block "a".');
    expect(held(s.core, (n) => concerning("a")(n) || concerning("b")(n))).toEqual([
      {
        severity: "error",
        message: "The evaluation could not be sent to R: The transport is closed.",
        source: { kind: "core", code: "evaluation_unsent" },
        about: { block: "a" },
      },
    ]);
    s.stop();
  });

  test("go as the block leaves the board, those that core and plugins raise included", async () => {
    const reported = reports();
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "block_eval", message: "Few rows." };
    const s = await conditioned(() => [few]);
    s.core.notifications.notify(FILE, { severity: "error", code: "share_failed", message: "Not shared.", about: { block: "b" } });
    s.core.notifications.notify(FILE, { severity: "message", code: "saved", message: "Saved." });
    // The UI module of b's class, which the test gives none of, fails to load.
    s.core.mountBlock(document.createElement("div"), "b");
    await s.settled();
    expect(s.core.notifications.get().filter(concerning("b")).map(codeOf)).toEqual(["simpleWarning", "share_failed", "module_failed"]);

    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    await s.settled();
    reported.restore();
    expect(held(s.core, concerning("b"))).toEqual([]);
    expect(held(s.core).map((n) => n.message)).toContain("Saved.");
    s.stop();
  });
});

describe("the notifications about a link or a stack", () => {
  test("go as it leaves the board, and one raised about it once it has left holds nothing", async () => {
    const s = await stubCore();
    await s.settled();
    s.core.notifications.notify(FILE, { severity: "warning", code: "slow", message: "Slow link.", about: { link: "ab" } });
    s.core.notifications.notify(FILE, { severity: "warning", code: "crowded", message: "Crowded stack.", about: { stack: "s1" } });
    expect(held(s.core, fromModules).map(codeOf)).toEqual(["slow", "crowded"]);

    await s.core.commands.run("blockr.core-remove_link", { id: "ab" });
    expect(held(s.core, fromModules).map(codeOf)).toEqual(["crowded"]);
    await s.core.commands.run("blockr.core-remove_stack", { id: "s1" });
    expect(held(s.core, fromModules)).toEqual([]);

    const reported = reports();
    s.core.notifications.notify(FILE, { severity: "warning", code: "slow", message: "Slow link.", about: { link: "ab" } });
    reported.restore();
    expect(reported.errors()).toEqual([]);
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });
});

describe("the notifications of R's replies to requests", () => {
  test("are one for each condition of an error reply, which concerns what the request was made for, until R next replies to a request of the same name", async () => {
    let full = true;
    const s = await stubCore({
      answer: (request) => {
        if (request.kind !== "request" || request.type !== "plugin") return undefined;
        if (full) throw new Error("The disk is full.");
        return "saved";
      },
    });
    await s.settled();
    const ofRequests = (n: Notification): boolean => n.source.kind === "r" && n.source.request.type === "plugin";

    await expect(s.core.request(FILE, "save")).rejects.toThrow("The disk is full.");
    await expect(s.core.request(FILE, "save")).rejects.toThrow("The disk is full.");
    expect(held(s.core, ofRequests)).toEqual([
      {
        severity: "error",
        message: "The disk is full.",
        source: { kind: "r", class: ["blockr_error", "error", "condition"], request: { type: "plugin", name: "save" } },
        about: { plugin: FILE },
      },
    ]);

    full = false;
    await expect(s.core.request(FILE, "save")).resolves.toBe("saved");
    expect(held(s.core, ofRequests)).toEqual([]);
    s.stop();
  });
});

describe("the notifications of the modules of packages", () => {
  test("that a block's UI module raises through its host come from the block, with the package of its class, and concern the block, until the mount ends", async () => {
    const hosts: BlockHost[] = [];
    const s = await stubCore({
      modules: {
        [HEAD]: {
          api: 1,
          default: {
            render: ({ host }: { host: BlockHost }) => {
              hosts.push(host);
            },
          },
        },
      },
    });
    const unmount = s.core.mountBlock(document.createElement("div"), "b");
    await s.settled();
    hosts[0]?.notify({ severity: "warning", code: "few_rows", message: "Few rows." });

    expect(held(s.core, fromModules)).toEqual([
      {
        severity: "warning",
        message: "Few rows.",
        source: { kind: "module", object: { block: "b" }, code: "few_rows", package: "blockr.core" },
        about: { block: "b" },
      },
    ]);
    unmount();
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });

  test("that a board option's onChange() raises come from the board option, with the package that lists it, until its next call returns", async () => {
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: { render: () => undefined },
      onChange(value, _core, notify) {
        if (value === 3) notify({ severity: "warning", code: "zoomed", message: "Text may not fit." });
      },
    };
    const s = await stubCore({ hello: withOptions(["zoom"]), modules: listedModules([], [zoom]) });
    await s.settled();

    await s.core.commands.run("blockr.core-set_option", { id: "zoom", value: 3 });
    expect(held(s.core, fromModules)).toEqual([
      {
        severity: "warning",
        message: "Text may not fit.",
        source: { kind: "module", object: { option: "zoom" }, code: "zoomed", package: "blockr.test" },
        about: { option: "zoom" },
      },
    ]);
    await s.core.commands.run("blockr.core-set_option", { id: "zoom", value: 2 });
    expect(held(s.core, fromModules)).toEqual([]);
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

    const module = HEAD.replace(/[?#].*$/, "");
    expect(held(s.core, (n) => codeOf(n) === "module_failed")).toEqual([
      {
        severity: "error",
        message: first.textContent,
        source: { kind: "core", code: "module_failed", package: "blockr.core" },
        about: { block: "b" },
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
    expect(held(s.core, (n) => codeOf(n) === "module_failed")).toEqual([
      {
        severity: "error",
        message: 'The UI of the board option "zoom" failed in render(): No slider.',
        source: { kind: "core", code: "module_failed", package: "blockr.test" },
        about: { option: "zoom" },
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

    expect(held(s.core, (n) => n.about !== undefined && "plugin" in n.about)).toEqual([
      {
        severity: "error",
        message: 'The start of the plugin class "blockr.test-notes_plugin" failed: No notes.',
        source: { kind: "core", code: "plugin_start_failed", package: "blockr.test" },
        about: { plugin: "blockr.test-notes_plugin" },
      },
      {
        severity: "error",
        message: 'The ui of the plugin class "blockr.test-notes_plugin" failed in render(): No pad.',
        source: { kind: "core", code: "module_failed", package: "blockr.test" },
        about: { plugin: "blockr.test-notes_plugin" },
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

    expect(held(s.core, (n) => n.about !== undefined && "option" in n.about && n.about.option === "nowhere")).toEqual([
      {
        severity: "error",
        message: 'The board option "nowhere", which the plugin class "blockr.test-needy_plugin" requires, is defined by no package loaded.',
        source: { kind: "core", code: "board_option_failed" },
        about: { option: "nowhere" },
      },
    ]);
    s.stop();
  });
});
