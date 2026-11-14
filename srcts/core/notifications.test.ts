// The notifications of the open board on the CoreApi for tests: one for each
// condition of each block's status, one for each condition of R's error reply
// to a request, those that core raises where a block's UI, a board option's UI
// or a plugin fails, those that a module raises through its host or an
// onChange() through its `notify`, those that core raises where the store or
// the command registry refuses an edit of the user that a plugin makes, and
// their withdrawal as the board object that they concern leaves the board
// (./notifications.ts, ./core.ts). The checks of what a plugin raises
// are tested with the other members that reach plugins (./plugins.test.ts),
// and what core raises as it starts the page and opens a board with the start
// (./start.test.ts).

import { describe, expect, test } from "vitest";
import type { Condition } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { codeOf, concerning, held } from "../testing/notifications.ts";
import { listedModules, pluginUi } from "../testing/plugins.ts";
import { logged } from "../testing/logged.ts";
import { hello, withOptions, withPlugins } from "../testing/store.ts";
import type { Notification } from "../types/core-api.ts";
import type { BlockHost, OptionHost, OutputHost } from "../types/hosts.ts";
import { coreNotification } from "../util/index.ts";
import { createNotifications } from "./notifications.ts";

const FILE = "blockr.core-file_storage_plugin";

// The URL of the UI module of the class of block b, the head block, and of
// the renderer of its output.
const HEAD = HELLO.block_registry["blockr.core-head_block"]?.module ?? "";
const TABLE = HELLO.block_registry["blockr.core-head_block"]?.renderer?.module ?? "";

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
    const reported = logged();
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

    const reported = logged();
    s.core.notifications.notify(FILE, { severity: "warning", code: "slow", message: "Slow link.", about: { link: "ab" } });
    reported.restore();
    expect(reported.lines().map((line) => [line.level, line.message])).toEqual([
      ["debug", `Core drops a notification that the plugin "${FILE}" raised about the link "ab", which has left the board.`],
    ]);
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });
});

describe("what the holder drops", () => {
  test("is logged at debug where core raises a notification, or R replies with conditions, about a block that the board does not hold", async () => {
    const store = createStore({ hello });
    await store.open(hello);
    const holder = createNotifications(store, { plugins: hello.plugins });
    const ui = { object: { block: "nowhere" } };
    const renderer = { object: { block: "nowhere" }, renderer: "blockr.core-table_output", package: "blockr.core" };
    const reported = logged();

    holder.raise(coreNotification("module_failed", "error", "No module.", { about: { block: "nowhere" } }));
    holder.replied(ui, { type: "block_class", name: "columns" }, [
      { severity: "error", class: ["error", "condition"], message: "No columns." },
    ]);
    holder.replied(renderer, { type: "renderer", name: "page" }, [
      { severity: "warning", class: ["warning", "condition"], message: "No page." },
    ]);
    holder.replied(ui, { type: "block_class", name: "columns" }, []);
    reported.restore();

    expect(holder.api.get()).toEqual([]);
    expect(reported.lines().map((line) => [line.level, line.message])).toEqual([
      ["debug", 'Core drops a notification that core raised about the block "nowhere", which the board does not hold.'],
      ["debug", 'Core drops the notifications of R\'s reply to the request "columns" about the block "nowhere", which the board does not hold.'],
      [
        "debug",
        'Core drops the notifications of R\'s reply to the request "page" of the renderer "blockr.core-table_output" about the block "nowhere", which the board does not hold.',
      ],
    ]);
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
        source: {
          kind: "r",
          class: ["blockr_error", "error", "condition"],
          request: { type: "plugin", name: "save" },
          module: { object: { plugin: FILE }, package: "blockr.core" },
        },
        about: { plugin: FILE },
      },
    ]);

    full = false;
    await expect(s.core.request(FILE, "save")).resolves.toBe("saved");
    expect(held(s.core, ofRequests)).toEqual([]);
    s.stop();
  });

  test("name the module that made the request: the block's UI module, the renderer of the block's output, whichever scope its request has, and the board option's module", async () => {
    const hosts: { block?: BlockHost; output?: OutputHost; option?: OptionHost } = {};
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: {
        render: ({ host }: { host: OptionHost }) => {
          hosts.option = host;
        },
      },
    };
    const s = await stubCore({
      hello: withOptions(["zoom"]),
      modules: {
        ...listedModules([], [zoom]),
        [HEAD]: {
          api: 1,
          default: {
            render: ({ host }: { host: BlockHost }) => {
              hosts.block = host;
            },
          },
        },
        [TABLE]: {
          api: 1,
          default: {
            options: ["n_rows"],
            render: ({ host }: { host: OutputHost }) => {
              hosts.output = host;
            },
          },
        },
      },
      answer: () => {
        throw new Error("No answer.");
      },
    });
    s.core.scheduler.demand("test", ["b"]);
    s.core.mountBlock(document.createElement("div"), "b");
    s.core.mountOutput(document.createElement("div"), "b");
    s.core.mountOption(document.createElement("div"), "zoom");
    await s.settled();

    await expect(hosts.block?.request("n_max")).rejects.toMatchObject({ status: "error" });
    await expect(hosts.output?.request("download")).rejects.toMatchObject({ status: "error" });
    await expect(hosts.output?.request("labels", {}, { scope: "renderer" })).rejects.toMatchObject({ status: "error" });
    await expect(hosts.option?.request("list_dir")).rejects.toMatchObject({ status: "error" });
    const renderer = { object: { block: "b" }, renderer: "blockr.core-table_output", package: "blockr.core" };
    const replied = held(s.core, (n) => n.source.kind === "r");
    expect(replied.map(({ source, about }) => [source.kind === "r" ? source.request : undefined, source.module, about])).toEqual([
      [{ type: "block_class", name: "n_max" }, { object: { block: "b" }, package: "blockr.core" }, { block: "b" }],
      [{ type: "output_class", name: "download" }, renderer, { block: "b" }],
      [{ type: "renderer", name: "labels" }, renderer, { block: "b" }],
      [{ type: "option", name: "list_dir" }, { object: { option: "zoom" }, package: "blockr.test" }, { option: "zoom" }],
    ]);
    s.stop();
  });
});

describe("the notifications of the modules of packages", () => {
  test("that a block's UI module raises through its host name that module, with the package of its class, and concern the block, until the mount ends", async () => {
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
        source: { kind: "module", module: { object: { block: "b" }, package: "blockr.core" }, code: "few_rows" },
        about: { block: "b" },
      },
    ]);
    unmount();
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });

  test("that a block's module raises about another board object go as the block leaves the board", async () => {
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
    s.core.mountBlock(document.createElement("div"), "b");
    await s.settled();
    hosts[0]?.notify({ severity: "message", code: "copied", message: "Copied the rows of a.", about: { block: "a" } });
    hosts[0]?.notify({ severity: "warning", code: "slow", message: "The board is slow.", about: null });
    expect(held(s.core, fromModules).map((n) => [n.about, n.source.module])).toEqual([
      [{ block: "a" }, { object: { block: "b" }, package: "blockr.core" }],
      [undefined, { object: { block: "b" }, package: "blockr.core" }],
    ]);

    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    await s.settled();
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });

  test("that the renderer of a block's output raises through its host name the renderer, with its package, and concern the block, until the renderer is unmounted", async () => {
    const hosts: OutputHost[] = [];
    const s = await stubCore({
      modules: {
        [TABLE]: {
          api: 1,
          default: {
            options: ["n_rows"],
            render: ({ host }: { host: OutputHost }) => {
              hosts.push(host);
            },
          },
        },
      },
    });
    s.core.scheduler.demand("test", ["b"]);
    const unmount = s.core.mountOutput(document.createElement("div"), "b");
    await s.settled();
    hosts[0]?.notify({ severity: "warning", code: "truncated", message: "Only the first rows show." });

    expect(held(s.core, fromModules)).toEqual([
      {
        severity: "warning",
        message: "Only the first rows show.",
        source: {
          kind: "module",
          module: { object: { block: "b" }, renderer: "blockr.core-table_output", package: "blockr.core" },
          code: "truncated",
        },
        about: { block: "b" },
      },
    ]);
    unmount();
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });

  test("that a board option's onChange() raises name the board option's module, with the package that lists it, until its next call returns", async () => {
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
        source: { kind: "module", module: { object: { option: "zoom" }, package: "blockr.test" }, code: "zoomed" },
        about: { option: "zoom" },
      },
    ]);
    await s.core.commands.run("blockr.core-set_option", { id: "zoom", value: 2 });
    expect(held(s.core, fromModules)).toEqual([]);
    s.stop();
  });
});

describe("core's notifications", () => {
  test("of a block's UI module that cannot draw are one, however many mounts fail, which names the module, with its package, concerns the block and leaves each element empty", async () => {
    const reported = logged();
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
        message: expect.stringContaining(`The module ${module} of blockr.core failed to load`) as string,
        source: { kind: "core", code: "module_failed", module: { object: { block: "b" }, package: "blockr.core" } },
        about: { block: "b" },
      },
    ]);
    expect([first.textContent, second.textContent]).toEqual(["", ""]);
    s.stop();
  });

  test("of a renderer that cannot draw name the renderer, with its package, concern the block and leave the output's element empty", async () => {
    const reported = logged();
    const s = await stubCore({ modules: { [TABLE]: { api: 2, default: { render: () => undefined } } } });
    s.core.scheduler.demand("test", ["b"]);
    const el = document.createElement("div");
    s.core.mountOutput(el, "b");
    await s.settled();
    reported.restore();

    const module = TABLE.replace(/[?#].*$/, "");
    expect(held(s.core, (n) => codeOf(n) === "module_failed")).toEqual([
      {
        severity: "error",
        message: `The module ${module} of blockr.core was built against major 2 of core's JS API, where core's is 1.`,
        source: {
          kind: "core",
          code: "module_failed",
          module: { object: { block: "b" }, renderer: "blockr.core-table_output", package: "blockr.core" },
        },
        about: { block: "b" },
      },
    ]);
    expect(el.textContent).toBe("");
    s.stop();
  });

  test("of a board option's UI that fails in render() name the board option's module, with the package that lists it, and concern the board option", async () => {
    const reported = logged();
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

    expect(el.textContent).toBe("");
    expect(held(s.core, (n) => codeOf(n) === "module_failed")).toEqual([
      {
        severity: "error",
        message: 'The UI of the board option "zoom" failed in render(): No slider.',
        source: { kind: "core", code: "module_failed", module: { object: { option: "zoom" }, package: "blockr.test" } },
        about: { option: "zoom" },
      },
    ]);
    s.stop();
  });

  test("of a plugin whose start throws or whose ui fails in render() name the plugin class's module, with its package, and concern the plugin", async () => {
    const reported = logged();
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
        source: {
          kind: "core",
          code: "plugin_start_failed",
          module: { object: { plugin: "blockr.test-notes_plugin" }, package: "blockr.test" },
        },
        about: { plugin: "blockr.test-notes_plugin" },
      },
      {
        severity: "error",
        message: 'The ui of the plugin class "blockr.test-notes_plugin" failed in render(): No pad.',
        source: { kind: "core", code: "module_failed", module: { object: { plugin: "blockr.test-notes_plugin" }, package: "blockr.test" } },
        about: { plugin: "blockr.test-notes_plugin" },
      },
    ]);
    s.stop();
  });

  test("of a board option whose default fails name the board option's module, with the package that lists it, and concern the board option", async () => {
    const reported = logged();
    const zoom: Option = {
      id: "zoom",
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: () => {
        throw new Error("No zoom today.");
      },
      ui: { render: () => undefined },
    };
    const zoomed: PluginClass = { id: "blockr.test-zoomed_plugin", options: ["zoom"], ui: pluginUi().ui };
    const s = await stubCore({
      hello: withOptions(["zoom"], withPlugins({ [zoomed.id]: { name: "Zoomed" } })),
      modules: listedModules([zoomed], [zoom]),
    });
    s.core.mountPlugin(document.createElement("div"), zoomed.id);
    await s.settled();
    reported.restore();

    expect(held(s.core, (n) => codeOf(n) === "board_option_failed")).toEqual([
      {
        severity: "error",
        message: 'The default of the board option "zoom", which the plugin class "blockr.test-zoomed_plugin" requires, failed: No zoom today.',
        source: { kind: "core", code: "board_option_failed", module: { object: { option: "zoom" }, package: "blockr.test" } },
        about: { option: "zoom" },
      },
    ]);
    s.stop();
  });

  test("of a board option that no loaded package defines concern the board option, are one for the board and name no module", async () => {
    const reported = logged();
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

  test("of an edit of the user that the command registry or the store refuses concern the board, whatever block the edit names, and the edit still rejects or throws, with the reason logged at debug", async () => {
    const s = await stubCore();
    await s.settled();
    const unknownInput = { links: { add: { db: { from: "d", to: "b", input: "nowhere" } } } };
    const reported = logged();

    // A command that the command registry refuses, and a delta that the store
    // refuses.
    await expect(s.core.commands.run("blockr.core-remove_block", { id: "nowhere" })).rejects.toThrow(
      'The command "blockr.core-remove_block" refused to run',
    );
    expect(() => {
      s.core.store.apply(unknownInput);
    }).toThrow("The store rejected the delta");
    // The user removes the link from block a to block b, and the assistant
    // then feeds b's one input from block d, which an undo would give a second
    // link.
    await s.core.commands.run("blockr.core-remove_link", { id: "ab" });
    await s.core.commands.run("blockr.core-add_link", { from: "d", to: "b", input: "data" }, "assistant");
    expect(() => {
      s.core.store.undo();
    }).toThrow("The store refused to undo the step");
    // The user feeds block f's one input from block d, in place of block a,
    // and takes that back. The assistant then feeds it from block b, which a
    // redo would give a second link.
    await s.core.commands.run("blockr.core-remove_link", { id: "keen_finch" });
    await s.core.commands.run("blockr.core-add_link", { id: "df", from: "d", to: "f", input: "data" });
    s.core.store.undo();
    await s.core.commands.run("blockr.core-add_link", { from: "b", to: "f", input: "data" }, "assistant");
    expect(() => {
      s.core.store.redo();
    }).toThrow("The store refused to redo the step");
    await s.settled();
    reported.restore();

    const refused = held(s.core, (n) => codeOf(n) === "edit_refused");
    expect(refused.map(({ severity, source, about }) => [severity, source, about])).toEqual(
      Array.from({ length: 4 }, () => ["error", { kind: "core", code: "edit_refused" }, undefined]),
    );
    expect(refused.map((n) => n.message)).toEqual([
      expect.stringMatching(/^The command "blockr\.core-remove_block" refused to run/),
      expect.stringMatching(/^The store rejected the delta/),
      expect.stringMatching(/^The store refused to undo the step/),
      expect.stringMatching(/^The store refused to redo the step/),
    ]);
    expect(reported.lines().map((line) => [line.level, line.package, line.message])).toEqual(
      refused.map((n) => ["debug", "blockr.core", n.message]),
    );
    s.stop();
  });

  test("of an edit that the user makes again are raised anew, so that a frontend that has shown one shows it again", async () => {
    const s = await stubCore();
    await s.settled();
    const heard: string[][] = [];
    s.core.notifications.subscribe((all) => {
      heard.push(all.filter((n) => codeOf(n) === "edit_refused").map((n) => n.message));
    });
    const remove = (): Promise<void> => s.core.commands.run("blockr.core-remove_block", { id: "nowhere" });

    await expect(remove()).rejects.toThrow();
    const [first] = s.core.notifications.get().filter((n) => codeOf(n) === "edit_refused");
    await expect(remove()).rejects.toThrow();

    // Core withdrew the notification, and raised it again with the same id.
    const message = first?.message ?? "";
    expect(heard).toEqual([[message], [], [message]]);
    expect(s.core.notifications.get().filter((n) => codeOf(n) === "edit_refused")).toEqual([first]);
    s.stop();
  });

  test("of an edit of another origin are none, and core logs no line of it: the assistant's, whose tool hands the refusal to its model, and core's own, which puts the default of a board option on the board", async () => {
    const reported = logged();
    const unfit: Option = { id: "unfit", label: "Unfit", category: "View options", schema: { type: "number" }, default: "big", ui: { render: () => undefined } };
    const needy: PluginClass = { id: "blockr.test-needy_plugin", options: ["unfit"], ui: pluginUi().ui };
    const s = await stubCore({
      hello: withOptions(["unfit"], withPlugins({ [needy.id]: { name: "Needy" } })),
      modules: listedModules([needy], [unfit]),
    });
    await s.settled();
    reported.restore();

    const quiet = logged();
    await expect(s.core.commands.run("blockr.core-remove_block", { id: "nowhere" }, "assistant")).rejects.toThrow();
    expect(() => {
      s.core.store.apply({ links: { add: { db: { from: "d", to: "b", input: "nowhere" } } } }, "assistant");
    }).toThrow();
    quiet.restore();

    expect(held(s.core, (n) => codeOf(n) === "edit_refused")).toEqual([]);
    expect(quiet.lines()).toEqual([]);
    // The store refused core's edit of the default, which core names as the
    // board option's failure alone.
    expect(held(s.core, (n) => codeOf(n) === "board_option_failed").map((n) => n.about)).toEqual([{ option: "unfit" }]);
    s.stop();
  });

  test("of a block's or a board option's UI module whose submit() the store refuses are none, since the module says why itself, as core's inputs do below themselves, and submit() rejects with the reason", async () => {
    const blocks: BlockHost[] = [];
    const options: OptionHost[] = [];
    const rows = HELLO.options.n_rows?.module ?? "";
    const s = await stubCore({
      modules: {
        [HEAD]: {
          api: 1,
          default: {
            render: ({ host }: { host: BlockHost }) => {
              blocks.push(host);
            },
          },
        },
        [rows]: {
          api: 1,
          default: {
            id: "n_rows",
            label: "Rows",
            category: "Table options",
            schema: { type: "integer", minimum: 1 },
            default: 50,
            ui: {
              render: ({ host }: { host: OptionHost }) => {
                options.push(host);
              },
            },
          },
        },
      },
    });
    s.core.mountBlock(document.createElement("div"), "b");
    s.core.mountOption(document.createElement("div"), "n_rows");
    await s.settled();

    await expect(blocks[0]?.submit({ n: -1 })).rejects.toThrow('The state of block "b" fails its schema at /n');
    await expect(options[0]?.submit(0)).rejects.toThrow('The value of the board option "n_rows" fails its schema');

    expect(held(s.core, (n) => codeOf(n) === "edit_refused")).toEqual([]);
    s.stop();
  });
});
