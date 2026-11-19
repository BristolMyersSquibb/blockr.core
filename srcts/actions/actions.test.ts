// Board actions on the CoreApi for tests: those that core registers, its own and
// those that the classes of the app's plugins list, the classes that core
// refuses for a board action that breaks a rule, the board actions that on()
// gives for a target, and triggering one that asks the user nothing.

import { describe, expect, test } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import type { PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import { logged } from "../testing/logged.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import { listedModules } from "../testing/plugins.ts";
import { withPlugins } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";

// Core's board actions on a block, which come first in every menu of one.
const ON_BLOCK = ["blockr.core-copy_block_id_action", "blockr.core-remove_block_action"];

// A board action of blockr.test on the kinds of target given, labelled by its
// name, which runs no batch unless `more` gives a run of its own.
function action(name: string, on: BoardAction["on"], more: Partial<BoardAction> = {}): BoardAction {
  return { id: `blockr.test-${name}_action`, label: name, on, category: "test", run: () => undefined, ...more };
}

// A plugin class of blockr.test, which lists the board actions given.
function listing(name: string, boardActions: readonly unknown[], more: Partial<PluginClass> = {}): PluginClass {
  return { id: `blockr.test-${name}_plugin`, boardActions: boardActions as readonly BoardAction[], ...more };
}

// The CoreApi for tests on the board of the fixture hello, whose app runs core's
// plugins and one of each class given, after core's, in the order given.
function stubbed(classes: readonly PluginClass[], replied: HelloReply = HELLO) {
  return stubCore({
    hello: withPlugins(Object.fromEntries(classes.map(({ id }) => [id, { name: id }])), replied),
    modules: listedModules(classes),
  });
}

function ids(actions: readonly BoardAction[]): string[] {
  return actions.map(({ id }) => id);
}

// The notifications of module_failed that hold, without their ids.
function moduleFailures(core: CoreApi) {
  return held(core, (n) => notificationCode(n) === "module_failed");
}

// The module_failed about the class given, with the message given, as core
// raises it for a class that it refuses.
function refusedClass(plugin: string, message: string) {
  return {
    severity: "error",
    message,
    source: { kind: "core", code: "module_failed", module: { object: { plugin }, package: "blockr.test" } },
    about: { plugin },
  };
}

describe("the board actions that core registers", () => {
  test("are core's own, then those that each plugin class lists, in the order of the app's plugins and as each class lists them, as each board opens", async () => {
    const one = action("one", ["block"]);
    const two = action("two", ["block", "board"]);
    const three = action("three", ["block"]);
    const s = await stubbed([listing("first", [one, two]), listing("second", [three])]);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([...ON_BLOCK, one.id, two.id, three.id]);
    expect(s.core.boardActions.on({ board: true })).toEqual([two]);
    expect(s.core.boardActions.on({ board: true })[0]).toBe(two);

    // Opening a board ends the board actions of the board before, and core
    // registers them again for the next.
    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();
    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([...ON_BLOCK, one.id, two.id, three.id]);
    s.stop();
  });

  test.each<[string, Partial<BoardAction> & Readonly<Record<string, unknown>>, string]>([
    [
      "has the id of one of core's",
      { id: "blockr.core-remove_block_action" },
      'gives the board action "blockr.core-remove_block_action", which has the id of another board action.',
    ],
    ["has an empty on", { on: [] }, 'gives the board action "blockr.test-faulty_action", whose on is empty, so that no menu would offer it.'],
    [
      "has run beside quick",
      { quick: () => ({ rows: [] }) },
      'gives the board action "blockr.test-faulty_action", which has run beside quick, while run is for a board action that asks the user nothing.',
    ],
    [
      "has run beside advanced",
      { advanced: { render: () => () => undefined } },
      'gives the board action "blockr.test-faulty_action", which has run beside advanced, while run is for a board action that asks the user nothing.',
    ],
    [
      "has none of run, quick and advanced",
      { run: undefined },
      'gives the board action "blockr.test-faulty_action", which has none of run, quick and advanced.',
    ],
  ])("refuse the whole class where one of its board actions %s, with module_failed about the class", async (_rule, given, message) => {
    const fine = action("fine", ["board"]);
    const faulty = { ...action("faulty", ["block"]), ...given };
    const cls = listing("faulty", [fine, faulty], {
      commands: [{ id: "blockr.test-noop", label: "Do nothing", run: () => ({}) }],
    });
    const reported = logged();
    const s = await stubbed([cls]);
    reported.restore();

    const why = `The plugin class "blockr.test-faulty_plugin" ${message}`;
    expect(moduleFailures(s.core)).toEqual([refusedClass(cls.id, why)]);
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
    // Core registers neither the class, nor any of its board actions, nor its
    // commands.
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(cls.id);
    expect(s.core.boardActions.on({ board: true })).toEqual([]);
    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    expect(Object.keys(s.core.commands.registered())).not.toContain("blockr.test-noop");
    s.stop();
  });

  test("refuse a class whose board action has the id of one that the class lists before it, or that a class before it lists", async () => {
    const twice = listing("twice", [action("same", ["board"]), action("same", ["block"])]);
    const first = listing("first", [action("taken", ["board"])]);
    const second = listing("second", [action("taken", ["block"]), action("free", ["board"])]);
    const reported = logged();
    const s = await stubbed([twice, first, second]);
    reported.restore();

    expect(moduleFailures(s.core).map((n) => n.message)).toEqual([
      'The plugin class "blockr.test-twice_plugin" gives the board action "blockr.test-same_action", which has the id of another board action.',
      'The plugin class "blockr.test-second_plugin" gives the board action "blockr.test-taken_action", which has the id of another board action.',
    ]);
    expect(reported.errors()).toHaveLength(2);
    expect(Object.keys(s.core.plugins.appPlugins())).toContain(first.id);
    expect(ids(s.core.boardActions.on({ board: true }))).toEqual(["blockr.test-taken_action"]);
    s.stop();
  });

  test.each<[string, unknown, string]>([
    ["board actions that are no array", { not: "an array" }, "gives board actions that are no array."],
    ["a board action that is no object", ["remove"], "gives a board action that is no object."],
    [
      "a board action whose id names no package",
      [{ ...action("x", ["block"]), id: "remove_action" }],
      'gives a board action whose id "remove_action" is not named as a command\'s id is, "{pkg}-{name}".',
    ],
    ["a board action without a label", [{ ...action("x", ["block"]), label: "" }], 'gives the board action "blockr.test-x_action" without a label.'],
    [
      "a board action whose icon is no string",
      [{ ...action("x", ["block"]), icon: 1 }],
      'gives the board action "blockr.test-x_action", whose icon is no string of SVG markup.',
    ],
    [
      "a board action whose on is no array",
      [{ ...action("x", ["block"]), on: "block" }],
      'gives the board action "blockr.test-x_action", whose on is no array of the kinds of target that it applies to.',
    ],
    [
      "a board action whose on names no kind of target",
      [{ ...action("x", ["block"]), on: ["block", "node"] }],
      'gives the board action "blockr.test-x_action", whose on names "node", which is none of the kinds of target, "board", "block", "link", "stack", "option", "plugin" and "selection".',
    ],
    ["a board action without a category", [{ ...action("x", ["block"]), category: undefined }], 'gives the board action "blockr.test-x_action" without a category.'],
    [
      "a board action whose run is no function",
      [{ ...action("x", ["block"]), run: [] }],
      'gives the board action "blockr.test-x_action", whose run is no function.',
    ],
    [
      "a board action whose offers is no function",
      [{ ...action("x", ["block"]), offers: true }],
      'gives the board action "blockr.test-x_action", whose offers is no function.',
    ],
  ])("refuse a class that gives %s, as a package that writes its JS without types may", async (_what, given, message) => {
    const cls: PluginClass = { id: "blockr.test-untyped_plugin", boardActions: given as readonly BoardAction[] };
    const reported = logged();
    const s = await stubbed([cls]);
    reported.restore();

    expect(moduleFailures(s.core)).toEqual([refusedClass(cls.id, `The plugin class "blockr.test-untyped_plugin" ${message}`)]);
    expect(reported.errors()).toHaveLength(1);
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(cls.id);
    s.stop();
  });

  test("are kept for a class whose command the command registry refuses, which core only logs", async () => {
    const kept = action("kept", ["board"]);
    const cls = listing("clashing", [kept], {
      commands: [{ id: "blockr.core-remove_block", label: "Remove block", run: () => ({}) }],
    });
    const reported = logged();
    const s = await stubbed([cls]);
    reported.restore();

    expect(moduleFailures(s.core)).toEqual([]);
    expect(reported.errors().map((line) => line.message)).toEqual([
      'A command of the plugin class "blockr.test-clashing_plugin" is refused: The command "blockr.core-remove_block" is registered already.',
    ]);
    expect(Object.keys(s.core.plugins.appPlugins())).toContain(cls.id);
    expect(s.core.boardActions.on({ board: true })).toEqual([kept]);
    s.stop();
  });
});

describe("on()", () => {
  test("gives the board actions whose on names the kind of the target and whose offers passes for it or which have none, handing offers only targets of those kinds", async () => {
    const got: [Board, Target, unknown][] = [];
    const picky = action("picky", ["block", "selection"], {
      offers(board, target, lock) {
        got.push([board, target, lock]);
        return "selection" in target || ("block" in target && target.block === "b");
      },
    });
    const open = action("open", ["link"]);
    const s = await stubbed([listing("test", [picky, open])]);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([...ON_BLOCK, picky.id]);
    expect(ids(s.core.boardActions.on({ block: "c" }))).toEqual(ON_BLOCK);
    expect(ids(s.core.boardActions.on({ link: "ab" }))).toEqual(["blockr.core-remove_link_action", open.id]);
    expect(ids(s.core.boardActions.on({ selection: [{ block: "b" }] }))).toEqual(["blockr.core-remove_selected_action", picky.id]);
    expect(s.core.boardActions.on({ board: true })).toEqual([]);
    expect(s.core.boardActions.on({ plugin: "blockr.core-edit_board_plugin" })).toEqual([]);

    // The board, a copy of the target and the app's locks.
    expect(got.map(([, target]) => target)).toEqual([{ block: "b" }, { block: "c" }, { selection: [{ block: "b" }] }]);
    expect(got.every(([board, , lock]) => board === s.core.store.board() && lock === s.core.lock)).toBe(true);
    const [first] = got;
    expect(first !== undefined && Object.isFrozen(first[1])).toBe(true);
    s.stop();
  });

  test("leaves out a board action whose offers throws, as a fault of its package, which core logs", async () => {
    const broken = action("broken", ["block"], {
      offers: () => {
        throw new Error("boom");
      },
    });
    const s = await stubbed([listing("test", [broken])]);

    const reported = logged();
    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    reported.restore();
    expect(reported.errors().map((line) => line.message)).toEqual(['The offers of the board action "blockr.test-broken_action" failed: boom']);
    s.stop();
  });

  test("throws for what is no target, as a plugin in plain JS may give", async () => {
    const s = await stubCore();
    const on = (given: unknown) => () => s.core.boardActions.on(given as Target);

    for (const given of [undefined, "b", { block: "" }, { block: "b", link: "ab" }, { board: false }, { selection: [{ blok: "b" }] }]) {
      expect(on(given)).toThrow(TypeError);
    }
    expect(on({ blok: "b" })).toThrow(
      'A board action acts on the board, as { board: true }, on a board object, as { block: "a" }, or on a selection of them, as { selection: [{ block: "a" }, { link: "ab" }] }, not {"blok":"b"}.',
    );
    s.stop();
  });
});

describe("trigger()", () => {
  // A plugin class whose board action on blocks runs the batch that `run`
  // returns, and records what it got.
  function running(run: NonNullable<BoardAction["run"]>, more: Partial<BoardAction> = {}) {
    const got: { board: Board; target: Target; core: CoreApi }[] = [];
    const runs = action("runs", ["block"], {
      run(board, target, core) {
        got.push({ board, target, core });
        return run(board, target, core);
      },
      ...more,
    });
    return { got, cls: listing("test", [runs]), id: runs.id };
  }

  const rename = (name: string): CommandCall[] => [{ id: "blockr.core-rename_block", args: { id: "b", name } }];

  test("runs the batch that run returns as one step of the user, which one undo takes back, and calls run at once with the board, the target and CoreApi", async () => {
    const { got, cls, id } = running(() => [...rename("First"), { id: "blockr.core-remove_link", args: { id: "ab" } }]);
    const s = await stubbed([cls]);
    const board = s.core.store.board();

    const triggered = s.core.boardActions.trigger(id, { block: "b" });
    // Before trigger() waits for anything, as a copy within the user's click
    // needs.
    expect(got).toHaveLength(1);
    await expect(triggered).resolves.toBeUndefined();

    expect(got).toEqual([{ board, target: { block: "b" }, core: s.core }]);
    expect(s.core.store.board().blocks.b?.name).toBe("First");
    expect(Object.hasOwn(s.core.store.board().links, "ab")).toBe(false);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("waits for the batch where run returns a promise of it", async () => {
    const { cls, id } = running(() => Promise.resolve(rename("Later")));
    const s = await stubbed([cls]);

    await s.core.boardActions.trigger(id, { block: "b" });

    expect(s.core.store.board().blocks.b?.name).toBe("Later");
    s.stop();
  });

  test("hands extend the deltas that the batch will apply, and runs the commands that it returns after the batch, in the same step", async () => {
    const { cls, id } = running(() => [{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "n" } }]);
    const s = await stubbed([cls]);
    const board = s.core.store.board();
    const seen: unknown[] = [];

    await s.core.boardActions.trigger(id, { block: "b" }, {
      extend(deltas) {
        seen.push(...deltas.map((delta) => Object.keys(delta.blocks?.add ?? {})));
        return [{ id: "blockr.core-rename_block", args: { id: "n", name: "Placed" } }];
      },
    });

    expect(seen).toEqual([["n"]]);
    expect(s.core.store.board().blocks.n?.name).toBe("Placed");
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    s.stop();
  });

  test("runs nothing where on() leaves the board action out for the target, and resolves", async () => {
    const { got, cls, id } = running(() => rename("Never"), { offers: (_board, target) => "block" in target && target.block === "c" });
    const s = await stubbed([cls]);
    const board = s.core.store.board();

    await s.core.boardActions.trigger(id, { block: "b" });
    // A link is no target of the board action, whose on names blocks alone.
    await s.core.boardActions.trigger(id, { link: "ab" });

    expect(got).toEqual([]);
    expect(s.core.store.board()).toBe(board);
    s.stop();
  });

  test("runs nothing, not even extend, where run returns no batch or an empty one", async () => {
    const none = action("none", ["block"]);
    const empty = action("empty", ["block"], { run: () => [] });
    const s = await stubbed([listing("test", [none, empty])]);
    const board = s.core.store.board();
    let extended = 0;
    const extend = (): CommandCall[] => {
      extended++;
      return rename("Never");
    };

    await s.core.boardActions.trigger(none.id, { block: "b" }, { extend });
    await s.core.boardActions.trigger(empty.id, { block: "b" }, { extend });

    expect(extended).toBe(0);
    expect(s.core.store.board()).toBe(board);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("tells the user why the command registry refused the batch, or hands that to the notifier, and rejects with the refusal", async () => {
    const { cls, id } = running(() => [...rename("First"), { id: "blockr.core-remove_link", args: { id: "nowhere" } }]);
    const s = await stubbed([cls]);
    const board = s.core.store.board();
    const refusals = () => held(s.core, (n) => notificationCode(n) === "board_edit_failed");

    await expect(s.core.boardActions.trigger(id, { block: "b" })).rejects.toThrow(
      'The list of commands was refused at command 2, "blockr.core-remove_link"',
    );
    expect(refusals()).toEqual([
      {
        severity: "error",
        message: expect.stringContaining('The list of commands was refused at command 2, "blockr.core-remove_link"') as string,
        source: { kind: "core", code: "board_edit_failed" },
      },
    ]);
    expect(s.core.store.board()).toBe(board);

    const handed: (readonly Notification[])[] = [];
    await expect(
      s.core.boardActions.trigger(id, { block: "b" }, {
        notifier(notifications) {
          handed.push(notifications);
        },
      }),
    ).rejects.toThrow('The list of commands was refused at command 2, "blockr.core-remove_link"');
    expect(handed.map((notifications) => notifications.map(notificationCode))).toEqual([["board_edit_failed"]]);
    // The refusal before still holds, and the notifier took the second.
    expect(refusals()).toHaveLength(1);
    s.stop();
  });

  test("hands the notifier why the command registry refused to plan the batch for extend, which it does not call", async () => {
    const { cls, id } = running(() => [{ id: "blockr.core-remove_link", args: { id: "nowhere" } }]);
    const s = await stubbed([cls]);
    const handed: (readonly Notification[])[] = [];
    let extended = false;

    await expect(
      s.core.boardActions.trigger(id, { block: "b" }, {
        extend() {
          extended = true;
          return [];
        },
        notifier(notifications) {
          handed.push(notifications);
        },
      }),
    ).rejects.toThrow('The list of commands was refused at command 1, "blockr.core-remove_link"');

    expect(extended).toBe(false);
    expect(handed.map((notifications) => notifications.map(notificationCode))).toEqual([["board_edit_failed"]]);
    expect(held(s.core, (n) => notificationCode(n) === "board_edit_failed")).toEqual([]);
    s.stop();
  });

  test.each<[string, NonNullable<BoardAction["run"]>, string]>([
    [
      "throws",
      () => {
        throw new Error("boom");
      },
      'The board action "blockr.test-runs_action" failed: boom',
    ],
    ["rejects", () => Promise.reject(new Error("boom")), 'The board action "blockr.test-runs_action" failed: boom'],
    [
      "returns no batch",
      () => ({ id: "blockr.core-remove_block" }) as unknown as CommandCall[],
      'The run of the board action "blockr.test-runs_action" returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].',
    ],
  ])("rejects where run %s, as a fault of the board action's package, which core logs and tells nobody else", async (_what, run, message) => {
    const { cls, id } = running(run);
    const s = await stubbed([cls]);

    const reported = logged();
    await expect(s.core.boardActions.trigger(id, { block: "b" })).rejects.toThrow(message);
    reported.restore();

    expect(reported.errors().map((line) => line.message)).toEqual([message]);
    expect(held(s.core, (n) => n.source.kind === "core")).toEqual([]);
    s.stop();
  });

  test("rejects for an id that no board action has, for a board action without run and for what is no target, no notifier or no extend", async () => {
    const asks = { ...action("asks", ["block"]), run: undefined, quick: () => ({ rows: [] }) };
    const s = await stubbed([listing("test", [asks])]);
    const trigger = (...args: unknown[]) => (s.core.boardActions.trigger as (...given: unknown[]) => Promise<void>)(...args);

    await expect(trigger("blockr.test-nowhere_action", { block: "b" })).rejects.toThrow('No board action "blockr.test-nowhere_action" is registered.');
    await expect(trigger(asks.id, { block: "b" })).rejects.toThrow(
      'The board action "blockr.test-asks_action" asks the user through its quick action or its advanced action, which core does not open yet.',
    );
    await expect(trigger("blockr.core-remove_block_action", { blok: "b" })).rejects.toThrow(TypeError);
    await expect(trigger("blockr.core-remove_block_action", { block: "b" }, { notifier: "toast" })).rejects.toThrow(
      new TypeError("The `notifier` of trigger() is a function."),
    );
    await expect(trigger("blockr.core-remove_block_action", { block: "b" }, { extend: [] })).rejects.toThrow(
      new TypeError("The `extend` of trigger() is a function."),
    );
    await expect(trigger("blockr.core-remove_block_action", { block: "b" }, { extend: () => "rename" })).rejects.toThrow(
      'The `extend` of trigger() returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].',
    );
    expect(Object.hasOwn(s.core.store.board().blocks, "b")).toBe(true);
    s.stop();
  });
});
