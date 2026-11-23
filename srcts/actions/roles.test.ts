// Roles on the CoreApi for tests: core's, which core's board actions play,
// those that the classes of the app's plugins list, the classes that core
// refuses for a role that breaks a rule, the board action that the blockr
// option board_actions picks for a role, which plays it in place of its
// default, the picks for which core opens no board, and the check of each
// batch that the board action that plays a role runs.
// The fixture's board holds the blocks a, Iris; b, Head; c, Merge, with the
// inputs x and y; d, Wide sepals; e, Bind, a variadic block; and f, Scatter.
// Its links are ab, ac (x), bc (y), ad, be, de and keen_finch (a to f), and
// its stack s1, Input, holds a and b.

import { describe, expect, test } from "vitest";
import type { PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCore } from "../stubs/index.ts";
import { logged } from "../testing/logged.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import { listedModules } from "../testing/plugins.ts";
import { withPlugins } from "../testing/store.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { createBoardActions } from "./actions.ts";
import { batchFailure, checkedPlayer, CORE_ROLES } from "./roles.ts";
import type { Role } from "./roles.ts";

// Core's board actions on the block b, as on() gives them, which come first
// in every menu of it.
const ON_BLOCK = [
  "blockr.core-append_block_action",
  "blockr.core-add_link_action",
  "blockr.core-add_stack_action",
  "blockr.core-rename_action",
  "blockr.core-edit_inputs_action",
  "blockr.core-copy_block_id_action",
  "blockr.core-remove_block_action",
];

// What the page shows after why a pick of board_actions fails.
const NO_BOARD = "Core opens no board, since the blockr option board_actions holds for every board of the app.";

const ADD_BLOCK = "blockr.core-add_block";
const ADD_LINK = "blockr.core-add_link";

// The element that the user triggers from, which a trigger() needs.
const el = document.body;

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
// plugins and one of each class given, after core's, in the order given, with
// the value of the blockr option board_actions given, where given.
function stubbed(classes: readonly PluginClass[], picks?: Json): Promise<StubCore> {
  return stubCore({
    hello: withPlugins(Object.fromEntries(classes.map(({ id }) => [id, { name: id }])), HELLO),
    modules: listedModules(classes),
    ...(picks === undefined ? {} : { blockrOptions: { board_actions: picks } }),
  });
}

function ids(actions: readonly BoardAction[]): string[] {
  return actions.map(({ id }) => id);
}

// The notifications of the code given that hold, without their ids.
function raised(core: CoreApi, code: string) {
  return held(core, (n) => notificationCode(n) === code);
}

// A notification of the code given about the class of blockr.test given,
// with the message given, as core raises it for a fault of the class's
// package.
function about(plugin: string, code: string, message: string) {
  return {
    severity: "error",
    message,
    source: { kind: "core", code, module: { object: { plugin }, package: "blockr.test" } },
    about: { plugin },
  };
}

// The batch that adds the block n, a Head, after the block that the target
// names, as Append block's does.
function appending(target: Target, from?: string): CommandCall[] {
  return [
    { id: ADD_BLOCK, args: { registry_id: "blockr.core-head_block", id: "n" } },
    { id: ADD_LINK, args: { from: from ?? ("block" in target ? target.block : ""), to: "n" } },
  ];
}

// A role of blockr.test on blocks, whose board action renames the block.
const notesRole: Role = {
  id: "notes_role",
  on: ["block"],
  default: "blockr.test-notes_action",
  commands: [{ id: "blockr.core-rename_block", fixes: (_board, target, args) => "block" in target && (args as { id?: unknown }).id === target.block }],
};

// The board action that plays notes_role by default, which renames the block
// to the name given.
function notes(name: string): BoardAction {
  return action("notes", ["block"], {
    role: "notes_role",
    run: (_board, target) => ("block" in target ? [{ id: "blockr.core-rename_block", args: { id: target.block, name } }] : undefined),
  });
}

describe("core's roles", () => {
  test("each have one of core's board actions as their default, which can play them", () => {
    const actions = createBoardActions({ raise: () => () => undefined, preflight: () => true, blockRegistry: () => ({}) });

    for (const role of CORE_ROLES) {
      expect(checkedPlayer(role, actions.registered().find(({ id }) => id === role.default))).toEqual({ player: expect.objectContaining({ id: role.default }) as unknown });
    }
    // Every one of core's board actions but Copy block ID names one of them.
    expect(actions.registered().filter((action) => !CORE_ROLES.some((role) => role.default === action.id)).map(({ id }) => id)).toEqual([
      "blockr.core-copy_block_id_action",
    ]);
  });

  test("are played by core's board actions, so that trigger() and mountAdvanced() take a role's id in place of the board action's", async () => {
    const s = await stubCore();

    await s.core.boardActions.trigger("remove_block_role", { block: "f" }, { el });
    expect(Object.hasOwn(s.core.store.board().blocks, "f")).toBe(false);

    const opened = s.core.boardActions.trigger("append_block_role", { block: "b" }, { el });
    expect(s.menu()?.level()).toMatchObject({ label: "Append block" });
    s.menu()?.escape();
    await opened;

    const room = document.createElement("div");
    const unmount = s.core.boardActions.mountAdvanced(room, "add_block_role", { board: true });
    expect(room.querySelector('form[aria-label="Add block"]')).not.toBeNull();
    unmount();
    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    s.stop();
  });

  test("throw for the id of a role that no package registers, as for a board action", async () => {
    const s = await stubCore();

    await expect(s.core.boardActions.trigger("nowhere_role", { block: "b" }, { el })).rejects.toThrow('No role "nowhere_role" is registered.');
    expect(() => s.core.boardActions.mountAdvanced(document.createElement("div"), "nowhere_role", { board: true })).toThrow(
      'No role "nowhere_role" is registered.',
    );
    expect(() => s.core.boardActions.mountAdvanced(document.createElement("div"), "remove_block_role", { block: "b" })).toThrow(
      'The board action "blockr.core-remove_block_action", which plays the role "remove_block_role", has no advanced action to mount.',
    );
    s.stop();
  });
});

describe("the roles that a plugin class lists", () => {
  test("are registered with the class, so that a board action of the class plays its role by default", async () => {
    const s = await stubbed([listing("notes", [notes("Notes")], { roles: [notesRole] })]);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([...ON_BLOCK, "blockr.test-notes_action"]);
    await s.core.boardActions.trigger("notes_role", { block: "b" }, { el });
    expect(s.core.store.board().blocks.b?.name).toBe("Notes");
    s.stop();
  });

  test.each<[string, unknown, string]>([
    ["are no array", { notes: notesRole }, "gives roles that are no array."],
    ["are no object", ["notes_role"], "gives a role that is no object."],
    ["has an id that is no string", [{ ...notesRole, id: 3 }], 'gives a role whose id 3 is no string, as "append_block_role".'],
    [
      "has an id that names a package",
      [{ ...notesRole, id: "blockr.test-notes_role" }],
      'gives the role "blockr.test-notes_role", whose id names a package, while a role\'s id names none, as "append_block_role".',
    ],
    [
      "has the id of one of core's",
      [{ ...notesRole, id: "append_block_role" }],
      ', of the package blockr.test, gives the role "append_block_role", which the package blockr.core gives already.',
    ],
    ["has an empty on", [{ ...notesRole, on: [] }], 'gives the role "notes_role", whose on is empty, so that no board action could play it.'],
    [
      "names what is no kind of target in on",
      [{ ...notesRole, on: ["blocks"] }],
      'gives the role "notes_role", whose on names "blocks", which is none of the kinds of target, "board", "block", "link", "stack", "option", "plugin" and "selection".',
    ],
    [
      "has a default that is not named as a board action's id is",
      [{ ...notesRole, default: "notes" }],
      'gives the role "notes_role", whose default "notes" is not named as a board action\'s id is, "{pkg}-{name}".',
    ],
    ["has an advanced that is not true", [{ ...notesRole, advanced: false }], 'gives the role "notes_role", whose advanced is neither true nor left out.'],
    ["has commands that are no array", [{ ...notesRole, commands: {} }], 'gives the role "notes_role", whose commands are no array.'],
    ["runs no command", [{ ...notesRole, commands: [] }], 'gives the role "notes_role", which runs no command.'],
    ["has a command that is no object", [{ ...notesRole, commands: ["rename"] }], 'gives the role "notes_role", with a command that is no object.'],
    [
      "has a command whose id is not named as a command's id is",
      [{ ...notesRole, commands: [{ id: "rename_block" }] }],
      'gives the role "notes_role", with a command whose id "rename_block" is not named as a command\'s id is, "{pkg}-{name}".',
    ],
    [
      "has a command whose each names no kind that each takes",
      [{ ...notesRole, commands: [{ id: "blockr.core-rename_block", each: "blocks" }] }],
      'gives the role "notes_role", whose command "blockr.core-rename_block" has each "blocks", which is none of "block", "input", "link" and "stack".',
    ],
    [
      "has a command whose optional is not true",
      [{ ...notesRole, commands: [{ id: "blockr.core-rename_block", optional: false }] }],
      'gives the role "notes_role", whose command "blockr.core-rename_block" has optional false, which is neither true nor left out.',
    ],
    [
      "has a command whose fixes is no function",
      [{ ...notesRole, commands: [{ id: "blockr.core-rename_block", fixes: "b" }] }],
      'gives the role "notes_role", whose command "blockr.core-rename_block" has a fixes that is no function.',
    ],
  ])("refuse the whole class where its roles %s, with module_failed about the class", async (_rule, roles, message) => {
    const fine = action("fine", ["board"]);
    const cls = listing("faulty", [fine], {
      roles: roles as readonly Role[],
      commands: [{ id: "blockr.test-noop", label: "Do nothing", run: () => ({}) }],
    });
    const reported = logged();
    const s = await stubbed([cls]);
    reported.restore();

    const why = `The plugin class "blockr.test-faulty_plugin"${message.startsWith(",") ? "" : " "}${message}`;
    expect(raised(s.core, "module_failed")).toEqual([about(cls.id, "module_failed", why)]);
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
    // Core registers neither the class, nor any of its board actions, nor its
    // commands.
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(cls.id);
    expect(ids(s.core.boardActions.on({ board: true }))).not.toContain(fine.id);
    expect(Object.keys(s.core.commands.registered())).not.toContain("blockr.test-noop");
    s.stop();
  });

  test.each<[string, readonly BoardAction[], Role, string]>([
    [
      "is no board action that the class lists",
      [],
      notesRole,
      'gives the role "notes_role", whose default "blockr.test-notes_action" cannot play it, since it is no board action that the class lists.',
    ],
    [
      "names no role",
      [action("notes", ["block"])],
      notesRole,
      'gives the role "notes_role", whose default "blockr.test-notes_action" cannot play it, since that board action names no role.',
    ],
    [
      "names another role",
      [action("notes", ["block"], { role: "append_block_role" })],
      notesRole,
      'gives the role "notes_role", whose default "blockr.test-notes_action" cannot play it, since that board action names the role "append_block_role".',
    ],
    [
      "names other targets in on than the role",
      [notes("Notes")],
      { ...notesRole, on: ["link"] },
      'gives the role "notes_role", whose default "blockr.test-notes_action" cannot play it, since the on of that board action names "block", where the targets of the role are "link".',
    ],
    [
      "has no advanced action, while callers of the role mount one",
      [notes("Notes")],
      { ...notesRole, advanced: true },
      'gives the role "notes_role", whose default "blockr.test-notes_action" cannot play it, since that board action has no advanced action, which callers of the role mount.',
    ],
  ])("refuse the whole class where a role's default %s, with module_failed about the class, as for any role that breaks a rule", async (_rule, actions, role, message) => {
    const cls = listing("notes", actions, { roles: [role] });
    const reported = logged();
    const s = await stubbed([cls]);
    reported.restore();

    const why = `The plugin class "blockr.test-notes_plugin" ${message}`;
    expect(raised(s.core, "module_failed")).toEqual([about(cls.id, "module_failed", why)]);
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(cls.id);
    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    await expect(s.core.boardActions.trigger("notes_role", { block: "b" }, { el })).rejects.toThrow('No role "notes_role" is registered.');
    s.stop();
  });

  test("refuse a class whose role has the id of one of another package's classes, or of one that the class lists before it, naming both packages", async () => {
    const first = listing("first", [notes("First")], { roles: [notesRole] });
    const other: PluginClass = { id: "blockr.other-second_plugin", roles: [notesRole] };
    const twiceRole: Role = { ...notesRole, id: "twice_role", default: "blockr.test-twice_action" };
    const twice = listing("twice", [action("twice", ["block"], { role: "twice_role" })], { roles: [twiceRole, twiceRole] });
    const reported = logged();
    const s = await stubbed([first, other, twice]);
    reported.restore();

    expect(raised(s.core, "module_failed").map(({ message }) => message)).toEqual([
      'The plugin class "blockr.other-second_plugin", of the package blockr.other, gives the role "notes_role", which the package blockr.test gives already.',
      'The plugin class "blockr.test-twice_plugin", of the package blockr.test, gives the role "twice_role", which the package blockr.test gives already.',
    ]);
    expect(Object.keys(s.core.plugins.appPlugins())).toContain(first.id);
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(other.id);
    expect(Object.keys(s.core.plugins.appPlugins())).not.toContain(twice.id);
    s.stop();
  });
});

describe("the board action that plays a role", () => {
  test("is the one that board_actions picks for the role, in the place of the role's default in every menu, which trigger() and mountAdvanced() act on by the role's id", async () => {
    const append = action("kit_append", ["block"], {
      role: "append_block_role",
      run: undefined,
      quick: (_board, target) => ({ rows: [{ kind: "item", label: "Head", pick: () => appending(target) }] }),
    });
    const renders: Target[] = [];
    const add = action("kit_add", ["board"], {
      role: "add_block_role",
      run: undefined,
      advanced: {
        render({ target }) {
          renders.push(target);
          return () => undefined;
        },
      },
    });
    const s = await stubbed([listing("kit", [append, add])], {
      append_block_role: append.id,
      add_block_role: add.id,
    });

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([append.id, ...ON_BLOCK.slice(1)]);
    expect(ids(s.core.boardActions.on({ board: true }))[0]).toBe(add.id);
    expect(ids(s.core.boardActions.on({ board: true }))).not.toContain("blockr.core-add_block_action");

    const triggered = s.core.boardActions.trigger("append_block_role", { block: "b" }, { el });
    expect(s.menu()?.level()).toMatchObject({ label: "kit_append" });
    await s.menu()?.pick("Head");
    await triggered;
    expect(Object.values(s.core.store.board().links)).toContainEqual({ from: "b", to: "n", input: "data" });

    // Core's Append block, which names the role, no longer plays it, so that
    // trigger() runs nothing for its id.
    await s.core.boardActions.trigger("blockr.core-append_block_action", { block: "b" }, { el });
    expect(s.menu()).toBeUndefined();

    const room = document.createElement("div");
    s.core.boardActions.mountAdvanced(room, "add_block_role", { board: true })();
    s.core.boardActions.mountAdvanced(room, "blockr.core-add_block_action", { board: true })();
    expect(renders).toEqual([{ board: true }]);
    expect(room.childElementCount).toBe(0);
    s.stop();
  });

  test("is the role's default where board_actions holds an empty list, as R sends list()", async () => {
    const s = await stubbed([], []);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    s.stop();
  });

  test("is the only one in a menu: a board action that names a role that it does not play is in none, and trigger() and mountAdvanced() run nothing for its id", async () => {
    const renders: Target[] = [];
    const stray = action("stray", ["block"], { role: "append_block_role", run: () => [{ id: "blockr.core-remove_block", args: { id: "b" } }] });
    const lost = action("lost", ["block"], {
      role: "nowhere_role",
      run: undefined,
      advanced: {
        render({ target }) {
          renders.push(target);
          return () => undefined;
        },
      },
    });
    const s = await stubbed([listing("kit", [stray, lost])]);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual(ON_BLOCK);
    await s.core.boardActions.trigger(stray.id, { block: "b" }, { el });
    expect(Object.hasOwn(s.core.store.board().blocks, "b")).toBe(true);
    s.core.boardActions.mountAdvanced(document.createElement("div"), lost.id, { block: "b" })();
    expect(renders).toEqual([]);
    s.stop();
  });

  test.each<[string, readonly BoardAction[], Json, string]>([
    [
      "picks a board action that no package registers",
      [],
      { append_block_role: "blockr.test-nowhere_action" },
      'The blockr option board_actions picks the board action "blockr.test-nowhere_action" to play the role "append_block_role", but no package registers that board action.',
    ],
    [
      "picks a board action that names no role",
      [action("kit_append", ["block"])],
      { append_block_role: "blockr.test-kit_append_action" },
      'The blockr option board_actions picks the board action "blockr.test-kit_append_action" to play the role "append_block_role", but that board action names no role.',
    ],
    [
      "picks a board action that names another role",
      [action("kit_append", ["block"], { role: "prepend_block_role" })],
      { append_block_role: "blockr.test-kit_append_action" },
      'The blockr option board_actions picks the board action "blockr.test-kit_append_action" to play the role "append_block_role", but that board action names the role "prepend_block_role".',
    ],
    [
      "picks a board action whose on names other targets than the role's",
      [action("kit_append", ["block", "link"], { role: "append_block_role" })],
      { append_block_role: "blockr.test-kit_append_action" },
      'The blockr option board_actions picks the board action "blockr.test-kit_append_action" to play the role "append_block_role", but the on of that board action names "block" and "link", where the targets of the role are "block".',
    ],
    [
      "picks a board action without an advanced action for a role that callers mount",
      [action("kit_add", ["board"], { role: "add_block_role" })],
      { add_block_role: "blockr.test-kit_add_action" },
      'The blockr option board_actions picks the board action "blockr.test-kit_add_action" to play the role "add_block_role", but that board action has no advanced action, which callers of the role mount.',
    ],
    [
      "picks a board action for a role that no package registers",
      [],
      { nowhere_role: "blockr.core-add_block_action" },
      'The blockr option board_actions picks the board action "blockr.core-add_block_action" to play the role "nowhere_role", but no package registers that role.',
    ],
    [
      "holds no list, as an environment variable gives it",
      [],
      "mykit-append_block_action",
      'The blockr option board_actions holds "mykit-append_block_action", where it picks a board action for a role by their ids, as list(append_block_role = "mykit-append_block_action") does in R.',
    ],
  ])("opens no board where board_actions %s, and says why, naming the pick, the role and what failed", async (_what, actions, picks, why) => {
    const reported = logged();
    await expect(stubbed([listing("kit", actions)], picks)).rejects.toThrow(`${why}\n${NO_BOARD}`);
    reported.restore();
  });
});

describe("the check of a batch", () => {
  // The board action of blockr.test that board_actions picks for
  // append_block_role, which runs the batch that `run` returns.
  function appendingKit(run: NonNullable<BoardAction["run"]>) {
    const append = action("kit_append", ["block"], { role: "append_block_role", run });
    return { cls: listing("kit", [append]), picks: { append_block_role: append.id } };
  }

  // What core says of a batch of the kit's board action that breaks its role,
  // for the reason given.
  const breaks = (why: string): string =>
    `Core refused the batch of the board action "blockr.test-kit_append_action", which breaks its role "append_block_role": ${why}. The role runs "blockr.core-add_block", then "blockr.core-add_link".`;

  test.each<[string, (target: Target) => CommandCall[], string]>([
    [
      "runs a command that the role does not run",
      (target) => [...appending(target), { id: "blockr.core-remove_block", args: { id: "f" } }],
      'its call 3 is of "blockr.core-remove_block", which the role does not run',
    ],
    ["links the new block to another block than the target", (target) => appending(target, "c"), 'the args of its call 2, of "blockr.core-add_link", do not keep what the target fixes'],
    ["runs the role's commands out of order", (target) => [...appending(target)].reverse(), 'its call 1, of "blockr.core-add_link", comes where the role runs "blockr.core-add_block"'],
    [
      "runs a command more often than the role runs it",
      (target) => [...appending(target), { id: ADD_LINK, args: { from: "b", to: "f" } }],
      'its call 3, of "blockr.core-add_link", comes out of the role\'s order, or more often than the role runs it',
    ],
    ["leaves out a command of the role", (target) => appending(target).slice(0, 1), 'its batch holds no call of "blockr.core-add_link"'],
  ])("refuses a batch that %s before anything runs, as a fault of the board action's package, naming the role and the call", async (_what, batch, why) => {
    const { cls, picks } = appendingKit((_board, target) => batch(target));
    const s = await stubbed([cls], picks);
    const board = s.core.store.board();

    const reported = logged();
    await expect(s.core.boardActions.trigger("append_block_role", { block: "b" }, { el })).rejects.toThrow(breaks(why));
    reported.restore();

    expect(s.core.store.board()).toEqual(board);
    expect(s.core.store.canUndo()).toBe(false);
    expect(raised(s.core, "board_action_failed")).toEqual([about(cls.id, "board_action_failed", breaks(why))]);
    expect(reported.errors().map((line) => line.message)).toEqual([breaks(why)]);
    s.stop();
  });

  test("checks the batch of a row of the quick action, and of done(), which rejects and leaves the advanced action open", async () => {
    let done: ((calls?: readonly CommandCall[]) => Promise<void>) | undefined;
    const append = action("kit_append", ["block"], {
      role: "append_block_role",
      run: undefined,
      quick: (_board, target) => ({ rows: [{ kind: "item", label: "Head", pick: () => appending(target, "c") }] }),
      advanced: {
        render(args) {
          ({ done } = args);
          return () => undefined;
        },
      },
    });
    const s = await stubbed([listing("kit", [append])], { append_block_role: append.id });
    const why = breaks('the args of its call 2, of "blockr.core-add_link", do not keep what the target fixes');

    const reported = logged();
    const picked = s.core.boardActions.trigger("append_block_role", { block: "b" }, { el });
    await expect(s.menu()?.pick("Head")).rejects.toThrow(why);
    await expect(picked).rejects.toThrow(why);

    void s.core.boardActions.trigger("append_block_role", { block: "b" }, { el });
    void s.menu()?.openAdvanced();
    await expect(done?.(appending({ block: "b" }, "c"))).rejects.toThrow(why);
    expect(s.advanced()?.closed()).toBe(false);
    reported.restore();

    expect(Object.hasOwn(s.core.store.board().blocks, "n")).toBe(false);
    expect(reported.errors().map((line) => line.message)).toEqual([why, why]);
    s.stop();
  });

  test("leaves out the commands that the caller's extend adds after the batch", async () => {
    const { cls, picks } = appendingKit((_board, target) => appending(target));
    const s = await stubbed([cls], picks);

    await s.core.boardActions.trigger("append_block_role", { block: "b" }, {
      el,
      extend: () => [{ id: "blockr.core-rename_block", args: { id: "n", name: "Appended" } }],
    });

    expect(s.core.store.board().blocks.n?.name).toBe("Appended");
    s.core.store.undo();
    expect(Object.hasOwn(s.core.store.board().blocks, "n")).toBe(false);
    s.stop();
  });
});

describe("the check of a batch against one of core's roles", () => {
  const role = (id: string): Role => {
    const found = CORE_ROLES.find((each) => each.id === id);
    if (found === undefined) throw new Error(`Core has no role "${id}".`);
    return found;
  };
  const call = (id: string, args: Json): CommandCall => ({ id: `blockr.core-${id}`, args });

  test.each<[string, string, Target, CommandCall[], string | undefined]>([
    [
      "removes each link, block and stack of a selection once, links first",
      "remove_selected_role",
      { selection: [{ stack: "s1" }, { block: "f" }, { link: "ab" }, { block: "f" }, { link: "nowhere" }] },
      [call("remove_link", { id: "ab" }), call("remove_block", { id: "f" }), call("remove_stack", { id: "s1" })],
      undefined,
    ],
    [
      "leaves out a block of the selection",
      "remove_selected_role",
      { selection: [{ block: "f" }, { block: "c" }] },
      [call("remove_block", { id: "f" })],
      'its batch holds 1 call of "blockr.core-remove_block" in a row, where the target holds 2 blocks',
    ],
    [
      "removes a block that the selection does not hold",
      "remove_selected_role",
      { selection: [{ block: "f" }] },
      [call("remove_block", { id: "c" })],
      'the args of its call 1, of "blockr.core-remove_block", do not keep what the target fixes',
    ],
    ["moves the link alone", "edit_link_role", { link: "be" }, [call("move_link", { id: "be" })], undefined],
    [
      "changes and moves the link out of order",
      "edit_link_role",
      { link: "be" },
      [call("move_link", { id: "be" }), call("change_link", { id: "be", from: "a" })],
      'its call 2, of "blockr.core-change_link", comes out of the role\'s order, or more often than the role runs it',
    ],
    [
      "changes another link",
      "edit_link_role",
      { link: "be" },
      [call("change_link", { id: "de", from: "a" })],
      'the args of its call 1, of "blockr.core-change_link", do not keep what the target fixes',
    ],
    [
      "adds any number of inputs to a variadic block",
      "edit_inputs_role",
      { block: "e" },
      [call("add_link", { from: "a", to: "e" }), call("add_link", { from: "c", to: "e" }), call("add_link", { from: "f", to: "e" })],
      undefined,
    ],
    [
      "removes the links of the inputs of a block that takes each by its name, once for each input",
      "edit_inputs_role",
      { block: "c" },
      [call("remove_link", { id: "ac" }), call("remove_link", { id: "bc" })],
      undefined,
    ],
    [
      "removes a link into another block",
      "edit_inputs_role",
      { block: "c" },
      [call("remove_link", { id: "ab" })],
      'the args of its call 1, of "blockr.core-remove_link", do not keep what the target fixes',
    ],
    [
      "moves a link into another block",
      "edit_inputs_role",
      { block: "c" },
      [call("change_link", { id: "ac", to: "d" })],
      'the args of its call 1, of "blockr.core-change_link", do not keep what the target fixes',
    ],
    ["renames the stack", "rename_role", { stack: "s1" }, [call("rename_stack", { id: "s1", name: "Sources" })], undefined],
    [
      "renames a block on a stack",
      "rename_role",
      { stack: "s1" },
      [call("rename_block", { id: "s1", name: "Sources" })],
      'the args of its call 1, of "blockr.core-rename_block", do not keep what the target fixes',
    ],
    [
      "renames the stack and changes it, in this order",
      "edit_stack_role",
      { stack: "s1" },
      [call("rename_stack", { id: "s1", name: "Sources" }), call("change_stack", { id: "s1", color: "#66c2a5" })],
      undefined,
    ],
    [
      "adds a stack of exactly the selection's blocks",
      "add_stack_role",
      { selection: [{ block: "c" }, { link: "ab" }, { block: "f" }] },
      [call("add_stack", { blocks: ["f", "c"] })],
      undefined,
    ],
    [
      "adds a stack of the selection's blocks and another",
      "add_stack_role",
      { selection: [{ block: "c" }, { link: "ab" }, { block: "f" }] },
      [call("add_stack", { blocks: ["c", "f", "d"] })],
      'the args of its call 1, of "blockr.core-add_stack", do not keep what the target fixes',
    ],
    [
      "adds a stack without a block of the selection",
      "add_stack_role",
      { selection: [{ block: "c" }, { block: "f" }] },
      [call("add_stack", { blocks: ["c"] })],
      'the args of its call 1, of "blockr.core-add_stack", do not keep what the target fixes',
    ],
    ["adds a stack of any blocks on the board", "add_stack_role", { board: true }, [call("add_stack", { blocks: ["c", "d"] })], undefined],
    [
      "puts a block on a link, from the block that the link comes from",
      "insert_block_role",
      { link: "ab" },
      [
        call("add_block", { registry_id: "blockr.core-head_block", id: "n" }),
        call("add_link", { from: "a", to: "n" }),
        call("change_link", { id: "ab", from: "n" }),
      ],
      undefined,
    ],
    [
      "puts a block on a link, from another block",
      "insert_block_role",
      { link: "ab" },
      [call("add_block", { registry_id: "blockr.core-head_block", id: "n" }), call("add_link", { from: "b", to: "n" }), call("change_link", { id: "ab", from: "n" })],
      'the args of its call 2, of "blockr.core-add_link", do not keep what the target fixes',
    ],
    ["adds a link into the block", "add_link_role", { block: "e" }, [call("add_link", { from: "f", to: "e" })], undefined],
    [
      "adds a link that neither starts nor ends at the block",
      "add_link_role",
      { block: "f" },
      [call("add_link", { from: "c", to: "e" })],
      'the args of its call 1, of "blockr.core-add_link", do not keep what the target fixes',
    ],
    ["adds any link on the board", "add_link_role", { board: true }, [call("add_link", { from: "c", to: "e" })], undefined],
  ])("%s keeps the role, or breaks it as said", async (_what, id, target, batch, why) => {
    const s = await stubCore();

    expect(batchFailure(role(id), s.core.store.board(), s.core.blockRegistry(), target, batch)).toBe(why);
    s.stop();
  });
});
