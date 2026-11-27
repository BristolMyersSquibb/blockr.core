// Roles on the CoreApi for tests: core's, which core's board actions play,
// those that the classes of the app's plugins list, a class's role that breaks
// a rule, for which core opens no board, the board action that the blockr
// option board_actions picks for a role, which plays it in place of its
// default, and the picks for which core opens no board. Core checks no batch
// of the board action that plays a role, whose edit is the role's contract.
// The fixture's board holds the blocks a, Iris; b, Head; c, Merge, with the
// inputs x and y; d, Wide sepals; e, Bind, a variadic block; and f, Scatter.
// Its links are ab, ac (x), bc (y), ad, be, de and keen_finch (a to f), and
// its stack s1, Input, holds a and b.

import { describe, expect, test } from "vitest";
import type { PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCore } from "../stubs/index.ts";
import { logged } from "../testing/logged.ts";
import { listedModules, noBoard, refusedClass } from "../testing/plugins.ts";
import { withPlugins } from "../testing/store.ts";
import type { Json } from "../types/json.ts";
import { interned, nestedRecord } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { createBoardActions } from "./actions.ts";
import { checkedPlayer, CORE_ROLES } from "./roles.ts";
import type { Role } from "./roles.ts";
import type { BoardActionId } from "../types/board.ts";

// Core's board actions on the block b, as on() gives them, which come first
// in every menu of it.
const ON_BLOCK = ofCore(
  "append_block_action",
  "add_link_action",
  "add_stack_action",
  "rename_action",
  "edit_inputs_action",
  "copy_block_id_action",
  "remove_block_action",
);

// What the page shows after why a pick of board_actions fails.
const NO_BOARD = "Core opens no board, since the blockr option board_actions holds for every board of the app.";

const ADD_BLOCK = interned("blockr.core", "add_block");
const ADD_LINK = interned("blockr.core", "add_link");

// The element that the user triggers from, which a trigger() needs.
const el = document.body;

// A board action as a plugin class defines it, without the package, which core
// sets to that of the class as it registers the class.
type Definition = Omit<BoardAction, "package">;

// A board action of blockr.test on the kinds of target given, labelled by its
// name, which runs no batch unless `more` gives a run of its own.
function action(name: string, on: BoardAction["on"], more: Partial<Definition> = {}): Definition {
  return { id: `${name}_action`, label: name, on, category: "test", run: () => undefined, ...more };
}

// A plugin class of blockr.test, which lists the board actions given.
function listing(name: string, boardActions: readonly unknown[], more: Partial<PluginClass> = {}): PluginClass {
  return { id: interned("blockr.test", `${name}_plugin`), boardActions: boardActions as readonly Omit<BoardAction, "package">[], ...more };
}

// The CoreApi for tests on the board of the fixture hello, whose app runs core's
// plugins and one of each class given, after core's, with the value of the
// blockr option board_actions given, where given.
// The app runs the classes of each package together, in the order given, and
// the packages in the order of their first classes.
function stubbed(classes: readonly PluginClass[], picks?: Json): Promise<StubCore> {
  return stubCore({
    hello: withPlugins(nestedRecord(classes.map(({ id }) => [id, { name: id.id }] as const)), HELLO),
    ...listedModules(classes),
    ...(picks === undefined ? {} : { blockrOptions: { board_actions: picks } }),
  });
}

// Each board action by its package and its id, as a caller names it.
function ids(actions: readonly BoardAction[]): { package: string; id: string }[] {
  return actions.map((each) => ({ package: each.package, id: each.id }));
}

// Core's board actions with the ids given, by their package and their ids.
function ofCore(...given: readonly string[]): { package: string; id: string }[] {
  return given.map((id) => ({ package: "blockr.core", id }));
}

// A board action of blockr.test by its package and its id, which core gives it
// as it registers the class that lists it.
function ofTest(action: Pick<Definition, "id">): BoardActionId {
  return interned("blockr.test", action.id);
}

// The batch that adds the block n, a Head, after the block that the target
// names, as Append block's does.
function appending(target: Target, from?: string): CommandCall[] {
  return [
    { id: ADD_BLOCK, args: { registry_id: { package: "blockr.core", id: "head_block" }, id: "n" } },
    { id: ADD_LINK, args: { from: from ?? ("block" in target ? target.block : ""), to: "n" } },
  ];
}

// A role of blockr.test on blocks, whose board action renames the block.
// Its default names a board action of its class by the id alone, within the
// class's package.
const notesRole: Role = { id: "notes_role", on: ["block"], default: "notes_action" };

// The board action that plays notes_role by default, which renames the block
// to the name given.
function notes(name: string): Definition {
  return action("notes", ["block"], {
    role: "notes_role",
    run: (_board, target) => ("block" in target ? [{ id: interned("blockr.core", "change_block"), args: { id: target.block, name } }] : undefined),
  });
}

describe("core's roles", () => {
  test("each have one of core's board actions as their default, which can play them", () => {
    const actions = createBoardActions({ raise: () => () => undefined, preflight: () => true, blockRegistry: () => new Map() });

    for (const role of CORE_ROLES) {
      const fallback = actions.registered().find((each) => each.package === "blockr.core" && each.id === role.default);
      expect(checkedPlayer(role, fallback)).toEqual({ player: expect.objectContaining({ package: "blockr.core", id: role.default }) as unknown });
    }
    // Every one of core's board actions but Copy block ID names one of them.
    expect(ids(actions.registered().filter((action) => !CORE_ROLES.some((role) => role.default === action.id)))).toEqual(
      ofCore("copy_block_id_action"),
    );
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
      'The board action "remove_block_action of blockr.core", which plays the role "remove_block_role", has no advanced action to mount.',
    );
    s.stop();
  });
});

describe("the roles that a plugin class lists", () => {
  test("are registered with the class, so that a board action of the class plays its role by default", async () => {
    const s = await stubbed([listing("notes", [notes("Notes")], { roles: [notesRole] })]);

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([...ON_BLOCK, ofTest({ id: "notes_action" })]);
    await s.core.boardActions.trigger("notes_role", { block: "b" }, { el });
    expect(s.core.store.board().blocks.b?.name).toBe("Notes");
    s.stop();
  });

  test.each<[string, unknown, string]>([
    ["are no array", { notes: notesRole }, "gives roles that are no array."],
    ["are no object", ["notes_role"], "gives a role that is no object."],
    ["has an id that is no string", [{ ...notesRole, id: 3 }], 'gives a role whose id 3 is no string, as "append_block_role".'],
    [
      "has the id of one of core's",
      [{ ...notesRole, id: "append_block_role" }],
      ', of the package blockr.test, gives the role "append_block_role", which the package blockr.core gives already.',
    ],
    ["has an empty on", [{ ...notesRole, on: [] }], 'gives the role "notes_role", whose on is empty, so that no board action could play it.'],
    [
      "names what is no kind of target in on",
      [{ ...notesRole, on: ["blocks"] }],
      'gives the role "notes_role", whose on names "blocks", which is none of the kinds of target, "board", "block", "input", "link", "stack", "option", "plugin" and "selection".',
    ],
    [
      "has a default that is no board action's id alone, as where it names the package too",
      [{ ...notesRole, default: { package: "blockr.test", id: "notes_action" } }],
      'gives the role "notes_role", whose default {"package":"blockr.test","id":"notes_action"} is no id of a board action of the class, as "append_block_action".',
    ],
    ["has an advanced that is not true", [{ ...notesRole, advanced: false }], 'gives the role "notes_role", whose advanced is neither true nor left out.'],
  ])("open no board where a class's roles %s, and say why, naming the class's module with its package", async (_rule, roles, message) => {
    const cls = listing("faulty", [action("fine", ["board"])], { roles: roles as readonly Role[] });
    const why = noBoard(refusedClass(cls.id, `The plugin class "faulty_plugin of blockr.test"${message.startsWith(",") ? "" : " "}${message}`));
    const reported = logged();
    await expect(stubbed([cls])).rejects.toThrow(why);
    reported.restore();

    expect(reported.errors().map((line) => line.message)).toEqual([why]);
  });

  test.each<[string, readonly Definition[], Role, string]>([
    [
      "is no board action that the class lists",
      [],
      notesRole,
      'gives the role "notes_role", whose default "notes_action" cannot play it, since it is no board action that the class lists.',
    ],
    [
      "names no role",
      [action("notes", ["block"])],
      notesRole,
      'gives the role "notes_role", whose default "notes_action" cannot play it, since that board action names no role.',
    ],
    [
      "names another role",
      [action("notes", ["block"], { role: "append_block_role" })],
      notesRole,
      'gives the role "notes_role", whose default "notes_action" cannot play it, since that board action names the role "append_block_role".',
    ],
    [
      "names other targets in on than the role",
      [notes("Notes")],
      { ...notesRole, on: ["link"] },
      'gives the role "notes_role", whose default "notes_action" cannot play it, since the on of that board action names "block", where the targets of the role are "link".',
    ],
    [
      "has no advanced action, while callers of the role mount one",
      [notes("Notes")],
      { ...notesRole, advanced: true },
      'gives the role "notes_role", whose default "notes_action" cannot play it, since that board action has no advanced action, which callers of the role mount.',
    ],
  ])("open no board where a role's default %s, as for any role that breaks a rule", async (_rule, actions, role, message) => {
    const cls = listing("notes", actions, { roles: [role] });
    const reported = logged();
    await expect(stubbed([cls])).rejects.toThrow(noBoard(refusedClass(cls.id, `The plugin class "notes_plugin of blockr.test" ${message}`)));
    reported.restore();
  });

  test("open no board where a class's role has the id of one of another package's classes, or of one that the class lists before it, naming both packages and the module of each such class", async () => {
    const first = listing("first", [notes("First")], { roles: [notesRole] });
    const other: PluginClass = { id: interned("blockr.other", "second_plugin"), roles: [notesRole] };
    const twiceRole: Role = { ...notesRole, id: "twice_role", default: "twice_action" };
    const twice = listing("twice", [action("twice", ["block"], { role: "twice_role" })], { roles: [twiceRole, twiceRole] });
    const reported = logged();
    await expect(stubbed([first, twice, other])).rejects.toThrow(
      noBoard(
        refusedClass(
          twice.id,
          'The plugin class "twice_plugin of blockr.test", of the package blockr.test, gives the role "twice_role", which the package blockr.test gives already.',
        ),
        refusedClass(
          other.id,
          'The plugin class "second_plugin of blockr.other", of the package blockr.other, gives the role "notes_role", which the package blockr.test gives already.',
        ),
      ),
    );
    reported.restore();
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
      append_block_role: { package: "blockr.test", id: append.id },
      add_block_role: { package: "blockr.test", id: add.id },
    });

    expect(ids(s.core.boardActions.on({ block: "b" }))).toEqual([ofTest(append), ...ON_BLOCK.slice(1)]);
    expect(ids(s.core.boardActions.on({ board: true }))[0]).toEqual(ofTest(add));
    expect(ids(s.core.boardActions.on({ board: true }))).not.toContainEqual({ package: "blockr.core", id: "add_block_action" });

    const triggered = s.core.boardActions.trigger("append_block_role", { block: "b" }, { el });
    expect(s.menu()?.level()).toMatchObject({ label: "kit_append" });
    await s.menu()?.pick("Head");
    await triggered;
    expect(Object.values(s.core.store.board().links)).toContainEqual({ from: "b", to: "n", input: "data" });

    // Core's Append block, which names the role, no longer plays it, so that
    // trigger() runs nothing for its id.
    await s.core.boardActions.trigger(interned("blockr.core", "append_block_action"), { block: "b" }, { el });
    expect(s.menu()).toBeUndefined();

    const room = document.createElement("div");
    s.core.boardActions.mountAdvanced(room, "add_block_role", { board: true })();
    s.core.boardActions.mountAdvanced(room, interned("blockr.core", "add_block_action"), { board: true })();
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
    const stray = action("stray", ["block"], { role: "append_block_role", run: () => [{ id: interned("blockr.core", "remove_block"), args: { id: "b" } }] });
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
    await s.core.boardActions.trigger(ofTest(stray), { block: "b" }, { el });
    expect(Object.hasOwn(s.core.store.board().blocks, "b")).toBe(true);
    s.core.boardActions.mountAdvanced(document.createElement("div"), ofTest(lost), { block: "b" })();
    expect(renders).toEqual([]);
    s.stop();
  });

  test.each<[string, readonly Definition[], Json, string]>([
    [
      "picks a board action that no package registers",
      [],
      { append_block_role: { package: "blockr.test", id: "nowhere_action" } },
      'The blockr option board_actions picks the board action "nowhere_action of blockr.test" to play the role "append_block_role", but no package registers that board action.',
    ],
    [
      "picks a board action that names no role",
      [action("kit_append", ["block"])],
      { append_block_role: { package: "blockr.test", id: "kit_append_action" } },
      'The blockr option board_actions picks the board action "kit_append_action of blockr.test" to play the role "append_block_role", but that board action names no role.',
    ],
    [
      "picks a board action that names another role",
      [action("kit_append", ["block"], { role: "prepend_block_role" })],
      { append_block_role: { package: "blockr.test", id: "kit_append_action" } },
      'The blockr option board_actions picks the board action "kit_append_action of blockr.test" to play the role "append_block_role", but that board action names the role "prepend_block_role".',
    ],
    [
      "picks a board action whose on names other targets than the role's",
      [action("kit_append", ["block", "link"], { role: "append_block_role" })],
      { append_block_role: { package: "blockr.test", id: "kit_append_action" } },
      'The blockr option board_actions picks the board action "kit_append_action of blockr.test" to play the role "append_block_role", but the on of that board action names "block" and "link", where the targets of the role are "block".',
    ],
    [
      "picks a board action without an advanced action for a role that callers mount",
      [action("kit_add", ["board"], { role: "add_block_role" })],
      { add_block_role: { package: "blockr.test", id: "kit_add_action" } },
      'The blockr option board_actions picks the board action "kit_add_action of blockr.test" to play the role "add_block_role", but that board action has no advanced action, which callers of the role mount.',
    ],
    [
      "picks a board action for a role that no package registers",
      [],
      { nowhere_role: { package: "blockr.core", id: "add_block_action" } },
      'The blockr option board_actions picks the board action "add_block_action of blockr.core" to play the role "nowhere_role", but no package registers that role.',
    ],
    [
      "holds no list, as an environment variable gives it",
      [],
      "append_block_action",
      'The blockr option board_actions holds "append_block_action", where it picks a board action for a role by the role\'s id and the board action\'s package and id, as list(append_block_role = list(package = "mykit", id = "append_block_action")) does in R.',
    ],
    [
      "picks a board action by a string, which names no package",
      [action("kit_append", ["block"], { role: "append_block_role" })],
      { append_block_role: "kit_append_action" },
      'The blockr option board_actions holds {"append_block_role":"kit_append_action"}, where it picks a board action for a role by the role\'s id and the board action\'s package and id, as list(append_block_role = list(package = "mykit", id = "append_block_action")) does in R.',
    ],
  ])("opens no board where board_actions %s, and says why, naming the pick, the role and what failed", async (_what, actions, picks, why) => {
    const reported = logged();
    await expect(stubbed([listing("kit", actions)], picks)).rejects.toThrow(`${why}\n${NO_BOARD}`);
    reported.restore();
  });
});
