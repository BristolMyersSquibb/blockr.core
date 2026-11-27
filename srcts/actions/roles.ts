// Roles, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#roles): an
// edit that callers rely on, which one board action plays in each app. So an
// app may swap the board action that plays a role for one drawn with another
// UI kit, and every caller that triggers or mounts the role meets the app's
// pick.
// What the board action that plays a role does is the role's contract, which
// core trusts and does not check, as Eclipse never checks what the one active
// handler of a command does
// (https://help.eclipse.org/latest/topic/org.eclipse.platform.doc.isv/reference/extension-points/org_eclipse_ui_handlers.html).
// A package's tests show that its board action keeps the contract, as they
// trigger the role through the CoreApi for tests and read the board after.
//
// A role's id is global, a string that names no package, as a board option's
// is, and ends in `_role` by convention, as "append_block_role", so that it
// reads as neither a board action's id nor a command's.
// A caller names a board action by its package and its id, and a role by its
// id alone, so that core tells the two apart by what the caller gives. Core defines a role for each of
// its board actions that edits the board (CORE_ROLES, below), and a plugin
// class lists roles of its own in `roles`, which core registers with the class
// (./actions.ts). Core refuses the whole class where one of its roles breaks a
// rule (checkedRoles(), below), and then opens no board (../core/start.ts).
//
// In each app, one board action plays each role: the one that the blockr
// option `board_actions` picks for the role, or else the role's default, one
// of the board actions of the role's class. Core checks that callers can reach
// the board action where they rely on the role (checkedPlayer(), below):
// - It names the role in `role`, so that a mistyped pick plays no role.
// - Its `on` names the role's targets alone, so that a trigger of the role on
//   one of them reaches it.
// - It has an advanced action where the role has `advanced`, since callers
//   mount the advanced action of the role.
// It checks a role's default as it registers the role, and the app's pick once
// every module is registered, since a pick may name a board action of any
// package.

import type { PluginClassId } from "../types/board.ts";
import { idText, isObject, quoted } from "../util/index.ts";
import type { BoardAction } from "./actions.ts";
import { kindsFailure } from "./targets.ts";

export interface Role {
  // Global, naming no package, as "append_block_role".
  readonly id: string;
  // The targets of the board action that plays it.
  readonly on: BoardAction["on"];
  // The board action that plays it where the app picks none, one of the
  // board actions of the role's class, by its id within the class's package,
  // as "append_block_action".
  readonly default: string;
  // Where a caller mounts the advanced action of the board action that plays
  // it, as the Edit board plugin mounts that of Add block.
  readonly advanced?: true;
}

// Core's roles, from the table of Roles, each with one of core's board actions
// as its default, and with the edit that the board action that plays it makes,
// which is the role's contract.
export const CORE_ROLES: readonly Role[] = [
  // Adds a block on its own.
  { id: "add_block_role", on: ["board"], default: "add_block_action", advanced: true },
  // Adds a block that takes the result of the block, through a link from it.
  { id: "append_block_role", on: ["block"], default: "append_block_action" },
  // Adds a block that feeds the block, through a link into one of its inputs,
  // or on an input, into that input. On the next place of a variadic block,
  // the link goes into that block, as on the block, under any name or none and
  // at any place among its links.
  { id: "prepend_block_role", on: ["block", "input"], default: "prepend_block_action" },
  // Adds a block on the link, which then comes from the new block, while a
  // new link feeds the new block from the block that the link came from.
  { id: "insert_block_role", on: ["link"], default: "insert_block_action" },
  // Adds a link that starts or ends at the block, or any link on the board.
  // On an input, the link goes into that input. On the next place of a
  // variadic block, it goes into that block, under any name or none and at
  // any place among its links.
  { id: "add_link_role", on: ["block", "input", "board"], default: "add_link_action", advanced: true },
  // Adds a stack of exactly the blocks that the target holds, or of any
  // blocks on the board.
  { id: "add_stack_role", on: ["block", "selection", "board"], default: "add_stack_action", advanced: true },
  // Renames the block or the stack.
  { id: "rename_role", on: ["block", "stack"], default: "rename_action" },
  // Changes the link, its ends, its input or its place among the links into
  // its block.
  { id: "edit_link_role", on: ["link"], default: "edit_link_action" },
  // Changes the links into the block's inputs: removes, changes, adds and
  // moves any number of them.
  { id: "edit_inputs_role", on: ["block"], default: "edit_inputs_action" },
  // Changes the stack, its name, its colour or its blocks.
  { id: "edit_stack_role", on: ["stack"], default: "edit_stack_action" },
  // Puts the blocks that the target holds into a stack that the board holds.
  { id: "add_to_stack_role", on: ["block", "selection"], default: "add_to_stack_action" },
  // Removes the block, the link or the stack.
  { id: "remove_block_role", on: ["block"], default: "remove_block_action" },
  { id: "remove_link_role", on: ["link"], default: "remove_link_action" },
  { id: "remove_stack_role", on: ["stack"], default: "remove_stack_action" },
  // Removes every link, block and stack of the selection.
  { id: "remove_selected_role", on: ["selection"], default: "remove_selected_action" },
];

// The board action given as the player of the role, once it passes the check
// of a player, or why it cannot play the role, a clause that follows the one
// that names the two. Undefined stands for a board action that no package
// registers.
export function checkedPlayer(role: Role, action: BoardAction | undefined): { readonly player: BoardAction } | { readonly why: string } {
  if (action === undefined) return { why: "no package registers that board action" };
  if (action.role !== role.id) {
    return { why: action.role === undefined ? "that board action names no role" : `that board action names the role "${action.role}"` };
  }
  if (action.on.length !== role.on.length || action.on.some((kind) => !role.on.includes(kind))) {
    return { why: `the on of that board action names ${quoted(action.on)}, where the targets of the role are ${quoted(role.on)}` };
  }
  if (role.advanced === true && action.advanced === undefined) {
    return { why: "that board action has no advanced action, which callers of the role mount" };
  }
  return { player: action };
}

// The roles that a plugin class lists, once each is a role that keeps the
// rules, or an Error that says why the class is refused. The map `taken` gives
// the package of each role registered already, by the role's id, so that the
// Error for a role that has one of those ids names both packages, and
// `actions` the board actions that the class lists. A role breaks a rule
// where:
// - Its id is no non-empty string.
// - Another role has its id, core's, another class's or one that the class
//   lists before it.
// - Its `on` is empty, or names what is no kind of target.
// - Its default is no id of a board action, a non-empty string.
// - Its `advanced` is neither true nor left out.
// - Its default is no board action that the class lists, or one that cannot
//   play the role (checkedPlayer(), above).
export function checkedRoles(
  plugin: PluginClassId,
  given: unknown,
  taken: ReadonlyMap<string, string>,
  actions: readonly BoardAction[],
): readonly Role[] {
  const of = `The plugin class "${idText(plugin)}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives roles that are no array.`);
  }
  const pkg = plugin.package;
  const ids = new Map(taken);
  return given.map((role: unknown): Role => {
    if (!isObject(role)) {
      throw new Error(`${of} gives a role that is no object.`);
    }
    const { id } = role;
    if (typeof id !== "string" || id === "") {
      throw new Error(`${of} gives a role whose id ${JSON.stringify(id)} is no string, as "append_block_role".`);
    }
    const other = ids.get(id);
    if (other !== undefined) {
      throw new Error(`${of}, of the package ${pkg}, gives the role "${id}", which the package ${other} gives already.`);
    }
    ids.set(id, pkg);
    const named = `${of} gives the role "${id}"`;
    const kinds = kindsFailure(role.on, "is empty, so that no board action could play it");
    if (kinds !== undefined) {
      throw new Error(`${named}, whose on ${kinds}.`);
    }
    const fallback = role.default;
    if (typeof fallback !== "string" || fallback === "") {
      throw new Error(
        `${named}, whose default ${JSON.stringify(fallback)} is no id of a board action of the class, as "append_block_action".`,
      );
    }
    if (role.advanced !== undefined && role.advanced !== true) {
      throw new Error(`${named}, whose advanced is neither true nor left out.`);
    }
    const checked = role as unknown as Role;
    const listed = actions.find((action) => action.id === fallback);
    const found = checkedPlayer(checked, listed);
    if ("why" in found) {
      const why = listed === undefined ? "it is no board action that the class lists" : found.why;
      throw new Error(`${named}, whose default "${fallback}" cannot play it, since ${why}.`);
    }
    return checked;
  });
}
