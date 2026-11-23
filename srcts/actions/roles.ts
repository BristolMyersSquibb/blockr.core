// Roles, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#roles): an
// edit that callers rely on, which one board action plays in each app. A role
// ties the board action that plays it to commands. It names the commands that
// each batch of the board action runs, in a fixed order, and which of their
// args the target fixes. So a board action that an app swaps in does what the
// one it replaces does, and only its UI differs.
//
// A role's id is global and names no package, as a board option's does, and
// ends in `_role` by convention, as "append_block_role", so that it reads as
// neither a board action's id nor a command's. Core defines a role for each of
// its board actions that runs commands (CORE_ROLES, below), and a plugin class
// lists roles of its own in `roles`, which core registers with the class
// (./actions.ts). Core refuses the whole class where one of its roles breaks a
// rule (checkedRoles(), below).
//
// In each app, one board action plays each role: the one that the blockr
// option `board_actions` picks for the role, or else the role's default. Core
// checks that board action once every module is registered
// (checkedPlayer(), below):
// - It names the role in `role`.
// - Its `on` names the role's targets alone.
// - It has an advanced action where the role has `advanced`, since callers
//   mount the advanced action of the role.
//
// Core checks each batch of the board action that plays a role before the
// command registry plans it (batchFailure(), below). The batch holds the
// role's commands alone, in their order:
// - A command comes exactly once, or at most once where it has `optional`.
// - A command with `each` comes that often for each board object or input of
//   its kind that the target holds, as a selection holds blocks and a block
//   its inputs. A variadic block takes any number of inputs, so that a command
//   for each of its inputs may come any number of times.
// - Each call has args that pass the `fixes` of its command.

import { isCommandId } from "../commands/index.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import type { Board, PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { classInputs } from "../ui/index.ts";
import { errorMessage, getAt, isObject, own, packageFromId, quoted } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { held, heldBlock, heldBlocks, idOf, kindsFailure } from "./targets.ts";

export interface Role {
  // Global, naming no package, as "append_block_role".
  readonly id: string;
  // The targets of the board action that plays it.
  readonly on: BoardAction["on"];
  // The board action that plays it where the app picks none, as
  // "blockr.core-append_block_action".
  readonly default: string;
  // Where a caller mounts the advanced action of the board action that plays
  // it, as the Edit board plugin mounts that of Add block.
  readonly advanced?: true;
  // What each batch of that board action runs, in this order.
  readonly commands: readonly RoleCommand[];
}

export interface RoleCommand {
  // A command of any package, as "blockr.core-add_link".
  readonly id: string;
  // Once for each board object or input of this kind that the target holds,
  // as a selection holds blocks and a block its inputs.
  readonly each?: "block" | "input" | "link" | "stack";
  // At most once, rather than exactly once, and for each where it has `each`.
  readonly optional?: true;
  // Whether the args of a call of the command keep what the target fixes, as
  // the `from` of add_link on Append block.
  fixes?(board: Board, target: Target, args: Json): boolean;
}

// The kinds of board object or input that `each` names.
const EACH: readonly NonNullable<RoleCommand["each"]>[] = ["block", "input", "link", "stack"];

const ADD_BLOCK = "blockr.core-add_block";
const RENAME_BLOCK = "blockr.core-rename_block";
const REMOVE_BLOCK = "blockr.core-remove_block";
const ADD_LINK = "blockr.core-add_link";
const CHANGE_LINK = "blockr.core-change_link";
const MOVE_LINK = "blockr.core-move_link";
const REMOVE_LINK = "blockr.core-remove_link";
const ADD_STACK = "blockr.core-add_stack";
const CHANGE_STACK = "blockr.core-change_stack";
const RENAME_STACK = "blockr.core-rename_stack";
const REMOVE_STACK = "blockr.core-remove_stack";

// Core's roles, from the table of Roles, each with one of core's board actions
// as its default.
// No target names an input of a block yet, so that prepend_block_role and
// add_link_role leave out the inputs that the table gives them as targets.
// Edit stack's form renames the stack too, so that edit_stack_role runs
// rename_stack before change_stack, each at most once, where the table names
// change_stack alone.
// On a block or a selection, add_stack_role has the new stack take exactly
// the blocks that the target holds, so that Create stack's form names them in
// place of a select (./stack-form.tsx).
export const CORE_ROLES: readonly Role[] = [
  {
    id: "add_block_role",
    on: ["board"],
    default: "blockr.core-add_block_action",
    advanced: true,
    commands: [{ id: ADD_BLOCK }],
  },
  {
    id: "append_block_role",
    on: ["block"],
    default: "blockr.core-append_block_action",
    commands: [{ id: ADD_BLOCK }, { id: ADD_LINK, fixes: names("block", "from") }],
  },
  {
    id: "prepend_block_role",
    on: ["block"],
    default: "blockr.core-prepend_block_action",
    commands: [{ id: ADD_BLOCK }, { id: ADD_LINK, fixes: names("block", "to") }],
  },
  {
    id: "insert_block_role",
    on: ["link"],
    default: "blockr.core-insert_block_action",
    commands: [{ id: ADD_BLOCK }, { id: ADD_LINK, fixes: fromSource }, { id: CHANGE_LINK, fixes: names("link") }],
  },
  {
    id: "add_link_role",
    on: ["block", "board"],
    default: "blockr.core-add_link_action",
    advanced: true,
    commands: [{ id: ADD_LINK, fixes: endsAtBlock }],
  },
  {
    id: "add_stack_role",
    on: ["block", "selection", "board"],
    default: "blockr.core-add_stack_action",
    advanced: true,
    commands: [{ id: ADD_STACK, fixes: takesBlocks }],
  },
  {
    id: "rename_role",
    on: ["block", "stack"],
    default: "blockr.core-rename_action",
    commands: [
      { id: RENAME_BLOCK, optional: true, fixes: names("block") },
      { id: RENAME_STACK, optional: true, fixes: names("stack") },
    ],
  },
  {
    id: "edit_link_role",
    on: ["link"],
    default: "blockr.core-edit_link_action",
    commands: [
      { id: CHANGE_LINK, optional: true, fixes: names("link") },
      { id: MOVE_LINK, optional: true, fixes: names("link") },
    ],
  },
  {
    id: "edit_inputs_role",
    on: ["block"],
    default: "blockr.core-edit_inputs_action",
    commands: [REMOVE_LINK, CHANGE_LINK, ADD_LINK, MOVE_LINK].map((id) => ({ id, each: "input", optional: true, fixes: intoBlock })),
  },
  {
    id: "edit_stack_role",
    on: ["stack"],
    default: "blockr.core-edit_stack_action",
    commands: [
      { id: RENAME_STACK, optional: true, fixes: names("stack") },
      { id: CHANGE_STACK, optional: true, fixes: names("stack") },
    ],
  },
  {
    id: "remove_block_role",
    on: ["block"],
    default: "blockr.core-remove_block_action",
    commands: [{ id: REMOVE_BLOCK, fixes: names("block") }],
  },
  {
    id: "remove_link_role",
    on: ["link"],
    default: "blockr.core-remove_link_action",
    commands: [{ id: REMOVE_LINK, fixes: names("link") }],
  },
  {
    id: "remove_stack_role",
    on: ["stack"],
    default: "blockr.core-remove_stack_action",
    commands: [{ id: REMOVE_STACK, fixes: names("stack") }],
  },
  {
    id: "remove_selected_role",
    on: ["selection"],
    default: "blockr.core-remove_selected_action",
    commands: [
      { id: REMOVE_LINK, each: "link", fixes: selected("link") },
      { id: REMOVE_BLOCK, each: "block", fixes: selected("block") },
      { id: REMOVE_STACK, each: "stack", fixes: selected("stack") },
    ],
  },
];

// A fixes by which the call's arg of the name given is the block, the link or
// the stack that the target names.
function names(kind: "block" | "link" | "stack", arg = "id"): NonNullable<RoleCommand["fixes"]> {
  return (_board, target, args) => {
    const id = idOf(target, kind);
    return id !== undefined && getAt(args, [arg]) === id;
  };
}

// The fixes of Create link's add_link: on a block, the link starts or ends at
// the block. On the board, the target fixes neither end.
function endsAtBlock(_board: Board, target: Target, args: Json): boolean {
  return !("block" in target) || getAt(args, ["from"]) === target.block || getAt(args, ["to"]) === target.block;
}

// The fixes of Insert block's add_link: the new link comes from the block that
// the target's link comes from.
function fromSource(board: Board, target: Target, args: Json): boolean {
  const link = "link" in target ? own(board.links, target.link) : undefined;
  return link !== undefined && getAt(args, ["from"]) === link.from;
}

// The fixes of add_stack: on a block or a selection, the new stack takes
// exactly the blocks that the target holds. On the board, which holds none,
// the target fixes no blocks.
function takesBlocks(board: Board, target: Target, args: Json): boolean {
  if ("board" in target) return true;
  const blocks = getAt(args, ["blocks"]);
  const taken: readonly unknown[] = Array.isArray(blocks) ? blocks : [];
  const held = heldBlocks(board, target);
  return taken.length === held.length && held.every((block) => taken.includes(block));
}

// The fixes of Edit inputs' commands: the link that the call names goes into
// the block that the target names, and still does after it, or the link that
// it adds does.
function intoBlock(board: Board, target: Target, args: Json): boolean {
  if (!("block" in target)) return false;
  const id = getAt(args, ["id"]);
  const link = typeof id === "string" ? own(board.links, id) : undefined;
  const to = getAt(args, ["to"]);
  return link === undefined ? to === target.block : link.to === target.block && (to === undefined || to === target.block);
}

// A fixes by which the call removes a board object of the kind given that the
// selection holds.
function selected(kind: "block" | "link" | "stack"): NonNullable<RoleCommand["fixes"]> {
  return (board, target, args) => {
    const id = getAt(args, ["id"]);
    return typeof id === "string" && held(board, target, kind).includes(id);
  };
}

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

// Why the batch breaks the role on the target, a clause, or undefined where
// it keeps the role. The board is the one that the batch would change.
export function batchFailure(role: Role, board: Board, registry: BlockRegistry, target: Target, batch: readonly CommandCall[]): string | undefined {
  let at = 0;
  for (const command of role.commands) {
    const most = times(command, board, registry, target);
    const least = command.optional === true ? 0 : Number.isFinite(most) ? most : 1;
    let count = 0;
    while (count < most) {
      const call = batch[at];
      if (call?.id !== command.id) break;
      const why = keeps(command, board, target, call, at);
      if (why !== undefined) return why;
      count++;
      at++;
    }
    if (count >= least) continue;
    const next = batch[at];
    if (command.each !== undefined) {
      const calls = `${String(count)} ${count === 1 ? "call" : "calls"}`;
      const objects = `${String(least)} ${command.each}${least === 1 ? "" : "s"}`;
      return `its batch holds ${calls} of "${command.id}" in a row, where the target holds ${objects}`;
    }
    return next === undefined ? `its batch holds no call of "${command.id}"` : `its call ${String(at + 1)}, of "${next.id}", comes where the role runs "${command.id}"`;
  }
  const extra = batch[at];
  if (extra === undefined) return undefined;
  return role.commands.some((command) => command.id === extra.id)
    ? `its call ${String(at + 1)}, of "${extra.id}", comes out of the role's order, or more often than the role runs it`
    : `its call ${String(at + 1)} is of "${extra.id}", which the role does not run`;
}

// What the role runs, as the sentence after a broken batch says it: each
// command, then how often.
export function described(role: Role): string {
  return role.commands
    .map(({ id, each, optional }) => {
      const often = each === undefined ? (optional === true ? " at most once" : "") : `${optional === true ? " at most" : ""} once for each ${each}`;
      return `"${id}"${often}`;
    })
    .join(", then ");
}

// How often a command of the role may come in a batch on the target: once, or
// for a command with `each`, once for each board object or input of its kind
// that the target holds. A variadic block takes any number of inputs, so a
// command for each of its inputs may come any number of times.
function times(command: RoleCommand, board: Board, registry: BlockRegistry, target: Target): number {
  const { each } = command;
  if (each === undefined) return 1;
  if (each !== "input") return held(board, target, each).length;
  const block = heldBlock(board, target);
  if (block === undefined) return 0;
  const inputs = classInputs(board, registry, block);
  return inputs.includes("...") ? Infinity : inputs.length;
}

// Why the call at the index given does not keep what the target fixes, or
// undefined where it does. A fixes that throws fails, as a fault of the role's
// package.
function keeps(command: RoleCommand, board: Board, target: Target, call: CommandCall, at: number): string | undefined {
  if (command.fixes === undefined) return undefined;
  const which = `its call ${String(at + 1)}, of "${command.id}"`;
  try {
    return command.fixes(board, target, call.args ?? {}) ? undefined : `the args of ${which}, do not keep what the target fixes`;
  } catch (error) {
    return `the fixes of the role failed on ${which}: ${errorMessage(error)}`;
  }
}

// The roles that a plugin class lists, once each is a role that keeps the
// rules, or an Error that says why the class is refused. The map `taken` gives
// the package of each role registered already, by the role's id, so that the
// Error for a role that has one of those ids names both packages. A role
// breaks a rule where:
// - Its id is no string, or names a package.
// - Another role has its id, core's, another class's or one that the class
//   lists before it.
// - Its `on` is empty, or names what is no kind of target.
// - Its default is not named as a board action's id is.
// - Its `advanced` is neither true nor left out.
// - It runs no command, or a command of it breaks a rule of its own: its id
//   is not named as a command's id is, its `each` names no kind that `each`
//   takes, its `optional` is neither true nor left out, or its `fixes` is no
//   function.
export function checkedRoles(plugin: PluginClassId, given: unknown, taken: ReadonlyMap<string, string>): readonly Role[] {
  const of = `The plugin class "${plugin}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives roles that are no array.`);
  }
  const pkg = packageFromId(plugin) ?? plugin;
  const ids = new Map(taken);
  return given.map((role: unknown): Role => {
    if (!isObject(role)) {
      throw new Error(`${of} gives a role that is no object.`);
    }
    const { id } = role;
    if (typeof id !== "string" || id === "") {
      throw new Error(`${of} gives a role whose id ${JSON.stringify(id)} is no string, as "append_block_role".`);
    }
    if (packageFromId(id) !== null) {
      throw new Error(`${of} gives the role "${id}", whose id names a package, while a role's id names none, as "append_block_role".`);
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
    if (typeof fallback !== "string" || !isCommandId(fallback)) {
      throw new Error(`${named}, whose default ${JSON.stringify(fallback)} is not named as a board action's id is, "{pkg}-{name}".`);
    }
    if (role.advanced !== undefined && role.advanced !== true) {
      throw new Error(`${named}, whose advanced is neither true nor left out.`);
    }
    const { commands } = role;
    if (!Array.isArray(commands)) {
      throw new Error(`${named}, whose commands are no array.`);
    }
    if (commands.length === 0) {
      throw new Error(`${named}, which runs no command.`);
    }
    for (const command of commands as readonly unknown[]) {
      const why = commandFailure(command);
      if (why !== undefined) throw new Error(`${named}, ${why}.`);
    }
    return role as unknown as Role;
  });
}

// Why a command of a role breaks a rule, a clause, or undefined where it keeps
// them.
function commandFailure(command: unknown): string | undefined {
  if (!isObject(command)) return "with a command that is no object";
  const { id, each, optional, fixes } = command;
  if (typeof id !== "string" || !isCommandId(id)) {
    return `with a command whose id ${JSON.stringify(id)} is not named as a command's id is, "{pkg}-{name}"`;
  }
  if (each !== undefined && (typeof each !== "string" || !(EACH as readonly string[]).includes(each))) {
    return `whose command "${id}" has each ${JSON.stringify(each)}, which is none of ${quoted(EACH)}`;
  }
  if (optional !== undefined && optional !== true) {
    return `whose command "${id}" has optional ${JSON.stringify(optional)}, which is neither true nor left out`;
  }
  if (fixes !== undefined && typeof fixes !== "function") {
    return `whose command "${id}" has a fixes that is no function`;
  }
  return undefined;
}
