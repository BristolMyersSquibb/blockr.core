// Core's board actions that create, edit and join a stack, from the table of
// Core's board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions):
// - Create stack, on a block, on a selection that holds a block, of which it
//   takes the blocks, or on the board. Its quick action is a text row that
//   holds a name that no other stack has, which the user may change.
//   Submitting it runs blockr.core-add_stack, with the blocks, or with none on
//   the board, the name, a colour that no other stack has, and an ID that
//   CoreApi mints as the user submits it. A block that the new stack takes
//   leaves the stack that held it. It plays add_stack_role, which adds a
//   stack, while Add to stack, below, puts blocks into a stack that the board
//   holds.
//   Its advanced action is the form of a stack (./stack-form.tsx), with the
//   blocks given, or with none on the board. Core's runtime loads the form as
//   it first renders it, as it loads each advanced action of its board actions
//   (./core.ts).
// - Edit stack, on a stack. Its quick action offers Colour…, which lists the
//   stack's colour and the colours of a palette that no other stack has, and
//   Blocks…, whose choices the user ticks, the stack's blocks and those in no
//   stack, which apply as the menu closes, as blockr.dock#550 had them. Each
//   runs blockr.core-change_stack. Its advanced action is the form of a stack,
//   editing it. Renaming and removing a stack are core's Rename and Remove
//   stack. It plays edit_stack_role.
// - Add to stack, on a block or on a selection that holds a block, of which
//   it takes the blocks. Its quick action lists the board's stacks that the
//   blocks join, as blockr.dock#550 had them, each with its colour as its
//   icon and how many blocks it holds at its end. A stack that holds every
//   one of the blocks already is left out, since joining it would change
//   nothing, and Add to stack fails its offers() where no stack is left. Each
//   row runs blockr.core-change_stack, which puts the blocks at the end of the
//   stack and takes them out of the stack that held them. It has no advanced
//   action, and plays add_to_stack_role.
// Under the board lock, all three fail their offers(), since the store
// refuses what they run.

import { suggestedName } from "../commands/index.ts";
import type { BlockId, Board, Stack, StackId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { blockName, loadedModule, own } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { PLUS, swatch } from "./icons.ts";
import { PALETTE, suggestedColor } from "./palette.ts";
import { levelItem, nameRow } from "./quick.ts";
import type { QuickRow } from "./quick.ts";
import { heldBlocks } from "./targets.ts";

const CREATE_STACK = "blockr.core-add_stack_action";
const EDIT_STACK = "blockr.core-edit_stack_action";
const ADD_TO_STACK = "blockr.core-add_to_stack_action";
const ADD_STACK = "blockr.core-add_stack";
const CHANGE_STACK = "blockr.core-change_stack";

export function createStackAction(): BoardAction {
  return {
    id: CREATE_STACK,
    label: "Create stack",
    icon: PLUS,
    on: ["block", "selection", "board"],
    category: "add",
    offers: (board, target, lock) => !lock.board && ("board" in target || heldBlocks(board, target).length > 0),
    quick(board, target, core) {
      const blocks = heldBlocks(board, target);
      if (!("board" in target) && blocks.length === 0) return undefined;
      const rows = [nameRow(suggestedName(Object.values(board.stacks)), (name) => newStack(core, blocks, name))];
      return blocks.length === 0 ? { rows } : { title: `New stack of ${blocksLabel(board, blocks)}`, rows };
    },
    advanced: loadedModule(async () => (await import("./stack-form.tsx")).createStackForm),
    role: "add_stack_role",
  };
}

export function editStackAction(): BoardAction {
  return {
    id: EDIT_STACK,
    label: "Edit stack",
    on: ["stack"],
    category: "edit",
    offers: (board, target, lock) => !lock.board && heldStack(board, target) !== undefined,
    quick(board, target) {
      const held = heldStack(board, target);
      if (held === undefined) return undefined;
      const [id, stack] = held;
      return {
        rows: [
          levelItem("Colour…", { title: `Colour of ${stack.name}`, rows: colourRows(board, id, stack) }),
          levelItem("Blocks…", { title: `Blocks in ${stack.name}`, filter: true, rows: [blocksRow(board, id, stack)] }),
        ],
      };
    },
    advanced: loadedModule(async () => (await import("./stack-form.tsx")).editStackForm),
    role: "edit_stack_role",
  };
}

export function addToStackAction(): BoardAction {
  return {
    id: ADD_TO_STACK,
    label: "Add to stack",
    on: ["block", "selection"],
    category: "edit",
    offers: (board, target, lock) => !lock.board && joinable(board, heldBlocks(board, target)).length > 0,
    quick(board, target) {
      const blocks = heldBlocks(board, target);
      const stacks = joinable(board, blocks);
      if (stacks.length === 0) return undefined;
      return {
        title: `Add ${blocksLabel(board, blocks)} to`,
        rows: stacks.map(
          ([id, stack]): QuickRow => ({
            kind: "item",
            label: stack.name,
            icon: swatch(stack.color),
            detail: count(stack.blocks.length),
            pick: () => [{ id: CHANGE_STACK, args: { id, blocks: [...stack.blocks, ...blocks.filter((block) => !stack.blocks.includes(block))] } }],
          }),
        ),
      };
    },
    role: "add_to_stack_role",
  };
}

// The batch that adds a stack of the blocks under the name given, with a
// colour that no other stack has, under an ID that CoreApi mints.
function newStack(core: CoreApi, blocks: readonly BlockId[], name: string): CommandCall[] {
  return [
    {
      id: ADD_STACK,
      args: {
        id: core.mintId((id) => Object.hasOwn(core.store.board().stacks, id)),
        name,
        color: suggestedColor(Object.values(core.store.board().stacks)),
        blocks,
      },
    },
  ];
}

// The board's stacks that the blocks join, with their IDs: each that lacks one
// of the blocks, so that none is left where no block is given.
function joinable(board: Board, blocks: readonly BlockId[]): [StackId, Stack][] {
  return Object.entries(board.stacks).filter(([, stack]) => blocks.some((block) => !stack.blocks.includes(block)));
}

// The rows of Colour…: the stack's colour, which changes nothing, and each
// colour of the palette that neither the stack nor another stack has, by its
// hex.
function colourRows(board: Board, id: StackId, stack: Stack): QuickRow[] {
  const used = new Set(
    Object.entries(board.stacks)
      .filter(([other]) => other !== id)
      .map(([, other]) => other.color?.toLowerCase()),
  );
  const current = stack.color?.toLowerCase();
  return [
    { kind: "item", label: "Current", icon: swatch(stack.color), detail: stack.color === null ? "None" : stack.color.toUpperCase(), pick: () => [] },
    ...PALETTE.filter((color) => color !== current && !used.has(color)).map(
      (color): QuickRow => ({
        kind: "item",
        label: color.toUpperCase(),
        icon: swatch(color),
        pick: () => [{ id: CHANGE_STACK, args: { id, color } }],
      }),
    ),
  ];
}

// The multi of Blocks…: the stack's blocks, ticked, and the blocks in no
// stack, by their names. The stack takes the blocks ticked as the menu
// closes, in that order, so that a block that the user ticks goes last.
function blocksRow(board: Board, id: StackId, stack: Stack): QuickRow {
  const stacked = new Set(Object.values(board.stacks).flatMap((other) => other.blocks));
  const members = stack.blocks.filter((block) => Object.hasOwn(board.blocks, block));
  const free = Object.keys(board.blocks).filter((block) => !stacked.has(block));
  return {
    kind: "multi",
    label: "Blocks",
    choices: [
      ...members.map((block) => ({ id: block, label: blockName(board, block), picked: true })),
      ...free.map((block) => ({ id: block, label: blockName(board, block), picked: false })),
    ],
    apply: (picked) => [{ id: CHANGE_STACK, args: { id, blocks: [...picked] } }],
  };
}

function count(blocks: number): string {
  return blocks === 1 ? "1 block" : `${String(blocks)} blocks`;
}

// The block by its name, or how many blocks there are.
function blocksLabel(board: Board, blocks: readonly BlockId[]): string {
  const [only] = blocks;
  return blocks.length === 1 && only !== undefined ? blockName(board, only) : count(blocks.length);
}

// The stack that the target names, with its ID, where the board holds it.
function heldStack(board: Board, target: Target): [StackId, Stack] | undefined {
  if (!("stack" in target)) return undefined;
  const stack = own(board.stacks, target.stack);
  return stack === undefined ? undefined : [target.stack, stack];
}
