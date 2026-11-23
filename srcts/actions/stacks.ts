// Core's board actions that create and edit a stack, from the table of Core's
// board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions):
// - Create stack, on a block, on a selection that holds a block, of which it
//   takes the blocks, or on the board. Its quick action holds New stack, which
//   runs blockr.core-add_stack, with the blocks, or with none on the board, a
//   name and a colour that no other stack has, and an ID that CoreApi mints as
//   the user picks it. A block that the new stack takes leaves the stack that
//   held it. It plays add_stack_role, which runs add_stack alone, so that its
//   quick action lists no stack that the board holds for the blocks to join.
//   Its advanced action is the form of a stack (./stack-form.tsx), with the
//   blocks given, or with none on the board.
// - Edit stack, on a stack. Its quick action offers Colour…, which lists the
//   stack's colour and the colours of a palette that no other stack has, and
//   Blocks…, whose choices the user ticks, the stack's blocks and those in no
//   stack, which apply as the menu closes, as blockr.dock#550 had them. Each
//   runs blockr.core-change_stack. Its advanced action is the form of a stack,
//   editing it. Renaming and removing a stack are core's Rename and Remove
//   stack. It plays edit_stack_role.
// Under the board lock, both fail their offers(), since the store refuses
// what they run.

import type { BlockId, Board, Stack, StackId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { blockName } from "../ui/index.ts";
import { own } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { PLUS, swatch } from "./icons.ts";
import { levelItem } from "./quick.ts";
import type { QuickRow } from "./quick.ts";
import { createStackForm, editStackForm, PALETTE, suggestedColor, suggestedName } from "./stack-form.tsx";
import { heldBlocks } from "./targets.ts";

const CREATE_STACK = "blockr.core-add_stack_action";
const EDIT_STACK = "blockr.core-edit_stack_action";
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
      const rows: QuickRow[] = [{ kind: "item", label: "New stack", icon: PLUS, pick: () => newStack(core, blocks) }];
      return blocks.length === 0 ? { rows } : { title: `Add ${blocksLabel(board, blocks)} to`, rows };
    },
    advanced: createStackForm,
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
    advanced: editStackForm,
    role: "edit_stack_role",
  };
}

// The batch that adds a stack of the blocks, with a name and a colour that no
// other stack has, under an ID that CoreApi mints.
function newStack(core: CoreApi, blocks: readonly BlockId[]): CommandCall[] {
  const stacks = core.store.board().stacks;
  const others = Object.values(stacks);
  return [
    {
      id: ADD_STACK,
      args: {
        id: core.mintId((id) => Object.hasOwn(core.store.board().stacks, id)),
        name: suggestedName(others),
        color: suggestedColor(others),
        blocks,
      },
    },
  ];
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
