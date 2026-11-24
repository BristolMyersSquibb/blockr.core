// Core's board actions, from the table of Core's board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions),
// in its order: Add block, Append block, Prepend block and Insert block,
// Create link, Create stack, Rename, Edit link, Edit inputs, Edit stack, Add
// to stack, Copy block ID, Remove block, Remove link, Remove stack, and Remove
// on a selection. Their ids are under blockr.core and keep the names of the
// dock's actions, as blockr.core-remove_block_action, but for Rename, Copy
// block ID and Add to stack, which are new.
// - Add block, Append block, Prepend block and Insert block add a block
//   through a pick in their quick actions, or through the block picker, their
//   advanced action (./add.tsx).
// - Create link and Edit link add and change a link, and Create stack and Edit
//   stack a stack, through their quick actions and their advanced actions,
//   the forms of a link and of a stack. Add to stack puts blocks into a stack
//   that the board holds, through its quick action alone (./links.ts,
//   ./stacks.ts).
// - Rename, on a block or a stack, asks for the name in its quick action, a
//   text row that holds the name, which refuses an empty name, and runs core's
//   command that renames the block or the stack. A name that the user leaves
//   as it was runs nothing.
// - Edit inputs, on a block whose class has inputs, has an advanced action
//   alone, which lists the block's inputs, each with its source, and applies
//   the user's edits as one batch through its Apply button (./inputs.tsx). It
//   has no quick action, since moving an input would take three picks at the
//   trigger, so that its row in a menu opens its advanced action at once.
// - Copy block ID puts the block's id on the clipboard itself, and raises a
//   notification of the code `copy_failed` about the block where the browser
//   refuses (./clipboard.ts). Its row in a menu shows the block's id.
// - Remove block, Remove link and Remove stack run core's command that removes
//   what they act on.
// - Remove on a selection removes every block, link and stack of the
//   selection in one step, and leaves out what else the selection holds.
// Each but Copy block ID plays the role named after its id, as Create link,
// blockr.core-add_link_action, plays add_link_role (./roles.ts). Copy block ID
// plays none, since it runs no command.
// Copy block ID and those that remove act at once, through their run(). Those
// that add carry a plus as their icon, in the shape of blockr.ui's `plus`
// (./icons.ts), and those that remove the bin of a row that destroys
// something, in the shape of blockr.ui's `trash`, since blockr.ui builds on
// blockr.core
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/icons/trash.svg).
// Under the board lock, every one but Copy block ID fails its offers(), since
// the store refuses what they run
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.11#three-locks).
// Each fails it as well where the board does not hold what it would act on,
// as where the block has left the board since the menu opened, so that no
// menu offers a board action that leads nowhere.

import type { Commands } from "../commands/index.ts";
import type { Board } from "../types/board.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import { classInputs } from "../ui/index.ts";
import { coreNotification, own } from "../util/index.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { addingBoardActions } from "./add.tsx";
import { copyText } from "./clipboard.ts";
import { editInputs } from "./inputs.tsx";
import { createLinkAction, editLinkAction } from "./links.ts";
import { nameRow } from "./quick.ts";
import { addToStackAction, createStackAction, editStackAction } from "./stacks.ts";
import { blockOf, held, heldBlock } from "./targets.ts";

// What core's board actions read beyond what their functions get: what raises
// their notifications, the preflights of the command registry, and the block
// registry of the open board, as Add block lists its classes, as Create
// link's offers() tells which blocks a new link can join, and as Edit inputs'
// tells whether a block's class has inputs, since offers() gets no CoreApi.
export interface CoreParts {
  readonly raise: Raise;
  readonly preflight: Commands["preflight"];
  readonly blockRegistry: () => BlockRegistry;
}

// Core's board actions.
export function coreBoardActions(parts: CoreParts): BoardAction[] {
  const { raise } = parts;
  return [
    ...addingBoardActions(parts.blockRegistry),
    createLinkAction(parts),
    createStackAction(),
    {
      id: "blockr.core-rename_action",
      label: "Rename",
      on: ["block", "stack"],
      category: "edit",
      offers: (board, target, lock) => !lock.board && named(board, target) !== undefined,
      quick(board, target) {
        const object = named(board, target);
        if (object === undefined) return undefined;
        return {
          rows: [nameRow(object.name, (name) => (name === object.name ? [] : [{ id: object.command, args: { id: object.id, name } }]))],
        };
      },
      role: "rename_role",
    },
    editLinkAction(parts),
    {
      id: "blockr.core-edit_inputs_action",
      label: "Edit inputs",
      on: ["block"],
      category: "edit",
      offers(board, target, lock) {
        const block = blockOf(target);
        return !lock.board && block !== undefined && classInputs(board, parts.blockRegistry(), block).length > 0;
      },
      advanced: editInputs,
      role: "edit_inputs_role",
    },
    editStackAction(),
    addToStackAction(),
    {
      id: "blockr.core-copy_block_id_action",
      label: "Copy block ID",
      detail: (_board, target) => blockOf(target),
      on: ["block"],
      category: "clipboard",
      offers: (board, target) => heldBlock(board, target) !== undefined,
      async run(_board, target) {
        const block = blockOf(target);
        if (block === undefined) return undefined;
        if (!(await copyText(document, block))) {
          raise(
            coreNotification("copy_failed", "error", `The browser did not let core copy the block ID "${block}".`, {
              about: { block },
            }),
          );
        }
        return undefined;
      },
    },
    {
      id: "blockr.core-remove_block_action",
      label: "Remove block",
      icon: TRASH,
      on: ["block"],
      category: "remove",
      offers: (board, target, lock) => !lock.board && heldBlock(board, target) !== undefined,
      run: (_board, target) => removing("blockr.core-remove_block", blockOf(target)),
      role: "remove_block_role",
    },
    {
      id: "blockr.core-remove_link_action",
      label: "Remove link",
      icon: TRASH,
      on: ["link"],
      category: "remove",
      offers: (board, target, lock) =>
        !lock.board && "link" in target && Object.hasOwn(board.links, target.link),
      run: (_board, target) => removing("blockr.core-remove_link", "link" in target ? target.link : undefined),
      role: "remove_link_role",
    },
    {
      id: "blockr.core-remove_stack_action",
      label: "Remove stack",
      icon: TRASH,
      on: ["stack"],
      category: "remove",
      offers: (board, target, lock) =>
        !lock.board && "stack" in target && Object.hasOwn(board.stacks, target.stack),
      run: (_board, target) => removing("blockr.core-remove_stack", "stack" in target ? target.stack : undefined),
      role: "remove_stack_role",
    },
    {
      id: "blockr.core-remove_selected_action",
      label: "Remove",
      icon: TRASH,
      on: ["selection"],
      category: "remove",
      offers: (board, target, lock) => !lock.board && removals(board, target).length > 0,
      run: (board, target) => removals(board, target),
      role: "remove_selected_role",
    },
  ];
}

// The block or the stack that the target names, where the board holds it,
// with its name and core's command that renames it.
function named(board: Board, target: Target): { id: string; name: string; command: string } | undefined {
  if ("block" in target) {
    const block = own(board.blocks, target.block);
    return block === undefined ? undefined : { id: target.block, name: block.name, command: "blockr.core-rename_block" };
  }
  if ("stack" in target) {
    const stack = own(board.stacks, target.stack);
    return stack === undefined ? undefined : { id: target.stack, name: stack.name, command: "blockr.core-rename_stack" };
  }
  return undefined;
}

// The batch that removes a block, a link or a stack by its id with the command
// given, or none without an id.
function removing(command: string, id: string | undefined): CommandCall[] | undefined {
  return id === undefined ? undefined : [{ id: command, args: { id } }];
}

// The batch that removes the blocks, the links and the stacks of a selection
// that the board holds, each once. The links come first, since removing a
// block takes its links along, and a command that removed a link that the
// board no longer holds would refuse the batch.
function removals(board: Board, target: Target): CommandCall[] {
  if (!("selection" in target)) return [];
  const removing = (kind: "block" | "link" | "stack"): CommandCall[] =>
    held(board, target, kind).map((id) => ({ id: `blockr.core-remove_${kind}`, args: { id } }));
  return [...removing("link"), ...removing("block"), ...removing("stack")];
}

// The bin of a row that destroys something, in the shape of blockr.ui's
// `trash`, in the colour of the text around it.
const TRASH =
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9"/></svg>';
