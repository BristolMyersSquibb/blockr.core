// Core's board actions that ask the user nothing, from the table of Core's
// board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions),
// in its order: Copy block ID, Remove block, Remove link, Remove stack, and
// Remove on a selection. Their ids are under blockr.core and keep the names of
// the dock's actions, as blockr.core-remove_block_action.
// Each acts at once, through its run():
// - Copy block ID puts the block's id on the clipboard itself, and raises a
//   notification of the code `copy_failed` about the block where the browser
//   refuses (./clipboard.ts). Its row in a menu shows the block's id.
// - Remove block, Remove link and Remove stack run core's command that removes
//   what they act on.
// - Remove on a selection removes every block, link and stack of the
//   selection in one step, and leaves out what else the selection holds.
// Under the board lock, every one but Copy block ID fails its offers(), since
// the store refuses what they run
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.11#three-locks).
// Each fails it as well where the board does not hold what it would act on,
// as where the block has left the board since the menu opened, so that no
// menu offers a board action that leads nowhere.

import type { BlockId, Board } from "../types/board.ts";
import { coreNotification } from "../util/index.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { copyText } from "./clipboard.ts";

// Core's board actions, whose notifications core raises through `raise`.
export function coreBoardActions(raise: Raise): BoardAction[] {
  return [
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
      on: ["block"],
      category: "remove",
      offers: (board, target, lock) => !lock.board && heldBlock(board, target) !== undefined,
      run: (_board, target) => removing("blockr.core-remove_block", blockOf(target)),
    },
    {
      id: "blockr.core-remove_link_action",
      label: "Remove link",
      on: ["link"],
      category: "remove",
      offers: (board, target, lock) =>
        !lock.board && "link" in target && Object.hasOwn(board.links, target.link),
      run: (_board, target) => removing("blockr.core-remove_link", "link" in target ? target.link : undefined),
    },
    {
      id: "blockr.core-remove_stack_action",
      label: "Remove stack",
      on: ["stack"],
      category: "remove",
      offers: (board, target, lock) =>
        !lock.board && "stack" in target && Object.hasOwn(board.stacks, target.stack),
      run: (_board, target) => removing("blockr.core-remove_stack", "stack" in target ? target.stack : undefined),
    },
    {
      id: "blockr.core-remove_selected_action",
      label: "Remove",
      on: ["selection"],
      category: "remove",
      offers: (board, target, lock) => !lock.board && removals(board, target).length > 0,
      run: (board, target) => removals(board, target),
    },
  ];
}

// The block that the target names, for a board action on blocks alone, which
// core hands no other target.
function blockOf(target: Target): BlockId | undefined {
  return "block" in target ? target.block : undefined;
}

// The block that the target names, where the board holds it.
function heldBlock(board: Board, target: Target): BlockId | undefined {
  const block = blockOf(target);
  return block !== undefined && Object.hasOwn(board.blocks, block) ? block : undefined;
}

// The batch that removes a block, a link or a stack by its id with the command
// given, or none without an id.
function removing(command: string, id: string | undefined): CommandCall[] | undefined {
  return id === undefined ? undefined : [{ id: command, args: { id } }];
}

// The batch that removes the blocks, the links and the stacks of a selection
// that the board holds. The links come first, since removing a block takes its
// links along, and a command that removed a link that the board no longer
// holds would refuse the batch.
function removals(board: Board, target: Target): CommandCall[] {
  if (!("selection" in target)) return [];
  const links: CommandCall[] = [];
  const blocks: CommandCall[] = [];
  const stacks: CommandCall[] = [];
  for (const object of target.selection) {
    if ("link" in object && Object.hasOwn(board.links, object.link)) {
      links.push({ id: "blockr.core-remove_link", args: { id: object.link } });
    } else if ("block" in object && Object.hasOwn(board.blocks, object.block)) {
      blocks.push({ id: "blockr.core-remove_block", args: { id: object.block } });
    } else if ("stack" in object && Object.hasOwn(board.stacks, object.stack)) {
      stacks.push({ id: "blockr.core-remove_stack", args: { id: object.stack } });
    }
  }
  return [...links, ...blocks, ...stacks];
}
