// Core's board actions, from the table of Core's board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions),
// in its order: Add block, Append block, Prepend block and Insert block,
// Rename, Copy block ID, Remove block, Remove link, Remove stack, and Remove
// on a selection. Their ids are under blockr.core and keep the names
// of the dock's actions, as blockr.core-remove_block_action, but for Rename and
// Copy block ID, which are new.
// - Add block, Append block, Prepend block and Insert block add a block
//   through a pick in their quick actions, or through the block picker, their
//   advanced action (./add.tsx).
// - Rename, on a block or a stack, asks for the name in its quick action, a
//   text row that holds the name, which refuses an empty name, and runs core's
//   command that renames the block or the stack. A name that the user leaves
//   as it was runs nothing.
// - Copy block ID puts the block's id on the clipboard itself, and raises a
//   notification of the code `copy_failed` about the block where the browser
//   refuses (./clipboard.ts). Its row in a menu shows the block's id.
// - Remove block, Remove link and Remove stack run core's command that removes
//   what they act on.
// - Remove on a selection removes every block, link and stack of the
//   selection in one step, and leaves out what else the selection holds.
// Each but Rename and the four that add a block acts at once, through its
// run(). Those that add a block carry a plus as their icon. Those that remove
// carry the bin of a row that destroys something, in the shape of blockr.ui's
// `trash`, since blockr.ui builds on blockr.core
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/icons/trash.svg).
// Under the board lock, every one but Copy block ID fails its offers(), since
// the store refuses what they run
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.11#three-locks).
// Each fails it as well where the board does not hold what it would act on,
// as where the block has left the board since the menu opened, so that no
// menu offers a board action that leads nowhere.

import type { Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { coreNotification, own } from "../util/index.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { addingBoardActions } from "./add.tsx";
import { copyText } from "./clipboard.ts";
import { blockOf, heldBlock } from "./targets.ts";

// Core's board actions, whose notifications core raises through `raise`, and
// which read the block registry of the open board through `registry`.
export function coreBoardActions(raise: Raise, registry: () => Readonly<Record<RegistryId, RegistryEntry>>): BoardAction[] {
  return [
    ...addingBoardActions(registry),
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
          rows: [
            {
              kind: "text",
              label: "Name",
              value: object.name,
              check: (value) => (value.trim() === "" ? "A name cannot be empty." : undefined),
              submit(value) {
                const name = value.trim();
                return name === object.name ? [] : [{ id: object.command, args: { id: object.id, name } }];
              },
            },
          ],
        };
      },
    },
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
    },
    {
      id: "blockr.core-remove_selected_action",
      label: "Remove",
      icon: TRASH,
      on: ["selection"],
      category: "remove",
      offers: (board, target, lock) => !lock.board && removals(board, target).length > 0,
      run: (board, target) => removals(board, target),
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

// The bin of a row that destroys something, in the shape of blockr.ui's
// `trash`, in the colour of the text around it.
const TRASH =
  '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9"/></svg>';
