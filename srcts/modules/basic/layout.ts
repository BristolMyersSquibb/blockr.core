// Where the basic frontend draws each block: the board's blocks in the
// board's order, each stack as a group at the place of its first block, with
// its blocks in the stack's own order.

import type { BlockId, Board, StackId } from "../../types/board.ts";

// A stack with its blocks, or a block in no stack.
export type Item = { readonly stack: StackId; readonly blocks: readonly BlockId[] } | { readonly block: BlockId };

// The items that the frontend draws, top to bottom. A stack without blocks
// comes last, so that the user still sees it and can fill or remove it.
export function layoutItems(board: Pick<Board, "blocks" | "stacks">): Item[] {
  const stackByBlock = new Map<BlockId, StackId>();
  for (const [id, stack] of Object.entries(board.stacks)) {
    for (const block of stack.blocks) stackByBlock.set(block, id);
  }

  const items: Item[] = [];
  const placed = new Set<StackId>();
  for (const block of Object.keys(board.blocks)) {
    const stack = stackByBlock.get(block);
    if (stack === undefined) {
      items.push({ block });
    } else if (!placed.has(stack)) {
      placed.add(stack);
      items.push({ stack, blocks: stackBlocks(board, stack) });
    }
  }
  for (const stack of Object.keys(board.stacks)) {
    if (!placed.has(stack)) items.push({ stack, blocks: [] });
  }
  return items;
}

function stackBlocks(board: Pick<Board, "blocks" | "stacks">, stack: StackId): BlockId[] {
  return (board.stacks[stack]?.blocks ?? []).filter((block) => Object.hasOwn(board.blocks, block));
}
