// What core's board actions read off a target: the block that it names, which
// core's board actions on blocks share (./core.ts, ./add.tsx).

import type { BlockId, Board } from "../types/board.ts";
import type { Target } from "./actions.ts";

// The block that the target names, for a board action on blocks alone, which
// core hands no other target.
export function blockOf(target: Target): BlockId | undefined {
  return "block" in target ? target.block : undefined;
}

// The block that the target names, where the board holds it.
export function heldBlock(board: Board, target: Target): BlockId | undefined {
  const block = blockOf(target);
  return block !== undefined && Object.hasOwn(board.blocks, block) ? block : undefined;
}
