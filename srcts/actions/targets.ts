// The kinds of target, which the `on` of a board action and of a role names,
// and what core's board actions and roles read off a target: the block, the
// link or the stack that it names, which core's board actions share
// (./core.ts, ./add.tsx), and the board objects of a kind that it holds, as
// Remove on a selection removes them and Create stack takes its blocks.

import type { BlockId, Board } from "../types/board.ts";
import { quoted } from "../util/index.ts";
import type { BoardAction, Target } from "./actions.ts";

// The kinds of target, each of which names the menus that offer a board
// action, and the targets of a role.
export const KINDS: Readonly<Record<BoardAction["on"][number], true>> = {
  board: true,
  block: true,
  link: true,
  stack: true,
  option: true,
  plugin: true,
  selection: true,
};

// Why what a board action or a role gives as its `on` names no kinds of
// target, a clause, or undefined where it names some. One that is empty fails
// with the clause given, which says what follows.
export function kindsFailure(on: unknown, empty: string): string | undefined {
  if (!Array.isArray(on)) return "is no array of the kinds of target that it applies to";
  if (on.length === 0) return empty;
  const other: unknown = on.find((kind) => typeof kind !== "string" || !Object.hasOwn(KINDS, kind));
  return other === undefined ? undefined : `names ${JSON.stringify(other)}, which is none of the kinds of target, ${quoted(Object.keys(KINDS))}`;
}

// The id of the block, the link or the stack that the target names, or
// undefined where it names none of the kind given.
export function idOf(target: Target, kind: "block" | "link" | "stack"): string | undefined {
  if (kind === "block") return "block" in target ? target.block : undefined;
  if (kind === "link") return "link" in target ? target.link : undefined;
  return "stack" in target ? target.stack : undefined;
}

// The block that the target names, for a board action on blocks alone, which
// core hands no other target.
export function blockOf(target: Target): BlockId | undefined {
  return idOf(target, "block");
}

// The block that the target names, where the board holds it.
export function heldBlock(board: Board, target: Target): BlockId | undefined {
  const block = blockOf(target);
  return block !== undefined && Object.hasOwn(board.blocks, block) ? block : undefined;
}

// The ids of the board objects of the kind given that the target holds, each
// once, where the board holds them: the one that the target names, or those
// of a selection, in its order. The board holds none for its own target.
export function held(board: Board, target: Target, kind: "block" | "link" | "stack"): string[] {
  const objects: readonly Target[] = "selection" in target ? target.selection : [target];
  const kept = { block: board.blocks, link: board.links, stack: board.stacks }[kind];
  const ids = objects.flatMap((object) => {
    const id = idOf(object, kind);
    return id !== undefined && Object.hasOwn(kept, id) ? [id] : [];
  });
  return [...new Set(ids)];
}

// The blocks that the target holds, where the board holds them: the block
// that it names, or those of a selection, and none on the board.
export function heldBlocks(board: Board, target: Target): BlockId[] {
  return held(board, target, "block");
}
