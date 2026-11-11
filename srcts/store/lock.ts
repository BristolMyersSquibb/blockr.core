// What the board lock lets a delta change, as Locked boards and the properties
// of blocks has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.11#the-board-lock).
// Under the board lock, the store refuses every change but three: one to the
// state of a block that the board lock leaves open, one to a board option's
// value, and one to a plugin's state, which the plugin locks itself. So the
// board lock fixes the board's blocks and links, its name, every block's name
// and lock, and its stacks, while the app sets the plugins that run, whose
// names are their classes'.

import type { BlockId, Board } from "../types/board.ts";
import { own, quoted } from "../util/index.ts";
import type { Delta } from "./delta.ts";

// Whether a lock fixes a block's state: the board lock is on, and the block's
// `locked` is not false, so that a block whose `locked` is null follows the
// board lock. Under the board lock, a block that the board lacks reads as fixed
// too, since no delta adds one there.
export function fixes(board: Board | undefined, block: BlockId, boardLock: boolean): boolean {
  if (!boardLock) return false;
  const held = board === undefined ? undefined : own(board.blocks, block);
  return held?.locked !== false;
}

// What the board lock refuses of a delta, each in a sentence, read from the
// inverse of the committed delta, which holds only what the delta changed, and
// from the board before it, whose blocks' locks say which states are fixed.
// An inverse holds no board option values, which the board lock leaves open.
export function lockFailures(before: Board, inverse: Delta): string[] {
  const failures: string[] = [];
  // A sentence for what the board lock refuses to do to the objects named,
  // such as `refuse("remove", "block", "blocks", ["b"])`.
  const refuse = (verb: string, one: string, several: string, ids: readonly string[]): void => {
    if (ids.length === 0) return;
    failures.push(`The board lock refuses to ${verb} ${ids.length === 1 ? one : several} ${quoted(ids)}.`);
  };

  // What the inverse removes, the delta added, and what the inverse adds back,
  // the delta removed.
  const blocks = inverse.blocks;
  const mods = Object.entries(blocks?.mod ?? {});
  refuse("add", "block", "blocks", blocks?.rm ?? []);
  refuse("remove", "block", "blocks", Object.keys(blocks?.add ?? {}));
  refuse("rename", "block", "blocks", mods.filter(([, mod]) => mod.name !== undefined).map(([id]) => id));
  refuse(
    "change",
    "the lock of block",
    "the locks of the blocks",
    mods.filter(([, mod]) => mod.locked !== undefined).map(([id]) => id),
  );
  for (const [id, mod] of mods) {
    if (mod.state === undefined && mod.unset === undefined) continue;
    const locked = own(before.blocks, id)?.locked;
    if (locked === true) {
      failures.push(`The lock of block "${id}" fixes its state under the board lock.`);
    } else if (locked === null) {
      failures.push(`The board lock fixes the state of block "${id}", whose lock follows the board lock.`);
    }
  }

  for (const [part, one, several] of [
    [inverse.links, "link", "links"],
    [inverse.stacks, "stack", "stacks"],
  ] as const) {
    refuse("add", one, several, part?.rm ?? []);
    refuse("change", one, several, Object.keys(part?.mod ?? {}));
    refuse("remove", one, several, Object.keys(part?.add ?? {}));
  }

  if (inverse.name !== undefined) failures.push("The board lock refuses to rename the board.");

  // A patch changes a plugin's state alone, which only the plugin locks.
  return failures;
}
