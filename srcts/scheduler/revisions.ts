// Revisions come from one counter for the page, as an auto-increment hands out
// ids. A revision is then unique across blocks too, and a block that comes back
// takes one above any its id had
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#revisions).

import type { Graph } from "../graph/index.ts";
import { jsonEqual, own } from "../store/util.ts";
import type { BlockId, Board } from "../types/board.ts";
import { sameInputs } from "./recipes.ts";
import type { Inputs } from "./recipes.ts";

// The revisions after `last`, one at a time. They stop at 2^53 - 1, the last
// whole number that a double holds exactly. Past it, adding 1 would no longer
// change a revision, and revisions would repeat.
export function counter(last = 0): () => number {
  return () => {
    if (last >= Number.MAX_SAFE_INTEGER) {
      throw new Error(
        "The page has used up its revisions, 2^53 - 1 of them, so the scheduler has stopped. Reload the page to start again.",
      );
    }
    last += 1;
    return last;
  };
}

// A new revision for every block of a board that opens, upstream first.
export function opening(graph: Graph, next: () => number): Map<BlockId, number> {
  return new Map(graph.order.map((id) => [id, next()]));
}

// The revisions after a change to the board, and the blocks that left it. A
// block takes a new revision where its class, its state or its recipe's inputs
// changed, and so does every block downstream of it. A block that a delta
// removed and added back counts as gone and comes back with a new revision.
// A rename, an attribute, a stack, the front-end's state and a board option
// change no revision.
export function revise(
  revisions: ReadonlyMap<BlockId, number>,
  before: {
    readonly board: Pick<Board, "blocks">;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
  },
  after: {
    readonly board: Pick<Board, "blocks">;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
    readonly graph: Graph;
  },
  removed: ReadonlySet<BlockId>,
  next: () => number,
): { revisions: Map<BlockId, number>; gone: BlockId[] } {
  const gone = [...revisions.keys()].filter(
    (id) => !Object.hasOwn(after.board.blocks, id) || removed.has(id),
  );
  const left = new Set(gone);

  // The blocks whose own recipe changed, or that are new to the board.
  const moved = new Set<BlockId>();
  for (const id of after.graph.order) {
    const was = left.has(id) ? undefined : own(before.board.blocks, id);
    const now = own(after.board.blocks, id);
    if (was === undefined || now === undefined) {
      moved.add(id);
      continue;
    }
    const same =
      was.registry_id === now.registry_id &&
      (was.state === now.state || jsonEqual(was.state, now.state)) &&
      sameInputs(before.inputs.get(id)?.recipe ?? [], after.inputs.get(id)?.recipe ?? []);
    if (!same) moved.add(id);
  }

  const kept = new Map([...revisions].filter(([id]) => !left.has(id)));
  return { revisions: bump(after.graph, kept, moved, next), gone };
}

// A new revision for each of the blocks given and for every block downstream of
// them, upstream first. A block missing from the revisions gets its first.
export function bump(
  graph: Graph,
  revisions: ReadonlyMap<BlockId, number>,
  roots: ReadonlySet<BlockId>,
  next: () => number,
): Map<BlockId, number> {
  const hit = graph.withDownstream(roots);
  const result = new Map<BlockId, number>();
  for (const id of graph.order) {
    const at = revisions.get(id);
    result.set(id, at === undefined || hit.has(id) ? next() : at);
  }
  return result;
}
