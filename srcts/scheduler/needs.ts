// What the scheduler keeps evaluated: the blocks that something needs, each at
// an evaluation priority.

import type { Graph } from "../graph/index.ts";
import type { BlockId } from "../types/board.ts";

// Blocks that one thing needs, such as a demand or a one-time evaluation, at
// its evaluation priority.
export interface Need {
  readonly blocks: Iterable<BlockId>;
  readonly priority: number;
}

// The blocks needed, with everything upstream of them. Each block takes the
// highest evaluation priority of anything that needs it: itself or a block
// downstream of it. Blocks that the board lacks are passed over.
export function needsOf(graph: Graph, needs: Iterable<Need>): Map<BlockId, number> {
  const onBoard = new Set(graph.order);
  const found = new Map<BlockId, number>();
  const raise = (id: BlockId, priority: number) => {
    if (!onBoard.has(id)) return;
    const was = found.get(id);
    if (was === undefined || priority > was) found.set(id, priority);
  };

  for (const need of needs) {
    for (const id of need.blocks) raise(id, need.priority);
  }
  // Downstream first, so that a block has its priority before it passes it on.
  for (const id of [...graph.order].reverse()) {
    const priority = found.get(id);
    if (priority === undefined) continue;
    for (const up of graph.upstream(id)) raise(up, priority);
  }
  return found;
}
