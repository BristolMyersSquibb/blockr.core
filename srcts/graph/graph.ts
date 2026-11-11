// The graph of a board's links. Each block is a node, and each link is an edge
// from the block it comes from to the block it feeds. The graph holds that
// structure alone. What a block's links mean for the block, such as the inputs
// of its recipe, is worked out on top of the graph by whatever needs it.

import type { BlockId, Board, LinkId } from "../types/board.ts";

export interface Graph {
  // Every block of the board, each after the blocks upstream of it. Where the
  // links form a cycle, the blocks that the order cannot place come last, in
  // the order the board holds them.
  readonly order: readonly BlockId[];
  // The links into a block, by id, in link order.
  linksInto(id: BlockId): readonly LinkId[];
  // The blocks that a block reads from, and the blocks that read from it. Each
  // comes once, in link order.
  upstream(id: BlockId): readonly BlockId[];
  downstream(id: BlockId): readonly BlockId[];
  // The blocks given, with every block upstream of them, or downstream of
  // them. Blocks that the board lacks are passed over.
  withUpstream(ids: Iterable<BlockId>): ReadonlySet<BlockId>;
  withDownstream(ids: Iterable<BlockId>): ReadonlySet<BlockId>;
  // The blocks of a cycle among the links, in order, if the links form one.
  cycle(): readonly BlockId[] | undefined;
}

export function createGraph(board: Pick<Board, "blocks" | "links">): Graph {
  const blocks = Object.keys(board.blocks);
  const into = new Map<BlockId, LinkId[]>();
  // The ends of each link, once per link, so that two links between the same
  // blocks count twice.
  const sources = new Map<BlockId, BlockId[]>();
  const targets = new Map<BlockId, BlockId[]>();
  for (const id of blocks) {
    into.set(id, []);
    sources.set(id, []);
    targets.set(id, []);
  }

  // A link whose ends the board lacks is no part of the graph. The store's
  // checks of the links name it.
  for (const [id, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === undefined || from === undefined || to === undefined) continue;
    linksIn.push(id);
    from.push(link.from);
    to.push(link.to);
  }

  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle: { found: readonly BlockId[] | undefined } | undefined;

  return {
    order,
    linksInto: (id) => into.get(id) ?? [],
    upstream: (id) => upstream.get(id) ?? [],
    downstream: (id) => downstream.get(id) ?? [],
    withUpstream: (ids) => closure(ids, upstream),
    withDownstream: (ids) => closure(ids, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    },
  };
}

// The blocks at each block's ends, each once.
function once(ends: ReadonlyMap<BlockId, readonly BlockId[]>): Map<BlockId, BlockId[]> {
  return new Map([...ends].map(([id, blocks]) => [id, [...new Set(blocks)]]));
}

// The blocks given, with every block that the steps reach from them.
function closure(
  ids: Iterable<BlockId>,
  steps: ReadonlyMap<BlockId, readonly BlockId[]>,
): Set<BlockId> {
  const reached = new Set<BlockId>();
  const todo = [...ids].filter((id) => steps.has(id));
  for (const id of todo) reached.add(id);
  // The loop visits the blocks it appends as well.
  for (const id of todo) {
    for (const next of steps.get(id) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}

// Kahn's algorithm. It takes the blocks that have nothing upstream left, in the
// order the board holds them.
function topological(
  blocks: readonly BlockId[],
  sources: ReadonlyMap<BlockId, readonly BlockId[]>,
  targets: ReadonlyMap<BlockId, readonly BlockId[]>,
): BlockId[] {
  const left = new Map(blocks.map((id) => [id, sources.get(id)?.length ?? 0]));
  const order = blocks.filter((id) => left.get(id) === 0);
  // The loop visits the blocks it appends as well.
  for (const id of order) {
    for (const next of targets.get(id) ?? []) {
      const count = (left.get(next) ?? 0) - 1;
      left.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id) => !placed.has(id))];
}

// A depth-first search for a cycle. It starts from the blocks in the order they
// first come as a link's source, and follows the links in link order.
function findCycle(
  board: Pick<Board, "links">,
  targets: ReadonlyMap<BlockId, readonly BlockId[]>,
): BlockId[] | undefined {
  const done = new Set<BlockId>();
  const path: BlockId[] = [];
  const onPath = new Set<BlockId>();

  function visit(block: BlockId): BlockId[] | undefined {
    if (onPath.has(block)) return path.slice(path.indexOf(block));
    if (done.has(block)) return undefined;
    path.push(block);
    onPath.add(block);
    for (const next of targets.get(block) ?? []) {
      const cycle = visit(next);
      if (cycle !== undefined) return cycle;
    }
    path.pop();
    onPath.delete(block);
    done.add(block);
    return undefined;
  }

  const starts = new Set(Object.values(board.links).map((link) => link.from));
  for (const block of starts) {
    if (!targets.has(block)) continue;
    const cycle = visit(block);
    if (cycle !== undefined) return cycle;
  }
  return undefined;
}
