// What the scheduler reads off the board's links and the block registry: the
// blocks in topological order, the inputs of each block's recipe, and the
// inputs that keep a block waiting.

import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";

// An input of a block's recipe, by the input it feeds, with the upstream block
// whose result it binds, or alone where R binds it itself, for a block that
// `serve()` serves alone.
export interface Input {
  readonly input: string;
  readonly block?: BlockId;
}

export interface Graph {
  // Every block, each after the blocks upstream of it.
  readonly order: readonly BlockId[];
  readonly index: ReadonlyMap<BlockId, number>;
  // In the order of the class's inputs, and a variadic block's in link order.
  readonly inputs: ReadonlyMap<BlockId, readonly Input[]>;
  readonly downstream: ReadonlyMap<BlockId, readonly BlockId[]>;
  // The inputs that are neither linked nor bound by R, which keep the block
  // waiting. A variadic block that has no link lacks its input "...".
  readonly unlinked: ReadonlyMap<BlockId, readonly string[]>;
}

export function graphOf(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  bound: ReadonlySet<string>,
): Graph {
  const into = new Map<BlockId, { from: BlockId; input: string }[]>();
  const downstream = new Map<BlockId, BlockId[]>();

  for (const link of Object.values(board.links)) {
    into.set(link.to, [...(into.get(link.to) ?? []), link]);
    downstream.set(link.from, [...(downstream.get(link.from) ?? []), link.to]);
  }

  const inputs = new Map<BlockId, Input[]>();
  const unlinked = new Map<BlockId, string[]>();

  for (const [id, block] of Object.entries(board.blocks)) {
    const declared = Object.hasOwn(registry, block.registry_id)
      ? (registry[block.registry_id]?.inputs ?? [])
      : [];
    const links = into.get(id) ?? [];
    const recipe: Input[] = [];
    const missing: string[] = [];

    if (declared.includes("...")) {
      recipe.push(...links.map((link) => ({ input: link.input, block: link.from })));
      for (const name of bound) {
        if (!links.some((link) => link.input === name)) recipe.push({ input: name });
      }
      if (recipe.length === 0) missing.push("...");
    }

    for (const name of declared) {
      if (name === "...") continue;
      const link = links.find((candidate) => candidate.input === name);
      if (declared.includes("...")) {
        if (link === undefined && !bound.has(name)) missing.push(name);
      } else if (link !== undefined) {
        recipe.push({ input: name, block: link.from });
      } else if (bound.has(name)) {
        recipe.push({ input: name });
      } else {
        missing.push(name);
      }
    }

    inputs.set(id, recipe);
    if (missing.length > 0) unlinked.set(id, missing);
  }

  const order = topological(Object.keys(board.blocks), into, downstream);

  return {
    order,
    index: new Map(order.map((id, i) => [id, i])),
    inputs,
    downstream,
    unlinked,
  };
}

// The blocks upstream of a block, each once, from its recipe's inputs.
export function upstreamOf(graph: Graph, id: BlockId): BlockId[] {
  const blocks = (graph.inputs.get(id) ?? []).flatMap((input) =>
    input.block === undefined ? [] : [input.block],
  );
  return [...new Set(blocks)];
}

// Kahn's algorithm, which takes the blocks that have nothing upstream left in
// the order the board holds them. The store refuses links that form a cycle.
function topological(
  blocks: readonly BlockId[],
  into: ReadonlyMap<BlockId, readonly unknown[]>,
  downstream: ReadonlyMap<BlockId, readonly BlockId[]>,
): BlockId[] {
  const pending = new Map(blocks.map((id) => [id, into.get(id)?.length ?? 0]));
  const order = blocks.filter((id) => pending.get(id) === 0);

  // The loop visits the blocks it appends as well.
  for (const id of order) {
    for (const next of downstream.get(id) ?? []) {
      const left = (pending.get(next) ?? 0) - 1;
      pending.set(next, left);
      if (left === 0) order.push(next);
    }
  }

  return order;
}
