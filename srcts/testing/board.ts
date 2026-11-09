// Boards for the tests of what reads a board's links: blocks of core's
// classes, as the block registry in R's reply to `hello` has them, and links
// between them.

import type { Board } from "../types/board.ts";

// A board of the blocks given, each by id with its class's name, and of the
// links given, each by id as [from, to, input].
export function boardOf(
  blocks: Readonly<Record<string, string>>,
  links: Readonly<Record<string, readonly [string, string, string]>> = {},
): Board {
  return {
    id: "board",
    name: "Board",
    blocks: Object.fromEntries(
      Object.entries(blocks).map(([id, name]) => [
        id,
        { registry_id: `blockr.core-${name}`, state: {}, name: id, locked: null },
      ]),
    ),
    links: Object.fromEntries(
      Object.entries(links).map(([id, [from, to, input]]) => [id, { from, to, input }]),
    ),
    stacks: {},
    options: {},
    plugins: {},
  };
}

// The sample board of the wire protocol's fixtures:
// - The dataset a feeds the head b, the subset d and the scatter f.
// - The merge c takes a as x and b as y.
// - The variadic rbind e binds b and d, in that order.
export const sample: Board = boardOf(
  {
    a: "dataset_block",
    b: "head_block",
    c: "merge_block",
    d: "subset_block",
    e: "rbind_block",
    f: "scatter_block",
  },
  {
    ab: ["a", "b", "data"],
    ac: ["a", "c", "x"],
    bc: ["b", "c", "y"],
    ad: ["a", "d", "data"],
    be: ["b", "e", ""],
    de: ["d", "e", ""],
    af: ["a", "f", "data"],
  },
);
