// @vitest-environment node

// The graph of a board's links, on the sample board of the wire protocol's
// fixtures.

import { describe, expect, test } from "vitest";
import { openedStore } from "../testing/store.ts";
import type { Block, Board } from "../types/board.ts";
import { graphOf } from "./index.ts";

// The sample board:
// - The dataset a feeds the head b, the subset d and the scatter f.
// - The merge c takes a as x and b as y.
// - The variadic rbind e binds b and d, in that order.
async function sample(): Promise<Board> {
  return (await openedStore()).board();
}

const block: Block = {
  registry_id: "blockr.core-head_block",
  state: {},
  name: "Head",
  attrs: {},
};

// A board of the blocks and links given, each link as [from, to, input].
function board(ids: string[], links: Record<string, [string, string, string]>): Board {
  return {
    id: "board",
    front_end: { id: "blockr.core-basic", state: {}, attrs: {} },
    name: "Board",
    blocks: Object.fromEntries(ids.map((id) => [id, block])),
    links: Object.fromEntries(
      Object.entries(links).map(([id, [from, to, input]]) => [id, { from, to, input }]),
    ),
    stacks: {},
    options: {},
  };
}

describe("the order", () => {
  test("puts every block after the blocks upstream of it, taking ties in the order the board holds them", async () => {
    expect(graphOf(await sample()).order).toEqual(["a", "b", "d", "f", "c", "e"]);
  });

  test("puts last, in board order, the blocks that a cycle leaves unplaced", () => {
    const graph = graphOf(board(["a", "b", "c"], { ab: ["a", "b", ""], ba: ["b", "a", ""] }));
    expect(graph.order).toEqual(["c", "a", "b"]);
  });
});

describe("a block's neighbours", () => {
  test("are the links into it in link order, and the blocks up and down from it, each once", async () => {
    const graph = graphOf(await sample());
    expect(graph.linksInto("e")).toEqual(["be", "de"]);
    expect(graph.upstream("c")).toEqual(["a", "b"]);
    expect(graph.downstream("b")).toEqual(["c", "e"]);
    expect(graph.upstream("a")).toEqual([]);
    expect(graph.upstream("z")).toEqual([]);

    const twice = graphOf(board(["a", "b"], { ab1: ["a", "b", ""], ab2: ["a", "b", ""] }));
    expect(twice.linksInto("b")).toEqual(["ab1", "ab2"]);
    expect(twice.upstream("b")).toEqual(["a"]);
    expect(twice.order).toEqual(["a", "b"]);
  });

  test("leave out a link whose ends the board lacks", () => {
    const graph = graphOf(board(["a", "b"], { ab: ["a", "b", ""], zb: ["z", "b", ""], az: ["a", "z", ""] }));
    expect(graph.linksInto("b")).toEqual(["ab"]);
    expect(graph.downstream("a")).toEqual(["b"]);
  });
});

describe("the blocks reached", () => {
  test("upstream of a set are the set with everything it reads, and downstream the set with everything that reads it", async () => {
    const graph = graphOf(await sample());
    expect([...graph.withUpstream(["c"])].sort()).toEqual(["a", "b", "c"]);
    expect([...graph.withUpstream(["e", "f"])].sort()).toEqual(["a", "b", "d", "e", "f"]);
    expect([...graph.withDownstream(["b"])].sort()).toEqual(["b", "c", "e"]);
    expect([...graph.withDownstream(["d", "z"])].sort()).toEqual(["d", "e"]);
  });
});

describe("a cycle", () => {
  test("is named by its blocks in order, and the sample board has none", async () => {
    expect(graphOf(await sample()).cycle()).toBeUndefined();
    expect(graphOf(board(["e"], { ee: ["e", "e", ""] })).cycle()).toEqual(["e"]);
    const graph = graphOf(board(["a", "b", "c"], { ab: ["a", "b", ""], bc: ["b", "c", ""], ca: ["c", "a", ""] }));
    expect(graph.cycle()).toEqual(["a", "b", "c"]);
  });
});
