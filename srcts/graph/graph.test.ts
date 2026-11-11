// @vitest-environment node

// The graph of a board's links, on the sample board of the wire protocol's
// fixtures.

import { describe, expect, test } from "vitest";
import { createBoard, sample } from "../testing/board.ts";
import { createGraph } from "./index.ts";

describe("the order", () => {
  test("puts every block after the blocks upstream of it, taking ties in the order the board holds them", () => {
    expect(createGraph(sample).order).toEqual(["a", "b", "d", "f", "c", "e"]);
  });

  test("puts last, in board order, the blocks that a cycle leaves unplaced", () => {
    const graph = createGraph(createBoard({ a: "head_block", b: "head_block", c: "head_block" }, { ab: ["a", "b", "data"], ba: ["b", "a", "data"] }));
    expect(graph.order).toEqual(["c", "a", "b"]);
  });
});

describe("a block's neighbours", () => {
  test("are the links into it in link order, and the blocks up and down from it, each once", () => {
    const graph = createGraph(sample);
    expect(graph.linksInto("e")).toEqual(["be", "de"]);
    expect(graph.upstream("c")).toEqual(["a", "b"]);
    expect(graph.downstream("b")).toEqual(["c", "e"]);
    expect(graph.upstream("a")).toEqual([]);
    expect(graph.upstream("z")).toEqual([]);

    const twice = createGraph(createBoard({ a: "dataset_block", b: "rbind_block" }, { ab1: ["a", "b", ""], ab2: ["a", "b", ""] }));
    expect(twice.linksInto("b")).toEqual(["ab1", "ab2"]);
    expect(twice.upstream("b")).toEqual(["a"]);
    expect(twice.order).toEqual(["a", "b"]);
  });

  test("leave out a link whose ends the board lacks", () => {
    const graph = createGraph(createBoard({ a: "dataset_block", b: "rbind_block" }, { ab: ["a", "b", ""], zb: ["z", "b", ""], az: ["a", "z", ""] }));
    expect(graph.linksInto("b")).toEqual(["ab"]);
    expect(graph.downstream("a")).toEqual(["b"]);
  });
});

describe("the blocks reached", () => {
  test("upstream of a set are the set with everything it reads, and downstream the set with everything that reads it", () => {
    const graph = createGraph(sample);
    expect([...graph.withUpstream(["c"])].sort()).toEqual(["a", "b", "c"]);
    expect([...graph.withUpstream(["e", "f"])].sort()).toEqual(["a", "b", "d", "e", "f"]);
    expect([...graph.withDownstream(["b"])].sort()).toEqual(["b", "c", "e"]);
    expect([...graph.withDownstream(["d", "z"])].sort()).toEqual(["d", "e"]);
  });
});

describe("a cycle", () => {
  test("is named by its blocks in order, and the sample board has none", () => {
    expect(createGraph(sample).cycle()).toBeUndefined();
    expect(createGraph(createBoard({ e: "rbind_block" }, { ee: ["e", "e", ""] })).cycle()).toEqual(["e"]);
    const graph = createGraph(createBoard({ a: "head_block", b: "head_block", c: "head_block" }, { ab: ["a", "b", "data"], bc: ["b", "c", "data"], ca: ["c", "a", "data"] }));
    expect(graph.cycle()).toEqual(["a", "b", "c"]);
  });
});
