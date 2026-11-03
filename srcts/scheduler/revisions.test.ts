// @vitest-environment node

// Revisions: one counter for the page, and what a change to the board does to
// them.

import { describe, expect, test } from "vitest";
import { graphOf } from "../graph/index.ts";
import { sample } from "../testing/board.ts";
import { hello } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import { inputsOf } from "./recipes.ts";
import { bump, counter, opening, revise } from "./revisions.ts";

function read(board: Board) {
  const graph = graphOf(board);
  return { board, graph, inputs: inputsOf(board, graph, hello.block_registry, new Set()) };
}

// The revisions that opening the sample board gives, and what revise() makes
// of them after a change to the board.
function change(after: Board, removed: string[] = []) {
  const next = counter();
  const start = read(sample);
  const before = opening(start.graph, next);
  return { before, ...revise(before, start, read(after), new Set(removed), next) };
}

function withState(board: Board, id: string, state: Record<string, number>): Board {
  const block = board.blocks[id];
  if (block === undefined) throw new Error(`The board holds no block "${id}".`);
  return { ...board, blocks: { ...board.blocks, [id]: { ...block, state } } };
}

describe("the counter", () => {
  test("stops at 2^53 - 1 with an error that asks for the page to be reloaded", () => {
    const next = counter(Number.MAX_SAFE_INTEGER - 1);
    expect(next()).toBe(Number.MAX_SAFE_INTEGER);
    expect(() => next()).toThrow(/Reload the page/);
    expect(() => next()).toThrow(/Reload the page/);
  });
});

describe("revisions", () => {
  test("of a board that opens come from the counter, upstream first", () => {
    expect(Object.fromEntries(opening(graphOf(sample), counter()))).toEqual({
      a: 1,
      b: 2,
      d: 3,
      f: 4,
      c: 5,
      e: 6,
    });
  });

  test("go up for a block whose state changes and for everything downstream of it", () => {
    const { before, revisions, gone } = change(withState(sample, "b", { n: 10 }));
    expect(gone).toEqual([]);
    for (const id of ["a", "d", "f"]) expect(revisions.get(id)).toBe(before.get(id));
    for (const id of ["b", "c", "e"]) expect(revisions.get(id)).toBeGreaterThan(6);
  });

  test("stay for a state set to what it holds and for a rename", () => {
    const block = sample.blocks.b;
    if (block === undefined) throw new Error("The sample board lacks block b.");
    const renamed: Board = {
      ...sample,
      blocks: { ...sample.blocks, b: { ...block, name: "First rows", state: { ...block.state } } },
    };
    const { before, revisions } = change(renamed);
    expect(revisions).toEqual(before);
  });

  test("go up for the target of a link that changes", () => {
    const { be, ...links } = sample.links;
    if (be === undefined) throw new Error("The sample board lacks link be.");
    const { before, revisions } = change({ ...sample, links: { ...links, be } });
    expect(revisions.get("e")).toBeGreaterThan(6);
    expect(revisions.get("b")).toBe(before.get("b"));
  });

  test("forget a block that left the board, and give one that a delta removed and added back a new one", () => {
    const { f, ...blocks } = sample.blocks;
    const { af, ...links } = sample.links;
    expect(f).toBeDefined();
    expect(af).toBeDefined();
    const without = change({ ...sample, blocks, links });
    expect(without.gone).toEqual(["f"]);
    expect(without.revisions.has("f")).toBe(false);

    const back = change(sample, ["d"]);
    expect(back.gone).toEqual(["d"]);
    expect(back.revisions.get("d")).toBeGreaterThan(6);
    expect(back.revisions.get("e")).toBeGreaterThan(6);
    expect(back.revisions.get("b")).toBe(back.before.get("b"));
  });

  test("go up for blocks that something invalidates and for everything downstream of them", () => {
    const next = counter();
    const graph = graphOf(sample);
    const revisions = opening(graph, next);
    const after = bump(graph, revisions, new Set(["d", "z"]), next);
    expect(Object.fromEntries(after)).toEqual({ ...Object.fromEntries(revisions), d: 7, e: 8 });
  });
});
