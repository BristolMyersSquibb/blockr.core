// @vitest-environment node

// What a block's links make of the inputs its class declares.

import { describe, expect, test } from "vitest";
import { graphOf } from "../graph/index.ts";
import { boardOf, sample } from "../testing/board.ts";
import { hello } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import { inputsOf } from "./recipes.ts";

function inputs(board: Board, bound: string[] = []) {
  return inputsOf(board, graphOf(board), hello.block_registry, new Set(bound));
}

describe("a block's inputs", () => {
  test("take each fixed input from the link that feeds it, in the order of the class's inputs", () => {
    const merge = boardOf(
      { a: "dataset_block", b: "dataset_block", c: "merge_block" },
      { bc: ["b", "c", "y"], ac: ["a", "c", "x"] },
    );
    expect(inputs(merge).get("c")).toEqual({
      recipe: [
        { input: "x", block: "a" },
        { input: "y", block: "b" },
      ],
      unlinked: [],
    });
    expect(inputs(sample).get("a")).toEqual({ recipe: [], unlinked: [] });
  });

  test("take a variadic block's links in link order", () => {
    expect(inputs(sample).get("e")?.recipe).toEqual([
      { input: "", block: "b" },
      { input: "", block: "d" },
    ]);
    const swapped = boardOf(
      { b: "dataset_block", d: "dataset_block", e: "rbind_block" },
      { de: ["d", "e", ""], be: ["b", "e", ""] },
    );
    expect(inputs(swapped).get("e")?.recipe.map((input) => input.block)).toEqual(["d", "b"]);
  });

  test("name the inputs that no link feeds and R does not bind, and a variadic block without a link", () => {
    const loose = boardOf(
      { a: "dataset_block", c: "merge_block", e: "rbind_block" },
      { ac: ["a", "c", "x"] },
    );
    expect(inputs(loose).get("c")?.unlinked).toEqual(["y"]);
    expect(inputs(loose).get("e")).toEqual({ recipe: [], unlinked: ["..."] });
  });

  test("count an input that R binds itself, which a link still takes the place of", () => {
    const alone = boardOf({ b: "head_block" });
    expect(inputs(alone, ["data"]).get("b")).toEqual({
      recipe: [{ input: "data" }],
      unlinked: [],
    });
    expect(inputs(sample, ["data"]).get("b")?.recipe).toEqual([{ input: "data", block: "a" }]);
  });
});
