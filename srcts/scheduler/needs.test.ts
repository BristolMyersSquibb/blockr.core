// @vitest-environment node

// The blocks needed, each at an evaluation priority.

import { describe, expect, test } from "vitest";
import { createGraph } from "../graph/index.ts";
import { sample } from "../testing/board.ts";
import { neededBlocks } from "./needs.ts";

describe("what is needed", () => {
  test("is every block needed and everything upstream of it, at the highest evaluation priority of anything that needs it", () => {
    const needs = neededBlocks(createGraph(sample), [
      { blocks: ["c"], priority: 1 },
      { blocks: ["d", "z"], priority: 0 },
      { blocks: ["a"], priority: -1 },
    ]);
    expect(Object.fromEntries(needs)).toEqual({ a: 1, b: 1, c: 1, d: 0 });
  });

  test("is nothing where nothing needs a block", () => {
    expect(neededBlocks(createGraph(sample), []).size).toBe(0);
  });
});
