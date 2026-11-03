// @vitest-environment node

// Every block's status, from its revision, its latest reply and its inputs.

import { describe, expect, test } from "vitest";
import { graphOf } from "../graph/index.ts";
import type { Condition } from "../protocol/index.ts";
import { boardOf, sample } from "../testing/board.ts";
import { hello } from "../testing/store.ts";
import type { Board, BlockId } from "../types/board.ts";
import { inputsOf } from "./recipes.ts";
import { counter, opening } from "./revisions.ts";
import { statusesOf, waitingError } from "./status.ts";
import type { BlockStatus, Outcome } from "./status.ts";

const failure: Condition = {
  severity: "error",
  class: ["simpleError", "error", "condition"],
  phase: "block_eval",
  message: "object 'Species' not found",
};

function outcome(status: Outcome["status"], revision: number, conditions: Condition[] = []): Outcome {
  return {
    status,
    revision,
    summary: status === "ok" ? "<data.frame> [150 × 5]" : null,
    conditions,
    omitted: { warning: 0, message: 0 },
    stdout: "",
    stderr: "",
  };
}

// The statuses of a board's blocks, each block at its revision from opening
// the board.
function statuses(
  board: Board,
  outcomes: (revisions: ReadonlyMap<BlockId, number>) => [BlockId, Outcome][] = () => [],
  {
    running = [],
    before = new Map(),
    revisions = opening(graphOf(board), counter()),
  }: {
    running?: BlockId[];
    before?: ReadonlyMap<BlockId, BlockStatus>;
    revisions?: ReadonlyMap<BlockId, number>;
  } = {},
) {
  const graph = graphOf(board);
  return statusesOf({
    graph,
    revisions,
    outcomes: new Map(outcomes(revisions)),
    inputs: inputsOf(board, graph, hello.block_registry, new Set()),
    running: new Set(running),
    before,
  });
}

function at(revisions: ReadonlyMap<BlockId, number>, id: BlockId): number {
  const revision = revisions.get(id);
  if (revision === undefined) throw new Error(`Block "${id}" has no revision.`);
  return revision;
}

describe("statuses", () => {
  test("read unevaluated before a reply, and then what the reply says for the block's revision", () => {
    const { statuses: found } = statuses(sample, (revisions) => [
      ["a", outcome("ok", at(revisions, "a"))],
      ["b", outcome("error", at(revisions, "b"), [failure])],
      ["d", outcome("invalid", at(revisions, "d"))],
    ]);
    expect(Object.fromEntries([...found].map(([id, status]) => [id, status.status]))).toEqual({
      a: "ready",
      b: "failed",
      d: "invalid",
      f: "unevaluated",
      c: "waiting",
      e: "waiting",
    });
    expect(found.get("b")).toMatchObject({ conditions: [failure], summary: null, evaluated: 2 });
    expect(found.get("e")?.conditions[0]?.message).toBe('This block is waiting for blocks "b" and "d".');
  });

  test("read stale where the reply is for an older revision, keeping what it said", () => {
    const { statuses: found } = statuses(sample, () => [["b", outcome("error", 1, [failure])]]);
    expect(found.get("b")).toMatchObject({ status: "stale", evaluated: 1, conditions: [failure] });
  });

  test("read waiting for an unlinked input, with one condition of the phase status", () => {
    const loose = boardOf({ a: "dataset_block", c: "merge_block" }, { ac: ["a", "c", "x"] });
    expect(statuses(loose).statuses.get("c")?.conditions).toEqual([
      {
        severity: "message",
        class: ["blockr_message", "message", "condition"],
        phase: "status",
        message: 'This block is waiting for its input "y" to be linked.',
      },
    ]);
  });

  test("keep each object that reads as before, and name the blocks that changed or left", () => {
    const first = statuses(sample);
    expect(first.changed).toHaveLength(6);

    const same = statuses(sample, undefined, { before: first.statuses });
    expect(same.changed).toEqual([]);
    expect(same.statuses.get("a")).toBe(first.statuses.get("a"));

    const running = statuses(sample, undefined, { before: first.statuses, running: ["a"] });
    expect(running.changed).toEqual(["a"]);
    expect(running.statuses.get("a")?.running).toBe(true);

    const { f, ...blocks } = sample.blocks;
    const { af, ...links } = sample.links;
    expect([f, af]).not.toContain(undefined);
    const revisions = opening(graphOf(sample), counter());
    revisions.delete("f");
    const smaller = statuses({ ...sample, blocks, links }, undefined, {
      before: first.statuses,
      revisions,
    });
    expect(smaller.changed).toEqual(["f"]);
  });
});

describe("the error of an evaluation with parameters of a waiting block", () => {
  test("names the block and what it waits for", () => {
    const loose = boardOf({ c: "merge_block" });
    const status = statuses(loose).statuses.get("c");
    if (status === undefined) throw new Error("Block c has no status.");
    expect(waitingError("c", status).message).toBe(
      'Block "c" is waiting for its inputs "x" and "y" to be linked. Its evaluation with parameters is not sent.',
    );
  });
});
