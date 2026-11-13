// @vitest-environment node

// What happens to one-time evaluations, on the sample board.

import { describe, expect, test } from "vitest";
import { createGraph } from "../graph/index.ts";
import type { CancelledReply, EvaluateReply, MissingReply, Recipe } from "../protocol/index.ts";
import { createBoard, sample } from "../testing/board.ts";
import { hello } from "../testing/store.ts";
import type { BlockId } from "../types/board.ts";
import { answered, createOneTime, left, neededBy, pendingEvaluations, sent, settle } from "./evaluations.ts";
import type { OneTime, Plain, WithParams } from "./evaluations.ts";
import { inputsByBlock } from "./recipes.ts";
import { counter, opening } from "./revisions.ts";
import { blockStatuses, replyOutcome } from "./status.ts";
import type { Outcome } from "./status.ts";

const graph = createGraph(sample);
const inputs = inputsByBlock(sample, graph, hello.block_registry, new Set());
// The revisions of the sample board as it opens: a 1, b 2, d 3, f 4, c 5, e 6.
const revisions = opening(graph, counter());

function at(id: BlockId): number {
  const revision = revisions.get(id);
  if (revision === undefined) throw new Error(`Block "${id}" has no revision.`);
  return revision;
}

function outcome(status: Outcome["status"], id: BlockId): Outcome {
  return {
    status,
    revision: at(id),
    summary: null,
    reason: status === "invalid" || status === "waiting" ? "Not yet." : null,
    conditions: [],
    stdout: "",
    stderr: "",
  };
}

// The statuses of the sample board, where the blocks given replied.
function statuses(replies: [BlockId, Outcome["status"]][] = []) {
  return blockStatuses({
    graph,
    revisions,
    outcomes: new Map(replies.map(([id, status]) => [id, outcome(status, id)])),
    inputs,
    running: new Set(),
    before: new Map(),
  }).statuses;
}

function plain(blocks: BlockId[], priority = 0): Plain {
  const oneTime = createOneTime(blocks, undefined, priority, statuses(), inputs);
  if (oneTime instanceof Error || "params" in oneTime) throw new Error("No one-time evaluation.");
  return oneTime;
}

function withParams(blocks: BlockId[], priority = 0): WithParams {
  const oneTime = createOneTime(blocks, { save: true }, priority, statuses(), inputs);
  if (oneTime instanceof Error || !("params" in oneTime)) {
    throw new Error("No one-time evaluation with parameters.");
  }
  return oneTime;
}

function errorMessage(oneTime: OneTime | Error): string {
  return oneTime instanceof Error ? oneTime.message : "";
}

const recipe: Recipe = {
  registry_id: "blockr.core-head_block",
  state: { n: 6 },
  inputs: [{ input: "data", block: "a", revision: 1 }],
};

const missing: MissingReply = { id: "1", status: "missing", missing: [{ block: "a", revision: 1 }] };
const cancelled: CancelledReply = { id: "1", status: "cancelled" };

// The one-time evaluations once R replied ok to the evaluation of a block for
// one with parameters.
function ok(oneTimes: ReadonlyMap<number, OneTime>, id: number, block: BlockId) {
  const reply: EvaluateReply = {
    id: "1",
    status: "ok",
    block,
    revision: at(block),
    summary: null,
    reason: null,
    conditions: [],
    stdout: "Wrote iris.csv",
    stderr: "",
  };
  return answered(oneTimes, id, block, reply, replyOutcome(reply, at(block)), at(block));
}

function oneTimePart(oneTimes: ReadonlyMap<number, OneTime>, id: number, block: BlockId) {
  const oneTime = oneTimes.get(id);
  return oneTime !== undefined && "params" in oneTime ? oneTime.parts.get(block) : undefined;
}

describe("a new one-time evaluation", () => {
  test("without parameters leaves out the blocks that the board lacks, and rejects where that leaves none", () => {
    expect(plain(["c", "z"], 1)).toEqual({ blocks: new Set(["c"]), priority: 1 });
    expect(errorMessage(createOneTime(["z", "y"], undefined, 0, statuses(), inputs))).toBe(
      'The board holds none of the blocks "z" and "y" to evaluate.',
    );
  });

  test("with parameters has a part for each block, and rejects where it names no block, a block the board lacks or one that an input or a block upstream keeps waiting", () => {
    const oneTime = withParams(["d", "b"]);
    expect([...oneTime.parts.keys()]).toEqual(["d", "b"]);
    expect(oneTime.parts.get("b")).toEqual({ recipe: undefined, sent: undefined, result: undefined });

    expect(errorMessage(createOneTime([], {}, 0, statuses(), inputs))).toBe("An evaluation with parameters names no block.");
    expect(errorMessage(createOneTime(["b", "z"], {}, 0, statuses(), inputs))).toBe(
      'The board holds no block "z" to evaluate.',
    );
    const loose = createBoard({ c: "merge_block" });
    const looseGraph = createGraph(loose);
    const looseInputs = inputsByBlock(loose, looseGraph, hello.block_registry, new Set());
    const waiting = blockStatuses({
      graph: looseGraph,
      revisions: opening(looseGraph, counter()),
      outcomes: new Map(),
      inputs: looseInputs,
      running: new Set(),
      before: new Map(),
    }).statuses;
    expect(errorMessage(createOneTime(["c"], {}, 0, waiting, looseInputs))).toBe(
      'Block "c" is waiting for its inputs "x" and "y" to be linked. Its evaluation with parameters is not sent.',
    );
    expect(errorMessage(createOneTime(["b"], {}, 0, statuses([["a", "error"]]), inputs))).toBe(
      'Block "b" is waiting for block "a". Its evaluation with parameters is not sent.',
    );
  });

  test("with parameters takes a block that R says waits, which it runs, as a save button does", () => {
    const now = statuses([
      ["a", "ok"],
      ["b", "waiting"],
    ]);
    expect(now.get("b")?.status).toBe("waiting");
    const oneTime = createOneTime(["b"], { save: true }, 0, now, inputs);
    if (oneTime instanceof Error || !("params" in oneTime)) throw new Error("No one-time evaluation with parameters.");
    expect([...oneTime.parts.keys()]).toEqual(["b"]);
  });
});

describe("one-time evaluations without parameters", () => {
  test("settle once none of their blocks reads stale or unevaluated, with each block's status", () => {
    const oneTimes = new Map<number, OneTime>([
      [1, plain(["a"])],
      [2, plain(["a", "b"])],
      [3, withParams(["a"])],
    ]);
    const now = statuses([["a", "ok"]]);
    const found = settle(oneTimes, now);
    expect(found.settled).toEqual([[1, { statuses: { a: now.get("a") } }]]);
    expect([...found.oneTimes.keys()]).toEqual([2, 3]);
    expect(settle(oneTimes, statuses()).oneTimes).toBe(oneTimes);

    // A block that cannot run is as current as it gets.
    const failed = statuses([["a", "error"]]);
    expect(settle(new Map([[4, plain(["c"])]]), failed).settled).toEqual([
      [4, { statuses: { c: failed.get("c") } }],
    ]);
  });

  test("leave out a block that leaves the board, and reject once they are left with none", () => {
    const found = left(
      new Map<number, OneTime>([
        [1, plain(["d", "f"])],
        [2, plain(["f"])],
      ]),
      new Set(["f"]),
    );
    expect(found.oneTimes.get(1)).toEqual({ blocks: new Set(["d"]), priority: 0 });
    expect(found.oneTimes.has(2)).toBe(false);
    expect(found.settled).toEqual([
      [2, { error: new Error("Every block of the one-time evaluation left the board.") }],
    ]);
  });
});

describe("a one-time evaluation with parameters", () => {
  test("resolves once R's replies give each of its blocks a result, with the statuses they give", () => {
    let oneTimes: ReadonlyMap<number, OneTime> = new Map([[7, withParams(["b", "d"])]]);
    oneTimes = sent(oneTimes, 7, "b", { id: 20, recipe });
    expect(pendingEvaluations(oneTimes)).toEqual([{ oneTime: 7, block: "d", priority: 0 }]);

    const first = ok(oneTimes, 7, "b");
    expect(first.settled).toEqual([]);
    expect(oneTimePart(first.oneTimes, 7, "b")?.result).toMatchObject({
      status: "ready",
      revision: at("b"),
      stdout: "Wrote iris.csv",
    });

    const second = ok(first.oneTimes, 7, "d");
    expect(second.oneTimes.has(7)).toBe(false);
    expect(second.settled.map(([id]) => id)).toEqual([7]);
    const result = second.settled[0]?.[1];
    const resolved = result !== undefined && "statuses" in result ? result.statuses : {};
    expect(Object.keys(resolved)).toEqual(["b", "d"]);
    expect(resolved.b).toBe(oneTimePart(first.oneTimes, 7, "b")?.result);
    expect(resolved.d).toMatchObject({ status: "ready", revision: at("d") });
  });

  test("sends a block again where R replies missing, and rejects where R replies cancelled", () => {
    const oneTimes = sent(new Map([[7, withParams(["b"])]]), 7, "b", { id: 20, recipe });

    const again = answered(oneTimes, 7, "b", missing, undefined, at("b"));
    expect(again.settled).toEqual([]);
    expect(oneTimePart(again.oneTimes, 7, "b")).toEqual({ recipe: undefined, sent: undefined, result: undefined });
    expect(pendingEvaluations(again.oneTimes)).toEqual([{ oneTime: 7, block: "b", priority: 0 }]);

    const dropped = answered(oneTimes, 7, "b", cancelled, undefined, at("b"));
    expect(dropped.oneTimes.size).toBe(0);
    expect(dropped.settled).toEqual([
      [
        7,
        {
          error: new Error(
            'R dropped the evaluation of block "b" with parameters, as it does for a block that leaves the board.',
          ),
        },
      ],
    ]);

    expect(answered(oneTimes, 8, "b", cancelled, undefined, at("b")).oneTimes).toBe(oneTimes);
  });

  test("rejects where a block leaves the board before its evaluation is sent, and not after", () => {
    const oneTimes = sent(new Map([[7, withParams(["b", "d"])]]), 7, "b", { id: 20, recipe });
    expect(left(oneTimes, new Set(["b"])).settled).toEqual([]);
    expect(left(oneTimes, new Set(["d"])).settled).toEqual([
      [7, { error: new Error('Block "d" left the board before its evaluation with parameters was sent.') }],
    ]);
  });
});

describe("what one-time evaluations need", () => {
  test("is the blocks of one without parameters, and what the blocks of one with parameters read, until R replies for them", () => {
    let oneTimes: ReadonlyMap<number, OneTime> = new Map<number, OneTime>([
      [1, plain(["d"], 1)],
      [2, withParams(["c"], 2)],
    ]);
    expect(neededBy(oneTimes, graph)).toEqual([
      { blocks: new Set(["d"]), priority: 1 },
      { blocks: ["a", "b"], priority: 2 },
    ]);

    oneTimes = sent(oneTimes, 2, "c", { id: 20, recipe });
    expect(neededBy(oneTimes, graph)[1]).toEqual({ blocks: ["a"], priority: 2 });

    oneTimes = ok(oneTimes, 2, "c").oneTimes;
    expect(neededBy(oneTimes, graph)).toEqual([{ blocks: new Set(["d"]), priority: 1 }]);
  });
});
