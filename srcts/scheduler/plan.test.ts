// @vitest-environment node

// What a pass of the scheduler decides, on the sample board.

import { describe, expect, test } from "vitest";
import { createGraph } from "../graph/index.ts";
import { sample } from "../testing/board.ts";
import { hello } from "../testing/store.ts";
import type { BlockId } from "../types/board.ts";
import { neededBlocks } from "./needs.ts";
import { evaluationPlan, GIVE_WAY } from "./plan.ts";
import type { Asked, Pending, Sent } from "./plan.ts";
import { inputsByBlock } from "./recipes.ts";
import { counter, opening } from "./revisions.ts";
import { blockStatuses } from "./status.ts";
import type { Outcome } from "./status.ts";

const graph = createGraph(sample);
const inputs = inputsByBlock(sample, graph, hello.block_registry, new Set());
// The revisions of the sample board as it opens: a 1, b 2, d 3, f 4, c 5, e 6.
const revisions = opening(graph, counter());
const now = 100_000;

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

let ids = 0;
function sent(block: BlockId, fields: Partial<Sent> = {}): Sent {
  return {
    id: ++ids,
    at: now,
    block,
    revision: at(block),
    oneTime: undefined,
    background: false,
    cancelled: false,
    ...fields,
  };
}

// The evaluation of a block that a one-time evaluation with parameters waits
// for, not sent yet.
function pending(block: BlockId, priority = 0): Pending {
  return { oneTime: ++ids, block, priority };
}

function plan(fields: Partial<Parameters<typeof evaluationPlan>[0]> = {}) {
  return evaluationPlan({
    graph,
    revisions,
    statuses: statuses(),
    inputs,
    needs: new Map(),
    sent: [],
    asked: [],
    pending: [],
    queue: 2,
    interrupt: false,
    holding: false,
    now,
    idleSince: undefined,
    delay: Infinity,
    ...fields,
  });
}

function blocksToSend(found: ReturnType<typeof plan>): BlockId[] {
  return found.send.map((item) => item.block);
}

describe("what to send", () => {
  test("is what is needed and not current, highest evaluation priority first and in topological order within one, as R has room", () => {
    const needs = neededBlocks(graph, [
      { blocks: ["c"], priority: 1 },
      { blocks: ["d"], priority: 0 },
    ]);
    expect(blocksToSend(plan({ needs }))).toEqual(["a", "b", "c"]);
    expect(blocksToSend(plan({ needs, queue: 0 }))).toEqual(["a"]);
    expect(blocksToSend(plan({ needs, queue: Infinity }))).toEqual(["a", "b", "c", "d"]);
    expect(blocksToSend(plan({ needs, sent: [sent("f")] }))).toEqual(["a", "b"]);
  });

  test("is nothing while the scheduler holds, and no block whose upstream R neither holds nor computes", () => {
    const needs = neededBlocks(graph, [{ blocks: ["b"], priority: 0 }]);
    expect(plan({ needs, holding: true }).send).toEqual([]);
    expect(blocksToSend(plan({ needs: new Map([["b", 0]]) }))).toEqual([]);
    expect(blocksToSend(plan({ needs: new Map([["b", 0]]), statuses: statuses([["a", "ok"]]) }))).toEqual(["b"]);
    expect(blocksToSend(plan({ needs: new Map([["b", 0]]), sent: [sent("a")] }))).toEqual(["b"]);
    expect(blocksToSend(plan({ needs: new Map([["b", 0]]), sent: [sent("a", { cancelled: true })] }))).toEqual([]);
  });
});

describe("what to cancel", () => {
  test("is an evaluation whose revision has moved, and one that nothing needs unless R may be running it", () => {
    const moved = sent("a", { revision: 0 });
    const idle = sent("b");
    const unneeded = sent("c");
    expect(plan({ sent: [moved, idle, unneeded] }).cancel).toEqual([moved.id, idle.id, unneeded.id]);

    const oldest = sent("a");
    const later = sent("b");
    expect(plan({ sent: [oldest, later] }).cancel).toEqual([later.id]);
    expect(plan({ sent: [oldest, later], needs: neededBlocks(graph, [{ blocks: ["b"], priority: 0 }]) }).cancel).toEqual([]);
  });

  test("is one below 0 that something else has waited behind for 5 s, where R can stop what it has started", () => {
    const behind = sent("d", { background: true });
    const waited: Asked = { id: ++ids, at: now - GIVE_WAY };
    expect(plan({ sent: [behind], asked: [waited], interrupt: true }).cancel).toEqual([behind.id]);
    expect(plan({ sent: [behind], asked: [waited], interrupt: false }).cancel).toEqual([]);

    const young: Asked = { id: ++ids, at: now - GIVE_WAY + 1 };
    const found = plan({ sent: [behind], asked: [young], interrupt: true });
    expect(found.cancel).toEqual([]);
    expect(found.wakeAt).toBe(now + 1);
  });

  test("is never an evaluation for a one-time evaluation with parameters", () => {
    const save = sent("b", { revision: 0, oneTime: { id: ++ids, priority: 0 } });
    expect(plan({ sent: [save] }).cancel).toEqual([]);
  });
});

describe("the evaluation of a block for a one-time evaluation with parameters", () => {
  test("goes once what the block reads is available, and the block takes no other evaluation meanwhile", () => {
    const save = pending("b");
    const needs = neededBlocks(graph, [{ blocks: ["b"], priority: 0 }]);
    expect(plan({ needs, pending: [save] }).send).toEqual([
      { block: "a" },
      { block: "b", oneTime: save.oneTime },
    ]);
    expect(plan({ needs, pending: [save], statuses: statuses([["a", "ok"]]) }).send).toEqual([
      { block: "b", oneTime: save.oneTime },
    ]);
  });

  test("rejects the one-time evaluation where a block upstream keeps the block waiting, whatever room R has", () => {
    const save = pending("b");
    const found = plan({ pending: [save], statuses: statuses([["a", "error"]]), queue: 0, sent: [sent("f")] });
    expect(found.send).toEqual([]);
    expect(found.reject.map(({ oneTime, error }) => [oneTime, error.message])).toEqual([
      [save.oneTime, 'Block "b" is waiting for block "a". Its evaluation with parameters is not sent.'],
    ]);
  });

  test("sends the one-time evaluation of a block that R says waits", () => {
    const save = pending("b");
    const found = plan({
      pending: [save],
      statuses: statuses([
        ["a", "ok"],
        ["b", "waiting"],
      ]),
    });
    expect(found.reject).toEqual([]);
    expect(found.send).toEqual([{ block: "b", oneTime: save.oneTime }]);
  });
});

describe("evaluation in the background", () => {
  test("starts once R has had nothing else to run for the delay, one block at a time, upstream first", () => {
    const first = plan({ delay: 2 });
    expect(first.send).toEqual([]);
    expect(first.idleSince).toBe(now);
    expect(first.wakeAt).toBe(now + 2000);

    const due = plan({ delay: 2, idleSince: now - 2000 });
    expect(due.send).toEqual([{ block: "a", background: true }]);

    expect(plan({ delay: 2, idleSince: now - 2000, sent: [sent("d", { background: true })] }).send).toEqual([]);
    expect(blocksToSend(plan({ delay: 2, idleSince: now - 2000, statuses: statuses([["a", "ok"]]) }))).toEqual(["b"]);
  });

  test("waits while R has anything else to run, an output, a request or an evaluation at 0 or above", () => {
    const asked = plan({ delay: 2, idleSince: now - 2000, asked: [{ id: ++ids, at: now }] });
    expect(asked.send).toEqual([]);
    expect(asked.idleSince).toBeUndefined();

    const needs = neededBlocks(graph, [{ blocks: ["a"], priority: 0 }]);
    const busy = plan({ delay: 2, idleSince: now - 2000, needs });
    expect(busy.send).toEqual([{ block: "a" }]);
    expect(busy.idleSince).toBeUndefined();
  });
});
