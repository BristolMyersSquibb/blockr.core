// @vitest-environment node

// The scheduler, on the sample board of the wire protocol's fixtures. R is
// played over the protocol's own client, so the tests check what the scheduler
// sends as messages.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { SessionEndedError } from "../protocol/index.ts";
import type {
  Capability,
  Condition,
  EvaluateRequest,
  HelloReply,
  OutputRequest,
} from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Store } from "../store/index.ts";
import { playR } from "../testing/r.ts";
import { reports } from "../testing/reports.ts";
import type { PlayedR } from "../testing/r.ts";
import { coreOptions, hello, reply as replyOf, withPlugins } from "../testing/store.ts";
import type { BlockId } from "../types/board.ts";
import { createScheduler } from "./index.ts";
import type { BlockStatus, Scheduler, SchedulerOptions } from "./index.ts";
import { counter } from "./revisions.ts";

// The sample board:
// - The dataset a feeds the head b, the subset d and the scatter f.
// - The merge c takes a as x and b as y.
// - The variadic rbind e binds b and d, in that order.
//
// And a block served alone, whose input R binds itself.
const helloBlock = replyOf("hello-block") as HelloReply;

const info = {
  width: 640,
  height: 400,
  pixel_ratio: 2,
  bg: "#ffffff",
  fg: "#212529",
  accent: "#0d6efd",
  font: "Inter",
};


async function setup(
  options: Omit<SchedulerOptions, "store" | "client"> = {},
  {
    capabilities,
    reply = hello,
    prepare,
  }: {
    capabilities?: Capability[];
    reply?: HelloReply;
    prepare?: (store: Store) => void;
  } = {},
): Promise<{ r: PlayedR; store: Store; scheduler: Scheduler }> {
  const r = playR();
  r.connect(capabilities);
  const store = createStore();
  for (const option of coreOptions) store.options.register(option);
  prepare?.(store);
  await store.open(reply);
  const scheduler = createScheduler({ store, client: r.client, ...options });
  return { r, store, scheduler };
}

// Lets the time pass, running the passes and the replies that are due.
async function tick(ms = 0): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
}

function statusOf(scheduler: Scheduler, id: BlockId): BlockStatus {
  const status = scheduler.status(id);
  if (status === undefined) throw new Error(`Block "${id}" has no status.`);
  return status;
}

function revisionsOf(scheduler: Scheduler): Record<BlockId, number> {
  return Object.fromEntries(
    ["a", "b", "c", "d", "e", "f"].flatMap((id) => {
      const status = scheduler.status(id);
      return status === undefined ? [] : [[id, status.revision]];
    }),
  );
}

// The request of the block's output at its current revision.
function output(scheduler: Scheduler, block: BlockId): Omit<OutputRequest, "id"> {
  return {
    kind: "output",
    block,
    revision: statusOf(scheduler, block).revision,
    options: { n_rows: 50 },
    info,
    params: {},
  };
}

// The latest evaluation of a block that R has not replied to.
function pendingOf(r: PlayedR, id: BlockId): EvaluateRequest {
  const request = r.pending().filter((item) => item.block === id).at(-1);
  if (request === undefined) throw new Error(`No evaluation of block "${id}" waits on R.`);
  return request;
}

// Answers every evaluation that waits on R with ok, again and again, until
// none waits.
async function evaluateAll(r: PlayedR): Promise<void> {
  await tick();
  for (let i = 0; i < 20 && r.pending().length > 0; i++) {
    for (const request of r.pending()) r.evaluated(request);
    await tick();
  }
}

const failure: Condition = {
  severity: "error",
  class: ["simpleError", "error", "condition"],
  phase: "block_eval",
  message: "object 'Species' not found",
};

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("revisions", () => {
  test("come from one counter, upstream first, and every block of a board that opens reads unevaluated", async () => {
    const { scheduler } = await setup();
    const revisions = revisionsOf(scheduler);

    expect(new Set(Object.values(revisions)).size).toBe(6);
    expect(revisions.a).toBeLessThan(revisions.b ?? 0);
    expect(revisions.b).toBeLessThan(revisions.c ?? 0);
    expect(revisions.d).toBeLessThan(revisions.e ?? 0);
    for (const id of Object.keys(revisions)) {
      expect(statusOf(scheduler, id)).toMatchObject({
        status: "unevaluated",
        evaluated: null,
        running: false,
        conditions: [],
        summary: null,
      });
    }
  });

  test("go up for a block whose state changes and for everything downstream of it", async () => {
    const { store, scheduler } = await setup();
    const before = revisionsOf(scheduler);
    const top = Math.max(...Object.values(before));

    store.apply({ blocks: { mod: { b: { state: { n: 10 } } } } });
    const after = revisionsOf(scheduler);

    for (const id of ["a", "d", "f"]) expect(after[id]).toBe(before[id]);
    for (const id of ["b", "c", "e"]) expect(after[id]).toBeGreaterThan(top);
  });

  test("stay through a rename, a stack, a board option, the board's name, a plugin's state and a state set to what it holds", async () => {
    const { store, scheduler } = await setup(
      {},
      {
        reply: withPlugins({ zoom: { class: "test.pkg-zoom_plugin", name: "Zoom", state: { zoom: 1 } } }),
        prepare: (store) =>
          store.plugins.register({
            id: "test.pkg-zoom_plugin",
            state: { schema: { type: "object", properties: { zoom: { type: "number" } } }, default: { zoom: 1 } },
          }),
      },
    );
    const before = revisionsOf(scheduler);

    store.apply({
      blocks: {
        mod: {
          b: { name: "First rows", state: { n: 6 } },
        },
      },
    });
    store.apply({ stacks: { mod: { s1: { name: "Inputs" } } } });
    store.apply({ options: { set: { n_rows: 20 } } });
    store.apply({ name: "Renamed" });
    store.apply({ patches: [{ op: "replace", path: "/plugins/zoom/state/zoom", value: 2 }] });

    expect(revisionsOf(scheduler)).toEqual(before);
  });

  test("go up for the target of a link that changes, as for the order of a variadic block's links", async () => {
    const { r, store, scheduler } = await setup();
    let before = revisionsOf(scheduler);

    store.apply({ links: { rm: ["be"] } });
    expect(revisionsOf(scheduler).e).toBeGreaterThan(before.e ?? 0);
    expect(revisionsOf(scheduler).b).toBe(before.b);

    before = revisionsOf(scheduler);
    store.apply({ links: { add: { be: { from: "b", to: "e", input: "" } } } });
    expect(revisionsOf(scheduler).e).toBeGreaterThan(before.e ?? 0);

    scheduler.demand("test", ["e"]);
    scheduler.start();
    await evaluateAll(r);
    const e = r.evaluations().find((request) => request.block === "e");
    expect(e?.recipe.inputs.map((input) => input.block)).toEqual(["d", "b"]);
  });

  test("go up for a block that something invalidates and for everything downstream of it", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["e"]);
    scheduler.start();
    await evaluateAll(r);
    const before = revisionsOf(scheduler);
    const sent = r.evaluations().length;

    scheduler.invalidate("d");
    scheduler.invalidate("z");
    const after = revisionsOf(scheduler);

    for (const id of ["a", "b", "c", "f"]) expect(after[id]).toBe(before[id]);
    expect(statusOf(scheduler, "d").status).toBe("stale");
    expect(statusOf(scheduler, "e").status).toBe("stale");
    await tick();
    expect(r.evaluations().slice(sent).map((request) => request.block)).toEqual(["d", "e"]);
  });

  test("of a block that leaves the board go with its status, as R forgets it, and one that comes back reads unevaluated above any revision its id had", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.demand("test", ["f"]);
    scheduler.start();
    await evaluateAll(r);
    expect(statusOf(scheduler, "f").status).toBe("ready");
    const top = Math.max(...Object.values(revisionsOf(scheduler)));

    store.apply({ blocks: { rm: ["f"] } });
    expect(scheduler.status("f")).toBeUndefined();
    expect(r.forgets()).toEqual([["f"]]);

    store.undo();
    expect(statusOf(scheduler, "f")).toMatchObject({ status: "unevaluated", evaluated: null });
    expect(statusOf(scheduler, "f").revision).toBeGreaterThan(top);
    await tick();
    expect(pendingOf(r, "f").revision).toBe(statusOf(scheduler, "f").revision);
  });

  test("of the scheduler of a board that opens later go above any the page has given, from the page's count, with nothing for R to forget", async () => {
    const page = counter();
    const controller = new AbortController();
    const { r, store, scheduler } = await setup({ nextRevision: page, signal: controller.signal });
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const top = Math.max(...Object.values(revisionsOf(scheduler)));

    controller.abort();
    const next = createStore({ hello });
    for (const option of coreOptions) next.options.register(option);
    await next.open(store.save());
    const after = createScheduler({ store: next, client: r.client, nextRevision: page });

    expect(r.forgets()).toEqual([]);
    for (const [id, revision] of Object.entries(revisionsOf(after))) {
      expect(revision).toBeGreaterThan(top);
      expect(statusOf(after, id).status).toBe("unevaluated");
    }
  });

  test("start with the first board the store opens, where the scheduler is created before the store holds one", async () => {
    const r = playR();
    r.connect();
    const store = createStore();
    for (const option of coreOptions) store.options.register(option);
    const scheduler = createScheduler({ store, client: r.client });
    expect(revisionsOf(scheduler)).toEqual({});

    await store.open(hello);

    expect(Object.keys(revisionsOf(scheduler))).toEqual(["a", "b", "c", "d", "e", "f"]);
    for (const id of ["a", "b", "c", "d", "e", "f"]) {
      expect(statusOf(scheduler, id).status).toBe("unevaluated");
    }
  });
});

describe("statuses", () => {
  test("follow R's replies: ready for ok, failed for error and invalid for invalid", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["b", "d"]);
    scheduler.start();
    await tick();

    r.evaluated(pendingOf(r, "a"));
    r.evaluated(pendingOf(r, "b"), "error", { conditions: [failure], stderr: "Error!" });
    r.evaluated(pendingOf(r, "d"), "invalid", {
      conditions: [{ ...failure, phase: "expr", message: "The subset is not set." }],
    });
    await tick();

    expect(statusOf(scheduler, "a")).toMatchObject({
      status: "ready",
      evaluated: statusOf(scheduler, "a").revision,
      running: false,
      summary: "<data.frame> [150 × 5]",
    });
    expect(statusOf(scheduler, "b")).toMatchObject({
      status: "failed",
      conditions: [failure],
      stderr: "Error!",
      summary: null,
    });
    expect(statusOf(scheduler, "d").status).toBe("invalid");
    expect(statusOf(scheduler, "c").conditions[0]?.message).toBe(
      'This block is waiting for block "b".',
    );
    expect(statusOf(scheduler, "e").conditions[0]?.message).toBe(
      'This block is waiting for blocks "b" and "d".',
    );
  });

  test("read stale once the revision moves, keeping what the latest reply said", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await tick();
    r.evaluated(pendingOf(r, "a"));
    r.evaluated(pendingOf(r, "b"), "error", { conditions: [failure] });
    await tick();
    const failed = statusOf(scheduler, "b");

    store.apply({ blocks: { mod: { b: { state: { n: 7 } } } } });

    expect(statusOf(scheduler, "b")).toMatchObject({
      status: "stale",
      evaluated: failed.revision,
      conditions: [failure],
    });
    expect(statusOf(scheduler, "b").revision).toBeGreaterThan(failed.revision);
  });

  test("read waiting for an unlinked input, a variadic block without a link and a block upstream that cannot run, with one condition of the phase status", async () => {
    const { store, scheduler } = await setup();

    store.apply({ links: { rm: ["ab", "be", "de"] } });

    expect(statusOf(scheduler, "b")).toEqual({
      status: "waiting",
      revision: statusOf(scheduler, "b").revision,
      evaluated: null,
      running: false,
      conditions: [
        {
          severity: "message",
          class: ["blockr_message", "message", "condition"],
          phase: "status",
          message: 'This block is waiting for its input "data" to be linked.',
        },
      ],
      omitted: { warning: 0, message: 0 },
      stdout: "",
      stderr: "",
      summary: null,
    });
    expect(statusOf(scheduler, "e").conditions[0]?.message).toBe(
      "This block is waiting for a block to be linked to it.",
    );
    expect(statusOf(scheduler, "c").conditions[0]?.message).toBe(
      'This block is waiting for block "b".',
    );
    expect(statusOf(scheduler, "d").status).toBe("unevaluated");
  });

  test("count an input that R binds itself as linked, naming it in the recipe alone", async () => {
    const { r, store, scheduler } = await setup({ bound: ["data"] }, { reply: helloBlock });
    const [id = ""] = Object.keys(store.board().blocks);
    expect(statusOf(scheduler, id).status).toBe("unevaluated");

    scheduler.demand("blockr.core", [id]);
    scheduler.start();
    await tick();
    expect(pendingOf(r, id).recipe.inputs).toEqual([{ input: "data" }]);

    const unbound = await setup({}, { reply: helloBlock });
    const [other = ""] = Object.keys(unbound.store.board().blocks);
    expect(statusOf(unbound.scheduler, other).status).toBe("waiting");
  });

  test("drop a reply for a superseded revision, cancelling its evaluation, and a reply for a block that reads waiting by then", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.demand("test", ["c"]);
    scheduler.start();
    await tick();
    const oldB = pendingOf(r, "b");
    const oldC = pendingOf(r, "c");

    store.apply({ blocks: { mod: { b: { state: { n: 8 } } } } });
    await tick();
    expect(r.cancels()).toEqual([oldB.id, oldC.id]);

    r.evaluated(pendingOf(r, "a"));
    r.evaluated(oldB);
    r.evaluated(oldC);
    await tick();
    expect(statusOf(scheduler, "b").status).toBe("unevaluated");

    r.evaluated(pendingOf(r, "b"), "error", { conditions: [failure] });
    await tick();
    expect(statusOf(scheduler, "c").status).toBe("waiting");
    const newC = r.pending().find((request) => request.block === "c");
    if (newC !== undefined) r.evaluated(newC);
    await tick();
    expect(statusOf(scheduler, "c").status).toBe("waiting");
  });

  test("count a cancelled or a missing reply as none, and invalidate a block whose current result R says it lacks", async () => {
    const { r, scheduler } = await setup({ evaluationQueue: 0 });
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await tick();
    const a = pendingOf(r, "a");
    r.reply({ id: a.id, status: "cancelled" });
    await tick();
    expect(statusOf(scheduler, "a").status).toBe("unevaluated");

    r.evaluated(pendingOf(r, "a"));
    await tick();
    const ready = statusOf(scheduler, "a");
    const b = pendingOf(r, "b");
    r.reply({ id: b.id, status: "missing", missing: [{ block: "a", revision: ready.revision }] });
    await tick();
    expect(statusOf(scheduler, "a")).toMatchObject({ status: "stale", evaluated: ready.revision });
    expect(statusOf(scheduler, "a").revision).toBeGreaterThan(ready.revision);
    expect(pendingOf(r, "a").revision).toBe(statusOf(scheduler, "a").revision);

    await evaluateAll(r);
    expect(statusOf(scheduler, "b").status).toBe("ready");
  });

  test("keep handing out one object until it changes, while subscribe() names the blocks whose status changed", async () => {
    const { store, scheduler } = await setup();
    const seen: (readonly BlockId[])[] = [];
    const unsubscribe = scheduler.subscribe((changed) => seen.push(changed));
    const a = statusOf(scheduler, "a");
    const b = statusOf(scheduler, "b");

    store.apply({ name: "Renamed" });
    expect(seen).toEqual([]);
    expect(scheduler.status("a")).toBe(a);

    store.apply({ blocks: { mod: { b: { state: { n: 9 } } } } });
    expect(seen).toEqual([["b", "c", "e"]]);
    expect(scheduler.status("a")).toBe(a);
    expect(scheduler.status("b")).not.toBe(b);

    unsubscribe();
    store.apply({ blocks: { mod: { b: { state: { n: 10 } } } } });
    expect(seen).toHaveLength(1);
  });
});

describe("demand and scheduling", () => {
  test("send nothing until start(), then what is demanded and everything upstream of it, in topological order, and nothing else", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["c"]);
    await tick(60_000);
    expect(r.evaluations()).toEqual([]);

    scheduler.start();
    await tick();
    expect(r.evaluations().map((request) => request.block)).toEqual(["a", "b", "c"]);
    expect(pendingOf(r, "c")).toMatchObject({
      kind: "evaluate",
      block: "c",
      revision: statusOf(scheduler, "c").revision,
      recipe: {
        registry_id: "blockr.core-merge_block",
        state: { by: ["Species"], all_x: false, all_y: false },
        inputs: [
          { input: "x", block: "a", revision: statusOf(scheduler, "a").revision },
          { input: "y", block: "b", revision: statusOf(scheduler, "b").revision },
        ],
      },
    });
    expect(statusOf(scheduler, "c").running).toBe(true);

    await evaluateAll(r);
    expect(r.evaluations()).toHaveLength(3);
    expect(statusOf(scheduler, "d").status).toBe("unevaluated");
  });

  test("keep at most evaluation_queue evaluations waiting in R behind the one it runs", async () => {
    const one = await setup({ evaluationQueue: 0 });
    one.scheduler.demand("test", ["e"]);
    one.scheduler.start();
    await tick();
    expect(one.r.pending().map((request) => request.block)).toEqual(["a"]);
    one.r.evaluated(pendingOf(one.r, "a"));
    await tick();
    expect(one.r.pending().map((request) => request.block)).toEqual(["b"]);

    const three = await setup({ gateVisibility: false });
    three.scheduler.start();
    await tick();
    expect(three.r.pending().map((request) => request.block)).toEqual(["a", "b", "d"]);
    three.r.evaluated(pendingOf(three.r, "a"));
    await tick();
    expect(three.r.pending().map((request) => request.block)).toEqual(["b", "d", "f"]);

    const all = await setup({ evaluationQueue: Infinity, gateVisibility: false });
    all.scheduler.start();
    await tick();
    expect(all.r.pending().map((request) => request.block)).toEqual(["a", "b", "d", "f", "c", "e"]);
  });

  test("send the highest evaluation priority first, each block at the highest of anything that needs it", async () => {
    const { r, scheduler } = await setup({ evaluationQueue: 0 });
    scheduler.demand("blockr.io", ["d"]);
    scheduler.demand("blockr.dock", ["c"], { priority: 1 });
    scheduler.start();

    const order: BlockId[] = [];
    for (let i = 0; i < 6; i++) {
      await tick();
      const [request] = r.pending();
      if (request === undefined) break;
      order.push(request.block);
      r.evaluated(request);
    }

    expect(order).toEqual(["a", "b", "c", "d"]);
  });

  test("call a function of the board again after every change, and one that throws demands nothing until it returns, naming its owner", async () => {
    const { r, store, scheduler } = await setup();
    const seen: string[] = [];
    scheduler.demand("blockr.io", (board) => {
      seen.push(board.name);
      if (board.name === "Broken") throw new Error("No such state.");
      return Object.keys(board.blocks).filter((id) => board.blocks[id]?.name === "Scatter");
    });
    scheduler.start();
    await evaluateAll(r);
    expect(r.evaluations().map((request) => request.block)).toEqual(["a", "f"]);

    const reported = reports();
    store.apply({ name: "Broken" });
    store.apply({ blocks: { mod: { f: { state: { x: "Petal.Length" } } } } });
    await tick(1);
    reported.restore();
    expect(reported.errors().map(String)).toEqual([
      "Error: The demand of blockr.io threw, so it demands nothing until it returns: No such state.",
    ]);
    expect(r.pending()).toEqual([]);

    store.apply({ name: "Fixed" });
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["f"]);
    expect(seen).toEqual(["Iris review", "Broken", "Fixed"]);
  });

  test("let a demand be replaced and released, cancelling what R has not started and nothing needs", async () => {
    const { r, scheduler } = await setup();
    const shown = scheduler.demand("blockr.dock", ["c"], { priority: 1 });
    scheduler.start();
    await tick();
    const [a, b, c] = r.pending();

    shown.set(["d"]);
    await tick();
    expect(r.cancels()).toEqual([b?.id, c?.id]);
    expect(r.pending().map((request) => request.block)).toEqual(["a", "b", "c"]);

    if (a !== undefined) r.evaluated(a);
    r.reply({ id: b?.id ?? "", status: "cancelled" });
    r.reply({ id: c?.id ?? "", status: "cancelled" });
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["d"]);

    shown.release();
    expect(() => {
      shown.set(["a"]);
    }).toThrow(/was released/);
  });

  test("demand every block with the blockr option gate_visibility set to FALSE", async () => {
    const { r, scheduler } = await setup({ gateVisibility: false });
    scheduler.start();
    await evaluateAll(r);
    expect(r.evaluations().map((request) => request.block).sort()).toEqual(["a", "b", "c", "d", "e", "f"]);
  });

  test("hold evaluations from hold() until start() again, as while the page switches front-ends", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["a"]);
    scheduler.start();
    await evaluateAll(r);

    scheduler.hold();
    scheduler.invalidate("a");
    await tick(60_000);
    expect(r.pending()).toEqual([]);

    scheduler.start();
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["a"]);
  });

  test("fail a block whose evaluation the transport could not send, naming why, until its revision moves", async () => {
    const { r, store, scheduler } = await setup();
    r.refuse.add("evaluate");
    scheduler.demand("test", ["a"]);
    scheduler.start();
    await tick();
    // The refusal comes back during that tick. Fake timers put a timer that is
    // set during a tick 1 ms later.
    await tick(1);

    expect(statusOf(scheduler, "a")).toMatchObject({
      status: "failed",
      conditions: [
        {
          severity: "error",
          class: ["blockr_error", "error", "condition"],
          message: "The evaluation could not be sent to R: The transport is closed.",
        },
      ],
    });
    expect(statusOf(scheduler, "b").status).toBe("waiting");

    r.refuse.clear();
    store.apply({ blocks: { mod: { a: { state: { dataset: "mtcars" } } } } });
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["a"]);
  });

  test("refuse an evaluation priority that is no number, and take the default of a blockr option that holds none", async () => {
    const reported = reports();
    const { scheduler } = await setup({ evaluationQueue: -1, backgroundEvaluationDelay: Number.NaN });
    reported.restore();
    expect(reported.errors().map(String)).toEqual([
      "Error: The blockr option evaluation_queue is -1, where it takes a number of 0 or more, so the scheduler takes 2.",
      "Error: The blockr option background_evaluation_delay is NaN, where it takes a number of 0 or more, so the scheduler takes Infinity.",
    ]);

    expect(() => scheduler.demand("test", ["a"], { priority: Number.NaN })).toThrow(
      "An evaluation priority is a number, not NaN.",
    );
    await expect(scheduler.evaluate(["a"], undefined, { priority: Number.NaN })).rejects.toThrow(
      "An evaluation priority is a number, not NaN.",
    );
  });

  test("work out what to send once every subscriber of the store has heard of a change", async () => {
    const { r, store, scheduler } = await setup();
    const shown = scheduler.demand("blockr.dock", ["d"]);
    store.subscribe(() => {
      shown.set(["c"]);
    });
    scheduler.start();

    store.apply({ name: "Renamed" });
    await tick();
    expect(r.evaluations().map((request) => request.block)).toEqual(["a", "b", "c"]);
  });
});

describe("one-time evaluations", () => {
  test("bring the blocks and their upstream up to date, without keeping them so, and resolve with their statuses by id", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.start();
    const evaluating = scheduler.evaluate(["c"]);
    await evaluateAll(r);

    const statuses = await evaluating;
    expect(Object.keys(statuses)).toEqual(["c"]);
    expect(statuses.c).toBe(scheduler.status("c"));
    expect(statuses.c?.status).toBe("ready");

    store.apply({ blocks: { mod: { b: { state: { n: 3 } } } } });
    await tick(1);
    expect(statusOf(scheduler, "c").status).toBe("stale");
    expect(r.pending()).toEqual([]);
  });

  test("resolve at whatever revision a block has by then, so that an edit made meanwhile delays them", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.start();
    let resolved: Record<BlockId, BlockStatus> | undefined;
    void scheduler.evaluate(["b"]).then((statuses) => (resolved = statuses));
    await tick();
    const first = pendingOf(r, "b");

    store.apply({ blocks: { mod: { b: { state: { n: 4 } } } } });
    await tick();
    r.evaluated(pendingOf(r, "a"));
    r.evaluated(first);
    await tick();
    expect(resolved).toBeUndefined();

    r.evaluated(pendingOf(r, "b"));
    await tick();
    expect(resolved?.b?.revision).toBe(statusOf(scheduler, "b").revision);
    expect(resolved?.b?.revision).toBeGreaterThan(first.revision);
  });

  test("resolve at once for blocks that are current or cannot run, and leave out a block the board lacks", async () => {
    const { store, scheduler } = await setup();
    store.apply({ links: { rm: ["ab"] } });
    await expect(scheduler.evaluate(["b", "z"])).resolves.toEqual({ b: scheduler.status("b") });
    await expect(scheduler.evaluate(["z"])).rejects.toThrow(/holds none of the blocks "z"/);
  });

  test("leave out a block that leaves the board, rejecting once left with none, and reject when the scheduler ends or the signal aborts", async () => {
    const ending = new AbortController();
    const { r, store, scheduler } = await setup({ signal: ending.signal });
    scheduler.start();
    const both = scheduler.evaluate(["d", "f"]);
    const alone = scheduler.evaluate(["f"]);
    store.apply({ blocks: { rm: ["f"] } });
    await expect(alone).rejects.toThrow(/left the board/);
    await evaluateAll(r);
    expect(Object.keys(await both)).toEqual(["d"]);

    const controller = new AbortController();
    const aborted = scheduler.evaluate(["c"], undefined, { signal: controller.signal });
    controller.abort(new Error("The step was given up."));
    await expect(aborted).rejects.toThrow("The step was given up.");

    const waiting = scheduler.evaluate(["e"]);
    ending.abort();
    await expect(waiting).rejects.toThrow("Another board opened.");
    await expect(scheduler.evaluate(["e"])).rejects.toThrow("Another board opened.");
  });
});

describe("the end", () => {
  test("comes as its signal aborts: the scheduler sends R nothing more, rejects every request, tells its subscribers nothing more, and takes no reply", async () => {
    const controller = new AbortController();
    const { r, store, scheduler } = await setup({ signal: controller.signal });
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await tick();
    const sent = r.evaluations().length;
    expect(sent).toBeGreaterThan(0);
    const heard: (readonly BlockId[])[] = [];
    scheduler.subscribe((changed) => heard.push(changed));

    controller.abort();
    for (const request of r.pending()) r.evaluated(request);
    store.apply({ blocks: { mod: { b: { state: { n: 3 } } } } });
    scheduler.demand("test", ["e"]);
    await tick(1000);

    expect(r.evaluations()).toHaveLength(sent);
    expect(heard).toEqual([]);
    expect(statusOf(scheduler, "a").status).toBe("unevaluated");
    await expect(
      scheduler.request({ kind: "request", type: "option", option: "data_dir", name: "list_dir", args: {} }),
    ).rejects.toThrow("Another board opened.");
  });

  test("sends no request that was waiting on R again once the session ends after it", async () => {
    const controller = new AbortController();
    const { r, scheduler } = await setup({ signal: controller.signal });
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const asking = scheduler.request(output(scheduler, "b"));
    await tick();
    expect(r.asked("output")).toHaveLength(1);

    controller.abort();
    r.end();
    await expect(asking).rejects.toBeInstanceOf(SessionEndedError);
    r.connect();
    await tick();
    expect(r.asked("output")).toHaveLength(1);
  });
});

describe("one-time evaluations with parameters", () => {
  test("evaluate each block once with the parameters, current or not, and resolve with the statuses R's replies give, which become the blocks' own", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const ready = statusOf(scheduler, "b");

    const saving = scheduler.evaluate(["b"], { save: true });
    await tick();
    const request = pendingOf(r, "b");
    expect(request).toMatchObject({ block: "b", revision: ready.revision, params: { save: true } });

    r.evaluated(request, "ok", { stdout: "Wrote iris.csv" });
    const statuses = await saving;
    expect(statuses.b).toMatchObject({ status: "ready", revision: ready.revision, stdout: "Wrote iris.csv" });
    expect(statusOf(scheduler, "b").stdout).toBe("Wrote iris.csv");
  });

  test("send no other evaluation of that revision, never cancel it and keep what it reads evaluated until R replies", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    const saving = scheduler.evaluate(["b"], { save: true });
    await tick();
    expect(r.pending().map((request) => [request.block, request.params])).toEqual([
      ["a", undefined],
      ["b", { save: true }],
    ]);
    const save = pendingOf(r, "b");

    store.apply({ blocks: { mod: { b: { state: { n: 2 } } } } });
    await tick();
    expect(r.cancels()).toEqual([]);
    expect(r.pending().map((request) => [request.block, request.params])).toEqual([
      ["a", undefined],
      ["b", { save: true }],
      ["b", undefined],
    ]);

    r.evaluated(pendingOf(r, "a"));
    r.evaluated(save);
    const statuses = await saving;
    expect(statuses.b).toMatchObject({ status: "ready", revision: save.revision });
    expect(statusOf(scheduler, "b").status).toBe("unevaluated");
  });

  test("wait while what it reads recomputes, following an edit made meanwhile, and reject where its block leaves the board before it is sent or a new session starts before R replies", async () => {
    const { r, store, scheduler } = await setup({ evaluationQueue: 0 });
    scheduler.start();
    const saving = scheduler.evaluate(["b"], { save: true });
    await tick();
    const old = pendingOf(r, "a");

    store.apply({ blocks: { mod: { a: { state: { dataset: "mtcars" } } } } });
    await tick();
    r.reply({ id: old.id, status: "cancelled" });
    await tick();
    r.evaluated(pendingOf(r, "a"));
    await tick();
    const save = pendingOf(r, "b");
    expect(save).toMatchObject({ revision: statusOf(scheduler, "b").revision, params: { save: true } });
    expect(save.recipe.inputs).toEqual([
      { input: "data", block: "a", revision: statusOf(scheduler, "a").revision },
    ]);
    r.evaluated(save);
    await expect(saving).resolves.toMatchObject({ b: { status: "ready", revision: save.revision } });

    const leaving = scheduler.evaluate(["f"], { save: true });
    store.apply({ blocks: { rm: ["f"] } });
    await expect(leaving).rejects.toThrow(/Block "f" left the board before/);

    const ending = scheduler.evaluate(["d"], { save: true });
    await tick();
    expect(pendingOf(r, "d").params).toEqual({ save: true });
    r.end();
    await expect(ending).rejects.toBeInstanceOf(SessionEndedError);
  });

  test("reject where its block cannot run, at once or once a block it reads fails while it waits, sending nothing for it", async () => {
    const { r, store, scheduler } = await setup({ evaluationQueue: 0 });
    scheduler.start();
    const failing = expect(scheduler.evaluate(["b"], { save: true })).rejects.toThrow(
      'Block "b" is waiting for block "a". Its evaluation with parameters is not sent.',
    );
    await tick();
    r.evaluated(pendingOf(r, "a"), "error", { conditions: [failure] });
    await tick();
    await failing;

    store.apply({ links: { rm: ["ad"] } });
    await expect(scheduler.evaluate(["d"], { save: true })).rejects.toThrow(
      'Block "d" is waiting for its input "data" to be linked. Its evaluation with parameters is not sent.',
    );
    await tick(1);
    expect(r.evaluations().map((request) => request.block)).toEqual(["a"]);
  });
});

describe("evaluation in the background", () => {
  test("evaluates nothing unless the blockr option background_evaluation_delay is set", async () => {
    const { r, scheduler } = await setup();
    scheduler.start();
    await tick(3_600_000);
    expect(r.evaluations()).toEqual([]);
  });

  test("evaluates the stale and unevaluated blocks that nothing demands, one at a time, once R has had nothing else to run for the delay", async () => {
    const { r, scheduler } = await setup({ backgroundEvaluationDelay: 2 });
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    await tick(1999);
    expect(r.evaluations().map((request) => request.block)).toEqual(["a", "b"]);

    await tick(1);
    expect(r.pending().map((request) => request.block)).toEqual(["d"]);
    r.evaluated(pendingOf(r, "d"));
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["f"]);
  });

  test("stops once anything else arrives, and waits the delay again", async () => {
    const { r, scheduler } = await setup({ backgroundEvaluationDelay: 1 });
    scheduler.start();
    await tick(1000);
    expect(r.pending().map((request) => request.block)).toEqual(["a"]);

    scheduler.demand("test", ["d"]);
    await tick();
    r.evaluated(pendingOf(r, "a"));
    await tick();
    expect(r.pending().map((request) => request.block)).toEqual(["d"]);
    r.evaluated(pendingOf(r, "d"));
    await tick(999);
    expect(r.pending()).toEqual([]);
    await tick(1);
    expect(r.pending().map((request) => request.block)).toEqual(["b"]);
  });

  test("gives way, where R names interrupt, once anything else has waited 5 s behind it, and goes again once R replies cancelled", async () => {
    const { r, scheduler } = await setup(
      { backgroundEvaluationDelay: 1 },
      { capabilities: ["cancel", "interrupt", "progress"] },
    );
    scheduler.start();
    await tick(1000);
    const a = pendingOf(r, "a");

    const listing = scheduler.request({ kind: "request", type: "option", option: "data_dir", name: "list_dir", args: {} });
    await tick(4999);
    expect(r.cancels()).toEqual([]);
    await tick(1);
    expect(r.cancels()).toEqual([a.id]);

    r.reply({ id: a.id, status: "cancelled" });
    const [asked] = r.asked("request");
    r.reply({ id: asked?.id ?? "", status: "ok", value: ["data"] });
    await expect(listing).resolves.toMatchObject({ reply: { status: "ok" } });
    await tick(999);
    expect(r.pending()).toEqual([]);
    await tick(1);
    expect(r.pending().map((request) => request.block)).toEqual(["a"]);
  });

  test("never gives way where R names no interrupt", async () => {
    const { r, scheduler } = await setup({ backgroundEvaluationDelay: 1 });
    scheduler.start();
    await tick(1000);
    void scheduler.request({ kind: "request", type: "option", option: "data_dir", name: "list_dir", args: {} });
    await tick(60_000);
    expect(r.cancels()).toEqual([]);
  });
});

describe("a new session", () => {
  test("gives every block a new revision, so that every block reads stale, and evaluates again what is demanded", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const before = revisionsOf(scheduler);
    const top = Math.max(...Object.values(before));
    const ready = statusOf(scheduler, "b");

    r.end();
    r.connect();
    scheduler.session();

    for (const id of Object.keys(before)) {
      expect(statusOf(scheduler, id).revision).toBeGreaterThan(top);
    }
    expect(statusOf(scheduler, "b")).toMatchObject({
      status: "stale",
      evaluated: ready.revision,
      summary: ready.summary,
    });
    expect(statusOf(scheduler, "d").status).toBe("unevaluated");
    await tick();
    expect(r.pending().map((request) => [request.block, request.revision])).toEqual([
      ["a", statusOf(scheduler, "a").revision],
      ["b", statusOf(scheduler, "b").revision],
    ]);
  });

  test("evaluates again what was unanswered when the session ended, and what a one-time evaluation still waits for", async () => {
    const { r, scheduler } = await setup();
    scheduler.start();
    const evaluating = scheduler.evaluate(["b"]);
    await tick();
    expect(r.pending()).toHaveLength(2);

    r.end();
    await tick();
    r.connect();
    scheduler.session();
    await evaluateAll(r);
    await expect(evaluating).resolves.toMatchObject({ b: { status: "ready" } });
  });
});

describe("requests", () => {

  test("go to R as they are and resolve with R's reply", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);

    const asking = scheduler.request(output(scheduler, "b"));
    const [sent] = r.asked("output");
    expect(sent).toMatchObject({ kind: "output", block: "b", revision: statusOf(scheduler, "b").revision });
    r.reply({
      id: sent?.id ?? "",
      status: "ok",
      output: { renderer: { id: "blockr.core-table_output", module: "blockr.core/js/basic-table.js" } },
    });
    await expect(asking).resolves.toMatchObject({ reply: { status: "ok" } });
  });

  test("hand a missing reply back to the caller, invalidating a block whose current result R says it lacks", async () => {
    const { r, store, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const b = statusOf(scheduler, "b");

    const asking = scheduler.request(output(scheduler, "b"));
    const [first] = r.asked("output");
    r.reply({ id: first?.id ?? "", status: "missing", missing: [{ block: "b", revision: b.revision }] });
    await expect(asking).resolves.toMatchObject({ reply: { status: "missing" } });
    expect(statusOf(scheduler, "b")).toMatchObject({ status: "stale", evaluated: b.revision });
    await tick();
    expect(pendingOf(r, "b").revision).toBe(statusOf(scheduler, "b").revision);

    await evaluateAll(r);
    const current = statusOf(scheduler, "b");
    const late = scheduler.request(output(scheduler, "b"));
    store.apply({ blocks: { mod: { b: { state: { n: 1 } } } } });
    const edited = statusOf(scheduler, "b").revision;
    const [, second] = r.asked("output");
    r.reply({ id: second?.id ?? "", status: "missing", missing: [{ block: "b", revision: current.revision }] });
    await expect(late).resolves.toMatchObject({ reply: { status: "missing" } });
    expect(statusOf(scheduler, "b").revision).toBe(edited);
  });

  test("go again to the next session for an output, an output class's and a block class's request, and reject for an action, a board option's and one of a type that a package adds", async () => {
    const { r, scheduler } = await setup();
    scheduler.demand("test", ["b"]);
    scheduler.start();
    await evaluateAll(r);
    const at = statusOf(scheduler, "b").revision;

    const again = [
      scheduler.request(output(scheduler, "b")),
      scheduler.request({
        kind: "request",
        type: "output_class",
        output: { block: "b", revision: at },
        name: "download",
        args: {},
        options: {},
        params: {},
      }),
      scheduler.request({
        kind: "request",
        type: "block_class",
        registry_id: "blockr.core-head_block",
        block: "b",
        name: "n_max",
        args: {},
        reads: [{ input: "data", block: "a", revision: statusOf(scheduler, "a").revision }],
      }),
    ];
    const rejected = [
      scheduler.request({
        kind: "request",
        type: "block_class",
        registry_id: "blockr.core-upload_block",
        block: "b",
        name: "upload",
        args: {},
        reads: [],
      }),
      scheduler.request({ kind: "request", type: "option", option: "data_dir", name: "list_dir", args: {} }),
      scheduler.request({ kind: "request", type: "code", name: "script", args: {}, reads: [] }),
    ];
    // The ids of the outputs and requests sent, in order.
    const asked = () =>
      r.sent.flatMap((item) => (item.kind === "output" || item.kind === "request" ? [item.id] : []));
    const before = asked().length;

    r.end();
    for (const asking of rejected) {
      await expect(asking).rejects.toBeInstanceOf(SessionEndedError);
    }
    r.connect();
    scheduler.session();
    await tick();
    const resent = asked().slice(before);
    expect(resent).toHaveLength(3);
    for (const id of resent) r.reply({ id, status: "ok", value: [1] });
    for (const asking of again) {
      await expect(asking).resolves.toMatchObject({ reply: { status: "ok" } });
    }
  });
});
