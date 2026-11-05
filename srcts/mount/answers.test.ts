// @vitest-environment node

// What core keeps of the answers of block classes' requests, with a scheduler
// whose revisions the tests set and whose requests they answer.

import { expect, test } from "vitest";
import type { Reply } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
import { createAnswers } from "./answers.ts";
import type { ClassRequest } from "./answers.ts";

function fakeScheduler() {
  const revisions = new Map<BlockId, number>([["a", 1]]);
  const subscribers = new Set<() => void>();
  const waiting: { request: ClassRequest; resolve: (reply: Reply) => void }[] = [];
  return {
    waiting,
    // Moves a block to a revision, as the scheduler announces.
    revise(id: BlockId, revision: number): void {
      revisions.set(id, revision);
      for (const fn of subscribers) fn();
    },
    status: (id: BlockId) =>
      revisions.has(id) ? ({ revision: revisions.get(id) } as unknown as BlockStatus) : undefined,
    subscribe(fn: (changed: readonly BlockId[]) => void) {
      const heard = (): void => {
        fn([]);
      };
      subscribers.add(heard);
      return () => {
        subscribers.delete(heard);
      };
    },
    request(request: ClassRequest) {
      return new Promise<{ reply: Reply; body: Blob | null }>((resolve) => {
        waiting.push({
          request,
          resolve: (reply) => {
            resolve({ reply, body: null });
          },
        });
      });
    },
    // Answers the request that waits longest with ok.
    answer(value: unknown = [1]): void {
      const next = waiting.shift();
      if (next === undefined) throw new Error("No request waits.");
      next.resolve({ id: "1", status: "ok", value } as Reply);
    },
  };
}

function ask(block: BlockId, args: Record<string, number> = {}, revision?: number): ClassRequest {
  return {
    kind: "request",
    type: "block_class",
    registry_id: "blockr.core-head_block",
    block,
    name: "n_max",
    args,
    reads: revision === undefined ? [] : [{ input: "data", block: "a", revision }],
  };
}

test("drops an answer once the revisions it read are superseded", async () => {
  const scheduler = fakeScheduler();
  const answers = createAnswers(scheduler);

  for (const limit of [1, 2]) {
    const asked = answers.ask({ request: ask("b", { limit }, 1), share: "keep" });
    scheduler.answer([limit]);
    await asked;
  }
  expect(answers.size()).toBe(2);

  scheduler.revise("a", 2);
  expect(answers.size()).toBe(0);

  const next = answers.ask({ request: ask("b", { limit: 2 }, 2), share: "keep" });
  scheduler.answer([20]);
  await next;
  await expect(answers.ask({ request: ask("g", { limit: 2 }, 2), share: "keep" })).resolves.toMatchObject({
    reply: { value: [20] },
  });
  expect(answers.size()).toBe(1);
});

test("keeps no answer that its reads superseded while it was on its way", async () => {
  const scheduler = fakeScheduler();
  const answers = createAnswers(scheduler);

  const asked = answers.ask({ request: ask("b", {}, 1), share: "keep" });
  scheduler.revise("a", 2);
  scheduler.answer();
  await asked;

  expect(answers.size()).toBe(0);
});

test("keeps the answers of every args that the revisions they read leave current, shared by the blocks of the class", async () => {
  const scheduler = fakeScheduler();
  const answers = createAnswers(scheduler);

  for (const args of [{ limit: 1, offset: 0 }, { offset: 0, limit: 2 }]) {
    const asked = answers.ask({ request: ask("b", args, 1), share: "keep" });
    scheduler.answer();
    await asked;
  }
  expect(answers.size()).toBe(2);

  // Another block of the class, and args in another order, are the same ask.
  await answers.ask({ request: ask("g", { offset: 0, limit: 1 }, 1), share: "keep" });
  expect(scheduler.waiting).toHaveLength(0);

  // An answer that reads nothing stays whatever revisions move.
  const nothing = answers.ask({ request: ask("b", { limit: 3 }), share: "keep" });
  scheduler.answer();
  await nothing;
  scheduler.revise("a", 2);
  expect(answers.size()).toBe(1);
});

test("keeps nothing of a request that is only merged, and neither keeps nor merges one asked once", async () => {
  const scheduler = fakeScheduler();
  const answers = createAnswers(scheduler);

  const merged = [
    answers.ask({ request: ask("b"), share: "merge" }),
    answers.ask({ request: ask("b"), share: "merge" }),
  ];
  expect(scheduler.waiting).toHaveLength(1);
  scheduler.answer();
  await Promise.all(merged);
  expect(answers.size()).toBe(0);

  const once = [
    answers.ask({ request: ask("b"), share: "once" }),
    answers.ask({ request: ask("b"), share: "once" }),
  ];
  expect(scheduler.waiting).toHaveLength(2);
  scheduler.answer();
  scheduler.answer();
  await Promise.all(once);
  expect(answers.size()).toBe(0);
});

test("keeps no answer that R refused", async () => {
  const scheduler = fakeScheduler();
  const answers = createAnswers(scheduler);

  const refused = answers.ask({ request: ask("b", {}, 1), share: "keep" });
  scheduler.waiting.shift()?.resolve({ id: "1", status: "error", conditions: [], omitted: { warning: 0, message: 0 } });
  await expect(refused).resolves.toMatchObject({ reply: { status: "error" } });
  expect(answers.size()).toBe(0);
});
