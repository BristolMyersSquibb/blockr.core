// A request of a type that a package adds, as CoreApi.request() sends it,
// with a scheduler whose evaluations and replies the test gives.

import { describe, expect, test } from "vitest";
import type { Reply } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import { READY } from "../stubs/index.ts";
import type { BlockId } from "../types/board.ts";
import { RequestError } from "../mount/index.ts";
import { addedRequest } from "./index.ts";

type Sent = Parameters<Scheduler["request"]>[0];

// A scheduler that holds the blocks a and b at the statuses given, has them
// evaluated as the test says, and replies to each request in turn with the
// replies given.
function scheduler(replies: Reply[], statuses: Record<BlockId, BlockStatus> = { a: READY, b: { ...READY, revision: 3, evaluated: 3 } }) {
  const sent: Sent[] = [];
  const evaluated: BlockId[][] = [];
  const fake: Pick<Scheduler, "evaluate" | "request" | "status"> = {
    status: (id) => statuses[id],
    evaluate(blocks) {
      const ids = [...blocks];
      evaluated.push(ids);
      return Promise.resolve(Object.fromEntries(ids.flatMap((id) => (statuses[id] === undefined ? [] : [[id, statuses[id]]]))));
    },
    request(request) {
      sent.push(request);
      const reply = replies.shift();
      if (reply === undefined) return Promise.reject(new Error("The test gives no more replies."));
      return Promise.resolve({ reply, body: null });
    },
  };
  return { fake, sent, evaluated, statuses };
}

const ok = (value: unknown): Reply => ({ id: "1", status: "ok", value: value as never });

describe("a request of a type that a package adds", () => {
  test("has the blocks it reads evaluated first, and goes to R with their revisions and the package's fields", async () => {
    const s = scheduler([ok(["a <- 1"])]);
    await expect(addedRequest({ scheduler: s.fake }, "code", { name: "script", args: {} }, { reads: ["a", "b", "a"] })).resolves.toEqual(["a <- 1"]);

    expect(s.evaluated).toEqual([["a", "b"]]);
    expect(s.sent).toEqual([
      { kind: "request", type: "code", name: "script", args: {}, reads: [{ block: "a", revision: 1 }, { block: "b", revision: 3 }] },
    ]);
  });

  test("that reads nothing goes to R at once, without reads", async () => {
    const s = scheduler([ok(null)]);
    await addedRequest({ scheduler: s.fake }, "session", { action: "save" }, {});
    expect(s.evaluated).toEqual([]);
    expect(s.sent).toEqual([{ kind: "request", type: "session", action: "save" }]);
  });

  test("asks again where R replies missing, and gives up after a few attempts", async () => {
    const missing: Reply = { id: "1", status: "missing", missing: [{ block: "a", revision: 1 }] };
    const s = scheduler([missing, ok("done")]);
    await expect(addedRequest({ scheduler: s.fake }, "code", {}, { reads: ["a"] })).resolves.toBe("done");
    expect(s.evaluated).toHaveLength(2);

    const again = scheduler([missing, missing, missing]);
    await expect(addedRequest({ scheduler: again.fake }, "code", {}, { reads: ["a"] })).rejects.toMatchObject({ status: "missing" });
  });

  test("rejects with the statuses of the blocks it reads that are not ready, and does not go to R", async () => {
    const failed: BlockStatus = { ...READY, status: "failed" };
    const s = scheduler([], { a: READY, b: failed });
    const sent = addedRequest({ scheduler: s.fake }, "code", {}, { reads: ["a", "b"] });
    await expect(sent).rejects.toBeInstanceOf(RequestError);
    await expect(sent).rejects.toMatchObject({ blocks: [{ block: "b", status: failed }] });
    expect(s.sent).toEqual([]);
  });

  test("rejects with the reply's status and conditions", async () => {
    const condition = { severity: "error" as const, class: ["simpleError", "error", "condition"], phase: "request" as const, message: "no" };
    const s = scheduler([{ id: "1", status: "error", conditions: [condition], omitted: { warning: 0, message: 0 } }]);
    await expect(addedRequest({ scheduler: s.fake }, "code", {}, {})).rejects.toMatchObject({ status: "error", conditions: [condition] });
  });

  test.each([
    ["core's own type", "block_class", {}, {}, 'A request of type "block_class" is core\'s own'],
    ["a field of core's", "code", { reads: [] }, {}, 'cannot hold the fields "reads", which are core\'s'],
    ["no type", "", {}, {}, "A request names its type"],
    ["a block that the board lacks", "code", {}, { reads: ["z"] }, 'reads the block "z", which the board lacks'],
  ])("refuses %s", async (_, type, fields, opts, message) => {
    const s = scheduler([]);
    await expect(addedRequest({ scheduler: s.fake }, type, fields, opts)).rejects.toThrow(message);
    expect(s.sent).toEqual([]);
  });
});
