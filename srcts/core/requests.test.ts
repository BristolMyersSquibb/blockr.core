// A plugin's request, as CoreApi.request() sends it, with a board that holds
// the plugins the test gives, and a scheduler whose evaluations and replies the
// test gives.

import { describe, expect, test } from "vitest";
import type { Reply } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import { READY } from "../stubs/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import { RequestError } from "../mount/index.ts";
import { pluginRequest } from "./index.ts";

type Sent = Parameters<Scheduler["request"]>[0];
type Options = Parameters<Scheduler["request"]>[1];

// The board's plugins: blockr.code's, and a document of blockr.md.
const PLUGINS: Board["plugins"] = {
  code: { class: "blockr.code-code_plugin", name: "Code", state: {} },
  report: { class: "blockr.md-md_plugin", name: "Report", state: { content: "# Report" } },
};

// A board that holds the plugins above, and a scheduler that holds the blocks
// a and b at the statuses given, has them evaluated as the test says, and
// replies to each request in turn with the replies given.
function setup(replies: Reply[], statuses: Record<BlockId, BlockStatus> = { a: READY, b: { ...READY, revision: 3, evaluated: 3 } }) {
  const sent: Sent[] = [];
  const options: Options[] = [];
  const evaluated: BlockId[][] = [];
  const scheduler: Pick<Scheduler, "evaluate" | "request" | "status"> = {
    status: (id) => statuses[id],
    evaluate(blocks) {
      const ids = [...blocks];
      evaluated.push(ids);
      return Promise.resolve(Object.fromEntries(ids.flatMap((id) => (statuses[id] === undefined ? [] : [[id, statuses[id]]]))));
    },
    request(request, given) {
      sent.push(request);
      options.push(given);
      const reply = replies.shift();
      if (reply === undefined) return Promise.reject(new Error("The test gives no more replies."));
      return Promise.resolve({ reply, body: null });
    },
  };
  const store = { board: () => ({ plugins: PLUGINS }) as Board };
  return { deps: { store, scheduler }, sent, options, evaluated };
}

const ok = (value: unknown): Reply => ({ id: "1", status: "ok", value: value as never });

describe("a plugin's request", () => {
  test("has the blocks it reads evaluated first, and goes to R with their revisions and the plugin's class", async () => {
    const s = setup([ok(["a <- 1"])]);
    await expect(pluginRequest(s.deps, "code", "script", {}, { reads: ["a", "b", "a"] })).resolves.toEqual(["a <- 1"]);

    expect(s.evaluated).toEqual([["a", "b"]]);
    expect(s.sent).toEqual([
      {
        kind: "request",
        type: "plugin",
        plugin: "blockr.code-code_plugin",
        name: "script",
        args: {},
        reads: [{ block: "a", revision: 1 }, { block: "b", revision: 3 }],
      },
    ]);
  });

  test("that reads nothing goes to R at once, with its args and the file it sends", async () => {
    const s = setup([ok(null)]);
    const file = new File(["# Report"], "report.md", { type: "text/markdown" });
    const progress = (): void => undefined;
    await pluginRequest(s.deps, "report", "html", { content: "# Report" }, { file, progress });

    expect(s.evaluated).toEqual([]);
    expect(s.sent).toEqual([
      { kind: "request", type: "plugin", plugin: "blockr.md-md_plugin", name: "html", args: { content: "# Report" }, reads: [] },
    ]);
    expect(s.options[0]?.body).toBe(file);
    expect(s.options[0]?.progress).toBeTypeOf("function");
  });

  test("asks again where R replies missing, and gives up after a few attempts", async () => {
    const missing: Reply = { id: "1", status: "missing", missing: [{ block: "a", revision: 1 }] };
    const s = setup([missing, ok("done")]);
    await expect(pluginRequest(s.deps, "code", "script", {}, { reads: ["a"] })).resolves.toBe("done");
    expect(s.evaluated).toHaveLength(2);

    const again = setup([missing, missing, missing]);
    await expect(pluginRequest(again.deps, "code", "script", {}, { reads: ["a"] })).rejects.toMatchObject({ status: "missing" });
  });

  test("rejects with the statuses of the blocks it reads that are not ready, and does not go to R", async () => {
    const failed: BlockStatus = { ...READY, status: "failed" };
    const s = setup([], { a: READY, b: failed });
    const sent = pluginRequest(s.deps, "code", "script", {}, { reads: ["a", "b"] });
    await expect(sent).rejects.toBeInstanceOf(RequestError);
    await expect(sent).rejects.toMatchObject({ blocks: [{ block: "b", status: failed }] });
    expect(s.sent).toEqual([]);
  });

  test("rejects with the reply's status and conditions", async () => {
    const condition = { severity: "error" as const, class: ["simpleError", "error", "condition"], phase: "request" as const, message: "no" };
    const s = setup([{ id: "1", status: "error", conditions: [condition], omitted: { warning: 0, message: 0 } }]);
    const sent = pluginRequest(s.deps, "code", "script", {}, {});
    await expect(sent).rejects.toMatchObject({ status: "error", conditions: [condition] });
    await expect(sent).rejects.toThrow('R replied error to the request "script" of the plugin "code".');
  });

  test.each([
    ["no plugin", "", "script", {}, {}, "names the plugin by its id"],
    ["a plugin that the board lacks", "dag", "script", {}, {}, 'The board holds no plugin "dag"'],
    ["a plugin's class in place of its id", "blockr.code-code_plugin", "script", {}, {}, "The board holds no plugin"],
    ["no name", "code", "", {}, {}, 'A request of the plugin "code" is named by a non-empty string'],
    ["args that are no object", "code", "script", [], {}, "are an object"],
    ["a block that the board lacks", "code", "script", {}, { reads: ["z"] }, 'reads the block "z", which the board lacks'],
  ])("refuses %s", async (_, plugin, name, args, opts, message) => {
    const s = setup([]);
    await expect(pluginRequest(s.deps, plugin, name, args as never, opts)).rejects.toThrow(message);
    expect(s.sent).toEqual([]);
  });
});
