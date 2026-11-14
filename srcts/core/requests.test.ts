// A plugin's request, as CoreApi.request() sends it for one of the app's
// plugins that run, which the test gives, by the id of the plugin's class, with
// a scheduler whose evaluations and replies the test gives.

import { describe, expect, test } from "vitest";
import type { Condition, Reply } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import { READY } from "../stubs/index.ts";
import type { BlockId, PluginClassId } from "../types/board.ts";
import type { Plugin } from "../types/core-api.ts";
import type { Replied } from "../util/index.ts";
import { RequestError } from "../mount/index.ts";
import { pluginRequest } from "./index.ts";

type Sent = Parameters<Scheduler["request"]>[0];
type Options = Parameters<Scheduler["request"]>[1];

const CODE = "blockr.code-code_plugin";
const DOCUMENT = "blockr.md-md_plugin";

// The app's plugins that run: blockr.code's, and blockr.md's document, by the
// ids of their classes.
const APP_PLUGINS: Readonly<Record<PluginClassId, Plugin>> = {
  [CODE]: { name: "Code" },
  [DOCUMENT]: { name: "Document" },
};

// The plugins above, which run, and a scheduler that holds the blocks a and b
// at the statuses given, has them evaluated as the test says, and replies to
// each request in turn with the replies given.
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
  const plugins = { appPlugins: () => APP_PLUGINS };
  // What the request handed on of each of R's replies.
  const handed: Parameters<Replied>[] = [];
  const replied: Replied = (...args) => {
    handed.push(args);
  };
  return { deps: { plugins, scheduler, replied }, sent, options, evaluated, handed };
}

const ok = (value: unknown, conditions: Condition[] = []): Reply => ({ id: "1", status: "ok", value: value as never, conditions });

describe("a plugin's request", () => {
  test("has the blocks it reads evaluated first, and goes to R with their revisions and the plugin's class", async () => {
    const s = setup([ok(["a <- 1"])]);
    await expect(pluginRequest(s.deps, CODE, "script", {}, { reads: ["a", "b", "a"] })).resolves.toEqual(["a <- 1"]);

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
    await pluginRequest(s.deps, DOCUMENT, "html", { content: "# Report" }, { file, progress });

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
    await expect(pluginRequest(s.deps, CODE, "script", {}, { reads: ["a"] })).resolves.toBe("done");
    expect(s.evaluated).toHaveLength(2);

    const again = setup([missing, missing, missing]);
    await expect(pluginRequest(again.deps, CODE, "script", {}, { reads: ["a"] })).rejects.toMatchObject({ status: "missing" });
  });

  test("rejects with the statuses of the blocks it reads that are not ready, and does not go to R", async () => {
    const failed: BlockStatus = { ...READY, status: "failed" };
    const s = setup([], { a: READY, b: failed });
    const sent = pluginRequest(s.deps, CODE, "script", {}, { reads: ["a", "b"] });
    await expect(sent).rejects.toBeInstanceOf(RequestError);
    await expect(sent).rejects.toMatchObject({ blocks: [{ block: "b", status: failed }] });
    expect(s.sent).toEqual([]);
  });

  test("rejects with the reply's status and conditions, which it hands on as R's latest reply to a request of the plugin of its name, as it does an ok reply's", async () => {
    const condition = { severity: "error" as const, class: ["simpleError", "error", "condition"], phase: "request" as const, message: "no" };
    const warning = { severity: "warning" as const, class: ["simpleWarning", "warning", "condition"], phase: "request" as const, message: "NAs introduced by coercion" };
    const s = setup([{ id: "1", status: "error", conditions: [condition] }, ok("done", [warning])]);
    const sent = pluginRequest(s.deps, CODE, "script", {}, {});
    await expect(sent).rejects.toMatchObject({ status: "error", conditions: [condition] });
    await expect(sent).rejects.toThrow('R replied error to the request "script" of the plugin "blockr.code-code_plugin".');
    await pluginRequest(s.deps, CODE, "script", {}, {});

    expect(s.handed).toEqual([
      [{ plugin: CODE }, { type: "plugin", name: "script" }, [condition]],
      [{ plugin: CODE }, { type: "plugin", name: "script" }, [warning]],
    ]);
  });

  test.each([
    ["no plugin", "", "script", {}, {}, "names the plugin by the id of its class"],
    [
      "a plugin that the app does not run",
      "blockr.dag-dag_plugin",
      "script",
      {},
      {},
      'The app runs no plugin "blockr.dag-dag_plugin" on the open board, so its request did not go to R.',
    ],
    ["a name in place of the id of the plugin's class", "code", "script", {}, {}, 'The app runs no plugin "code"'],
    ["no name", CODE, "", {}, {}, 'A request of the plugin "blockr.code-code_plugin" is named by a non-empty string'],
    ["args that are no object", CODE, "script", [], {}, "are an object"],
    ["a block that the board lacks", CODE, "script", {}, { reads: ["z"] }, 'reads the block "z", which the board lacks'],
  ])("refuses %s", async (_, plugin, name, args, opts, message) => {
    const s = setup([]);
    await expect(pluginRequest(s.deps, plugin, name, args as never, opts)).rejects.toThrow(message);
    expect(s.sent).toEqual([]);
  });
});
