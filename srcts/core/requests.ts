// A plugin's request, which code in the client sends through
// `CoreApi.request()` for one of the app's plugins that runs, by the id of its
// class, and the method of R's plugin_requests() for the class answers
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#a-plugins-requests):
// 1. Core has the blocks that the request reads evaluated first, even where
//    nothing demands them, and sends the request only once each reads ready.
// 2. It sends the request through the scheduler, naming the plugin's class by
//    the class's id, with the request's name, its args and, in `reads`, the
//    blocks it reads at the revisions they then have.
// 3. Where R replies `missing`, since a revision moved on before R ran the
//    request, core has the blocks evaluated again and asks again.
// Core keeps no answer of a plugin's request, since it may write, and the
// scheduler sends it to no later session, since it may have run. Each
// condition of R's reply, ok or error, raises a notification that concerns
// the plugin, which holds until R next replies to a request of the same name.

import type { BlockRevision } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { BlockId, PluginClassId } from "../types/board.ts";
import type { RequestOpts } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { isObject, own, quoted } from "../util/index.ts";
import type { Replied } from "../util/index.ts";
import { RequestError } from "../mount/index.ts";
import type { RunningPlugins } from "./plugins.ts";

// How many times a request asks R before it gives up on results that R keeps
// lacking, which only a fault in R causes once the scheduler has had the
// blocks it lacks evaluated again.
const ATTEMPTS = 3;

export interface PluginRequestDeps {
  readonly plugins: Pick<RunningPlugins, "appPlugins">;
  readonly scheduler: Pick<Scheduler, "evaluate" | "request" | "status">;
  // Holds a notification for each condition of R's latest reply to each of
  // the plugin's requests, which concern the plugin (./notifications.ts).
  readonly replied: Replied;
}

// Sends the request `name` of the plugin that runs whose class has the id
// `plugin`, and resolves with its answer as R writes it, or with the File of a
// download.
export async function pluginRequest(
  deps: PluginRequestDeps,
  plugin: PluginClassId,
  name: string,
  args: Readonly<Record<string, Json>>,
  opts: RequestOpts,
): Promise<Json | Blob> {
  if (typeof plugin !== "string" || plugin === "") {
    throw new TypeError("A plugin's request names the plugin by the id of its class, a non-empty string.");
  }
  if (!Object.hasOwn(deps.plugins.appPlugins(), plugin)) {
    throw new Error(`The app runs no plugin "${plugin}" on the open board, so its request did not go to R.`);
  }
  if (typeof name !== "string" || name === "") {
    throw new TypeError(`A request of the plugin "${plugin}" is named by a non-empty string.`);
  }
  if (!isObject(args)) {
    throw new TypeError(`The args of the request "${name}" of the plugin "${plugin}" are an object.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that the request "${name}" of the plugin "${plugin}" reads are an array of their ids.`);
  }
  const what = `The request "${name}" of the plugin "${plugin}"`;

  for (let attempt = 1; ; attempt++) {
    const revisions = await evaluateReads(deps, what, reads, opts.signal);
    const { reply, body } = await deps.scheduler.request(
      { kind: "request", type: "plugin", plugin, name, args: { ...args }, reads: revisions },
      {
        ...(opts.file === undefined ? {} : { body: opts.file }),
        ...(opts.signal === undefined ? {} : { signal: opts.signal }),
        ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
      },
    );

    const conditions = "conditions" in reply ? reply.conditions : [];
    switch (reply.status) {
      case "ok":
        deps.replied({ plugin }, { type: "plugin", name }, conditions);
        if (body !== null) return body;
        return "value" in reply && reply.value !== undefined ? reply.value : null;
      case "missing":
        if (attempt < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, { status: "missing" });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name}" of the plugin "${plugin}".`, { status: "cancelled" });
      default:
        deps.replied({ plugin }, { type: "plugin", name }, conditions);
        throw new RequestError(`R replied ${reply.status} to the request "${name}" of the plugin "${plugin}".`, {
          status: reply.status,
          conditions,
        });
    }
  }
}

// The blocks that the request reads, at the revisions they have once they are
// evaluated, or a RequestError where one could not be evaluated, with the
// status of each that reads anything but ready.
async function evaluateReads(
  deps: PluginRequestDeps,
  what: string,
  blocks: readonly BlockId[],
  signal: AbortSignal | undefined,
): Promise<BlockRevision[]> {
  if (blocks.length === 0) return [];
  const absent = blocks.filter((block) => deps.scheduler.status(block) === undefined);
  if (absent.length > 0) {
    throw new Error(`${what} reads ${absent.length === 1 ? "the block" : "the blocks"} ${quoted(absent)}, which the board lacks.`);
  }
  const unique = [...new Set(blocks)];
  const statuses = await deps.scheduler.evaluate(unique, undefined, signal === undefined ? {} : { signal });
  const unready = unique.flatMap((block) => {
    const status = own(statuses, block);
    return status === undefined || status.status === "ready" ? [] : [{ block, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ block, status }) => `"${block}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since a block it reads is not ready: ${listed.join(", ")}.`, {
      blocks: unready,
    });
  }
  const left = unique.filter((block) => own(statuses, block) === undefined);
  if (left.length > 0) {
    throw new Error(`${what} did not go to R, since ${left.length === 1 ? "the block" : "the blocks"} ${quoted(left)} left the board.`);
  }
  return unique.map((block) => ({ block, revision: own(statuses, block)?.revision ?? 0 }));
}
