// A request of a type that a package adds, which code in the client sends
// through `CoreApi.request()` and the package's method of request_call()
// answers in R, as the dock might for the requests of its extensions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#requests):
// 1. Core has the blocks that the request reads evaluated first, even where
//    nothing demands them, and sends the request only once each reads ready.
// 2. It sends the request through the scheduler, with the fields that the
//    package gives it and, in `reads`, the blocks it reads at the revisions they
//    then have.
// 3. Where R replies `missing`, since a revision moved on before R ran the
//    request, core has the blocks evaluated again and asks again.
// Core keeps no answer of such a request, since it may write, and the
// scheduler sends it to no later session, since it may have run.

import type { BlockRevision } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
import type { RequestOpts } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { isObject, own, quoted } from "../util/index.ts";
import { RequestError } from "../mount/index.ts";

// The types of core's own requests, which the hosts send.
const CORE_TYPES = ["block_class", "output_class", "renderer", "option"];

// The fields that core gives a request, or that its own types hold, which a
// package's fields cannot take.
const CORE_FIELDS = ["id", "kind", "type", "reads", "body", "registry_id", "option", "output", "renderer"];

// How many times a request asks R before it gives up on results that R keeps
// lacking, which only a fault in R causes once the scheduler has had the
// blocks it lacks evaluated again.
const ATTEMPTS = 3;

export interface AddedDeps {
  readonly scheduler: Pick<Scheduler, "evaluate" | "request" | "status">;
}

export async function addedRequest(
  deps: AddedDeps,
  type: string,
  fields: Readonly<Record<string, Json>>,
  opts: RequestOpts,
): Promise<Json | Blob> {
  if (typeof type !== "string" || type === "") {
    throw new TypeError("A request names its type, a non-empty string.");
  }
  if (CORE_TYPES.includes(type)) {
    throw new Error(`A request of type "${type}" is core's own, which a host sends, rather than one that a package adds.`);
  }
  if (!isObject(fields)) {
    throw new TypeError(`The fields of a request of type "${type}" are an object.`);
  }
  const taken = Object.keys(fields).filter((name) => CORE_FIELDS.includes(name));
  if (taken.length > 0) {
    throw new Error(`A request of type "${type}" cannot hold the fields ${quoted(taken)}, which are core's.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that a request of type "${type}" reads are an array of their ids.`);
  }
  const what = `The request of type "${type}"`;

  for (let attempt = 1; ; attempt++) {
    const revisions = await readsOf(deps, what, reads, opts.signal);
    const { reply, body } = await deps.scheduler.request(
      { kind: "request", type, ...fields, ...(reads.length === 0 ? {} : { reads: revisions }) },
      {
        ...(opts.signal === undefined ? {} : { signal: opts.signal }),
        ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
      },
    );

    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== undefined ? reply.value : null;
      case "missing":
        if (attempt < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, { status: "missing" });
      case "cancelled":
        throw new RequestError(`R cancelled the request of type "${type}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request of type "${type}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : [],
        });
    }
  }
}

// The blocks that the request reads, at the revisions they have once they are
// evaluated, or a RequestError where one could not be evaluated, with the
// status of each that reads anything but ready.
async function readsOf(
  deps: AddedDeps,
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
