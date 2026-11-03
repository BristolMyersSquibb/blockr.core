// One-time evaluations, as evaluate() starts them. One without parameters
// brings its blocks up to date and resolves once none of them reads stale or
// unevaluated. One with parameters evaluates each of its blocks once, and
// resolves with the statuses that R's replies give. The scheduler keeps them by
// id. The functions here say what happens to them, and change nothing.

import type { Graph } from "../graph/index.ts";
import type { Recipe, Reply } from "../protocol/messages.ts";
import { quoted } from "../store/util.ts";
import type { BlockId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Need } from "./needs.ts";
import type { Pending } from "./plan.ts";
import { replied, waitingError } from "./status.ts";
import type { BlockStatus, Outcome } from "./status.ts";

// A one-time evaluation without parameters, waiting on its blocks.
export interface Plain {
  readonly blocks: ReadonlySet<BlockId>;
  readonly priority: number;
}

// A one-time evaluation with parameters. It is an evaluation of each of its
// blocks, one part each, in the order the blocks were given.
export interface WithParams {
  readonly priority: number;
  readonly params: Record<string, Json>;
  readonly parts: ReadonlyMap<BlockId, Part>;
}

export type OneTime = Plain | WithParams;

// The evaluation with parameters of one block.
export interface Part {
  // The recipe it went to R with, at the revision the block had then. It is
  // kept until R replies.
  readonly recipe: Recipe | undefined;
  // The evaluation it went to R as, by id, until R replies.
  readonly sent: number | undefined;
  // The block's status from R's reply.
  readonly result: BlockStatus | undefined;
}

// What a one-time evaluation settles with: the statuses it resolves with, by
// block id, or why it rejects.
export type Settled =
  | { readonly statuses: Record<BlockId, BlockStatus> }
  | { readonly error: unknown };

// The one-time evaluations after a change, and those that the change settled.
export interface Next {
  readonly oneTimes: ReadonlyMap<number, OneTime>;
  readonly settled: readonly (readonly [number, Settled])[];
}

// A new one-time evaluation of the blocks, or why it rejects at once. One
// without parameters leaves out the blocks that the board lacks, and rejects
// where that leaves none. One with parameters rejects where it names no block,
// a block that the board lacks, or a block that reads waiting.
export function oneTimeOf(
  blocks: readonly BlockId[],
  params: Record<string, Json> | undefined,
  priority: number,
  statuses: ReadonlyMap<BlockId, BlockStatus>,
): OneTime | Error {
  if (params === undefined) {
    const onBoard = new Set(blocks.filter((id) => statuses.has(id)));
    if (onBoard.size === 0) {
      return new Error(`The board holds none of the blocks ${quoted(blocks)} to evaluate.`);
    }
    return { blocks: onBoard, priority };
  }

  if (blocks.length === 0) return new Error("An evaluation with parameters names no block.");
  const absent = blocks.filter((id) => !statuses.has(id));
  if (absent.length > 0) {
    const named = absent.length === 1 ? "block" : "blocks";
    return new Error(`The board holds no ${named} ${quoted(absent)} to evaluate.`);
  }
  const parts = new Map<BlockId, Part>();
  for (const id of blocks) {
    const status = statuses.get(id);
    if (status?.status === "waiting") return waitingError(id, status);
    parts.set(id, { recipe: undefined, sent: undefined, result: undefined });
  }
  return { priority, params, parts };
}

// The one-time evaluations without parameters whose blocks are all current
// settle, with each block's status.
export function settle(
  oneTimes: ReadonlyMap<number, OneTime>,
  statuses: ReadonlyMap<BlockId, BlockStatus>,
): Next {
  const settled: [number, Settled][] = [];
  for (const [id, oneTime] of oneTimes) {
    if ("params" in oneTime) continue;
    const blocks = [...oneTime.blocks];
    if (blocks.some((block) => !current(statuses.get(block)))) continue;
    const result: Record<BlockId, BlockStatus> = {};
    for (const block of blocks) {
      const status = statuses.get(block);
      if (status !== undefined) result[block] = status;
    }
    settled.push([id, { statuses: result }]);
  }
  return { oneTimes: without(oneTimes, settled.map(([id]) => id)), settled };
}

// What blocks that left the board do to the one-time evaluations. One without
// parameters leaves them out, and rejects once it is left with none. One with
// parameters rejects where a block left before its evaluation was sent.
export function left(oneTimes: ReadonlyMap<number, OneTime>, gone: ReadonlySet<BlockId>): Next {
  const kept = new Map<number, OneTime>();
  const settled: [number, Settled][] = [];
  for (const [id, oneTime] of oneTimes) {
    if (!("params" in oneTime)) {
      const blocks = new Set([...oneTime.blocks].filter((block) => !gone.has(block)));
      if (blocks.size === 0) {
        settled.push([id, { error: new Error("Every block of the one-time evaluation left the board.") }]);
      } else {
        kept.set(id, blocks.size === oneTime.blocks.size ? oneTime : { ...oneTime, blocks });
      }
      continue;
    }
    const early = [...oneTime.parts].find(
      ([block, part]) => gone.has(block) && part.sent === undefined && part.result === undefined,
    );
    if (early === undefined) {
      kept.set(id, oneTime);
    } else {
      const error = new Error(
        `Block "${early[0]}" left the board before its evaluation with parameters was sent.`,
      );
      settled.push([id, { error }]);
    }
  }
  return { oneTimes: kept, settled };
}

// The one-time evaluations once the evaluation of a block for one with
// parameters went to R, with its id and the recipe it went with.
export function sent(
  oneTimes: ReadonlyMap<number, OneTime>,
  id: number,
  block: BlockId,
  evaluation: { readonly id: number; readonly recipe: Recipe },
): ReadonlyMap<number, OneTime> {
  return withPart(oneTimes, id, block, { recipe: evaluation.recipe, sent: evaluation.id });
}

// What R's reply to the evaluation of a block does to the one-time evaluation
// with parameters that it was for. A reply with an outcome gives the block its
// result, and the one-time evaluation resolves once every block has one. A
// reply of `missing` means that R ran nothing, so the block's evaluation goes
// again, with the recipe the block has once R holds what the block reads. A
// reply of `cancelled` rejects the one-time evaluation.
export function answered(
  oneTimes: ReadonlyMap<number, OneTime>,
  id: number,
  block: BlockId,
  reply: Reply,
  outcome: Outcome | undefined,
  revision: number,
): Next {
  const oneTime = oneTimes.get(id);
  if (oneTime === undefined || !("params" in oneTime)) return { oneTimes, settled: [] };
  const part = oneTime.parts.get(block);
  if (part === undefined) return { oneTimes, settled: [] };

  if (outcome !== undefined) {
    const result = Object.freeze(replied(outcome, revision, false));
    const parts = new Map(oneTime.parts).set(block, { ...part, result });
    const results = [...parts].flatMap(([at, { result: status }]) =>
      status === undefined ? [] : [[at, status] as const],
    );
    if (results.length < parts.size) {
      return { oneTimes: new Map(oneTimes).set(id, { ...oneTime, parts }), settled: [] };
    }
    return {
      oneTimes: without(oneTimes, [id]),
      settled: [[id, { statuses: Object.fromEntries(results) }]],
    };
  }

  if (reply.status === "missing") {
    return { oneTimes: withPart(oneTimes, id, block, { recipe: undefined, sent: undefined }), settled: [] };
  }

  const error = new Error(
    `R dropped the evaluation of block "${block}" with parameters, as it does for a block that leaves the board.`,
  );
  return { oneTimes: without(oneTimes, [id]), settled: [[id, { error }]] };
}

// What the one-time evaluations need evaluated, each at its evaluation
// priority: the blocks of one without parameters, and what the blocks of one
// with parameters read. What such a block reads stays needed until R has
// replied to its evaluation, so that nothing cancels an evaluation that it
// waits on in R. Until its evaluation is sent, that is what the block reads
// now. From then on, it is what its recipe names.
export function neededBy(oneTimes: ReadonlyMap<number, OneTime>, graph: Graph): Need[] {
  const needs: Need[] = [];
  for (const oneTime of oneTimes.values()) {
    if (!("params" in oneTime)) {
      needs.push({ blocks: oneTime.blocks, priority: oneTime.priority });
      continue;
    }
    for (const [block, part] of oneTime.parts) {
      if (part.result !== undefined) continue;
      const reads =
        part.recipe === undefined
          ? graph.upstream(block)
          : part.recipe.inputs.flatMap((input) => input.block ?? []);
      needs.push({ blocks: reads, priority: oneTime.priority });
    }
  }
  return needs;
}

// The evaluations of blocks that one-time evaluations with parameters wait for
// and that are not sent yet.
export function pendingOf(oneTimes: ReadonlyMap<number, OneTime>): Pending[] {
  const pending: Pending[] = [];
  for (const [id, oneTime] of oneTimes) {
    if (!("params" in oneTime)) continue;
    for (const [block, part] of oneTime.parts) {
      if (part.sent === undefined && part.result === undefined) {
        pending.push({ oneTime: id, block, priority: oneTime.priority });
      }
    }
  }
  return pending;
}

// Whether a block is current, as a one-time evaluation without parameters
// waits for: neither stale nor unevaluated.
function current(status: BlockStatus | undefined): boolean {
  return status?.status !== "stale" && status?.status !== "unevaluated";
}

// The one-time evaluations, with one part of a one-time evaluation with
// parameters changed.
function withPart(
  oneTimes: ReadonlyMap<number, OneTime>,
  id: number,
  block: BlockId,
  change: Partial<Part>,
): ReadonlyMap<number, OneTime> {
  const oneTime = oneTimes.get(id);
  if (oneTime === undefined || !("params" in oneTime)) return oneTimes;
  const part = oneTime.parts.get(block);
  if (part === undefined) return oneTimes;
  const parts = new Map(oneTime.parts).set(block, { ...part, ...change });
  return new Map(oneTimes).set(id, { ...oneTime, parts });
}

// The one-time evaluations without those of the ids.
function without(
  oneTimes: ReadonlyMap<number, OneTime>,
  ids: readonly number[],
): ReadonlyMap<number, OneTime> {
  if (ids.length === 0) return oneTimes;
  const kept = new Map(oneTimes);
  for (const id of ids) kept.delete(id);
  return kept;
}
