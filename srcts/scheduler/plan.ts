// What a pass of the scheduler decides: which evaluations to cancel, which to
// send, which evaluations with parameters reject, and when to look again. The
// plan comes from what it is given alone, and changes nothing.

import type { Graph } from "../graph/index.ts";
import type { BlockId } from "../types/board.ts";
import { waitingError } from "./status.ts";
import type { BlockStatus } from "./status.ts";

// How long anything else may wait in R behind an evaluation below evaluation
// priority 0. After that, the scheduler cancels that evaluation.
export const GIVE_WAY = 5000;

// The evaluation with parameters of one block.
export interface Part {
  readonly block: BlockId;
  // The one-time evaluation with parameters that it belongs to.
  readonly of: { readonly priority: number; readonly done: boolean };
}

// An evaluation sent to R and not yet answered.
export interface Sent {
  // When it was sent. One count orders everything the scheduler sends.
  readonly seq: number;
  readonly at: number;
  readonly block: BlockId;
  readonly revision: number;
  // The evaluation with parameters that it is, if it is one.
  readonly part: Part | undefined;
  // Whether it was sent as evaluation in the background.
  readonly background: boolean;
  readonly cancelled: boolean;
  // Whether another board opened since. Its reply then counts for nothing.
  readonly orphaned: boolean;
}

// An output or a request sent and not yet answered.
export interface Asked {
  readonly seq: number;
  readonly at: number;
}

export interface Plan<S extends Sent, P extends Part> {
  readonly cancel: readonly S[];
  // What to send, in order: a block, the evaluation with parameters of a
  // block, or a block in the background.
  readonly send: readonly {
    readonly block: BlockId;
    readonly part?: P;
    readonly background?: true;
  }[];
  // The evaluations with parameters whose block cannot run.
  readonly reject: readonly { readonly part: P; readonly error: Error }[];
  // Since when R has had nothing else to run than evaluations below 0.
  readonly idleSince: number | undefined;
  // When to look again, where something waits on time.
  readonly wakeAt: number | undefined;
}

export function planOf<S extends Sent, P extends Part>(input: {
  readonly graph: Graph;
  readonly revisions: ReadonlyMap<BlockId, number>;
  readonly statuses: ReadonlyMap<BlockId, BlockStatus>;
  // The blocks needed, each at its evaluation priority.
  readonly needs: ReadonlyMap<BlockId, number>;
  readonly sent: Iterable<S>;
  readonly asked: Iterable<Asked>;
  // The evaluations with parameters not sent yet.
  readonly parts: Iterable<P>;
  // The `evaluation_queue` blockr option.
  readonly queue: number;
  // Whether R can stop an evaluation that it has started.
  readonly interrupt: boolean;
  readonly holding: boolean;
  readonly now: number;
  readonly idleSince: number | undefined;
  // The `background_evaluation_delay` blockr option, in seconds.
  readonly delay: number;
}): Plan<S, P> {
  const { graph, revisions, statuses, needs, now } = input;
  const sent = [...input.sent];
  const asked = [...input.asked];
  const parts = [...input.parts];
  const index = new Map(graph.order.map((id, i) => [id, i]));
  const deadlines: number[] = [];

  // What the plan has decided so far: an evaluation it cancels no longer
  // counts as in flight, and a block it sends counts as in flight.
  const cancel: S[] = [];
  const cancelled = new Set<S>();
  const planned = new Set<BlockId>();

  // The evaluation priority an evaluation has now. It is that of what needs
  // its block, or -1 for evaluation in the background. It is undefined where
  // nothing needs the block any more.
  const priorityOf = (evaluation: Sent): number | undefined =>
    evaluation.part !== undefined
      ? evaluation.part.of.priority
      : (needs.get(evaluation.block) ?? (evaluation.background ? -1 : undefined));

  const inFlight = (id: BlockId, at: number): boolean =>
    planned.has(id) ||
    sent.some(
      (evaluation) =>
        evaluation.block === id &&
        evaluation.revision === at &&
        !evaluation.orphaned &&
        !evaluation.cancelled &&
        !cancelled.has(evaluation),
    );

  const needsEvaluation = (id: BlockId): boolean => {
    const status = statuses.get(id)?.status;
    return status === "stale" || status === "unevaluated";
  };

  // Whether R holds the block's result at its current revision. A result that
  // is being computed counts too, since R computes it before anything sent
  // after it.
  const available = (id: BlockId): boolean => {
    const at = revisions.get(id);
    return at !== undefined && (statuses.get(id)?.status === "ready" || inFlight(id, at));
  };

  // Since when something else has waited in R behind an evaluation: an
  // evaluation at a higher evaluation priority, an output or a request.
  const waitedSince = (evaluation: Sent, priority: number): number | undefined => {
    let since: number | undefined;
    for (const other of asked) {
      if (other.seq > evaluation.seq) since = Math.min(since ?? other.at, other.at);
    }
    for (const other of sent) {
      if (other.seq <= evaluation.seq || other.orphaned) continue;
      if ((priorityOf(other) ?? -Infinity) > priority) {
        since = Math.min(since ?? other.at, other.at);
      }
    }
    return since;
  };

  // Whether an evaluation with parameters still has to evaluate the block at
  // that revision. The scheduler then evaluates the block no other way. One
  // not sent yet takes the revision the block has when it goes.
  const paramsFor = (id: BlockId, at: number): boolean =>
    parts.some((part) => part.block === id && !part.of.done) ||
    sent.some(
      (evaluation) =>
        evaluation.part?.block === id && evaluation.revision === at && !evaluation.orphaned,
    );

  // What to cancel, in three cases:
  // - An evaluation whose revision has moved.
  // - One that R has not started and nothing needs any more.
  // - One below 0 that something else has waited behind for 5 s, where R can
  //   stop what it has started.
  let oldest = Infinity;
  for (const item of [...sent, ...asked]) oldest = Math.min(oldest, item.seq);
  const drop = (evaluation: S) => {
    cancel.push(evaluation);
    cancelled.add(evaluation);
  };

  for (const evaluation of sent) {
    if (evaluation.cancelled || evaluation.orphaned || evaluation.part !== undefined) continue;
    const at = revisions.get(evaluation.block);
    // R drops the evaluation of a block that left the board, as it forgets the
    // block.
    if (at === undefined) continue;
    if (at !== evaluation.revision) {
      drop(evaluation);
      continue;
    }
    const priority = priorityOf(evaluation);
    if (priority === undefined) {
      if (evaluation.seq !== oldest) drop(evaluation);
      continue;
    }
    if (!input.interrupt || priority >= 0) continue;
    const since = waitedSince(evaluation, priority);
    if (since === undefined) continue;
    if (now - since >= GIVE_WAY) drop(evaluation);
    else deadlines.push(since + GIVE_WAY);
  }

  // What to send: the blocks that are needed and not current, and the
  // evaluations with parameters. They go highest evaluation priority first,
  // and in topological order within one priority. Each goes once R holds what
  // it reads, or computes it first.
  const queued: { block: BlockId; priority: number; part?: P }[] = [];
  for (const [id, priority] of needs) {
    const at = revisions.get(id);
    if (at === undefined || !needsEvaluation(id)) continue;
    if (inFlight(id, at) || paramsFor(id, at)) continue;
    queued.push({ block: id, priority });
  }
  for (const part of parts) {
    if (!part.of.done) queued.push({ block: part.block, priority: part.of.priority, part });
  }
  queued.sort(
    (a, b) =>
      (a.priority === b.priority ? 0 : a.priority > b.priority ? -1 : 1) ||
      (index.get(a.block) ?? 0) - (index.get(b.block) ?? 0),
  );

  let slots = input.queue + 1 - sent.length;
  const send: { block: BlockId; part?: P; background?: true }[] = [];
  const reject: { part: P; error: Error }[] = [];
  const rejected = new Set<Part["of"]>();

  if (!input.holding) {
    for (const item of queued) {
      // An evaluation with parameters whose block cannot run rejects, whether
      // R has room for another evaluation or not.
      if (item.part !== undefined) {
        if (rejected.has(item.part.of)) continue;
        const status = statuses.get(item.block);
        if (status?.status === "waiting") {
          reject.push({ part: item.part, error: waitingError(item.block, status) });
          rejected.add(item.part.of);
          continue;
        }
      }
      if (slots <= 0 || !graph.upstream(item.block).every(available)) continue;
      send.push(item.part === undefined ? { block: item.block } : { block: item.block, part: item.part });
      planned.add(item.block);
      slots -= 1;
    }
  }

  // Evaluation in the background. R counts as idle while it has no evaluation
  // at 0 or above, no output and no request to run. Once R has been idle for
  // the delay, the scheduler evaluates the stale and unevaluated blocks that
  // nothing needs, one at a time, at -1.
  const busy =
    asked.length > 0 ||
    sent.some((evaluation) => (priorityOf(evaluation) ?? 0) >= 0 || evaluation.orphaned) ||
    queued.some((item) => item.priority >= 0);

  let idleSince = input.idleSince;
  if (busy || input.holding || !Number.isFinite(input.delay)) {
    idleSince = undefined;
  } else {
    idleSince ??= now;
    const due = idleSince + input.delay * 1000;
    if (now < due) {
      deadlines.push(due);
    } else if (slots > 0 && !sent.some((evaluation) => evaluation.background)) {
      const block = inBackground();
      if (block !== undefined) send.push({ block, background: true });
    }
  }

  return {
    cancel,
    send,
    reject,
    idleSince,
    wakeAt: deadlines.length > 0 ? Math.min(...deadlines) : undefined,
  };

  // The first block, upstream first, that nothing needs and that is not
  // current, where R holds what it reads already.
  function inBackground(): BlockId | undefined {
    for (const id of graph.order) {
      if (needs.has(id) || !needsEvaluation(id)) continue;
      const at = revisions.get(id);
      if (at === undefined || inFlight(id, at)) continue;
      if (graph.upstream(id).every((up) => statuses.get(up)?.status === "ready")) return id;
    }
    return undefined;
  }
}
