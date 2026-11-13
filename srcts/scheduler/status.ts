// A block's status, as the scheduler keeps it for whatever draws the block. The
// statuses keep today's names, but `invalid` takes the place of `unset`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#statuses).

import type { Graph } from "../graph/index.ts";
import type {
  Condition,
  EvaluateReply,
  Reason,
  Reply,
} from "../protocol/index.ts";
import type { BlockId } from "../types/board.ts";
import { jsonEqual, quoted } from "../util/index.ts";
import type { Inputs } from "./recipes.ts";

// A block's status is the first of these that holds:
// - It reads `waiting` while an input or a block upstream keeps it from
//   running.
// - It reads `unevaluated` before its first reply.
// - It reads `stale` while its revision is newer than its latest reply's.
// - Otherwise it reads what that reply says: `ready` for ok, `failed` for
//   error, `invalid` for invalid and `waiting` for waiting, which R replies for
//   a block that runs only once asked, such as by its save button.
export type Status =
  | "unevaluated"
  | "stale"
  | "waiting"
  | "invalid"
  | "ready"
  | "failed";

export interface BlockStatus {
  readonly status: Status;
  // The block's current revision.
  readonly revision: number;
  // The revision that its latest reply was for, null before its first.
  readonly evaluated: number | null;
  // Whether an evaluation of it is sent and not yet answered.
  readonly running: boolean;
  // From the latest reply, which replaces those of the reply before. A block
  // that an input or a block upstream keeps waiting has none.
  readonly conditions: readonly Condition[];
  // Its standard output and error.
  readonly stdout: string;
  readonly stderr: string;
  // The result's summary from that reply, as "<data.frame> [150 × 7]".
  readonly summary: string | null;
  // Why the block is invalid or waiting, from that reply, as R sends it.
  // Where an input or a block upstream keeps the block waiting, the scheduler
  // writes it, naming what the block waits for.
  readonly reason: Reason | null;
}

// What a reply to an evaluation says about the block. An error that R raised
// before it read the request, such as a refusal by the lock, comes without a
// summary or an output.
export interface Outcome {
  readonly status: "ok" | "error" | "invalid" | "waiting";
  readonly revision: number;
  readonly summary: string | null;
  readonly reason: Reason | null;
  readonly conditions: readonly Condition[];
  readonly stdout: string;
  readonly stderr: string;
}

const NO_CONDITIONS: readonly Condition[] = Object.freeze([]);

// The outcome of an evaluation of a revision, from R's reply to it. A reply of
// `cancelled` or `missing` counts as none and gives undefined.
export function replyOutcome(reply: Reply, revision: number): Outcome | undefined {
  if (reply.status === "cancelled" || reply.status === "missing") return undefined;
  const evaluated = reply as Partial<EvaluateReply> & Pick<EvaluateReply, "status">;
  return {
    status: reply.status,
    revision,
    summary: evaluated.summary ?? null,
    reason: evaluated.reason ?? null,
    conditions: evaluated.conditions ?? NO_CONDITIONS,
    stdout: evaluated.stdout ?? "",
    stderr: evaluated.stderr ?? "",
  };
}

// The outcome of an evaluation that the transport could not send. It fails with
// the transport's error, as it would with R's.
export function unsent(revision: number, message: string): Outcome {
  return {
    status: "error",
    revision,
    summary: null,
    reason: null,
    conditions: [
      {
        severity: "error",
        class: ["blockr_error", "error", "condition"],
        message: `The evaluation could not be sent to R: ${message}`,
      },
    ],
    stdout: "",
    stderr: "",
  };
}

// What a block reads with no reply to show.
export function unevaluated(revision: number, running: boolean): BlockStatus {
  return {
    status: "unevaluated",
    revision,
    evaluated: null,
    running,
    conditions: NO_CONDITIONS,
    stdout: "",
    stderr: "",
    summary: null,
    reason: null,
  };
}

// The status that a reply for the block's current revision gives it.
const REPLIED: Readonly<Record<Outcome["status"], Status>> = {
  ok: "ready",
  error: "failed",
  invalid: "invalid",
  waiting: "waiting",
};

// What a block reads with a reply, for its revision now. Where the reply is for
// an older revision, the block reads stale but keeps the reply's conditions and
// reason. A block that failed before it went stale then shows why.
export function replied(
  outcome: Outcome,
  revision: number,
  running: boolean,
): BlockStatus {
  return {
    status: outcome.revision !== revision ? "stale" : REPLIED[outcome.status],
    revision,
    evaluated: outcome.revision,
    running,
    conditions: outcome.conditions,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary,
    reason: outcome.reason,
  };
}

// What a block reads while an input or a block upstream keeps it from running.
// It has no summary and no conditions, and its reason names what it waits for.
export function waiting(
  why: Reason,
  revision: number,
  evaluated: number | null,
  running: boolean,
): BlockStatus {
  return {
    status: "waiting",
    revision,
    evaluated,
    running,
    conditions: NO_CONDITIONS,
    stdout: "",
    stderr: "",
    summary: null,
    reason: why,
  };
}

// The reason of a block that an input or a block upstream keeps waiting. It
// names the inputs that no link feeds, or else the blocks upstream that keep
// the block waiting, and concerns no state arg.
export function waitingFor(
  what: { unlinked: readonly string[] } | { upstream: readonly BlockId[] },
): Reason {
  let message: string;
  if ("upstream" in what) {
    const blocks = what.upstream.length === 1 ? "block" : "blocks";
    message = `This block is waiting for ${blocks} ${quoted(what.upstream)}.`;
  } else if (what.unlinked.includes("...")) {
    message = "This block is waiting for a block to be linked to it.";
  } else {
    const inputs = what.unlinked.length === 1 ? "input" : "inputs";
    message = `This block is waiting for its ${inputs} ${quoted(what.unlinked)} to be linked.`;
  }
  return { message, arg: [] };
}

// Whether two statuses read the same. The scheduler then keeps handing out the
// status it has, as a getter must for React's useSyncExternalStore.
export function sameStatus(a: BlockStatus, b: BlockStatus): boolean {
  return (
    a.status === b.status &&
    a.revision === b.revision &&
    a.evaluated === b.evaluated &&
    a.running === b.running &&
    a.stdout === b.stdout &&
    a.stderr === b.stderr &&
    a.summary === b.summary &&
    // The scheduler writes the reason of a block that it keeps waiting afresh
    // in each pass.
    jsonEqual(a.reason, b.reason) &&
    a.conditions.length === b.conditions.length &&
    a.conditions.every((condition, i) => condition === b.conditions[i])
  );
}

// Every block's status, upstream first. Each comes from the block's revision,
// its latest reply and its inputs, and from the statuses of the blocks it
// reads. A status that reads as before keeps its object. The blocks whose
// status changed include those that left the board.
export function blockStatuses(input: {
  readonly graph: Graph;
  readonly revisions: ReadonlyMap<BlockId, number>;
  readonly outcomes: ReadonlyMap<BlockId, Outcome>;
  readonly inputs: ReadonlyMap<BlockId, Inputs>;
  // The blocks with an evaluation sent and not yet answered.
  readonly running: ReadonlySet<BlockId>;
  readonly before: ReadonlyMap<BlockId, BlockStatus>;
}): { statuses: Map<BlockId, BlockStatus>; changed: BlockId[] } {
  const statuses = new Map<BlockId, BlockStatus>();
  const changed: BlockId[] = [];

  for (const id of input.graph.order) {
    const revision = input.revisions.get(id);
    if (revision === undefined) continue;
    const outcome = input.outcomes.get(id);
    const running = input.running.has(id);
    const reason = keptWaiting(input.inputs.get(id), statuses);
    const now =
      reason !== undefined
        ? waiting(reason, revision, outcome?.revision ?? null, running)
        : outcome === undefined
          ? unevaluated(revision, running)
          : replied(outcome, revision, running);
    const before = input.before.get(id);
    if (before !== undefined && sameStatus(before, now)) {
      statuses.set(id, before);
    } else {
      statuses.set(id, Object.freeze(now));
      changed.push(id);
    }
  }

  for (const id of input.before.keys()) {
    if (!statuses.has(id)) changed.push(id);
  }
  return { statuses, changed };
}

// Why an input or a block upstream keeps a block from running, if one does,
// from the block's inputs and the statuses of the blocks it reads. That is so
// in three cases:
// - An input has no link, and R does not bind it.
// - A variadic block has no link.
// - A block that it reads has the status failed, invalid or waiting.
// A block that R says waits is not kept waiting so, since it runs once a
// one-time evaluation asks, as a save button does.
export function keptWaiting(
  inputs: Inputs | undefined,
  statuses: ReadonlyMap<BlockId, BlockStatus>,
): Reason | undefined {
  const unlinked = inputs?.unlinked ?? [];
  if (unlinked.length > 0) return waitingFor({ unlinked });
  const reads = (inputs?.recipe ?? []).flatMap((read) => read.block ?? []);
  const blocked = [...new Set(reads)].filter((up) => {
    const status = statuses.get(up)?.status;
    return status === "failed" || status === "invalid" || status === "waiting";
  });
  return blocked.length > 0 ? waitingFor({ upstream: blocked }) : undefined;
}

// Why an evaluation with parameters is not sent: an input or a block upstream
// keeps the block waiting, as the reason of its status names.
export function waitingError(id: BlockId, status: BlockStatus): Error {
  const why = status.reason?.message ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id}"`)} Its evaluation with parameters is not sent.`,
  );
}
