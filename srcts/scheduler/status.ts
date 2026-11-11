// A block's status, as the scheduler keeps it for whatever draws the block. The
// statuses keep today's names, but `invalid` takes the place of `unset`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#statuses).

import type { Graph } from "../graph/index.ts";
import type {
  Condition,
  EvaluateReply,
  Reply,
} from "../protocol/index.ts";
import type { BlockId } from "../types/board.ts";
import { quoted } from "../util/index.ts";
import type { Inputs } from "./recipes.ts";

// A block's status is the first of these that holds:
// - It reads `waiting` while it cannot run.
// - It reads `unevaluated` before its first reply.
// - It reads `stale` while its revision is newer than its latest reply's.
// - Otherwise it reads what that reply says: `ready` for ok, `failed` for error
//   and `invalid` for invalid.
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
  // From the latest reply, which replaces those of the reply before. While the
  // block is waiting, it holds the one condition the scheduler writes, of the
  // phase `status`.
  readonly conditions: readonly Condition[];
  // Its standard output and error.
  readonly stdout: string;
  readonly stderr: string;
  // The result's summary from that reply, as "<data.frame> [150 × 7]".
  readonly summary: string | null;
}

// What a reply to an evaluation says about the block. An error that R raised
// before it read the request, such as a refusal by the lock, comes without a
// summary or an output.
export interface Outcome {
  readonly status: "ok" | "error" | "invalid";
  readonly revision: number;
  readonly summary: string | null;
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
  };
}

// What a block reads with a reply, for its revision now. Where the reply is for
// an older revision, the block reads stale but keeps the reply's conditions. A
// block that failed before it went stale then shows why.
export function replied(
  outcome: Outcome,
  revision: number,
  running: boolean,
): BlockStatus {
  return {
    status:
      outcome.revision !== revision
        ? "stale"
        : outcome.status === "ok"
          ? "ready"
          : outcome.status === "error"
            ? "failed"
            : "invalid",
    revision,
    evaluated: outcome.revision,
    running,
    conditions: outcome.conditions,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary,
  };
}

// What a block reads while it cannot run. It has no summary, and its one
// condition names what it waits for.
export function waiting(
  why: Condition,
  revision: number,
  evaluated: number | null,
  running: boolean,
): BlockStatus {
  return {
    status: "waiting",
    revision,
    evaluated,
    running,
    conditions: [why],
    stdout: "",
    stderr: "",
    summary: null,
  };
}

// The condition of a waiting block. It names the inputs that no link feeds,
// or else the blocks upstream that keep the block waiting. It is a message,
// since the status already says that the block waits.
export function waitingFor(
  what: { unlinked: readonly string[] } | { upstream: readonly BlockId[] },
): Condition {
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
  return {
    severity: "message",
    class: ["blockr_message", "message", "condition"],
    phase: "status",
    message,
  };
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
    a.conditions.length === b.conditions.length &&
    a.conditions.every(
      (condition, i) =>
        condition === b.conditions[i] ||
        (condition.phase === "status" && condition.message === b.conditions[i]?.message),
    )
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
    const reason = why(id);
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

  // Why a block cannot run, if it cannot. A block cannot run in three cases:
  // - An input has no link, and R does not bind it.
  // - A variadic block has no link.
  // - A block that it reads has the status failed, invalid or waiting.
  function why(id: BlockId): Condition | undefined {
    const inputs = input.inputs.get(id);
    const unlinked = inputs?.unlinked ?? [];
    if (unlinked.length > 0) return waitingFor({ unlinked });
    const reads = (inputs?.recipe ?? []).flatMap((read) => read.block ?? []);
    const blocked = [...new Set(reads)].filter((up) => {
      const status = statuses.get(up)?.status;
      return status === "failed" || status === "invalid" || status === "waiting";
    });
    return blocked.length > 0 ? waitingFor({ upstream: blocked }) : undefined;
  }
}

// Why an evaluation with parameters is not sent: the block reads waiting, for
// what its status's condition names.
export function waitingError(id: BlockId, status: BlockStatus): Error {
  const why = status.conditions[0]?.message ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id}"`)} Its evaluation with parameters is not sent.`,
  );
}
