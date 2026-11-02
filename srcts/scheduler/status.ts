// A block's status, which the scheduler keeps for whatever draws the block,
// with the names of today's statuses but `invalid` in place of `unset`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#statuses).

import type {
  Condition,
  EvaluateReply,
  Omitted,
  Reply,
} from "../protocol/messages.ts";
import type { BlockId } from "../types/board.ts";
import { quoted } from "../store/util.ts";

// A block's status is the first of these that holds: `waiting` while it cannot
// run, `unevaluated` before its first reply, `stale` while its revision is
// newer than its latest reply's, and otherwise what that reply says, `ready`
// for ok, `failed` for error and `invalid` for invalid.
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
  // From the latest reply, which replaces the ones before, and while the block
  // is waiting, the one condition the scheduler writes, of the phase `status`.
  readonly conditions: readonly Condition[];
  readonly omitted: Omitted;
  // Its standard output and error, as R bounded them.
  readonly stdout: string;
  readonly stderr: string;
  // The result's summary from that reply, as "<data.frame> [150 × 7]".
  readonly summary: string | null;
}

// What a reply to an evaluation says about the block, which an error that R
// raised before it read the request, such as a refusal by the lock, says
// without a summary or an output.
export interface Outcome {
  readonly status: "ok" | "error" | "invalid";
  readonly revision: number;
  readonly summary: string | null;
  readonly conditions: readonly Condition[];
  readonly omitted: Omitted;
  readonly stdout: string;
  readonly stderr: string;
}

const NONE: Omitted = Object.freeze({ warning: 0, message: 0 });
const NO_CONDITIONS: readonly Condition[] = Object.freeze([]);

// The outcome of an evaluation of a revision, from R's reply to it, or
// undefined for a reply that counts as none, `cancelled` or `missing`.
export function outcomeOf(reply: Reply, revision: number): Outcome | undefined {
  if (reply.status === "cancelled" || reply.status === "missing") return undefined;
  const evaluated = reply as Partial<EvaluateReply> & Pick<EvaluateReply, "status">;
  return {
    status: reply.status,
    revision,
    summary: evaluated.summary ?? null,
    conditions: evaluated.conditions ?? NO_CONDITIONS,
    omitted: evaluated.omitted ?? NONE,
    stdout: evaluated.stdout ?? "",
    stderr: evaluated.stderr ?? "",
  };
}

// The outcome of an evaluation that the transport could not send, which
// fails with the transport's error, as R's error would.
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
    omitted: NONE,
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
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null,
  };
}

// What a block reads with a reply, for its revision now: stale where the reply
// is for an older one, whose conditions it keeps, so that a block that failed
// before it went stale shows why.
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
    omitted: outcome.omitted,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary,
  };
}

// What a block reads while it cannot run: no summary, and as its conditions
// only the one that names what it waits for.
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
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null,
  };
}

// The condition of a waiting block, which names the inputs that no link feeds
// or the blocks upstream that keep it waiting, of the severity of today's
// note.
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
    severity: "warning",
    class: ["blockr_warning", "warning", "condition"],
    phase: "status",
    message,
  };
}

// Whether two statuses read the same, so that the scheduler keeps handing out
// the one it has, as a getter must for React's useSyncExternalStore.
export function sameStatus(a: BlockStatus, b: BlockStatus): boolean {
  return (
    a.status === b.status &&
    a.revision === b.revision &&
    a.evaluated === b.evaluated &&
    a.running === b.running &&
    a.omitted === b.omitted &&
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
