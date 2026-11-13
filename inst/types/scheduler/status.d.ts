import type { Graph } from "../graph/index.ts";
import type { Condition, Reply } from "../protocol/index.ts";
import type { BlockId } from "../types/board.ts";
import type { Inputs } from "./recipes.ts";
export type Status = "unevaluated" | "stale" | "waiting" | "invalid" | "ready" | "failed";
export interface BlockStatus {
    readonly status: Status;
    readonly revision: number;
    readonly evaluated: number | null;
    readonly running: boolean;
    readonly conditions: readonly Condition[];
    readonly stdout: string;
    readonly stderr: string;
    readonly summary: string | null;
    readonly reason: string | null;
}
export interface Outcome {
    readonly status: "ok" | "error" | "invalid" | "waiting";
    readonly revision: number;
    readonly summary: string | null;
    readonly reason: string | null;
    readonly conditions: readonly Condition[];
    readonly stdout: string;
    readonly stderr: string;
}
export declare function replyOutcome(reply: Reply, revision: number): Outcome | undefined;
export declare function unsent(revision: number, message: string): Outcome;
export declare function unevaluated(revision: number, running: boolean): BlockStatus;
export declare function replied(outcome: Outcome, revision: number, running: boolean): BlockStatus;
export declare function waiting(why: string, revision: number, evaluated: number | null, running: boolean): BlockStatus;
export declare function waitingFor(what: {
    unlinked: readonly string[];
} | {
    upstream: readonly BlockId[];
}): string;
export declare function sameStatus(a: BlockStatus, b: BlockStatus): boolean;
export declare function blockStatuses(input: {
    readonly graph: Graph;
    readonly revisions: ReadonlyMap<BlockId, number>;
    readonly outcomes: ReadonlyMap<BlockId, Outcome>;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
    readonly running: ReadonlySet<BlockId>;
    readonly before: ReadonlyMap<BlockId, BlockStatus>;
}): {
    statuses: Map<BlockId, BlockStatus>;
    changed: BlockId[];
};
export declare function keptWaiting(inputs: Inputs | undefined, statuses: ReadonlyMap<BlockId, BlockStatus>): string | undefined;
export declare function waitingError(id: BlockId, status: BlockStatus): Error;
