import type { Graph } from "../graph/index.ts";
import type { BlockId } from "../types/board.ts";
import type { Inputs } from "./recipes.ts";
import type { BlockStatus } from "./status.ts";
export declare const GIVE_WAY = 5000;
export interface Sent {
    readonly id: number;
    readonly at: number;
    readonly block: BlockId;
    readonly revision: number;
    readonly oneTime: {
        readonly id: number;
        readonly priority: number;
    } | undefined;
    readonly background: boolean;
    readonly cancelled: boolean;
}
export interface Asked {
    readonly id: number;
    readonly at: number;
}
export interface Pending {
    readonly oneTime: number;
    readonly block: BlockId;
    readonly priority: number;
}
export interface Plan {
    readonly cancel: readonly number[];
    readonly send: readonly {
        readonly block: BlockId;
        readonly oneTime?: number;
        readonly background?: true;
    }[];
    readonly reject: readonly {
        readonly oneTime: number;
        readonly error: Error;
    }[];
    readonly idleSince: number | undefined;
    readonly wakeAt: number | undefined;
}
export declare function evaluationPlan(input: {
    readonly graph: Graph;
    readonly revisions: ReadonlyMap<BlockId, number>;
    readonly statuses: ReadonlyMap<BlockId, BlockStatus>;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
    readonly needs: ReadonlyMap<BlockId, number>;
    readonly sent: Iterable<Sent>;
    readonly asked: Iterable<Asked>;
    readonly pending: Iterable<Pending>;
    readonly queue: number;
    readonly interrupt: boolean;
    readonly holding: boolean;
    readonly now: number;
    readonly idleSince: number | undefined;
    readonly delay: number;
}): Plan;
