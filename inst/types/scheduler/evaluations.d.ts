import type { Graph } from "../graph/index.ts";
import type { Recipe, Reply } from "../protocol/index.ts";
import type { BlockId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Need } from "./needs.ts";
import type { Pending } from "./plan.ts";
import type { BlockStatus, Outcome } from "./status.ts";
export interface Plain {
    readonly blocks: ReadonlySet<BlockId>;
    readonly priority: number;
}
export interface WithParams {
    readonly priority: number;
    readonly params: Record<string, Json>;
    readonly parts: ReadonlyMap<BlockId, Part>;
}
export type OneTime = Plain | WithParams;
export interface Part {
    readonly recipe: Recipe | undefined;
    readonly sent: number | undefined;
    readonly result: BlockStatus | undefined;
}
export type Settled = {
    readonly statuses: Record<BlockId, BlockStatus>;
} | {
    readonly error: unknown;
};
export interface Next {
    readonly oneTimes: ReadonlyMap<number, OneTime>;
    readonly settled: readonly (readonly [number, Settled])[];
}
export declare function oneTimeOf(blocks: readonly BlockId[], params: Record<string, Json> | undefined, priority: number, statuses: ReadonlyMap<BlockId, BlockStatus>): OneTime | Error;
export declare function settle(oneTimes: ReadonlyMap<number, OneTime>, statuses: ReadonlyMap<BlockId, BlockStatus>): Next;
export declare function left(oneTimes: ReadonlyMap<number, OneTime>, gone: ReadonlySet<BlockId>): Next;
export declare function sent(oneTimes: ReadonlyMap<number, OneTime>, id: number, block: BlockId, evaluation: {
    readonly id: number;
    readonly recipe: Recipe;
}): ReadonlyMap<number, OneTime>;
export declare function answered(oneTimes: ReadonlyMap<number, OneTime>, id: number, block: BlockId, reply: Reply, outcome: Outcome | undefined, revision: number): Next;
export declare function neededBy(oneTimes: ReadonlyMap<number, OneTime>, graph: Graph): Need[];
export declare function pendingOf(oneTimes: ReadonlyMap<number, OneTime>): Pending[];
