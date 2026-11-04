import type { Graph } from "../graph/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { Inputs } from "./recipes.ts";
export declare function counter(last?: number): () => number;
export declare function opening(graph: Graph, next: () => number): Map<BlockId, number>;
export declare function revise(revisions: ReadonlyMap<BlockId, number>, before: {
    readonly board: Pick<Board, "blocks">;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
}, after: {
    readonly board: Pick<Board, "blocks">;
    readonly inputs: ReadonlyMap<BlockId, Inputs>;
    readonly graph: Graph;
}, removed: ReadonlySet<BlockId>, next: () => number): {
    revisions: Map<BlockId, number>;
    gone: BlockId[];
};
export declare function bump(graph: Graph, revisions: ReadonlyMap<BlockId, number>, roots: ReadonlySet<BlockId>, next: () => number): Map<BlockId, number>;
