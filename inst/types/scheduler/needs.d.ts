import type { Graph } from "../graph/index.ts";
import type { BlockId } from "../types/board.ts";
export interface Need {
    readonly blocks: Iterable<BlockId>;
    readonly priority: number;
}
export declare function needsOf(graph: Graph, needs: Iterable<Need>): Map<BlockId, number>;
