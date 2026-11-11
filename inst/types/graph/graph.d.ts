import type { BlockId, Board, LinkId } from "../types/board.ts";
export interface Graph {
    readonly order: readonly BlockId[];
    linksInto(id: BlockId): readonly LinkId[];
    upstream(id: BlockId): readonly BlockId[];
    downstream(id: BlockId): readonly BlockId[];
    withUpstream(ids: Iterable<BlockId>): ReadonlySet<BlockId>;
    withDownstream(ids: Iterable<BlockId>): ReadonlySet<BlockId>;
    cycle(): readonly BlockId[] | undefined;
}
export declare function createGraph(board: Pick<Board, "blocks" | "links">): Graph;
