import type { BlockId, Board, StackId } from "../../types/board.ts";
export type Item = {
    readonly stack: StackId;
    readonly blocks: readonly BlockId[];
} | {
    readonly block: BlockId;
};
export declare function layoutItems(board: Pick<Board, "blocks" | "stacks">): Item[];
