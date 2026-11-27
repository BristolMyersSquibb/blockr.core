import type { ReactElement } from "react";
import type { BlockId, Board } from "../types/board.ts";
import type { BlockRegistry } from "../types/core-api.ts";
export interface BlockItem {
    readonly id: BlockId;
    readonly name: string;
    readonly detail: string;
}
export declare function blockItem(board: Pick<Board, "blocks">, registry: BlockRegistry, id: BlockId): BlockItem;
export declare function blockTexts(item: BlockItem): readonly string[];
export declare function blockRow(item: BlockItem): ReactElement;
