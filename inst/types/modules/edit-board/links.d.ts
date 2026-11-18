import type { ReactElement } from "react";
import type { BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { RegistryEntry } from "../../types/registry.ts";
type Registry = Readonly<Record<string, RegistryEntry>>;
export declare function LinksSection({ core, board, locked }: {
    core: CoreApi;
    board: Board;
    locked: boolean;
}): ReactElement;
export interface BlockItem {
    readonly id: BlockId;
    readonly name: string;
    readonly detail: string;
}
export declare function blockItem(board: Pick<Board, "blocks">, registry: Registry, id: BlockId): BlockItem;
export declare function blockTexts(item: BlockItem): readonly string[];
export declare function blockRow(item: BlockItem): ReactElement;
export declare function blockName(board: Pick<Board, "blocks">, block: BlockId): string;
export {};
