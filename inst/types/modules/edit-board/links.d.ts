import type { ReactElement } from "react";
import type { Choice } from "../../binding/index.ts";
import type { BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { RegistryEntry } from "../../types/registry.ts";
type Registry = Readonly<Record<string, RegistryEntry>>;
export declare function LinksSection({ core, board, locked }: {
    core: CoreApi;
    board: Board;
    locked: boolean;
}): ReactElement;
export declare function blockOption(board: Pick<Board, "blocks">, registry: Registry, id: BlockId): Choice;
export declare function blockName(board: Pick<Board, "blocks">, block: BlockId): string;
export {};
