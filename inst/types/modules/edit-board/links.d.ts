import type { ReactElement } from "react";
import type { BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { RegistryEntry } from "../../types/registry.ts";
import type { ComboOption } from "../../ui/index.ts";
type Registry = Readonly<Record<string, RegistryEntry>>;
export declare function LinksSection({ core, board, locked }: {
    core: CoreApi;
    board: Board;
    locked: boolean;
}): ReactElement;
export declare function blockOption(board: Pick<Board, "blocks">, registry: Registry, id: BlockId): ComboOption;
export declare function blockName(board: Pick<Board, "blocks">, block: BlockId): string;
export {};
