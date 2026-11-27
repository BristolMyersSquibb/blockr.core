import type { BlockId, Board } from "../types/board.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import type { RegistryEntry } from "../types/registry.ts";
export declare function blockName(board: Pick<Board, "blocks">, block: BlockId): string;
export declare function classInputs(board: Pick<Board, "blocks">, registry: BlockRegistry, block: BlockId): readonly string[];
export declare function isVariadicBlock(board: Pick<Board, "blocks">, registry: BlockRegistry, block: BlockId): boolean;
export declare function namesInto(board: Pick<Board, "links">, to: BlockId, except?: string): readonly string[];
export declare function inputNameError(value: string, taken: readonly string[]): string | undefined;
export interface ClassItem {
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly icon: string;
    readonly pkg: string | null;
    readonly category: string;
    readonly keywords: readonly string[];
}
export declare function blockClasses(entries: readonly RegistryEntry[]): ClassItem[];
