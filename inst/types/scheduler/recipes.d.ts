import type { Graph } from "../graph/index.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
export interface Input {
    readonly input: string;
    readonly block?: BlockId;
}
export interface Inputs {
    readonly recipe: readonly Input[];
    readonly unlinked: readonly string[];
}
export declare function inputsOf(board: Pick<Board, "blocks" | "links">, graph: Graph, registry: Readonly<Record<RegistryId, RegistryEntry>>, bound: ReadonlySet<string>): Map<BlockId, Inputs>;
export declare function sameInputs(a: readonly Input[], b: readonly Input[]): boolean;
