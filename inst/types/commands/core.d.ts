import type { Store } from "../store/index.ts";
import type { BlockId, Board, LinkId, PluginClassId, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Command } from "./commands.ts";
export interface CoreCommandsOptions {
    readonly locked?: boolean;
}
export declare function coreCommands(store: Store, options?: CoreCommandsOptions): Command<Json, unknown>[];
export declare function freeInputs(board: Pick<Board, "blocks" | "links">, registry: Readonly<Record<RegistryId, RegistryEntry>>, to: BlockId, except?: LinkId): readonly string[] | undefined;
export declare function blockName(registryId: RegistryId): string;
export declare function pluginName(id: PluginClassId): string;
