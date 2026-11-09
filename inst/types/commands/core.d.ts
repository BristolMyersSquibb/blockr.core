import type { RandId, Store } from "../store/index.ts";
import type { BlockId, Board, LinkId, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Command } from "./commands.ts";
export declare function coreCommands(store: Store, randId?: RandId): Command<Json, unknown>[];
export declare function freeInputs(board: Pick<Board, "blocks" | "links">, registry: Readonly<Record<RegistryId, RegistryEntry>>, to: BlockId, except?: LinkId): readonly string[] | undefined;
export declare function blockName(registryId: RegistryId): string;
