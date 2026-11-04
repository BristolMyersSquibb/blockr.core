import type { Store } from "../store/index.ts";
import type { RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Command } from "./commands.ts";
export declare function coreCommands(store: Store): Command<Json, unknown>[];
export declare function blockName(registryId: RegistryId): string;
