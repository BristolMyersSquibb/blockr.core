import type { PluginClassId, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { HelloReply, PluginEntry } from "./messages.ts";
export declare function blockRegistry(reply: Pick<HelloReply, "block_registry">): ReadonlyMap<RegistryId, RegistryEntry>;
export declare function appPlugins(reply: Pick<HelloReply, "plugins">): ReadonlyMap<PluginClassId, PluginEntry>;
