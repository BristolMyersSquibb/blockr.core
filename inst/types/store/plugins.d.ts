import type { PluginEntry } from "../protocol/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { PluginClass } from "./registrations.ts";
export type Role = "frontend" | "storage";
export declare function inRole(plugins: Readonly<Record<PluginClassId, unknown>>, registeredClass: (id: PluginClassId) => PluginClass | undefined, role: Role): PluginClassId[];
export declare function keptPlugins(board: Readonly<Record<PluginClassId, unknown>>, app: Readonly<Record<PluginClassId, PluginEntry>>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<PluginClassId, Json>;
export declare function droppedPlugins(board: Readonly<Record<PluginClassId, unknown>>, app: Readonly<Record<PluginClassId, PluginEntry>>, registeredClass: (id: PluginClassId) => PluginClass | undefined): PluginClassId[];
export declare function pluginsWithDefaults(kept: Readonly<Record<PluginClassId, Json>>, app: Readonly<Record<PluginClassId, PluginEntry>>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<PluginClassId, Json>;
