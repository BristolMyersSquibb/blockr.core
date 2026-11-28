import type { PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { PluginClass } from "./registrations.ts";
export type PluginStates<T = Json> = Readonly<Record<string, Readonly<Record<string, T>>>>;
export type Role = "frontend" | "storage";
export declare function inRole(plugins: Iterable<PluginClassId>, registeredClass: (id: PluginClassId) => PluginClass | undefined, role: Role): PluginClassId[];
export declare function keptPlugins(board: PluginStates<unknown>, app: ReadonlyMap<PluginClassId, unknown>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<string, Record<string, Json>>;
export declare function droppedPlugins(board: PluginStates<unknown>, app: ReadonlyMap<PluginClassId, unknown>, registeredClass: (id: PluginClassId) => PluginClass | undefined): PluginClassId[];
export declare function pluginsWithDefaults(kept: PluginStates, app: ReadonlyMap<PluginClassId, unknown>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<string, Record<string, Json>>;
