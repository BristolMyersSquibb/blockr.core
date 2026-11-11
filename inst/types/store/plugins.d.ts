import type { Plugin, PluginClassId, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { PluginClass } from "./registrations.ts";
export type Role = "frontend" | "storage";
export declare const ROLES: readonly Role[];
export declare function inRole(plugins: Readonly<Record<PluginId, Plugin>>, registeredClass: (id: PluginClassId) => PluginClass | undefined, role: Role): PluginId[];
export declare function defaultState(def: PluginClass): Json;
export declare function keptPlugins(board: Readonly<Record<PluginId, unknown>>, opening: Readonly<Record<PluginId, Plugin>>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<PluginId, Plugin>;
export declare function appPlugins(board: Readonly<Record<PluginId, unknown>>, opening: Readonly<Record<PluginId, Plugin>>, registeredClass: (id: PluginClassId) => PluginClass | undefined): Record<PluginId, Plugin>;
