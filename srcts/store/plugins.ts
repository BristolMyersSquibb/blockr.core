// What the store and core share about a board's plugins: the plugins of the
// app's opening board, which set the plugins that every board holds as it
// opens in the app, which of them is a frontend or a storage plugin, of which
// a board holds at most one each, and the state that a plugin starts with.

import type { Plugin, PluginClassId, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { frozenCopy, isObject, own } from "../util/index.ts";
import type { PluginClass } from "./registrations.ts";

// The roles of which a board holds at most one plugin each: a frontend, whose
// class has `frontend`, and a storage plugin, whose class has `storage`.
export type Role = "frontend" | "storage";

export const ROLES: readonly Role[] = ["frontend", "storage"];

// The plugins of the board in each role, by the ids of the plugins, of the
// classes that `classOf` gives.
export function inRole(
  plugins: Readonly<Record<PluginId, Plugin>>,
  classOf: (id: PluginClassId) => PluginClass | undefined,
  role: Role,
): PluginId[] {
  return Object.entries(plugins)
    .filter(([, plugin]) => classOf(plugin.class)?.[role] !== undefined)
    .map(([id]) => id);
}

// The state that a plugin of the class starts with: its class's default state,
// or an empty object for a class that gives none.
export function defaultState(def: PluginClass): Json {
  return frozenCopy(def.state === undefined ? {} : def.state.default);
}

// The plugins of a board that it keeps as it opens in the app: those that the
// app's opening board holds under the same id and of the same class, where a
// package registers the class, each with its name and its state.
export function keptPlugins(
  board: Readonly<Record<PluginId, unknown>>,
  opening: Readonly<Record<PluginId, Plugin>>,
  classOf: (id: PluginClassId) => PluginClass | undefined,
): Record<PluginId, Plugin> {
  return Object.fromEntries(
    Object.entries(board).filter(
      (entry): entry is [PluginId, Plugin] =>
        isObject(entry[1]) &&
        typeof entry[1].class === "string" &&
        own(opening, entry[0])?.class === entry[1].class &&
        classOf(entry[1].class) !== undefined,
    ),
  );
}

// The plugins of a board as it opens in the app, by id, in the order of the
// app's opening board, which sets them: each plugin of the opening board whose
// class a package registers, as the board holds it where it keeps it, and else
// under its id and name with its class's default state, since what the opening
// board's plugins keep is about the opening board's blocks.
export function appPlugins(
  board: Readonly<Record<PluginId, unknown>>,
  opening: Readonly<Record<PluginId, Plugin>>,
  classOf: (id: PluginClassId) => PluginClass | undefined,
): Record<PluginId, Plugin> {
  const kept = keptPlugins(board, opening, classOf);
  const plugins: Record<PluginId, Plugin> = {};

  for (const [id, plugin] of Object.entries(opening)) {
    const def = classOf(plugin.class);
    if (def === undefined) continue;
    plugins[id] = own(kept, id) ?? { class: plugin.class, name: plugin.name, state: defaultState(def) };
  }

  return plugins;
}
