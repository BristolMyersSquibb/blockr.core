// What the store and core share about the app's plugins, which R names in its
// reply to `hello` and which run on every board that the app opens, each under
// the id of its class: which of them is a frontend or a storage plugin, and the
// state that the board holds for each of them as it opens.
// A board holds the state of its plugins by the package of each one's class
// and then by the class's id within it, as { "blockr.dag": { dag_plugin: … } }.

import type { PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { frozenCopy, nestedEntries, nestedRecord, nestedValue } from "../util/index.ts";
import type { PluginClass } from "./registrations.ts";

// The state of a board's plugins, by package and then by the id of each
// plugin's class.
export type PluginStates<T = Json> = Readonly<Record<string, Readonly<Record<string, T>>>>;

// The roles of which an app runs one plugin and one at most: a frontend, whose
// class has `frontend`, and a storage plugin, whose class has `storage`.
export type Role = "frontend" | "storage";

// The ids of the classes of the plugins in the role, among the plugins given
// by the ids of their classes, of the classes that `registeredClass` gives.
export function inRole(
  plugins: Iterable<PluginClassId>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
  role: Role,
): PluginClassId[] {
  return [...plugins].filter((id) => registeredClass(id)?.[role] !== undefined);
}

// The state that the board keeps of what it holds as it opens in the app: that
// of each of the app's plugins whose class a package registers and has
// `state`.
export function keptPlugins(
  board: PluginStates<unknown>,
  app: ReadonlyMap<PluginClassId, unknown>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): Record<string, Record<string, Json>> {
  return nestedRecord(
    nestedEntries(board).filter(
      (entry): entry is [PluginClassId, Json] => app.has(entry[0]) && registeredClass(entry[0])?.state !== undefined,
    ),
  );
}

// The ids of the classes whose state the board drops as it opens in the app,
// since the app runs no plugin of the class or no package registers it, which
// the store names. The state that the board holds for a plugin of the app
// whose class keeps none goes too, unnamed, since the plugin runs all the same.
export function droppedPlugins(
  board: PluginStates<unknown>,
  app: ReadonlyMap<PluginClassId, unknown>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): PluginClassId[] {
  return nestedEntries(board)
    .map(([id]) => id)
    .filter((id) => !app.has(id) || registeredClass(id) === undefined);
}

// The state of the plugins of a board as it opens in the app, in the order of
// the app's plugins: for each of the app's plugins whose class a package
// registers and has `state`, the state that the board keeps of it, and else
// its class's default state.
export function pluginsWithDefaults(
  kept: PluginStates,
  app: ReadonlyMap<PluginClassId, unknown>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): Record<string, Record<string, Json>> {
  const plugins: [PluginClassId, Json][] = [];

  for (const id of app.keys()) {
    const state = registeredClass(id)?.state;
    if (state === undefined) continue;
    const held = nestedValue(kept, id);
    plugins.push([id, held === undefined ? frozenCopy(state.default) : held]);
  }

  return nestedRecord(plugins);
}
