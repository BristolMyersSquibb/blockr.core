// What the store and core share about the app's plugins, which R names in its
// reply to `hello` and which run on every board that the app opens, each under
// the id of its class: which of them is a frontend or a storage plugin, and the
// state that the board holds for each of them as it opens.

import type { PluginEntry } from "../protocol/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { frozenCopy, own } from "../util/index.ts";
import type { PluginClass } from "./registrations.ts";

// The roles of which an app runs one plugin and one at most: a frontend, whose
// class has `frontend`, and a storage plugin, whose class has `storage`.
export type Role = "frontend" | "storage";

// The ids of the classes of the plugins in the role, among the plugins given
// by the ids of their classes, of the classes that `registeredClass` gives.
export function inRole(
  plugins: Readonly<Record<PluginClassId, unknown>>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
  role: Role,
): PluginClassId[] {
  return Object.keys(plugins).filter((id) => registeredClass(id)?.[role] !== undefined);
}

// The state that the board keeps of what it holds as it opens in the app, by
// the ids of the plugins' classes: that of each of the app's plugins whose
// class a package registers and has `state`.
export function keptPlugins(
  board: Readonly<Record<PluginClassId, unknown>>,
  app: Readonly<Record<PluginClassId, PluginEntry>>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): Record<PluginClassId, Json> {
  return Object.fromEntries(
    Object.entries(board).filter(
      (entry): entry is [PluginClassId, Json] =>
        Object.hasOwn(app, entry[0]) && registeredClass(entry[0])?.state !== undefined,
    ),
  );
}

// The ids of the classes whose state the board drops as it opens in the app,
// since the app runs no plugin of the class or no package registers it, which
// the store names. The state that the board holds for a plugin of the app
// whose class keeps none goes too, unnamed, since the plugin runs all the same.
export function droppedPlugins(
  board: Readonly<Record<PluginClassId, unknown>>,
  app: Readonly<Record<PluginClassId, PluginEntry>>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): PluginClassId[] {
  return Object.keys(board).filter((id) => !Object.hasOwn(app, id) || registeredClass(id) === undefined);
}

// The state of the plugins of a board as it opens in the app, by the ids of
// their classes, in the order of the app's plugins: for each of the app's
// plugins whose class a package registers and has `state`, the state that the
// board keeps of it, and else its class's default state.
export function pluginsWithDefaults(
  kept: Readonly<Record<PluginClassId, Json>>,
  app: Readonly<Record<PluginClassId, PluginEntry>>,
  registeredClass: (id: PluginClassId) => PluginClass | undefined,
): Record<PluginClassId, Json> {
  const plugins: Record<PluginClassId, Json> = {};

  for (const id of Object.keys(app)) {
    const state = registeredClass(id)?.state;
    if (state === undefined) continue;
    const held = own(kept, id);
    plugins[id] = held === undefined ? frozenCopy(state.default) : held;
  }

  return plugins;
}
