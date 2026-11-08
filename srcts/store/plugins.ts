// What the store and core's commands share about a board's plugins: the
// plugins that a board takes over from the app's opening board, which of them
// is a frontend or a storage plugin, of which a board holds at most one each,
// and the state that a plugin starts with
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#which-plugins-a-board-has).

import type { Board, Plugin, PluginClassId, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { frozenCopy, packageOf } from "../util/index.ts";
import { mintId } from "./ids.ts";
import type { PluginClass } from "./registrations.ts";

// The roles of which a board holds at most one plugin each: a frontend, whose
// class has `frontend`, and a storage plugin, whose class has `storage`.
export type Role = "frontend" | "storage";

export const ROLES: readonly Role[] = ["frontend", "storage"];

export function rolesOf(def: PluginClass): Role[] {
  return ROLES.filter((role) => def[role] !== undefined);
}

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

// The name of a plugin class without its package and without `_plugin`,
// "dag" for "blockr.dag-dag_plugin", which a plugin that its list names no id
// for takes as its id.
export function shortName(id: PluginClassId): string {
  const pkg = packageOf(id);
  const name = pkg === null ? id : id.slice(pkg.length + 1);
  return name.replace(/_plugin$/, "");
}

// The plugins that the board takes over from the plugins of the app's opening
// board, by id, in the order of the opening board:
// - Each plugin of a class that the board holds no plugin of, where the app's
//   packages register the class, so that a board that holds no document of
//   blockr.md takes over each document of the opening board.
// - The opening board's frontend or storage plugin, which it takes only where
//   it holds none.
// A plugin that it takes over keeps its class, its id and its name, with its
// class's default state, since what the opening board's plugins keep is about
// the opening board's blocks.
// Where the board holds another plugin under that plugin's id, the plugin it
// takes over gets an id that the client mints.
export function takeOver(
  board: Pick<Board, "plugins">,
  opening: Readonly<Record<PluginId, Plugin>>,
  classOf: (id: PluginClassId) => PluginClass | undefined,
): Record<PluginId, Plugin> {
  const held = new Set(Object.values(board.plugins).map((plugin) => plugin.class));
  const filled = new Set(ROLES.filter((role) => inRole(board.plugins, classOf, role).length > 0));
  const adds: Record<PluginId, Plugin> = {};
  const holds = (id: string) => Object.hasOwn(board.plugins, id) || Object.hasOwn(adds, id);

  for (const [id, plugin] of Object.entries(opening)) {
    const def = classOf(plugin.class);
    if (def === undefined) continue;
    const roles = rolesOf(def);
    if (roles.length > 0 ? roles.some((role) => filled.has(role)) : held.has(plugin.class)) continue;
    for (const role of roles) filled.add(role);
    adds[holds(id) ? mintId(holds) : id] = { class: plugin.class, name: plugin.name, state: defaultState(def) };
  }

  return adds;
}
