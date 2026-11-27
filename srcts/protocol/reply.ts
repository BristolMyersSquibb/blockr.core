// The block registry and the app's plugins of R's reply to `hello`, each by the
// id of its class as core's table holds it, so that core finds an entry by the
// registry id of a block or by the id of a plugin's class.
// R sends both as lists of entries that each hold the package and the id of
// their class, and each map keeps the order of its list.
// A reply's maps are made once, as first asked for.

import type { PluginClassId, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { interned } from "../util/index.ts";
import type { HelloReply, PluginEntry } from "./messages.ts";

const registries = new WeakMap<readonly RegistryEntry[], ReadonlyMap<RegistryId, RegistryEntry>>();
const plugins = new WeakMap<readonly PluginEntry[], ReadonlyMap<PluginClassId, PluginEntry>>();

// The entries of the reply's block registry by registry id, in R's order.
export function blockRegistry(reply: Pick<HelloReply, "block_registry">): ReadonlyMap<RegistryId, RegistryEntry> {
  const list = reply.block_registry;
  let map = registries.get(list);
  if (map === undefined) {
    map = new Map(list.map((entry) => [interned(entry.package, entry.id), entry]));
    registries.set(list, map);
  }
  return map;
}

// The entries of the app's plugins by the ids of their classes, in the order
// of the rows that `serve()` takes.
export function appPlugins(reply: Pick<HelloReply, "plugins">): ReadonlyMap<PluginClassId, PluginEntry> {
  const list = reply.plugins;
  let map = plugins.get(list);
  if (map === undefined) {
    map = new Map(list.map((entry) => [interned(entry.package, entry.id), entry]));
    plugins.set(list, map);
  }
  return map;
}
