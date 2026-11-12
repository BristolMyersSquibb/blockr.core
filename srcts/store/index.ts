// The store, which holds a board and through which every change to it passes,
// with what the command registry, saving, the scheduler and `CoreApi` build on.

export { createStore } from "./store.ts";
export type { Change, Store, StoreOptions } from "./store.ts";
export { isDeltas } from "./delta.ts";
export type { BlockMod, Delta, Origin, Patch, Values } from "./delta.ts";
export { pathFromSegments, pluginClassFromPath, segmentsFromPath } from "./patch.ts";
export { inRole } from "./plugins.ts";
export type {
  Migration,
  Option,
  Options,
  PluginClass,
  Plugins,
  Storage,
} from "./registrations.ts";
export { HEX_COLOR, stateFailures } from "./check.ts";
export { FORMAT } from "./open.ts";
export type { Dropped } from "./open.ts";
export { isId, isRandId, mintId, RAND_IDS, sentenceCase } from "./ids.ts";
export type { RandId } from "./ids.ts";
export { StoreError } from "./error.ts";
