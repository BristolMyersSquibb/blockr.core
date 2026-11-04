// The store, which holds a board and through which every change to it passes,
// with what the command registry, saving, the scheduler and `CoreApi` build on.

export { createStore } from "./store.ts";
export type { Change, Store, StoreOptions } from "./store.ts";
export { isDeltas, isValueDelta } from "./delta.ts";
export type {
  BlockMod,
  CollectionDelta,
  Delta,
  Entry,
  KeyChange,
  KeyDelta,
  Origin,
} from "./delta.ts";
export type {
  Attribute,
  Migration,
  Option,
  Options,
  Saved,
  StateKey,
} from "./registrations.ts";
export { BASIC_FRONT_END, FORMAT } from "./open.ts";
export type { Dropped } from "./open.ts";
export { isId, isRandId, mintId, RAND_IDS, sentenceCase } from "./ids.ts";
export type { RandId } from "./ids.ts";
export { StoreError } from "./error.ts";
