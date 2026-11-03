// The store, which holds a board and through which every change to it passes,
// with what the command registry, saving, the scheduler and `CoreApi` build on.

export { createStore } from "./store.ts";
export type { Change, Store, StoreOptions } from "./store.ts";
export type { Delta, Entry, KeyDelta, Origin } from "./delta.ts";
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
export { isId, mintId, sentenceCase } from "./ids.ts";
export { StoreError } from "./error.ts";
