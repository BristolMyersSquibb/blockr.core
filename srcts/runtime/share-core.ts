// Puts core's React SDK, its binding and its controls on the page, beside the
// copy of React that ./share-react.ts put there, under the same global, and
// then freezes the global.
// The build preset maps a module's imports of them to these copies, so that
// every module runs the SDK of the core it runs on.
// Core's runtime draws with the same copies, since its bundle holds them.

import * as binding from "../binding/index.ts";
import * as controls from "../controls/index.ts";
import * as sdk from "../react/index.ts";
import { SHARED_GLOBAL } from "../shared-global/index.ts";
import type { REACT_ENTRIES, SHARED_ENTRIES } from "../shared-global/index.ts";

const entries: Record<Exclude<(typeof SHARED_ENTRIES)[number], (typeof REACT_ENTRIES)[number]>, unknown> = {
  "@blockr/core/react": sdk,
  "@blockr/core/binding": binding,
  "@blockr/core/controls": controls,
};

const shared: unknown = Reflect.get(globalThis, SHARED_GLOBAL);

if (typeof shared === "object" && shared !== null && !Object.isFrozen(shared)) {
  Object.freeze(Object.assign(shared, entries));
}
