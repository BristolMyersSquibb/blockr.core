// The one object for each pair of a package and an id, the ids of block
// classes, plugin classes, renderers, modules, commands and board actions
// (Id, ../types/board.ts).
// A table holds each object that interned() made, by package and then by id,
// so that it gives the same object for the same pair, as interning does: as
// Java interns strings, and as the TC39 proposal for composites makes
// composites (https://github.com/tc39/proposal-composites).
// So ===, a Set, a Map and includes() compare them as they compare strings,
// and a React dependency or a signal that holds one changes only where the pair
// does.
// Core makes each id that it holds here: from what R sends, from what a JSON
// board holds, and from what a caller passes.
// The table holds one object for each pair that the page has met, a few
// hundred, and frees none.
// The page has one table, in core's runtime, which packages reach through
// CoreApi's id() and a block's host through its own id(), since a copy of this
// module would make objects of its own, which would never be === to core's.
// So the build fails where a file other than core's runtime would bundle it,
// as a module or a library of core's would where it made an id itself
// (../build/build.ts).

import type { Id } from "../types/board.ts";
import { isPackageName } from "./util.ts";

// Pure, so that a file that bundles none of the functions below holds no
// table.
const made = /* @__PURE__ */ new Map<string | null, Map<string, Id<string | null>>>();

// The id of `id` in `pkg`, the same object for the same pair, or for a pkg of
// null, the id of what an app script defines.
// It throws a TypeError for a package that is not named as R names one and
// for an id that is no string or is empty.
export function interned<P extends string | null>(pkg: P, id: string): Id<P> {
  if (pkg !== null && !isPackageName(pkg)) {
    throw new TypeError(`The package ${JSON.stringify(pkg)} is not named as R names a package.`);
  }
  if (typeof id !== "string" || id === "") {
    throw new TypeError(`The id ${JSON.stringify(id)} of ${pkg ?? "an app script"} is no non-empty string.`);
  }
  let ids = made.get(pkg);
  if (ids === undefined) {
    ids = new Map();
    made.set(pkg, ids);
  }
  let found = ids.get(id);
  if (found === undefined) {
    found = Object.freeze({ package: pkg, id }) as unknown as Id<string | null>;
    ids.set(id, found);
  }
  return found as Id<P>;
}
