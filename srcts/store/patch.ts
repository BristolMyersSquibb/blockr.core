// Patches, the changes to the board's plugins and inside their state,
// applied as JSON Patch applies its `add`, `remove` and `replace` (RFC 6902):
// at a path from the board's root, without knowing the state's structure.
// A patch copies only the objects and arrays along its path,
// so that the new board shares the rest with the old one,
// and returns the patch that takes it back, which undo applies.
// A path holds where a patch changes something, however deeply nested, and an
// index holds where an element sits in its array.
// Where a member of an object sits among the object's members, a path does
// not hold.
// So a patch that removes a member names the member that followed it, which
// the store keeps for undo, and the patch that takes the removal back adds the
// member before that one.

import { isObject, jsonEqual, own, without } from "../util/index.ts";
import type { Patch } from "./delta.ts";

// The segments of a JSON Pointer, its reference tokens in RFC 6901, reading
// "~1" as "/" and "~0" as "~", as "/plugins/dock" gives ["plugins", "dock"].
// They are undefined where the text is no pointer below the board's root: where
// it is empty, does not start with "/", or holds a "~" that escapes nothing.
export function segmentsFromPath(path: string): string[] | undefined {
  if (!path.startsWith("/")) return undefined;
  const segments = path.slice(1).split("/");
  if (segments.some((segment) => /~(?![01])/.test(segment))) return undefined;
  return segments.map((segment) => segment.replaceAll("~1", "/").replaceAll("~0", "~"));
}

// The plugin that a path leads into, "dag" for "/plugins/dag/state/positions/a"
// and for "/plugins/dag", or undefined where it leads into no plugin.
export function pluginIdFromPath(path: string): string | undefined {
  const [boardProperty, plugin] = segmentsFromPath(path) ?? [];
  return boardProperty === "plugins" ? plugin : undefined;
}

// The JSON Pointer to the segments.
export function pathFromSegments(segments: readonly string[]): string {
  return segments.map((segment) => `/${segment.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}

// Whether one path lies at or under the other in the board, so that a patch at
// one changes what is at the other.
// A segment that indexes an array, as an index or as "-" for its end, names no
// element, since a patch at an index changes whatever the array holds there by
// then.
// So two paths that both index one array never overlap, even at one index, and
// two patches into the array both apply.
// The board tells which segments index an array, since a member of an object
// may have a name made of digits.
export function overlap(a: readonly string[], b: readonly string[], board: unknown): boolean {
  let at = board;
  for (const [i, segment] of a.slice(0, b.length).entries()) {
    if (Array.isArray(at) || segment !== b[i]) return false;
    at = isObject(at) ? own(at, segment) : undefined;
  }
  return true;
}

// What a shape check of a patch finds wrong with it, each in a clause,
// as a delta built in plain JS or by a model may get it wrong.
export function malformed(patch: unknown): string[] {
  if (!isObject(patch)) return ["is no object"];
  const problems: string[] = [];
  const { op, path, edit } = patch;
  if (op !== "add" && op !== "remove" && op !== "replace") {
    problems.push(`has the op ${JSON.stringify(op)}, where it takes "add", "remove" or "replace"`);
  }
  if (typeof path !== "string" || segmentsFromPath(path) === undefined) {
    problems.push(`has the path ${JSON.stringify(path)}, which is no JSON Pointer below the board's root`);
  }
  if ((op === "add" || op === "replace") && patch.value === undefined) {
    problems.push("has no value");
  }
  if (edit !== undefined && (typeof edit !== "number" || !Number.isInteger(edit))) {
    problems.push(`has the edit id ${JSON.stringify(edit)}, which is no whole number`);
  }
  for (const field of Object.keys(patch)) {
    if (!["op", "path", "value", "edit"].includes(field)) {
      problems.push(`holds "${field}", which a patch does not keep`);
    }
  }
  return problems;
}

export type Applied =
  | {
      readonly next: Readonly<Record<string, unknown>>;
      // The patch that takes this one back, or none where it changes nothing.
      readonly inverse: Patch | undefined;
      // Where the patch removed a member of an object: the member that
      // followed it, or null where it was the last.
      readonly place?: string | null;
    }
  | { readonly failure: string };

// The board after the patch, whose path `segments` gives, with the patch that
// takes it back.
// An `add` of a member that an object lacks puts the member before the one
// that `place` names, where the object holds that one, and last otherwise.
// Where the patch cannot apply, the failure that keeps it from applying, in a
// clause.
export function applyPatch(
  board: Readonly<Record<string, unknown>>,
  patch: Patch,
  segments: readonly string[],
  place?: string | null,
): Applied {
  const result = step(board, segments, patch, [], place);
  if (typeof result === "string") return { failure: result };
  return { ...result, next: result.next as Readonly<Record<string, unknown>> };
}

interface Step {
  readonly next: unknown;
  readonly inverse: Patch | undefined;
  readonly place?: string | null;
}

// The patch applied below `at`, which `done` leads to from the board's root.
function step(
  at: unknown,
  segments: readonly string[],
  patch: Patch,
  done: readonly string[],
  place: string | null | undefined,
): Step | string {
  const [segment = "", ...rest] = segments;

  if (Array.isArray(at)) {
    const items: readonly unknown[] = at;
    const last = rest.length === 0;
    const index = last && segment === "-" && patch.op === "add" ? items.length : arrayIndex(segment);
    const bound = last && patch.op === "add" ? items.length : items.length - 1;
    if (index === undefined || index > bound) {
      return `finds no element "${segment}" in the array at "${pathFromSegments(done)}"`;
    }
    const here = pathFromSegments([...done, String(index)]);

    if (!last) {
      const inner = step(items[index], rest, patch, [...done, String(index)], place);
      if (typeof inner === "string") return inner;
      if (inner.next === items[index]) return { ...inner, next: at };
      return { ...inner, next: spliced(items, index, 1, inner.next) };
    }

    const old = items[index];
    switch (patch.op) {
      case "add":
        return {
          next: spliced(items, index, 0, patch.value),
          inverse: { op: "remove", path: here },
        };
      case "remove":
        return {
          next: spliced(items, index, 1),
          inverse: { op: "add", path: here, value: old as Patch["value"] },
        };
      case "replace":
        if (jsonEqual(old, patch.value)) return { next: at, inverse: undefined };
        return {
          next: spliced(items, index, 1, patch.value),
          inverse: { op: "replace", path: here, value: old as Patch["value"] },
        };
    }
  }

  if (!isObject(at)) {
    return `finds neither an object nor an array at "${pathFromSegments(done)}"`;
  }

  const has = Object.hasOwn(at, segment);
  const old = has ? at[segment] : undefined;
  const here = pathFromSegments([...done, segment]);

  if (rest.length > 0) {
    if (!has) return `finds no "${segment}" in "${pathFromSegments(done)}"`;
    const inner = step(old, rest, patch, [...done, segment], place);
    if (typeof inner === "string") return inner;
    if (inner.next === old) return { ...inner, next: at };
    return { ...inner, next: { ...at, [segment]: inner.next } };
  }

  switch (patch.op) {
    case "add":
    case "replace":
      if (!has && patch.op === "replace") return `finds no "${segment}" in "${pathFromSegments(done)}" to replace`;
      if (!has) {
        return { next: inserted(at, segment, patch.value, place), inverse: { op: "remove", path: here } };
      }
      if (jsonEqual(old, patch.value)) return { next: at, inverse: undefined };
      return {
        next: { ...at, [segment]: patch.value },
        inverse: { op: "replace", path: here, value: old as Patch["value"] },
      };
    case "remove": {
      if (!has) return `finds no "${segment}" in "${pathFromSegments(done)}" to remove`;
      const members = Object.keys(at);
      return {
        next: without(at, [segment]),
        inverse: { op: "add", path: here, value: old as Patch["value"] },
        place: members[members.indexOf(segment) + 1] ?? null,
      };
    }
  }
}

// The object with a member it lacks, put before the member that `before`
// names where the object holds that one, and last otherwise.
function inserted(
  record: Readonly<Record<string, unknown>>,
  key: string,
  value: unknown,
  before: string | null | undefined,
): Readonly<Record<string, unknown>> {
  if (typeof before !== "string" || !Object.hasOwn(record, before)) {
    return { ...record, [key]: value };
  }
  const members = Object.entries(record);
  members.splice(members.findIndex(([name]) => name === before), 0, [key, value]);
  return Object.fromEntries(members);
}

// A copy of the array with `count` elements at `index` replaced by those given.
function spliced(
  items: readonly unknown[],
  index: number,
  count: number,
  ...added: unknown[]
): unknown[] {
  const copy = [...items];
  copy.splice(index, count, ...added);
  return copy;
}

// An array's index, as JSON Pointer writes one: digits without a leading zero.
function arrayIndex(segment: string): number | undefined {
  return /^(0|[1-9]\d*)$/.test(segment) ? Number(segment) : undefined;
}
