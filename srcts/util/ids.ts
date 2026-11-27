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
// CoreApi's id(), since a copy of this module would make objects of its own,
// which would never be === to core's.

import type { Id } from "../types/board.ts";
import { isPackageName } from "./util.ts";

const made = new Map<string | null, Map<string, Id<string | null>>>();

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

// Whether `x` holds a package and an id as an id does, as JSON gives one: an
// object of exactly `package`, a package's name or null where `nullable` is
// true, and `id`, a non-empty string.
export function isIdRecord(x: unknown, nullable?: false): x is { readonly package: string; readonly id: string };
export function isIdRecord(x: unknown, nullable: true): x is { readonly package: string | null; readonly id: string };
export function isIdRecord(x: unknown, nullable = false): boolean {
  if (typeof x !== "object" || x === null || Array.isArray(x)) return false;
  const keys = Object.keys(x);
  if (keys.length !== 2 || !keys.includes("package") || !keys.includes("id")) return false;
  const { package: pkg, id } = x as Record<string, unknown>;
  return (pkg === null ? nullable : isPackageName(pkg)) && typeof id === "string" && id !== "";
}

// The id that a record holds, as interned() makes it, where JSON gave the
// record, which is no object of the table.
export function internedRecord<P extends string | null>(record: { readonly package: P; readonly id: string }): Id<P> {
  return interned(record.package, record.id);
}

// How a message names an id, as "head_block of blockr.core", or by the id
// alone for what an app script defines.
export function idText(x: { readonly package: string | null; readonly id: string }): string {
  return x.package === null ? x.id : `${x.id} of ${x.package}`;
}

// A number for each id, the same for the same pair, for what needs a string or
// a number for it, such as a React key.
const numbers = new Map<Id<string | null>, number>();

export function idNumber(x: Id<string | null>): number {
  let number = numbers.get(x);
  if (number === undefined) {
    number = numbers.size;
    numbers.set(x, number);
  }
  return number;
}

// The entries of a record by package and then by id, as a board holds the
// state of its plugins and R's reply to `hello` the modules that packages
// offer, each under its id as interned() makes it, in the record's order.
export function nestedEntries<T>(record: Readonly<Record<string, Readonly<Record<string, T>>>>): [Id, T][] {
  return Object.entries(record).flatMap(([pkg, byId]) =>
    Object.entries(byId).map(([id, value]): [Id, T] => [interned(pkg, id), value]),
  );
}

// What a record by package and then by id holds under the id, or undefined
// where it holds nothing there.
export function nestedValue<T>(record: Readonly<Record<string, Readonly<Record<string, T>>>>, id: Id): T | undefined {
  const byId = Object.hasOwn(record, id.package) ? record[id.package] : undefined;
  return byId !== undefined && Object.hasOwn(byId, id.id) ? byId[id.id] : undefined;
}

// A record by package and then by id of the entries, in their order.
export function nestedRecord<T>(entries: Iterable<readonly [Id, T]>): Record<string, Record<string, T>> {
  const record: Record<string, Record<string, T>> = {};
  for (const [id, value] of entries) {
    (record[id.package] ??= {})[id.id] = value;
  }
  return record;
}

// The attributes of a log line or a span that name what a package defines, such
// as the class of a plugin or of a block, a renderer or a command: the id
// within the package, under the attribute that names the object, as
// "blockr.plugin.class_id", and the package beside it, under the same
// namespace, as "blockr.plugin.package", as R names them too, or none for what
// an app script defines, since OpenTelemetry has no null.
export function idAttributes(of: string, x: { readonly package: string | null; readonly id: string }): Record<string, string> {
  const pkg = of.replace(/\.[a-z_]+$/, ".package");
  return { [of]: x.id, ...(x.package === null ? {} : { [pkg]: x.package }) };
}

// The id of a plugin's class that a caller gives, as core's table holds it, so
// that core finds the plugin under it whoever made the record, or a TypeError
// that names the call for what holds no package and id.
export function givenPluginClassId(given: unknown, call: string): Id {
  if (!isIdRecord(given)) {
    throw new TypeError(
      `${call} names a plugin by the id of its class, a package and an id, as { package: "blockr.dag", id: "dag_plugin" }, not by ${JSON.stringify(given)}.`,
    );
  }
  return interned(given.package, given.id);
}

// The id of a module that a package offers, as a caller gives it, as core's
// table holds it, or a TypeError for what holds no package and id, as a caller
// in plain JS may give.
export function givenModuleId(given: unknown): Id {
  if (!isIdRecord(given)) {
    throw new TypeError(
      `Core loads a module that a package offers by its package and id, as { package: "blockr.core", id: "controls" }, not by ${JSON.stringify(given)}.`,
    );
  }
  return interned(given.package, given.id);
}
