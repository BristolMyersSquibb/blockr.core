// What core's code does with the ids of what packages define, beside making
// them, which ./interned.ts does: how a message names one, what JSON holds of
// one, and the records by package and then by id that a board and R's reply to
// `hello` hold.

import type { Id } from "../types/board.ts";
import { interned } from "./interned.ts";
import { isPackageName } from "./util.ts";

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
      `${call} names a plugin by the id of its class, as core.id("blockr.dag", "dag_plugin") gives it, not by ${JSON.stringify(given)}.`,
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
      `Core loads a module that a package offers by its id, as core.id("blockr.core", "controls") gives it, not by ${JSON.stringify(given)}.`,
    );
  }
  return interned(given.package, given.id);
}
