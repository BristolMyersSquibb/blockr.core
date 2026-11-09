// What the client's modules share: reading and writing plain data without
// changing it in place, the text of messages, and errors.

import type { Json } from "../types/json.ts";

// An object's own value under a key, which an id such as "constructor" never
// reads off the prototype.
export function own<T>(
  record: Readonly<Record<string, T>>,
  key: string,
): T | undefined {
  return Object.hasOwn(record, key) ? record[key] : undefined;
}

// What an error says, without the name of its class.
export function messageOf(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

// What was thrown, as an Error. A value that is no Error, such as a string,
// becomes an Error that says it.
export function asError(error: unknown): Error {
  return error instanceof Error ? error : new Error(String(error));
}

// Reports an error that nobody waits on, such as what a subscriber threw. It
// throws the error from a microtask. The page hears of it as of any uncaught
// error, and the code that reports it goes on.
export function report(error: unknown): void {
  queueMicrotask(() => {
    throw error;
  });
}

export function isObject(x: unknown): x is Readonly<Record<string, unknown>> {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

export function jsonEqual(a: unknown, b: unknown): boolean {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return (
      Array.isArray(b) &&
      a.length === b.length &&
      a.every((x, i) => jsonEqual(x, b[i]))
    );
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return (
      keys.length === Object.keys(b).length &&
      keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]))
    );
  }
  return false;
}

// What a path leads to in nested objects, if anything.
export function getAt(root: unknown, path: readonly string[]): unknown {
  let at = root;
  for (const key of path) {
    if (!isObject(at) || !Object.hasOwn(at, key)) return undefined;
    at = at[key];
  }
  return at;
}

// The nested objects with the value at the path, copying only the objects
// along the path, or, where the value is undefined, without it and without the
// objects that removing it leaves empty, so that setting a value and removing
// it again leaves the objects as they were.
export function setAt(
  root: Readonly<Record<string, unknown>>,
  path: readonly string[],
  value: unknown,
): Readonly<Record<string, unknown>> {
  const [key, ...rest] = path;
  if (key === undefined) return root;
  if (rest.length === 0) {
    if (value === undefined) return without(root, [key]);
    return { ...root, [key]: value };
  }
  const below = own(root, key);
  if (value === undefined) {
    if (!isObject(below)) return root;
    const next = setAt(below, rest, undefined);
    if (next === below) return root;
    return Object.keys(next).length === 0 ? without(root, [key]) : { ...root, [key]: next };
  }
  return {
    ...root,
    [key]: setAt(isObject(below) ? below : {}, rest, value),
  };
}

export function without<T>(
  record: Readonly<Record<string, T>>,
  keys: readonly string[],
): Readonly<Record<string, T>> {
  if (!keys.some((key) => Object.hasOwn(record, key))) return record;
  return Object.fromEntries(
    Object.entries(record).filter(([key]) => !keys.includes(key)),
  );
}

// A copy that nothing can change, for code that the store hands plain data and
// that could change it in place, such as a migration.
export function frozenCopy<T extends Json>(x: T): T {
  return freeze(structuredClone(x));
}

function freeze<T extends Json>(x: T): T {
  if (typeof x === "object" && x !== null) {
    for (const value of Object.values(x)) freeze(value);
    Object.freeze(x);
  }
  return x;
}

// A list in prose: "a", "a and b", "a, b and c".
export function listing(items: readonly string[]): string {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}

export function quoted(items: readonly string[]): string {
  return listing(items.map((item) => `"${item}"`));
}

export function pathText(path: readonly string[]): string {
  return path.join("/");
}

// The package that an id names, "blockr.dag" for "blockr.dag-dag" or
// "blockr.core-head_block", or null where it names none, as a block class
// defined outside a package does.
export function packageOf(id: string): string | null {
  const at = id.indexOf("-");
  if (at < 0) return null;
  const pkg = id.slice(0, at);
  return isPackageName(pkg) ? pkg : null;
}

// Whether a name is one an R package can have: letters, digits and dots,
// starting with a letter and ending with no dot.
export function isPackageName(name: unknown): name is string {
  return typeof name === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name);
}

// The number that text holds, such as an environment variable's value, or NaN
// where it holds no finite number, as "", "a" and "Inf" do, while Number()
// reads "" as 0.
export function numberOf(text: string): number {
  const number = text.trim() === "" ? NaN : Number(text);
  return Number.isFinite(number) ? number : NaN;
}

// A logical as R's as.logical() reads one from text, such as an environment
// variable's value, or undefined.
export function logicalOf(text: string): boolean | undefined {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return undefined;
}
