// What the mounts of block UIs, outputs and board options share: checking what
// a module exports, giving a module the values of the board options that R
// names for it, saying why a module cannot draw without them, showing why core
// cannot draw the page, and running a cleanup.

import type { OptionId } from "../types/board.ts";
import type { Rendered } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { log } from "../log/index.ts";
import { asError, isObject, own, quoted } from "../util/index.ts";
import { API, ModuleError } from "./load.ts";

// What a module that core mounts exports as its default: its render().
export interface Mountable<A> {
  render(args: A): Rendered;
}

// What a module exports, where it declares the major of core's JS API that
// core speaks, as `export const api = 1`, or a ModuleError that says why it
// declares none.
export function moduleExports(exports: unknown, module: string, pkg: string | null): Readonly<Record<string, unknown>> {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`,
    );
  }
  return exports;
}

// What a module exports, as a module that core mounts, or a ModuleError that
// says why it is none.
export function mountableModule<A>(exports: unknown, module: string, pkg: string | null): Mountable<A> {
  const mod = moduleExports(exports, module, pkg).default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new ModuleError(module, pkg, "has no default export with a render() function.");
  }
  return mod as unknown as Mountable<A>;
}

// What gives a module the value of a board option that R names for it, by id,
// from `values`, the values that the board holds of those that `named` holds.
// It throws for any other id, a misspelled one included, with what `unnamed`
// says of the id: that what the module draws for names no such board option
// in R.
export function optionReader(
  values: Readonly<Record<OptionId, Json>>,
  named: readonly OptionId[],
  unnamed: (id: string) => string,
): (id: OptionId) => Json {
  return (id) => {
    if (!named.includes(id)) throw new Error(unnamed(id));
    return own(values, id) as Json;
  };
}

// Why a module cannot draw while board options that it requires are not on
// the board, as core's notification about what the module belongs to says it,
// beside the notification about each board option, which says why that is not
// on the board.
export function withoutOptions(module: string, ids: readonly OptionId[]): string {
  return `Without the board option${ids.length === 1 ? "" : "s"} ${quoted(ids)}, ${module} cannot draw.`;
}

// Shows why core cannot draw the page in the page's element, in place of
// what it held, where no frontend draws.
export function showError(el: HTMLElement, message: string): void {
  const box = document.createElement("div");
  box.setAttribute("role", "alert");
  box.className =
    "bc:rounded-sm bc:border bc:border-destructive bc:p-2 bc:font-sans bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap";
  box.textContent = message;
  el.replaceChildren(box);
}

// Runs a cleanup, logging what it throws.
export function attempt(fn: () => void): void {
  try {
    fn();
  } catch (error) {
    log.error(asError(error));
  }
}

// Calls `fn` once the signal aborts, and returns what stops that.
export function onAbort(signal: AbortSignal, fn: () => void): () => void {
  signal.addEventListener("abort", fn, { once: true });
  return () => {
    signal.removeEventListener("abort", fn);
  };
}

// Why core mounts nothing on a board that another has replaced.
export const REPLACED = "Another board opened, so core mounts nothing more of the board before.";

// Calls each listener with the args given, logging what one throws, so that
// the others still hear of it.
export function announceTo<A extends unknown[]>(listeners: Iterable<(...args: A) => void>, ...args: A): void {
  for (const fn of [...listeners]) {
    attempt(() => {
      fn(...args);
    });
  }
}
