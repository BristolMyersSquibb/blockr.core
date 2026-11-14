// What the mounts of block UIs, outputs and board options share: checking what
// a module exports, saying why a module cannot draw without its board options,
// showing why core cannot draw the page, and running a cleanup.

import type { OptionId } from "../types/board.ts";
import type { Rendered } from "../types/hosts.ts";
import { isObject, quoted, report } from "../util/index.ts";
import { API, ModuleError } from "./load.ts";

// What a module that core mounts exports as its default: the board options
// that it names, and its render().
export interface Mountable<A> {
  readonly options?: readonly string[];
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
  const named: unknown = mod.options;
  if (named !== undefined && !(Array.isArray(named) && named.every((id) => typeof id === "string"))) {
    throw new ModuleError(module, pkg, "names its board options in something other than an array of their ids.");
  }
  return mod as unknown as Mountable<A>;
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
    "bc:rounded bc:border bc:border-border-danger bc:p-2 bc:font-sans bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap";
  box.textContent = message;
  el.replaceChildren(box);
}

// Runs a cleanup, reporting what it throws.
export function attempt(fn: () => void): void {
  try {
    fn();
  } catch (error) {
    report(error);
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

// Calls each listener with the args given, reporting what one throws, so that
// the others still hear of it.
export function announceTo<A extends unknown[]>(listeners: Iterable<(...args: A) => void>, ...args: A): void {
  for (const fn of [...listeners]) {
    attempt(() => {
      fn(...args);
    });
  }
}
