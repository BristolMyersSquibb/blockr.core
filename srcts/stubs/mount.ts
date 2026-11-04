// Mounting a module in a test as core mounts it on the page, with a stub of
// its host: core checks what the module exports, calls its render() with an
// element, the host and a signal, and on unmounting aborts the signal and runs
// the cleanup that render() returned.

import { API } from "../mount/index.ts";
import type { Rendered } from "../types/hosts.ts";
import { isObject } from "../util/index.ts";

export interface Mounted {
  // The element the module draws in.
  readonly el: HTMLElement;
  // The signal that core gives the module, which unmount() aborts.
  readonly signal: AbortSignal;
  // Aborts the signal and runs the module's cleanup.
  unmount(): void;
}

// Mounts the module, given as what it exports, in a new element unless one is
// given, and resolves once its render() has. It rejects where the module does
// not declare core's JS API major, has no render() or its render() throws, as
// core refuses such a module and shows the error in its element.
export async function mount(
  exports: unknown,
  host: unknown,
  el: HTMLElement = document.createElement("div"),
): Promise<Mounted> {
  if (!isObject(exports) || exports.api !== API) {
    throw new Error(
      `The module declares ${isObject(exports) && typeof exports.api === "number" ? `major ${String(exports.api)}` : "no major"} of core's JS API, where core's is ${String(API)}, as \`export const api = ${String(API)}\`.`,
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new Error("The module has no default export with a render() function.");
  }
  const render = mod.render as (args: { el: HTMLElement; host: unknown; signal: AbortSignal }) => Rendered;
  const controller = new AbortController();
  const cleanup = await render.call(mod, { el, host, signal: controller.signal });

  return {
    el,
    signal: controller.signal,
    unmount() {
      if (controller.signal.aborted) return;
      controller.abort();
      if (typeof cleanup === "function") cleanup();
    },
  };
}
