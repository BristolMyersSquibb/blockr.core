// Drawing front-ends in the page's element, as core draws each board that it
// opens with core's basic front-end
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// - As the store opens a board, core imports the module of its front-end.
// - Once the board is open, core calls the front-end's render() with the
//   element, CoreApi and a signal, which aborts as the next board opens, since
//   core starts again for that board.
// - As the signal aborts, core runs the cleanup that render() returned, and the
//   element says that the board is loading until the next front-end draws.
// A front-end releases what it holds as its signal aborts, as a module does as
// core unmounts it. Core ends what the front-end mounted with the same abort,
// and drops the rest of what it ran for the board.

import { attempt, ModuleError, moduleOf, showError } from "../mount/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Rendered } from "../types/hosts.ts";
import { messageOf, packageOf, report } from "../util/index.ts";
import type { FrontEnds } from "./start.ts";

interface FrontEndArgs {
  el: HTMLElement;
  core: CoreApi;
  signal: AbortSignal;
}

interface Drawn {
  readonly controller: AbortController;
  readonly off: () => void;
  cleanup: (() => void) | undefined;
}

// What the element says while core starts, as the page starts and as another
// board opens.
export function loadingNote(): HTMLElement {
  const box = document.createElement("div");
  box.setAttribute("role", "status");
  box.className = "bc:p-4 bc:font-sans bc:text-sm bc:text-text-muted";
  box.textContent = "Loading the board…";
  return box;
}

// The front-ends of the page, drawn in the element.
export function pageFrontEnds(el: HTMLElement): FrontEnds {
  let drawn: Drawn | undefined;
  let next: { id: string; module: string; render?: (args: FrontEndArgs) => Rendered; error?: unknown } | undefined;

  function end(): void {
    const current = drawn;
    if (current === undefined) return;
    drawn = undefined;
    current.off();
    current.controller.abort();
    if (current.cleanup !== undefined) attempt(current.cleanup);
    el.replaceChildren(loadingNote());
  }

  return {
    async prepare(front, loader) {
      let prepared: NonNullable<typeof next>;
      const { id, source } = front;
      const pkg = packageOf(id);
      if (source === undefined) {
        prepared = { id, module: id, error: new Error(`R serves no module ${id}, the front-end that draws the board.`) };
      } else {
        try {
          const mod = moduleOf<FrontEndArgs>(await loader.load(source, pkg), source.module, pkg);
          prepared = { id, module: source.module, render: mod.render.bind(mod) };
        } catch (error) {
          prepared = { id, module: source.module, error };
        }
      }
      end();
      next = prepared;
    },
    async render(core, signal) {
      const prepared = next;
      next = undefined;
      if (prepared === undefined || signal.aborted) return;
      el.replaceChildren();
      if (prepared.render === undefined) {
        showError(el, `The front-end ${prepared.id} cannot draw the board: ${messageOf(prepared.error)}`);
        report(prepared.error);
        return;
      }
      const controller = new AbortController();
      const ending = (): void => {
        if (drawn === current) end();
      };
      signal.addEventListener("abort", ending, { once: true });
      const current: Drawn = {
        controller,
        off: () => {
          signal.removeEventListener("abort", ending);
        },
        cleanup: undefined,
      };
      drawn = current;
      try {
        const rendered = await prepared.render({ el, core, signal: controller.signal });
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new ModuleError(prepared.module, packageOf(prepared.id), `failed in render(): ${messageOf(error)}`, {
          cause: error,
        });
        showError(el, why.message);
        report(why);
      }
    },
  };
}
