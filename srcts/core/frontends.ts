// Drawing frontends in the page's element, as core draws each board that it
// opens with the app's frontend
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// - Once the store has opened a board, core takes the app's frontend: the
//   `frontend` of the class of the app's plugin that is one, core's basic
//   frontend among them, from the module of the class that core imported as
//   it registered the class. Where the app runs no frontend, as where the
//   module of the class of the frontend that R names failed to load, the
//   element shows why in place of the board.
// - Once the board options that the frontend requires are on the board, core
//   calls the frontend's render() with the element, CoreApi, the id of the
//   frontend's class and a signal, which aborts as the next board opens, since
//   core starts again for that board. Where a board option that the frontend
//   requires did not go onto the board, the element shows why in place of the
//   frontend.
// - As the signal aborts, core runs the cleanup that render() returned, and the
//   element says that the board is loading until the next frontend draws.
// A frontend releases what it holds as its signal aborts, as a module does as
// core unmounts it. Core ends what the frontend mounted with the same abort,
// and drops the rest of what it ran for the board.

import { attempt, showError } from "../mount/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { PluginModule } from "../types/core-api.ts";
import { errorMessage, report } from "../util/index.ts";
import type { FrontEnds } from "./start.ts";

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

// The frontends of the page, drawn in the element.
export function pageFrontEnds(el: HTMLElement): FrontEnds {
  let drawn: Drawn | undefined;
  // The frontend that prepare() took, or why the app runs none.
  let next: { frontend: PluginModule; plugin: PluginClassId } | { none: string } | undefined;

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
    prepare(front, signal) {
      if (signal.aborted) return;
      end();
      next = front;
    },
    async render(core, signal, failures) {
      if (signal.aborted) return;
      const prepared = next;
      next = undefined;
      if (prepared === undefined) return;
      el.replaceChildren();
      if ("none" in prepared) {
        showError(el, prepared.none);
        report(new Error(prepared.none));
        return;
      }
      if (failures.length > 0) {
        showError(el, failures.join("\n"));
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
        const rendered = await prepared.frontend.render({ el, core, plugin: prepared.plugin, signal: controller.signal });
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new Error(`The frontend of the plugin class "${prepared.plugin}" failed in render(): ${errorMessage(error)}`, {
          cause: error,
        });
        showError(el, why.message);
        report(why);
      }
    },
  };
}
