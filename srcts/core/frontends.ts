// Drawing frontends in the page's element, as core draws each board that it
// opens with its frontend
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board):
// - Once the store has opened a board, core imports the module of its
//   frontend: the `frontend` of the class of the board's plugin that is one,
//   or core's basic frontend where the board holds none.
// - Once the board options that the frontend requires are on the board, core
//   calls the frontend's render() with the element, CoreApi, the plugin's id
//   and a signal, which aborts as the next board opens, since core starts
//   again for that board. Core's basic frontend, which no board holds, gets no
//   plugin's id. Where a board option that the frontend requires did not go
//   onto the board, the element shows why in place of the frontend.
// - As the signal aborts, core runs the cleanup that render() returned, and the
//   element says that the board is loading until the next frontend draws.
// A frontend releases what it holds as its signal aborts, as a module does as
// core unmounts it. Core ends what the frontend mounted with the same abort,
// and drops the rest of what it ran for the board.

import { attempt, ModuleError, mountableModule, showError } from "../mount/index.ts";
import type { PluginId } from "../types/board.ts";
import type { Rendered } from "../types/hosts.ts";
import { errorMessage, packageFromId, report } from "../util/index.ts";
import type { FrontendArgs } from "./plugins.ts";
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
  let next:
    | {
        id: string;
        module: string;
        plugin: PluginId | undefined;
        render?: (args: FrontendArgs) => Rendered;
        error?: unknown;
      }
    | undefined;

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
    async prepare(front, loader, signal) {
      let prepared: NonNullable<typeof next>;
      const { id, source, plugin } = front;
      const pkg = packageFromId(id);
      if (source === undefined) {
        prepared = { id, module: id, plugin, error: new Error(`R serves no module ${id}, the frontend that draws the board.`) };
      } else {
        try {
          const mod = mountableModule<FrontendArgs>(await loader.load(source, pkg), source.module, pkg);
          prepared = { id, module: source.module, plugin, render: mod.render.bind(mod) };
        } catch (error) {
          prepared = { id, module: source.module, plugin, error };
        }
      }
      if (signal.aborted) return;
      end();
      next = prepared;
    },
    async render(core, signal, failures) {
      if (signal.aborted) return;
      const prepared = next;
      next = undefined;
      if (prepared === undefined) return;
      el.replaceChildren();
      if (failures.length > 0) {
        showError(el, failures.join("\n"));
        return;
      }
      if (prepared.render === undefined) {
        showError(el, `The frontend ${prepared.id} cannot draw the board: ${errorMessage(prepared.error)}`);
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
        const { plugin } = prepared;
        const rendered = await prepared.render({
          el,
          core,
          signal: controller.signal,
          ...(plugin === undefined ? {} : { plugin }),
        });
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new ModuleError(prepared.module, packageFromId(prepared.id), `failed in render(): ${errorMessage(error)}`, {
          cause: error,
        });
        showError(el, why.message);
        report(why);
      }
    },
  };
}
