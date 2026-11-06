// Drawing front-ends in the page's element, as core switches the page to the
// front-end of the class that each board it opens is built as
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#switching-front-ends):
// - As the store opens a board, core imports the module of its front-end and
//   ends the front-end on the page, whether it is another or the same. Ending
//   it aborts its signal and runs the cleanup that its render() returned.
// - Once the board is open, core calls the front-end's render() with the
//   element, CoreApi and the signal.
// A front-end releases what it holds as its signal aborts. Core releases it
// all the same: the CoreApi that a front-end gets keeps its demands,
// subscriptions and mounts, and core releases them as the front-end ends, so
// that nothing that a front-end kept of a board outlives the opening of
// another, as a page that starts with that board would hold none of it.

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
  readonly release: () => void;
  cleanup: (() => void) | undefined;
}

// The front-ends of the page, drawn in the element.
export function pageFrontEnds(el: HTMLElement): FrontEnds {
  let drawn: Drawn | undefined;
  let next: { id: string; module: string; render?: (args: FrontEndArgs) => Rendered; error?: unknown } | undefined;

  function end(): void {
    const current = drawn;
    if (current === undefined) return;
    drawn = undefined;
    current.controller.abort();
    current.release();
    if (current.cleanup !== undefined) attempt(current.cleanup);
    el.replaceChildren();
  }

  return {
    async prepare(front, loader) {
      let prepared: NonNullable<typeof next>;
      if ("error" in front) {
        end();
        next = { id: "", module: "", error: front.error };
        return;
      }
      const { id, source } = front;
      const pkg = packageOf(id);
      if (source === undefined) {
        prepared = { id, module: id, error: new Error(`R serves no module ${id}, which R names as the front-end of the board's class.`) };
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
    async render(core) {
      const prepared = next;
      next = undefined;
      if (prepared === undefined) return;
      el.replaceChildren();
      if (prepared.render === undefined) {
        showError(
          el,
          prepared.id === ""
            ? `No front-end draws the board: ${messageOf(prepared.error)}`
            : `The front-end ${prepared.id} cannot draw the board: ${messageOf(prepared.error)}`,
        );
        report(prepared.error);
        return;
      }
      const controller = new AbortController();
      const scope = scoped(core);
      const current: Drawn = { controller, release: scope.release, cleanup: undefined };
      drawn = current;
      try {
        const rendered = await prepared.render({ el, core: scope.core, signal: controller.signal });
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

// The CoreApi that one front-end gets, which keeps what the front-end holds,
// its demands, subscriptions and mounts, and releases them all at once.
export function scoped(core: CoreApi): { core: CoreApi; release: () => void } {
  const held = new Set<() => void>();
  const keep = (release: () => void): (() => void) => {
    const once = (): void => {
      if (!held.delete(once)) return;
      release();
    };
    held.add(once);
    return once;
  };

  const scheduler: CoreApi["scheduler"] = {
    ...core.scheduler,
    demand(owner, blocks, options) {
      const demand = core.scheduler.demand(owner, blocks, options);
      const release = keep(() => {
        demand.release();
      });
      return {
        set: (next, given) => {
          demand.set(next, given);
        },
        release,
      };
    },
    subscribe: (fn) => keep(core.scheduler.subscribe(fn)),
  };

  return {
    core: {
      ...core,
      store: { ...core.store, subscribe: (fn) => keep(core.store.subscribe(fn)) },
      scheduler,
      outputEvents: { subscribe: (fn) => keep(core.outputEvents.subscribe(fn)) },
      mountBlock: (el, block) => keep(core.mountBlock(el, block)),
      mountOutput: (el, block) => keep(core.mountOutput(el, block)),
      mountOption: (el, option) => keep(core.mountOption(el, option)),
    },
    release() {
      for (const release of [...held]) attempt(release);
    },
  };
}
