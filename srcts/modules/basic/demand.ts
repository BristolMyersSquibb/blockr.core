// What the basic frontend demands, as Evaluation in the client has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand),
// and what it has on screen, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#what-a-frontend-offers-other-plugins).
// The frontend shows every block, so it demands every block, above the
// default evaluation priority. It demands the blocks whose cards are in view
// higher still, so that those and everything upstream of them go first.
// It makes both demands within render(), from the board, so that the
// scheduler's first evaluations are those of what the page first shows.
// It sets CoreApi's onScreen to the blocks whose cards are in view and the
// plugins whose sections are, as the browser counts them, so that neither the
// cards of a collapsed group nor a collapsed section is on screen.
// So core asks R for the output of a card only while the card is in view, and
// for an output in a plugin's section only while the section is
// (../../core/core.ts).

import type { BlockId, PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";

// The owner of the demands, the frontend's package.
export const OWNER = "blockr.core";
// The evaluation priority of every block, and of the blocks in view.
export const SHOWN = 1;
export const IN_VIEW = 2;

// The page that the cards are drawn on: its size, and how it tells which
// elements are in view, which a test may play.
export type View = Pick<Window, "innerWidth" | "innerHeight"> & {
  readonly IntersectionObserver?: typeof IntersectionObserver;
};

// A block, by the element of its card, or a plugin, by the element in which
// its section mounts the plugin's UI.
export type Watched = { readonly block: BlockId } | { readonly plugin: PluginClassId };

export interface InView {
  // Counts the block or the plugin in view while the element is, until the
  // returned cleanup runs.
  watch(object: Watched, el: Element): () => void;
  // The element that the block or the plugin is watched by, if any.
  element(object: Watched): Element | undefined;
  // Demands the blocks whose cards are in view now, and sets what is on
  // screen, as the page lays them out, rather than once the browser reports
  // them.
  measure(): void;
}

export interface Shown extends InView {
  // Releases both demands, after which the cards that go set nothing.
  release(): void;
}

export function demandShown(core: Pick<CoreApi, "scheduler" | "onScreen">, view: View = window): Shown {
  const every = core.scheduler.demand(OWNER, (board) => Object.keys(board.blocks), { priority: SHOWN });
  const near = core.scheduler.demand(OWNER, [], { priority: IN_VIEW });

  const watched = new Map<Element, Watched>();
  const seen = new Set<Element>();
  let released = false;

  function update(): void {
    if (released) return;
    const blocks = new Set<BlockId>();
    const plugins = new Set<PluginClassId>();
    for (const el of seen) {
      const object = watched.get(el);
      if (object === undefined) continue;
      if ("block" in object) blocks.add(object.block);
      else plugins.add(object.plugin);
    }
    near.set([...blocks]);
    core.onScreen.set({ blocks: [...blocks], plugins: [...plugins] });
  }

  const Observer = view.IntersectionObserver;
  const observer =
    Observer === undefined
      ? undefined
      : new Observer((entries) => {
          for (const entry of entries) {
            if (!watched.has(entry.target)) continue;
            if (entry.isIntersecting) seen.add(entry.target);
            else seen.delete(entry.target);
          }
          update();
        });

  return {
    watch(object, el) {
      watched.set(el, object);
      observer?.observe(el);
      return () => {
        observer?.unobserve(el);
        watched.delete(el);
        if (seen.delete(el)) update();
      };
    },
    element(object) {
      for (const [el, at] of watched) {
        if ("block" in object ? "block" in at && at.block === object.block : "plugin" in at && at.plugin === object.plugin) return el;
      }
      return undefined;
    },
    measure() {
      seen.clear();
      for (const el of watched.keys()) if (inView(el, view)) seen.add(el);
      update();
    },
    release() {
      released = true;
      observer?.disconnect();
      every.release();
      near.release();
    },
  };
}

// Whether the element shows within the page's viewport.
function inView(el: Element, view: View): boolean {
  const rect = el.getBoundingClientRect();
  return (
    rect.width > 0 &&
    rect.height > 0 &&
    rect.bottom > 0 &&
    rect.right > 0 &&
    rect.top < view.innerHeight &&
    rect.left < view.innerWidth
  );
}
