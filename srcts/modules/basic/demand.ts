// What the basic front-end demands, as Evaluation in the client has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand).
// The front-end shows every block, so it demands every block, above the
// default evaluation priority. It demands the blocks whose cards are in view
// higher still, so that those and everything upstream of them go first.
// It makes both demands within render(), from the board, so that the
// scheduler's first evaluations are those of what the page first shows.

import type { BlockId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";

// The owner of the demands, the front-end's id.
export const OWNER = "blockr.core-basic";
// The evaluation priority of every block, and of the blocks in view.
export const SHOWN = 1;
export const IN_VIEW = 2;

// The page that the cards are drawn on: its size, and how it tells which
// elements are in view, which a test may play.
export type View = Pick<Window, "innerWidth" | "innerHeight"> & {
  readonly IntersectionObserver?: typeof IntersectionObserver;
};

export interface InView {
  // Counts the block in view while its card's element is, until the returned
  // cleanup runs.
  watch(block: BlockId, el: Element): () => void;
  // Demands the blocks whose cards are in view now, as the page lays them out,
  // rather than once the browser reports them.
  measure(): void;
}

export interface Shown extends InView {
  release(): void;
}

export function demandShown(core: Pick<CoreApi, "scheduler">, view: View = window): Shown {
  const every = core.scheduler.demand(OWNER, (board) => Object.keys(board.blocks), { priority: SHOWN });
  const near = core.scheduler.demand(OWNER, [], { priority: IN_VIEW });

  const watched = new Map<Element, BlockId>();
  const seen = new Set<Element>();

  function update(): void {
    near.set([...new Set([...seen].flatMap((el) => watched.get(el) ?? []))]);
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
    watch(block, el) {
      watched.set(el, block);
      observer?.observe(el);
      return () => {
        observer?.unobserve(el);
        watched.delete(el);
        if (seen.delete(el)) update();
      };
    },
    measure() {
      seen.clear();
      for (const el of watched.keys()) if (inView(el, view)) seen.add(el);
      update();
    },
    release() {
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
