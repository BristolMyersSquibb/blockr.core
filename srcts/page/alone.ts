// A block that serve() serves alone, which core draws in its card in the
// page's element, since no frontend draws such a block
// (https://github.com/BristolMyersSquibb/blockr.core/issues/442).
// Core demands the block as the board opens, and loads the card's code as it
// first draws the card (./card.tsx). So the page loads React and core's React
// SDK, which the card draws with, only then.
// While the code loads, the element keeps the loading note.
// Core starts the scheduler once the card has mounted the block, as it starts
// it once a frontend has drawn the board (../core/start.ts).
// Where the code fails to load, the element says why.

import { loadingNote } from "../core/index.ts";
import { log } from "../log/index.ts";
import { showError } from "../mount/index.ts";
import { errorMessage } from "../util/index.ts";
import type { CardCore } from "./card.tsx";

// The owner of the card's demand.
export const OWNER = "blockr.core";

// Draws the board's one block in its card in the element, and demands it,
// until the signal aborts, as the next board opens. The element then says that
// the board is loading, until core draws the next. Where the board holds no
// block or several, the element says so instead.
// It resolves once the card has mounted the block's UI and output, or once the
// element says why it shows no card.
export async function showCard(el: HTMLElement, core: CardCore, signal: AbortSignal): Promise<void> {
  if (signal.aborted) return;
  const ids = Object.keys(core.store.board().blocks);
  const [id] = ids;
  if (id === undefined || ids.length > 1) {
    showError(el, `A block that serve() serves alone comes as a board of one block, and this board holds ${String(ids.length)}.`);
    return;
  }
  const demand = core.scheduler.demand(OWNER, [id]);
  // What unmounts the card, once its code has loaded and drawn it.
  let unmount: (() => void) | undefined;
  signal.addEventListener(
    "abort",
    () => {
      demand.release();
      unmount?.();
      el.replaceChildren(loadingNote());
    },
    { once: true },
  );
  await import("./card.tsx").then(
    ({ drawCard }) => {
      if (signal.aborted) return;
      el.replaceChildren();
      unmount = drawCard(el, core, id);
    },
    (error: unknown) => {
      if (signal.aborted) return;
      const why = new Error(`Core cannot draw the card of the block "${id}", since the card's code failed to load: ${errorMessage(error)}`, {
        cause: error,
      });
      log.error(why);
      showError(el, why.message);
    },
  );
}
