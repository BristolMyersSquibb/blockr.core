// The card in which core draws a block that serve() serves alone, in the
// page's element, since no frontend draws such a block
// (https://github.com/BristolMyersSquibb/blockr.core/issues/442).
//
// The card shows these, top to bottom:
// - The block's name.
// - The block's UI, which core mounts where the board lock leaves the block
//   open, as the store's locked() says.
// - The block's output, as R's chosen renderer draws it, which core mounts.
// - The footer, one centred line of the block's status, with a pill for each
//   severity of the conditions of R's reply.
// - The banners of the conditions whose pills are open.
// The footer and the banners are those of srcts/card, which the cards of
// core's basic frontend draw as well.
// While R evaluates the block, the footer and the output dim, and keep what
// they show. The output also dims while the block is not ready, and keeps the
// last output that it drew. The card shows conditions in its footer and its
// banners alone, and never as toasts.
// The card draws in the page's theme, which the board option dark_mode
// switches, as on every page that core starts.
// Core asks R for an output only once the output's element measures more than
// 0 by 0, and R draws a plot at that size. So the card gives the output a
// height of its own from the block's first reply on, which the user may drag
// to another. The footer is the last line of the output's box, so that the
// handle sits at the end of the footer, clear of the output's own controls.

import { useCallback, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Banners, Footer, Mounted, useFooter, useStatus } from "../card/index.ts";
import { loadingNote } from "../core/index.ts";
import { showError } from "../mount/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { own } from "../util/index.ts";

// The owner of the card's demand.
export const OWNER = "blockr.core";

// What the card reads of CoreApi.
export type CardCore = Pick<CoreApi, "mountBlock" | "mountOutput"> & {
  readonly store: Pick<CoreApi["store"], "board" | "subscribe" | "locked">;
  readonly scheduler: Pick<CoreApi["scheduler"], "demand" | "status" | "subscribe">;
};

// Draws the board's one block in its card in the element, and demands it,
// until the signal aborts, as the next board opens. The element then says that
// the board is loading, until core draws the next. Where the board holds no
// block or several, the element says so instead.
export function drawCard(el: HTMLElement, core: CardCore, signal: AbortSignal): void {
  if (signal.aborted) return;
  const ids = Object.keys(core.store.board().blocks);
  const [id] = ids;
  if (id === undefined || ids.length > 1) {
    showError(el, `A block that serve() serves alone comes as a board of one block, and this board holds ${String(ids.length)}.`);
    return;
  }
  const demand = core.scheduler.demand(OWNER, [id]);
  el.replaceChildren();
  const root = createRoot(el);
  flushSync(() => {
    root.render(<AloneCard core={core} id={id} />);
  });
  signal.addEventListener(
    "abort",
    () => {
      demand.release();
      root.unmount();
      el.replaceChildren(loadingNote());
    },
    { once: true },
  );
}

function AloneCard({ core, id }: { core: CardCore; id: BlockId }): ReactElement {
  const name = useBlockName(core, id);
  const status = useStatus(core, id);
  const footer = useFooter(status);

  const mountUi = useCallback((el: HTMLElement) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);

  const inputs = !core.store.locked(id);
  const replied = status !== undefined && status.evaluated !== null;
  const current = status?.status === "ready" && !status.running;

  return (
    <article
      aria-label={name}
      className="bc:mx-auto bc:my-4 bc:grid bc:max-w-5xl bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-subtle bc:p-3 bc:font-sans bc:text-gray-900 bc:dark:text-gray-100"
    >
      <header>
        <h1 className="bc:m-0 bc:font-sans bc:text-base bc:font-semibold">{name}</h1>
      </header>
      {inputs ? <Mounted mount={mountUi} label="Inputs" className="bc:min-w-0" /> : null}
      <div
        className={`bc:min-w-0 ${replied ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`}
      >
        <Mounted
          mount={mountOutput}
          label="Output"
          data={{ "data-current": String(current) }}
          className={`bc:min-w-0 bc:transition-opacity ${current ? "" : "bc:opacity-50 bc:grayscale"} ${
            replied ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"
          }`}
        />
        <Footer {...footer} />
      </div>
      <Banners {...footer} />
    </article>
  );
}

// The block's name, which the store keeps until a change renames the block.
function useBlockName(core: Pick<CardCore, "store">, id: BlockId): string {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => own(core.store.board().blocks, id)?.name ?? id);
}
