// Starting core's client on the page that R serves: core starts over the Shiny
// transport, registers the plugin classes and board options that R's reply
// names, core's own among them, and draws each board that opens with its
// frontend in the page's element, as srcts/core starts it. Core draws a block that serve() serves alone in its card instead
// (./card.tsx).
// While core starts, as the page starts and as another board opens, the
// element says that the board is loading, as today's page waits for the board
// that a loader resolves. Where core cannot start, or R now runs other
// versions of packages than the page loaded, the element tells the user to
// reload the page.

import { showError } from "../mount/index.ts";
import type { LoaderOptions, Measure } from "../mount/index.ts";
import type { Transport } from "../protocol/index.ts";
import { shinyTransport } from "../transport/index.ts";
import { report } from "../util/index.ts";
import { loadingNote, pageFrontEnds, start } from "../core/index.ts";
import type { Changed, Started } from "../core/index.ts";
import { drawCard } from "./card.tsx";

export interface PageOptions {
  // The element that the frontends draw in.
  readonly el: HTMLElement;
  // The transport of the wire protocol, the Shiny transport unless given.
  readonly transport?: Transport;
  // How modules are imported and stylesheets put on the page, as on the page
  // unless given.
  readonly loader?: Omit<LoaderOptions, "modules" | "changed">;
  // Measures an output's element, through the DOM unless given.
  readonly measure?: Measure;
  // Whether the page serves a block alone, as serve() serves one, which core
  // draws in its card rather than with a frontend.
  readonly alone?: boolean;
}

export function startPage(options: PageOptions): Started {
  const { el } = options;
  el.replaceChildren(loadingNote());
  return start({
    transport: options.transport ?? shinyTransport(),
    ...(options.loader === undefined ? {} : { loader: options.loader }),
    ...(options.measure === undefined ? {} : { measure: options.measure }),
    frontEnds: {
      ...pageFrontEnds(el),
      card(core, signal) {
        drawCard(el, core, signal);
      },
    },
    ...(options.alone === true ? { alone: true } : {}),
    reload(changed) {
      reloadBanner(changed);
    },
    fail(error) {
      showError(el, error.message);
      report(error);
    },
  });
}

// Tells the user that R now runs other versions of packages than the page
// loaded, and offers to reload the page, which then loads the code that R
// serves, rather than load a second copy beside the first. The page carries on
// meanwhile, since a reload loses the board unless it is saved.
function reloadBanner(changed: readonly Changed[]): void {
  const id = "blockr-core-reload";
  document.getElementById(id)?.remove();
  const banner = document.createElement("div");
  banner.id = id;
  banner.setAttribute("role", "alert");
  banner.className =
    "bc:fixed bc:inset-x-0 bc:top-0 bc:z-50 bc:flex bc:items-center bc:gap-3 bc:border-b bc:border-border-default bc:bg-white bc:p-3 bc:font-sans bc:text-sm bc:text-black";
  const text = document.createElement("span");
  const versions = changed.map((c) => `${c.package} ${c.loaded} to ${c.now}`).join(", ");
  text.textContent = `R now runs other versions of packages than this page loaded (${versions}). Save the board, then reload the page.`;
  const button = document.createElement("button");
  button.type = "button";
  button.className = "bc:rounded-md bc:border bc:border-border-default bc:px-2 bc:py-1";
  button.textContent = "Reload";
  button.addEventListener("click", () => {
    location.reload();
  });
  banner.append(text, button);
  document.body.append(banner);
}
