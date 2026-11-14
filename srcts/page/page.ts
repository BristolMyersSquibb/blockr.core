// Starting core's client on the page that R serves: core starts over the Shiny
// transport, registers the plugin classes and board options that R's reply
// names, core's own among them, and draws each board that opens with its
// frontend in the page's element, as srcts/core starts it. Core draws a block that serve() serves alone in its card instead
// (./card.tsx).
// While core starts, as the page starts and as another board opens, the
// element says that the board is loading, as today's page waits for the board
// that a loader resolves. Where core cannot start, the element says why. Where
// R now runs other versions of packages than the page loaded, the frontend
// draws the notification in which core asks for the page to be reloaded.

import { showError } from "../mount/index.ts";
import type { LoaderOptions, Measure } from "../mount/index.ts";
import type { Transport } from "../protocol/index.ts";
import { shinyTransport } from "../transport/index.ts";
import { report } from "../util/index.ts";
import { loadingNote, pageFrontEnds, start } from "../core/index.ts";
import type { Started } from "../core/index.ts";
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
    fail(error) {
      showError(el, error.message);
      report(error);
    },
  });
}
