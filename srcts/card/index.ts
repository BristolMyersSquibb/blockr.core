// What the two cards that core draws share: the card of a block that serve()
// serves alone, in srcts/page, and the cards of core's basic frontend, in
// srcts/modules/basic.
// Both draw the block's status in the same footer, with the same pills and
// banners, follow the status through the same hooks, give the block's output a
// height of its own from the same moment on, and have core mount the block's
// UI and output in the same element.
// The bundles of core's runtime and of the basic frontend each hold a copy of
// it, as each holds one of srcts/util.

export { Banners, Footer, useFooter } from "./footer.tsx";
export type { FooterProps } from "./footer.tsx";
export { Mounted } from "./mounted.tsx";
export { useSized, useStatus } from "./status.ts";
