// What the two cards that core draws share: the card of a block that serve()
// serves alone, in srcts/page, and the cards of core's basic frontend, in
// srcts/modules/basic.
// Both draw the block's status and the notifications about it in the same
// footer, pills and banners, give the block's output a height of its own from
// the same moment on, and have core mount the block's UI and output in the
// same element.
// Both follow the block's status and the notifications about it through the
// hooks of core's React SDK.
// The basic frontend's toasts mark a notification's severity as the cards do,
// and its plugins' sections draw the notifications about a plugin in the same
// pills and banners. The banner of the Edit board plugin that says why an edit
// was refused marks an error as they do (../ui/ui.tsx).
// The bundles of core's runtime, of the basic frontend and of the Edit board
// plugin each hold a copy of it, as each holds one of srcts/util.

export { Banners, Footer, MARKS, Pills, SeverityIcon, useFooter, usePills } from "./footer.tsx";
export type { FooterProps } from "./footer.tsx";
export { Mounted } from "./mounted.tsx";
export { useSized } from "./status.ts";
