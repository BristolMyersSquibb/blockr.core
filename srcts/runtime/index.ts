// The build bundles core's runtime from here into inst/js/blockr-core.js,
// which imports React and core's React SDK, its binding and its controls
// through the page's import map. The page that R serves starts core with
// startPage(), in the element that the frontends draw in.

export { startPage } from "../page/index.ts";
export type { PageOptions } from "../page/index.ts";
export { createClient, SessionEndedError } from "../protocol/index.ts";
export { shinyTransport } from "../transport/index.ts";
