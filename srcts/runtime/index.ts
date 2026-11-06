// The build bundles core's runtime from here into inst/js/blockr-core.js,
// behind the one copy of React that ./share-react.ts puts on the page. The page
// that R serves starts core with startPage(), in the element that the
// front-ends draw in.

export { startPage } from "../page/index.ts";
export type { PageOptions } from "../page/index.ts";
export { createClient, SessionEndedError } from "../protocol/index.ts";
export { shinyTransport } from "../transport/index.ts";
