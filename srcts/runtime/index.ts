// The build bundles core's runtime from here into inst/js/blockr-core.js,
// behind the one copy of React that ./share-react.ts puts on the page.

export { createClient, SessionEndedError } from "../protocol/index.ts";
export { shinyTransport } from "../transport/index.ts";
