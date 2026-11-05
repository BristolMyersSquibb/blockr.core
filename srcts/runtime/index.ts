// The build bundles core's runtime from here into inst/js/blockr-core.js,
// with the one copy of React on the page.
import "./share-react.ts";

export { createClient, SessionEndedError } from "../protocol/index.ts";
export { shinyTransport } from "../transport/index.ts";
