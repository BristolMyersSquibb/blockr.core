// CoreApi and the start of the page: what core gives frontends and plugins,
// the order in which core starts the page and starts it again for each board
// that opens, and how it runs the app's plugins, its frontend among them.

export { createBlockrOptions } from "./blockr-options.ts";
export type { BlockrOptions } from "./blockr-options.ts";
export { createCore } from "./core.ts";
export type { CoreParts } from "./core.ts";
export { loadingNote, pageFrontEnds } from "./frontends.ts";
export { pluginRequest } from "./requests.ts";
export { start } from "./start.ts";
export type { Changed, Front, FrontEnds, Started, StartOptions } from "./start.ts";
