// CoreApi and the start of the page: what core gives frontends, plugins
// and startup modules, and the order in which core starts and switches the page
// to the frontend of each board that opens.

export { createBlockrOptions } from "./blockr-options.ts";
export type { BlockrOptions } from "./blockr-options.ts";
export { createCore } from "./core.ts";
export type { CoreParts } from "./core.ts";
export { loadingNote, pageFrontEnds } from "./frontends.ts";
export { pluginRequest } from "./requests.ts";
export { start } from "./start.ts";
export type { Changed, FrontEnds, Started, StartOptions } from "./start.ts";
