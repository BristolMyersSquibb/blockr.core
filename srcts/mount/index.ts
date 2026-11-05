// Loading the modules of packages once per page, and mounting them in a
// front-end's layout, each with a host narrowed to what it draws: a block's UI
// module, a block's output and a board option's UI.

export { createMounts } from "./block.ts";
export type { Mounts, MountsOptions } from "./block.ts";
export { API, createLoader, ModuleError } from "./load.ts";
export type { Loader, LoaderOptions, Source } from "./load.ts";
export { domMeasure } from "./measure.ts";
export type { Measure } from "./measure.ts";
export type { OutputEvent } from "./output.ts";
export { RequestError } from "./requests.ts";
export { attempt, moduleOf, showError } from "./shared.ts";
