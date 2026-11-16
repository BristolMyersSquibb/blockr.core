// The build preset that core ships to packages, as `@blockr/core/build`, the
// vendor check beside it, and what each file of a build imports up front,
// which R names in the head of the page that it serves.

export { preset } from "./preset.ts";
export { staticImports } from "./preload.ts";
export { vendorCheck } from "./vendor-check.ts";
