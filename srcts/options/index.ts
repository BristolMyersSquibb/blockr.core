// Core's board options, n_rows, show_conditions and dark_mode, each without
// the id that R gives it, which the module of each in ../modules exports, with
// its UI module, and the schemas of their values, which the build writes
// beside those modules for R.

export { coreOptions, darkMode, nRows, setTheme, showConditions } from "./options.ts";
export type { BlockrOptions } from "./options.ts";
export { optionSchemas } from "./schemas.ts";
