// Core's board options, n_rows, show_conditions and dark_mode, which core
// registers in the client, each with its UI module, and the schemas of their
// values, which the build writes next to core's runtime for R.

export { coreOptions, darkMode, nRows, setTheme, showConditions } from "./options.ts";
export type { BlockrOptions } from "./options.ts";
export { optionSchemas } from "./schemas.ts";
