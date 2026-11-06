// What core's board options n_rows, show_conditions and dark_mode define, but
// for their UI modules, which srcts/option-ui gives them, so that code without
// React registers them too, and the schemas of their values, which the build
// writes next to core's runtime for R.

export {
  coreDefinitions,
  darkModeDefinition,
  nRowsDefinition,
  setTheme,
  SEVERITIES,
  showConditionsDefinition,
} from "./definitions.ts";
export type { BlockrOptions, Definition } from "./definitions.ts";
export { optionSchemas } from "./schemas.ts";
