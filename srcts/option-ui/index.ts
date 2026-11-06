// Core's board options with their UI modules, written in React, which the page
// registers before the store opens a board. Their definitions without a UI are
// in srcts/options, for code without React.

export { coreOptions, darkMode, nRows, showConditions } from "./options.ts";
export { checklistUi, numberUi, selectUi } from "./ui.tsx";
