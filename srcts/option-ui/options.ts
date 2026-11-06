// Core's board options, each an instance of Option that a constructor makes
// from CoreApi, as Board options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#board-options):
// n_rows, the rows of a table output's page, show_conditions, the severities
// that a front-end shows, and dark_mode, which switches the theme. Each is its
// definition from srcts/options with its UI module from ./ui.tsx.

import type { Option } from "../store/index.ts";
import {
  darkModeDefinition,
  nRowsDefinition,
  optionSchemas,
  SEVERITIES,
  showConditionsDefinition,
} from "../options/index.ts";
import type { BlockrOptions } from "../options/index.ts";
import { checklistUi, numberUi, selectUi } from "./ui.tsx";

// Core's board options, which core registers before the store opens a board.
export function coreOptions(core: BlockrOptions): Option[] {
  return [nRows(core), showConditions(core), darkMode(core)];
}

// The rows of a table output's page, which the coercion of table outputs
// reads, so that every renderer of table outputs requires it.
export function nRows(core: BlockrOptions): Option {
  return { ...nRowsDefinition(core), ui: numberUi("Preview rows", optionSchemas.n_rows.minimum) };
}

// The severities of a block's conditions that a front-end shows, while R
// reports every severity.
export function showConditions(core: BlockrOptions): Option {
  return { ...showConditionsDefinition(core), ui: checklistUi("Message types to display", SEVERITIES) };
}

// Whether the board shows in its light or its dark theme.
export function darkMode(core: BlockrOptions): Option {
  return {
    ...darkModeDefinition(core),
    ui: selectUi("Light/dark mode", "Choose a mode", [
      { value: "light", label: "Light" },
      { value: "dark", label: "Dark" },
    ]),
  };
}
