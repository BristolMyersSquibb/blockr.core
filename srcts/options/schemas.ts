// The JSON Schemas of the values of core's board options, by the board option's
// id, which say what values each takes, with a description for a model.
// The build writes each beside the board option's module, in a file named by
// the module, as inst/js/n-rows-option.schema.json,
// by which R writes the value of a board option that an app script gives,
// so that show_conditions set to "error" in a script reaches the client as the
// array its schema has.

import type { OptionId } from "../types/board.ts";
import type { JsonSchema } from "../types/json.ts";

export const optionSchemas = {
  n_rows: {
    type: "integer",
    // A page of a table holds a row at least, as the coercion of table
    // outputs takes it.
    minimum: 1,
    description: "How many rows of a table a block's output shows",
  },
  show_conditions: {
    type: "array",
    items: { enum: ["message", "warning", "error"] },
    uniqueItems: true,
    description: "The severities of a block's conditions that the board shows",
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme",
  },
} satisfies Record<OptionId, JsonSchema>;
