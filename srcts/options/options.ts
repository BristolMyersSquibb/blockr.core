// Core's board options, each an instance of Option that a constructor makes
// from CoreApi, as Board options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#board-options):
// n_rows, the rows of a table output's page, show_conditions, the severities
// that a front-end shows, and dark_mode, which switches the theme. Each keeps
// the label and the category of today's R constructor, and takes its schema
// from ./schemas.ts and its UI module from ./inputs.ts.
//
// Each default is a function that reads the blockr option of the board
// option's name, which today's R constructor reads, through
// CoreApi.blockrOption(), with today's fallback, so that a deployment still
// sets the default. Core calls the function only once a component requires
// the board option, so that nothing is read before then. Where R holds a value
// that the board option's schema refuses, the default reports it and takes
// the fallback.

import type { Option } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, report } from "../util/index.ts";
import { checkboxGroupInput, numberInput, selectInput } from "./inputs.ts";
import { optionSchemas } from "./schemas.ts";

// What the constructors read of CoreApi.
export type BlockrOptions = Pick<CoreApi, "blockrOption">;

// Core's board options, which core registers before the store opens a board.
export function coreOptions(core: BlockrOptions): Option[] {
  return [nRows(core), showConditions(core), darkMode(core)];
}

// The rows of a table output's page, which the coercion of table outputs
// reads, so that every renderer of table outputs requires it.
export function nRows(core: BlockrOptions): Option {
  const schema = optionSchemas.n_rows;
  const fallback = 50;
  return {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema,
    default: async () =>
      checked("n_rows", schema, await core.blockrOption<Json>("n_rows", fallback), fallback),
    ui: numberInput("Preview rows", schema.minimum),
  };
}

const SEVERITIES = [
  { value: "message", label: "Messages" },
  { value: "warning", label: "Warnings" },
  { value: "error", label: "Errors" },
] as const;

// The severities of a block's conditions that a front-end shows, while R
// reports every severity.
export function showConditions(core: BlockrOptions): Option {
  const schema = optionSchemas.show_conditions;
  const fallback = ["warning", "error"] as const;
  return {
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema,
    // R writes a blockr option of one severity as a string, as an
    // environment variable gives it.
    default: async () => {
      const value = await core.blockrOption<Json>("show_conditions", fallback);
      return checked("show_conditions", schema, typeof value === "string" ? [value] : value, fallback);
    },
    ui: checkboxGroupInput("Message types to display", SEVERITIES),
  };
}

// Whether the board shows in its light or its dark theme. Today's fallback is
// NULL, under which the page follows the theme that the user's system
// prefers, so the default is that theme where no blockr option sets one. As
// today, TRUE stands for dark and FALSE for light.
export function darkMode(core: BlockrOptions): Option {
  const schema = optionSchemas.dark_mode;
  return {
    id: "dark_mode",
    label: "Light/dark mode",
    category: "Theme options",
    schema,
    default: async () => {
      const value = await core.blockrOption<Json>("dark_mode", null);
      const preferred = preferredTheme();
      if (value === null) return preferred;
      if (typeof value === "boolean") return value ? "dark" : "light";
      return checked("dark_mode", schema, value, preferred);
    },
    ui: selectInput("Light/dark mode", [
      { value: "light", label: "Light" },
      { value: "dark", label: "Dark" },
    ]),
    onChange(value) {
      setTheme(value);
    },
  };
}

// Switches the page to a theme, "light" or "dark", in the attribute
// data-bs-theme of its root element. Bootstrap and blockr.ui's design tokens
// read that attribute, as bslib's dark mode switch sets it today, and so does
// core's dark variant, as in `bc:dark:bg-gray-900`.
export function setTheme(theme: Json): void {
  document.documentElement.setAttribute("data-bs-theme", theme === "dark" ? "dark" : "light");
}

// The theme that the user's system prefers.
function preferredTheme(): "light" | "dark" {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}

// The value of the blockr option where the board option's schema takes it, or
// else the fallback, once the value it refuses is reported.
function checked(id: OptionId, schema: JsonSchema, value: Json, fallback: Json): Json {
  const failures = compile(`the board option "${id}"`, schema)(value);
  if (failures.length === 0) return value;
  report(
    new Error(
      `The blockr option ${id} is ${JSON.stringify(value)}, which the board option ${id} does not take, so its default is ${JSON.stringify(fallback)}.`,
    ),
  );
  return fallback;
}
