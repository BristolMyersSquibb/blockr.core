// Core's board options, each an instance of Option that a constructor makes
// from CoreApi, as Board options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#board-options):
// n_rows, the rows of a table output's page, show_conditions, the severities
// that a front-end shows, and dark_mode, which switches the theme. Each keeps
// the label and the category of today's R constructor, and takes its schema
// from ./schemas.ts and its UI module from ./ui.tsx.
//
// Each default is a function that reads the blockr option of the board
// option's name, which today's R constructor reads, through
// CoreApi.blockrOption(), so that a deployment still sets the default, and
// takes a fallback only where nothing sets the blockr option. The fallbacks
// are today's, but for n_rows, whose page holds 10 rows where today's holds
// 50. Core calls the function only once a component requires the board
// option, so that nothing is read before then.
// Each default reads a string, as an environment variable gives, as R reads
// it for the board option, and where R holds a value that the board option's
// schema then refuses, the default fails, naming the value as R sent it, so
// that no reader gets the fallback in place of the value that was set.

import type { Option } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, logicalOf, numberOf } from "../util/index.ts";
import { optionSchemas } from "./schemas.ts";
import { checklistUi, numberUi, selectUi } from "./ui.tsx";

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
  const fallback = 10;
  return {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema,
    // A string reads as R's as.numeric() reads it.
    default: async () => {
      const value = await core.blockrOption("n_rows");
      if (value === undefined) return fallback;
      return checked("n_rows", schema, value, typeof value === "string" ? numberOf(value) : value);
    },
    ui: numberUi("Preview rows", schema.minimum),
  };
}

const SEVERITIES = ["message", "warning", "error"] as const;

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
      const value = await core.blockrOption("show_conditions");
      if (value === undefined) return fallback;
      return checked("show_conditions", schema, value, typeof value === "string" ? [value] : value);
    },
    ui: checklistUi("Message types to display", SEVERITIES),
  };
}

// Whether the board shows in its light or its dark theme. Today's fallback is
// NULL, under which the page follows the theme that the user's system
// prefers, so the default is that theme where no blockr option sets one. As
// today, TRUE stands for dark and FALSE for light, and so does a string that
// R's as.logical() reads as one, as an environment variable gives it.
export function darkMode(core: BlockrOptions): Option {
  const schema = optionSchemas.dark_mode;
  return {
    id: "dark_mode",
    label: "Light/dark mode",
    category: "Theme options",
    schema,
    default: async () => {
      const value = await core.blockrOption("dark_mode");
      if (value === undefined) return preferredTheme();
      const logical = typeof value === "string" ? logicalOf(value) : value;
      if (typeof logical === "boolean") return logical ? "dark" : "light";
      return checked("dark_mode", schema, value);
    },
    ui: selectUi("Light/dark mode", "Choose a mode", [
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

// The value of the blockr option, as the default reads what R sent, where the
// board option's schema takes it. A value that the schema refuses fails the
// default, naming what R sent. So does a number that JSON cannot hold, as
// "Inf" reads, since R would get null in its place, while the schema's
// validator takes Infinity for an integer.
function checked(id: OptionId, schema: JsonSchema, sent: Json, value: Json = sent): Json {
  const json = typeof value !== "number" || Number.isFinite(value);
  if (json && compile(`the board option "${id}"`, schema)(value).length === 0) return value;
  throw new TypeError(`The blockr option ${id} is ${JSON.stringify(sent)}, which the board option ${id} does not take.`);
}
