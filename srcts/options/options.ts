// Core's board options, each an instance of Option without its id, which the
// module of the board option in ../modules exports as its default, and which
// core registers under the id that R's reply to `hello` gives it, as Board
// options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#defining-a-board-option):
// n_rows, the rows of a table output's page, show_conditions, the severities
// that a frontend shows, and dark_mode, which switches the theme. Each keeps
// the label and the category of today's R constructor, and takes its schema
// from ./schemas.ts and its UI module from ./ui.tsx.
//
// Each default is a function of CoreApi that reads the blockr option of the
// board option's name, which today's R constructor reads, through
// CoreApi.blockrOption(), so that a deployment still sets the default, and
// takes a fallback only where nothing sets the blockr option. The fallbacks
// are today's, but for n_rows, whose page holds 10 rows where today's holds
// 50. The fallback of show_conditions names warnings and errors, as today's
// does: why a block is invalid or waiting is part of its status, which a
// frontend shows whatever show_conditions names.
// Core calls the function only where something needs the board option and the
// board lacks its value, as a mounted renderer or an onChange() does, so that
// nothing is read before then.
// Each default reads a string, as an environment variable gives, as the
// number or the logical that it holds, and where R holds a value that the
// board option's schema then refuses, the default fails, naming the value as
// R sent it, so that no reader gets the fallback in place of the value that
// was set.

import type { Option } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { asLogical, asNumber } from "../util/index.ts";
import { optionSchemas } from "./schemas.ts";
import { checklistUi, numberUi, selectUi } from "./ui.tsx";

// What a board option's default reads of CoreApi.
export type BlockrOptions = Pick<CoreApi, "blockrOption">;

// The rows of a table output's page, which the coercion of table outputs
// reads, so that every renderer of table outputs requires it.
export const nRows: Omit<Option, "id"> = {
  label: "Preview rows",
  category: "Table options",
  schema: optionSchemas.n_rows,
  // A string reads as the number that it holds.
  default: async (core: BlockrOptions) => {
    const value = await core.blockrOption("n_rows");
    if (value === undefined) return 10;
    return checked("n_rows", takesRows, value, typeof value === "string" ? asNumber(value) : value);
  },
  ui: numberUi("Preview rows", optionSchemas.n_rows.minimum),
};

const SEVERITIES = ["message", "warning", "error"] as const;

// The severities of a block's conditions that a frontend shows, while R
// reports every severity.
export const showConditions: Omit<Option, "id"> = {
  label: "Message types to display",
  category: "Board options",
  schema: optionSchemas.show_conditions,
  // R writes a blockr option of one severity as a string, as an environment
  // variable gives it.
  default: async (core: BlockrOptions) => {
    const value = await core.blockrOption("show_conditions");
    if (value === undefined) return ["warning", "error"];
    return checked("show_conditions", takesSeverities, value, typeof value === "string" ? [value] : value);
  },
  ui: checklistUi("Message types to display", SEVERITIES),
};

// Whether the board shows in its light or its dark theme. Today's fallback is
// NULL, under which the page follows the theme that the user's system
// prefers, so the default is that theme where no blockr option sets one. As
// today, TRUE stands for dark and FALSE for light, and so does a string that
// R's as.logical() reads as one, as an environment variable gives it.
export const darkMode: Omit<Option, "id"> = {
  label: "Light/dark mode",
  category: "Theme options",
  schema: optionSchemas.dark_mode,
  default: async (core: BlockrOptions) => {
    const value = await core.blockrOption("dark_mode");
    if (value === undefined) return preferredTheme();
    const logical = typeof value === "string" ? asLogical(value) : value;
    if (typeof logical === "boolean") return logical ? "dark" : "light";
    return checked("dark_mode", takesTheme, value);
  },
  ui: selectUi("Light/dark mode", "Choose a mode", [
    { value: "light", label: "Light" },
    { value: "dark", label: "Dark" },
  ]),
  onChange(value) {
    setTheme(value);
  },
};

// Core's board options by id, as core lists them in R, each with its module.
export const coreOptions: Readonly<Record<OptionId, Omit<Option, "id">>> = {
  n_rows: nRows,
  show_conditions: showConditions,
  dark_mode: darkMode,
};

// Switches the page to a theme, "light" or "dark",
// in the attribute data-bc-theme of the page's root element.
// Like core's utilities and theme variables, the attribute takes core's prefix.
// The page holds no Bootstrap, so core sets no data-bs-theme.
// Core reads the attribute in its dark variant, as in `bc:dark:text-gray-100`,
// in the page's color-scheme and in the dark default of bg-raised.
// Core also measures an output's info again as the attribute changes.
// A package that styles the dark theme keys off the attribute as well.
export function setTheme(theme: Json): void {
  document.documentElement.setAttribute("data-bc-theme", theme === "dark" ? "dark" : "light");
}

// The theme that the user's system prefers.
function preferredTheme(): "light" | "dark" {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}

// The value of the blockr option, as the default reads what R sent, where the
// board option's schema takes it. A value that the schema refuses fails the
// default, naming what R sent.
function checked(id: OptionId, takes: (value: Json) => boolean, sent: Json, value: Json = sent): Json {
  if (takes(value)) return value;
  throw new TypeError(`The blockr option ${id} is ${JSON.stringify(sent)}, which the board option ${id} does not take.`);
}

// Whether each of core's board options takes a value, as its schema in
// ./schemas.ts has it. Each checks the schema's few rules itself, so that the
// module of each board option bundles no JSON Schema validator, which core's
// runtime holds, and with which the store checks every value that goes onto the
// board.
function takesRows(value: Json): boolean {
  return typeof value === "number" && Number.isInteger(value) && value >= optionSchemas.n_rows.minimum;
}

function takesSeverities(value: Json): boolean {
  const allowed: readonly Json[] = optionSchemas.show_conditions.items.enum;
  if (!Array.isArray(value)) return false;
  const given: readonly Json[] = value;
  return given.every((x) => allowed.includes(x)) && new Set(given).size === given.length;
}

function takesTheme(value: Json): boolean {
  const allowed: readonly Json[] = optionSchemas.dark_mode.enum;
  return allowed.includes(value);
}
