// Core's board options, each an instance of Option without its id, which the
// module of the board option in ../modules exports as its default, and which
// core registers under the id that R's reply to `hello` gives it, as Board
// options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#defining-a-board-option):
// n_rows, the rows of a table output's page, and dark_mode, which switches the
// theme. Each keeps the label and the category of today's R constructor, and
// takes its schema from ./schemas.ts and its UI module from ./ui.tsx.
//
// Each default is a function of CoreApi that reads the blockr option of the
// board option's name, which today's R constructor reads, through
// CoreApi.blockrOption(), so that a deployment still sets the default, and
// takes a fallback only where nothing sets the blockr option. The fallbacks
// are today's, but for n_rows, whose page holds 10 rows where today's holds
// 50.
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
import { numberUi, toggleButtonGroupUi } from "./ui.tsx";

// What a board option's default reads of CoreApi.
export type BlockrOptions = Pick<CoreApi, "blockrOption">;

// The rows of a table output's page, which the coercion of table outputs
// reads, so that the table family holds its constructor in R.
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

// Whether the board shows in its light or its dark theme. Today's fallback is
// NULL, under which the page follows the theme that the user's system
// prefers, so the default is that theme where no blockr option sets one. As
// today, TRUE stands for dark and FALSE for light, and so does a string that
// R's as.logical() reads as one, as an environment variable gives it.
// The mode is a reader's choice rather than the board's, so dark_mode keeps
// its value with the user, in the browser, and no board holds it, as Board
// options that stay with the user has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#board-options-that-stay-with-the-user).
// Core stores nothing for the default, so that a page follows the system's
// preference until the user picks a mode. The line that R writes into the
// page's head sets the mode before the page first paints from the same three:
// the user's pick, else the blockr option, else the system's preference
// (R/utils-serve.R). So the board option and the page agree once the board
// opens.
export const darkMode: Omit<Option, "id"> = {
  label: "Light/dark mode",
  category: "Theme options",
  schema: optionSchemas.dark_mode,
  scope: "user",
  default: async (core: BlockrOptions) => {
    const value = await core.blockrOption("dark_mode");
    if (value === undefined) return preferredTheme();
    const logical = typeof value === "string" ? asLogical(value) : value;
    if (typeof logical === "boolean") return logical ? "dark" : "light";
    return checked("dark_mode", takesTheme, value);
  },
  ui: toggleButtonGroupUi("Light/dark mode", [
    { id: "light", name: "Light" },
    { id: "dark", name: "Dark" },
  ]),
  onChange(value) {
    setTheme(value);
  },
};

// Core's board options by id, as core lists them in R, each with its module.
export const coreOptions: Readonly<Record<OptionId, Omit<Option, "id">>> = {
  n_rows: nRows,
  dark_mode: darkMode,
};

// Switches the page to a mode, "light" or "dark",
// in the attribute data-bk-mode of the page's root element
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#light-mode-and-dark-mode).
// The attribute takes blockr.ui's prefix, since blockr.ui's stylesheet keys
// the values of the theme variables in each mode on it, and sets the page's
// color-scheme by it.
// The dark variant of every package's classes keys on it through blockr.ui's
// build preset, and so do the colours of a block's statuses in core's
// stylesheet.
// The page holds no Bootstrap, so core sets no data-bs-theme.
// Core also measures an output's info again as the attribute changes.
export function setTheme(theme: Json): void {
  document.documentElement.setAttribute("data-bk-mode", theme === "dark" ? "dark" : "light");
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

function takesTheme(value: Json): boolean {
  const allowed: readonly Json[] = optionSchemas.dark_mode.enum;
  return allowed.includes(value);
}
