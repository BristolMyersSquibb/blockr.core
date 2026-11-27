// srcts/util/util.ts
function asNumber(text) {
  const number = text.trim() === "" ? NaN : Number(text);
  return Number.isFinite(number) ? number : NaN;
}
function asLogical(text) {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return void 0;
}

// srcts/util/modules.ts
function loadedModule(load) {
  return {
    async render(args) {
      const module = await load();
      if (args.signal.aborted) return void 0;
      return module.render(args);
    }
  };
}

// srcts/options/schemas.ts
var optionSchemas = {
  n_rows: {
    type: "integer",
    // A page of a table holds a row at least, as the coercion of table
    // outputs takes it.
    minimum: 1,
    description: "How many rows of a table a block's output shows"
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme"
  }
};

// srcts/options/options.ts
var nRows = {
  label: "Preview rows",
  category: "Table options",
  schema: optionSchemas.n_rows,
  // A string reads as the number that it holds.
  default: async (core) => {
    const value = await core.blockrOption("n_rows");
    if (value === void 0) return 10;
    return checked("n_rows", takesRows, value, typeof value === "string" ? asNumber(value) : value);
  },
  ui: loadedModule(async () => (await import("./options-ui-71b069b6.js")).numberUi("Preview rows", optionSchemas.n_rows.minimum))
};
var darkMode = {
  label: "Light/dark mode",
  category: "Theme options",
  schema: optionSchemas.dark_mode,
  default: async (core) => {
    const value = await core.blockrOption("dark_mode");
    if (value === void 0) return preferredTheme();
    const logical = typeof value === "string" ? asLogical(value) : value;
    if (typeof logical === "boolean") return logical ? "dark" : "light";
    return checked("dark_mode", takesTheme, value);
  },
  ui: loadedModule(
    async () => (await import("./options-ui-71b069b6.js")).toggleButtonGroupUi("Light/dark mode", [
      { id: "light", name: "Light" },
      { id: "dark", name: "Dark" }
    ])
  ),
  onChange(value) {
    setTheme(value);
  }
};
function setTheme(theme) {
  document.documentElement.setAttribute("data-bk-mode", theme === "dark" ? "dark" : "light");
}
function preferredTheme() {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}
function checked(id, takes, sent, value = sent) {
  if (takes(value)) return value;
  throw new TypeError(`The blockr option ${id} is ${JSON.stringify(sent)}, which the board option ${id} does not take.`);
}
function takesRows(value) {
  return typeof value === "number" && Number.isInteger(value) && value >= optionSchemas.n_rows.minimum;
}
function takesTheme(value) {
  const allowed = optionSchemas.dark_mode.enum;
  return allowed.includes(value);
}

// srcts/modules/n-rows-option/index.ts
var api = 1;
var n_rows_option_default = nRows;
export {
  api,
  n_rows_option_default as default
};
