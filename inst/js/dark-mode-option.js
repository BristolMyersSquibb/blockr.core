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

// srcts/options/ui.tsx
import { ComboBox, ComboBoxItem, NumberField } from "@blockr/core/react";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ jsx(NumberField, { bound: useValue(host), label, minValue: min, step: 1 });
  }
  return reactModule(NumberUi);
}
function selectUi(label, placeholder, items) {
  function SelectUi({ host }) {
    return /* @__PURE__ */ jsx(ComboBox, { bound: useValue(host), label, placeholder, defaultItems: items, isRequired: true, children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.name }) });
  }
  return reactModule(SelectUi);
}

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
  ui: numberUi("Preview rows", optionSchemas.n_rows.minimum)
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
  ui: selectUi("Light/dark mode", "Choose a mode", [
    { id: "light", name: "Light" },
    { id: "dark", name: "Dark" }
  ]),
  onChange(value) {
    setTheme(value);
  }
};
function setTheme(theme) {
  document.documentElement.setAttribute("data-bc-theme", theme === "dark" ? "dark" : "light");
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

// srcts/modules/dark-mode-option/index.ts
var api = 1;
var dark_mode_option_default = darkMode;
export {
  api,
  dark_mode_option_default as default
};
