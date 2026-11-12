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
  show_conditions: {
    type: "array",
    items: { enum: ["message", "warning", "error"] },
    uniqueItems: true,
    description: "The severities of a block's conditions that the board shows"
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme"
  }
};

// srcts/options/ui.tsx
import { Checklist, NumberInput, Select } from "@blockr/core/react";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ jsx(NumberInput, { bound: useValue(host), label, min });
  }
  return reactModule(NumberUi);
}
function checklistUi(legend, choices) {
  function ChecklistUi({ host }) {
    return /* @__PURE__ */ jsx(Checklist, { bound: useValue(host), legend, choices, ordered: true });
  }
  return reactModule(ChecklistUi);
}
function selectUi(label, placeholder, choices) {
  function SelectUi({ host }) {
    return /* @__PURE__ */ jsx(Select, { bound: useValue(host), label, placeholder, choices });
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
var SEVERITIES = ["message", "warning", "error"];
var showConditions = {
  label: "Message types to display",
  category: "Board options",
  schema: optionSchemas.show_conditions,
  // R writes a blockr option of one severity as a string, as an environment
  // variable gives it.
  default: async (core) => {
    const value = await core.blockrOption("show_conditions");
    if (value === void 0) return SEVERITIES;
    return checked("show_conditions", takesSeverities, value, typeof value === "string" ? [value] : value);
  },
  ui: checklistUi("Message types to display", SEVERITIES)
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
    { value: "light", label: "Light" },
    { value: "dark", label: "Dark" }
  ]),
  onChange(value) {
    setTheme(value);
  }
};
function setTheme(theme) {
  document.documentElement.setAttribute("data-bs-theme", theme === "dark" ? "dark" : "light");
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
function takesSeverities(value) {
  const allowed = optionSchemas.show_conditions.items.enum;
  if (!Array.isArray(value)) return false;
  const given = value;
  return given.every((x) => allowed.includes(x)) && new Set(given).size === given.length;
}
function takesTheme(value) {
  const allowed = optionSchemas.dark_mode.enum;
  return allowed.includes(value);
}

// srcts/modules/show-conditions-option/index.ts
var api = 1;
var show_conditions_option_default = showConditions;
export {
  api,
  show_conditions_option_default as default
};
