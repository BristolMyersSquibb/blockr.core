// srcts/modules/csv-block/index.tsx
import { FormLayout, TextField } from "@blockr/ui";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function CsvBlock({ host }) {
  return /* @__PURE__ */ jsxs(FormLayout, { children: [
    /* @__PURE__ */ jsx(TextField, { bound: useValue(host, "sep"), label: "Field separator", spellCheck: "false" }),
    /* @__PURE__ */ jsx(TextField, { bound: useValue(host, "quote"), label: "Quoting characters", spellCheck: "false" })
  ] });
}
var csv_block_default = reactModule(CsvBlock);
export {
  api,
  csv_block_default as default
};
