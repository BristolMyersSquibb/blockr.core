// srcts/modules/csv-block/index.tsx
import { reactModule, Stack, TextInput, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function CsvBlock({ host }) {
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(TextInput, { bound: useValue(host, "sep"), label: "Field separator" }),
    /* @__PURE__ */ jsx(TextInput, { bound: useValue(host, "quote"), label: "Quoting characters" })
  ] });
}
var csv_block_default = reactModule(CsvBlock);
export {
  api,
  csv_block_default as default
};
