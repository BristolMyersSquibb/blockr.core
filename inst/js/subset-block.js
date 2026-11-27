// srcts/modules/subset-block/index.tsx
import { FormLayout, TextField } from "@blockr/ui";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function SubsetBlock({ host }) {
  return /* @__PURE__ */ jsxs(FormLayout, { children: [
    /* @__PURE__ */ jsx(TextField, { bound: useValue(host, "subset"), label: "Subset", placeholder: "Enter a row subsetting expression.", spellCheck: "false" }),
    /* @__PURE__ */ jsx(TextField, { bound: useValue(host, "select"), label: "Select", placeholder: "Enter a column selection expression.", spellCheck: "false" })
  ] });
}
var subset_block_default = reactModule(SubsetBlock);
export {
  api,
  subset_block_default as default
};
