// srcts/modules/subset-block/index.tsx
import { reactModule, Stack, TextInput, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function SubsetBlock({ host }) {
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(TextInput, { bound: useValue(host, "subset"), label: "Subset", placeholder: "Enter a row subsetting expression." }),
    /* @__PURE__ */ jsx(TextInput, { bound: useValue(host, "select"), label: "Select", placeholder: "Enter a column selection expression." })
  ] });
}
var subset_block_default = reactModule(SubsetBlock);
export {
  api,
  subset_block_default as default
};
