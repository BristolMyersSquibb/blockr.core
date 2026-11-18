// srcts/modules/scatter-block/index.tsx
import { strings } from "@blockr/core/binding";
import { ComboBox, ComboBoxItem, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function ScatterBlock({ host }) {
  const columns = useRequest(host, "columns");
  const items = strings(columns.answer).map((id) => ({ id }));
  const outdated = columns.loading || columns.error !== null;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(ComboBox, { bound: useValue(host, "x"), label: "X-axis", placeholder: "Choose a column", defaultItems: items, outdated, children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.id }) }),
    /* @__PURE__ */ jsx(ComboBox, { bound: useValue(host, "y"), label: "Y-axis", placeholder: "Choose a column", defaultItems: items, outdated, children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.id }) })
  ] });
}
var scatter_block_default = reactModule(ScatterBlock);
export {
  api,
  scatter_block_default as default
};
