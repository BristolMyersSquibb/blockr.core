// srcts/modules/head-block/index.tsx
import { ComboBox, ComboBoxItem, NumberInput, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var ends = [
  { id: "head", name: "Head" },
  { id: "tail", name: "Tail" }
];
function HeadBlock({ host }) {
  const rows = useRequest(host, "n_max");
  const max = Array.isArray(rows.answer) && typeof rows.answer[0] === "number" ? rows.answer[0] : void 0;
  const outdated = rows.loading || rows.error !== null;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(NumberInput, { bound: useValue(host, "n"), label: "Number of rows", min: 0, max, outdated }),
    /* @__PURE__ */ jsx(ComboBox, { bound: useValue(host, "direction"), label: "Direction", placeholder: "Choose an end", defaultItems: ends, children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.name }) })
  ] });
}
var head_block_default = reactModule(HeadBlock);
export {
  api,
  head_block_default as default
};
