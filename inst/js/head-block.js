// srcts/modules/head-block/index.tsx
import { NumberInput, reactModule, Select, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var ends = [
  { value: "head", label: "Head" },
  { value: "tail", label: "Tail" }
];
function HeadBlock({ host }) {
  const rows = useRequest(host, "n_max").answer;
  const max = Array.isArray(rows) && typeof rows[0] === "number" ? rows[0] : void 0;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(NumberInput, { bound: useValue(host, "n"), label: "Number of rows", min: 0, max }),
    /* @__PURE__ */ jsx(Select, { bound: useValue(host, "direction"), label: "Direction", placeholder: "Choose an end", choices: ends })
  ] });
}
var head_block_default = reactModule(HeadBlock);
export {
  api,
  head_block_default as default
};
