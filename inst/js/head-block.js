// srcts/modules/head-block/index.tsx
import { NumberInput, reactModule, Select, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var ends = [
  { value: "head", label: "Head" },
  { value: "tail", label: "Tail" }
];
function HeadBlock({ host }) {
  const rows = useRequest(host, "n_max");
  const max = Array.isArray(rows.answer) && typeof rows.answer[0] === "number" ? rows.answer[0] : void 0;
  const outdated = rows.loading || rows.error !== null;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(NumberInput, { bound: useValue(host, "n"), label: "Number of rows", min: 0, max, outdated }),
    /* @__PURE__ */ jsx(Select, { bound: useValue(host, "direction"), label: "Direction", placeholder: "Choose an end", choices: ends })
  ] });
}
var head_block_default = reactModule(HeadBlock);
export {
  api,
  head_block_default as default
};
