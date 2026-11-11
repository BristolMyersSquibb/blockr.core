// srcts/modules/scatter-block/index.tsx
import { asChoices, strings } from "@blockr/core/binding";
import { reactModule, Select, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function ScatterBlock({ host }) {
  const columns = useRequest(host, "columns");
  const choices = columns.answer === void 0 ? void 0 : asChoices(strings(columns.answer));
  const outdated = columns.loading || columns.error !== null;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(
      Select,
      {
        bound: useValue(host, "x"),
        label: "X-axis",
        placeholder: "Choose a column",
        choices,
        outdated
      }
    ),
    /* @__PURE__ */ jsx(
      Select,
      {
        bound: useValue(host, "y"),
        label: "Y-axis",
        placeholder: "Choose a column",
        choices,
        outdated
      }
    )
  ] });
}
var scatter_block_default = reactModule(ScatterBlock);
export {
  api,
  scatter_block_default as default
};
