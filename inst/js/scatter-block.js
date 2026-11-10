// srcts/modules/scatter-block/index.tsx
import { asChoices, strings } from "@blockr/core/binding";
import { reactModule, Select, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function ScatterBlock({ host }) {
  const answer = useRequest(host, "columns").answer;
  const columns = answer === void 0 ? void 0 : asChoices(strings(answer));
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(Select, { bound: useValue(host, "x"), label: "X-axis", placeholder: "Choose a column", choices: columns }),
    /* @__PURE__ */ jsx(Select, { bound: useValue(host, "y"), label: "Y-axis", placeholder: "Choose a column", choices: columns })
  ] });
}
var scatter_block_default = reactModule(ScatterBlock);
export {
  api,
  scatter_block_default as default
};
