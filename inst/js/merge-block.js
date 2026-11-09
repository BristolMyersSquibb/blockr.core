// srcts/modules/merge-block/index.tsx
import { strings } from "@blockr/core/binding";
import { Checkbox, Checklist, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function MergeBlock({ host }) {
  const shared = useRequest(host, "shared").answer;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(
      Checklist,
      {
        bound: useValue(host, "by"),
        legend: "By columns",
        choices: shared === void 0 ? void 0 : strings(shared),
        empty: "The inputs share no column."
      }
    ),
    /* @__PURE__ */ jsx(Checkbox, { bound: useValue(host, "all_x"), label: "Keep the rows of x that y does not match" }),
    /* @__PURE__ */ jsx(Checkbox, { bound: useValue(host, "all_y"), label: "Keep the rows of y that x does not match" })
  ] });
}
var merge_block_default = reactModule(MergeBlock);
export {
  api,
  merge_block_default as default
};
