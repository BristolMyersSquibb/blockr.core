// srcts/modules/merge-block/index.tsx
import { strings } from "@blockr/core/binding";
import { Checkbox, ComboBox, ComboBoxItem, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function MergeBlock({ host }) {
  const shared = useRequest(host, "shared");
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(
      ComboBox,
      {
        bound: useValue(host, "by"),
        selectionMode: "multiple",
        label: "By columns",
        defaultItems: strings(shared.answer).map((id) => ({ id })),
        renderEmptyState: () => "The inputs share no column.",
        outdated: shared.loading || shared.error !== null,
        children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.id })
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
