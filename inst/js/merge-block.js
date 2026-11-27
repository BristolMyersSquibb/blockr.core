// srcts/modules/merge-block/index.tsx
import { ComboBox, ComboBoxItem, FormLayout, Select, SelectItem, Text } from "@blockr/ui";
import { strings } from "@blockr/core/binding";
import { reactModule, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var joins = [
  { id: "inner", name: "Inner", description: "Only the rows that match", args: { all_x: false, all_y: false } },
  { id: "left", name: "Left", description: "Every row of x", args: { all_x: true, all_y: false } },
  { id: "right", name: "Right", description: "Every row of y", args: { all_x: false, all_y: true } },
  { id: "full", name: "Full", description: "Every row of x and y", args: { all_x: true, all_y: true } }
];
function joinOf(state) {
  return joins.find(({ args }) => args.all_x === state.all_x && args.all_y === state.all_y)?.id;
}
function argsOf(id) {
  return joins.find((join) => join.id === id)?.args ?? {};
}
function MergeBlock({ host }) {
  const shared = useRequest(host, "shared");
  return /* @__PURE__ */ jsxs(FormLayout, { children: [
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
    /* @__PURE__ */ jsx(Select, { bound: useValue(host, joinOf, argsOf), label: "Join type", items: joins, isRequired: true, children: (item) => /* @__PURE__ */ jsxs(SelectItem, { id: item.id, textValue: item.name, children: [
      /* @__PURE__ */ jsx(Text, { slot: "label", children: item.name }),
      /* @__PURE__ */ jsx(Text, { slot: "description", children: item.description })
    ] }) })
  ] });
}
var merge_block_default = reactModule(MergeBlock);
export {
  api,
  merge_block_default as default
};
