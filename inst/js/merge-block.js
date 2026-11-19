// srcts/modules/merge-block/index.tsx
import { strings } from "@blockr/core/binding";
import { ComboBox, ComboBoxItem, reactModule, Stack, Text, useRequest, useValue } from "@blockr/core/react";
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var joins = [
  { id: "inner", name: "Inner", description: "Only the rows that match", args: { all_x: false, all_y: false } },
  { id: "left", name: "Left", description: "Every row of x", args: { all_x: true, all_y: false } },
  { id: "right", name: "Right", description: "Every row of y", args: { all_x: false, all_y: true } },
  { id: "full", name: "Full", description: "Every row of x and y", args: { all_x: true, all_y: true } }
];
function MergeBlock({ host }) {
  const shared = useRequest(host, "shared");
  const x = useValue(host, "all_x").value === true;
  const y = useValue(host, "all_y").value === true;
  const join = joins.find(({ args }) => args.all_x === x && args.all_y === y);
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
    /* @__PURE__ */ jsx(
      ComboBox,
      {
        label: "Join type",
        defaultItems: joins,
        value: join?.id ?? null,
        isRequired: true,
        onChange: (key) => {
          const picked = joins.find(({ id }) => id === key);
          if (picked !== void 0) submit(host, picked.args);
        },
        children: (item) => /* @__PURE__ */ jsxs(ComboBoxItem, { id: item.id, textValue: item.name, children: [
          /* @__PURE__ */ jsx(Text, { slot: "label", children: item.name }),
          /* @__PURE__ */ jsx(Text, { slot: "description", children: item.description })
        ] })
      }
    )
  ] });
}
function submit(host, args) {
  const state = host.state();
  if (state.all_x === args.all_x && state.all_y === args.all_y) return;
  host.submit(args).catch(() => void 0);
}
var merge_block_default = reactModule(MergeBlock);
export {
  api,
  merge_block_default as default
};
