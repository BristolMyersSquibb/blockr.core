// srcts/modules/dataset-block/index.tsx
import { strings } from "@blockr/core/binding";
import { ComboBox, ComboBoxItem, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
var api = 1;
function DatasetBlock({ host }) {
  const pkg = useValue(host, "package").value;
  const datasets = useRequest(host, "datasets", { reads: pkg ?? null });
  return /* @__PURE__ */ jsx(Stack, { children: /* @__PURE__ */ jsx(
    ComboBox,
    {
      bound: useValue(host, "dataset"),
      label: "Dataset",
      placeholder: "Choose a dataset",
      defaultItems: strings(datasets.answer).map((id) => ({ id })),
      outdated: datasets.loading || datasets.error !== null,
      children: (item) => /* @__PURE__ */ jsx(ComboBoxItem, { id: item.id, children: item.id })
    }
  ) });
}
var dataset_block_default = reactModule(DatasetBlock);
export {
  api,
  dataset_block_default as default
};
