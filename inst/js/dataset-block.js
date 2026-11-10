// srcts/modules/dataset-block/index.tsx
import { asChoices, strings } from "@blockr/core/binding";
import { reactModule, Select, Stack, useRequest, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
var api = 1;
function DatasetBlock({ host }) {
  const pkg = useValue(host, "package").value;
  const datasets = useRequest(host, "datasets", { reads: pkg ?? null }).answer;
  return /* @__PURE__ */ jsx(Stack, { children: /* @__PURE__ */ jsx(
    Select,
    {
      bound: useValue(host, "dataset"),
      label: "Dataset",
      placeholder: "Choose a dataset",
      choices: datasets === void 0 ? void 0 : asChoices(strings(datasets))
    }
  ) });
}
var dataset_block_default = reactModule(DatasetBlock);
export {
  api,
  dataset_block_default as default
};
