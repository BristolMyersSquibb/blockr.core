// The UI of the dataset block: a select of the data frames that the block's
// package ships, which the request datasets gives.

import type { ReactElement } from "react";
import { choicesOf, strings } from "../../binding/index.ts";
import { Select, Stack } from "../../inputs/index.ts";
import { reactModule, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function DatasetBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  // The request reads the state arg package, which the host sends from the
  // block's state, so it is asked again where package changes.
  const pkg = useValue(host, "package").value;
  const datasets = useRequest(host, "datasets", { reads: pkg ?? null }).answer;
  return (
    <Stack>
      <Select
        bound={useValue(host, "dataset")}
        label="Dataset"
        placeholder="Choose a dataset"
        choices={choicesOf(strings(datasets))}
      />
    </Stack>
  );
}

export default reactModule(DatasetBlock);
