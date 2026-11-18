// The UI of the dataset block: a select of the data frames that the block's
// package ships, which the request datasets gives.
// While the request asks again, as after the package changed,
// and after an ask failed, the data frames may be those of another package,
// so the select marks no dataset.

import type { ReactElement } from "react";
import { strings } from "../../binding/index.ts";
import { ComboBox, ComboBoxItem, reactModule, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function DatasetBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  // The request reads the state arg package, which the host sends from the
  // block's state, so it is asked again where package changes.
  const pkg = useValue(host, "package").value;
  const datasets = useRequest(host, "datasets", { reads: pkg ?? null });
  return (
    <Stack>
      <ComboBox
        bound={useValue(host, "dataset")}
        label="Dataset"
        placeholder="Choose a dataset"
        defaultItems={strings(datasets.answer).map((id) => ({ id }))}
        outdated={datasets.loading || datasets.error !== null}
      >
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
    </Stack>
  );
}

export default reactModule(DatasetBlock);
