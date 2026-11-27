// The UI of the scatter block: a select for each axis of the columns of its
// input, which the request columns gives.
// While the request asks again, as after the input changed,
// and after an ask failed, the columns are those of older data,
// so the selects mark no column.

import type { ReactElement } from "react";
import { ComboBox, ComboBoxItem, FormLayout } from "@blockr/ui";
import { strings } from "../../binding/index.ts";
import { reactModule, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function ScatterBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const columns = useRequest(host, "columns");
  const items = strings(columns.answer).map((id) => ({ id }));
  const outdated = columns.loading || columns.error !== null;
  return (
    <FormLayout>
      <ComboBox bound={useValue(host, "x")} label="X-axis" placeholder="Choose a column" defaultItems={items} outdated={outdated}>
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
      <ComboBox bound={useValue(host, "y")} label="Y-axis" placeholder="Choose a column" defaultItems={items} outdated={outdated}>
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
    </FormLayout>
  );
}

export default reactModule(ScatterBlock);
