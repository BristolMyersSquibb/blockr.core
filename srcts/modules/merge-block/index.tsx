// The UI of the merge block: a select of the columns its inputs share,
// which the request shared gives, to join by,
// and a checkbox for each input, to keep the rows of it that the other does
// not match.
// While the request asks again, as after an input changed,
// and after an ask failed, the columns are those of older data,
// so the select marks no column.

import type { ReactElement } from "react";
import { strings } from "../../binding/index.ts";
import { Checkbox, ComboBox, ComboBoxItem, reactModule, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function MergeBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const shared = useRequest(host, "shared");
  return (
    <Stack>
      <ComboBox
        bound={useValue(host, "by")}
        selectionMode="multiple"
        label="By columns"
        defaultItems={strings(shared.answer).map((id) => ({ id }))}
        renderEmptyState={() => "The inputs share no column."}
        outdated={shared.loading || shared.error !== null}
      >
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
      <Checkbox bound={useValue(host, "all_x")} label="Keep the rows of x that y does not match" />
      <Checkbox bound={useValue(host, "all_y")} label="Keep the rows of y that x does not match" />
    </Stack>
  );
}

export default reactModule(MergeBlock);
