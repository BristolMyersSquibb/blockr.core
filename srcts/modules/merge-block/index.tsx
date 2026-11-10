// The UI of the merge block: a checkbox for each column its inputs share,
// which the request shared gives, to join by,
// and one for each input, to keep the rows of it that the other does not
// match.
// While the request asks again, as after an input changed,
// and after an ask failed, the columns are those of older data,
// so the checklist marks no column.

import type { ReactElement } from "react";
import { strings } from "../../binding/index.ts";
import { Checkbox, Checklist, reactModule, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function MergeBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const shared = useRequest(host, "shared");
  return (
    <Stack>
      <Checklist
        bound={useValue(host, "by")}
        legend="By columns"
        choices={shared.answer === undefined ? undefined : strings(shared.answer)}
        empty="The inputs share no column."
        outdated={shared.loading || shared.error !== null}
      />
      <Checkbox bound={useValue(host, "all_x")} label="Keep the rows of x that y does not match" />
      <Checkbox bound={useValue(host, "all_y")} label="Keep the rows of y that x does not match" />
    </Stack>
  );
}

export default reactModule(MergeBlock);
