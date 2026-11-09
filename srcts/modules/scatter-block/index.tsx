// The UI of the scatter block: a select for each axis of the columns of its
// input, which the request columns gives.

import type { ReactElement } from "react";
import { choicesOf, strings } from "../../binding/index.ts";
import { reactModule, Select, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function ScatterBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const columns = choicesOf(strings(useRequest(host, "columns").answer));
  return (
    <Stack>
      <Select bound={useValue(host, "x")} label="X-axis" placeholder="Choose a column" choices={columns} />
      <Select bound={useValue(host, "y")} label="Y-axis" placeholder="Choose a column" choices={columns} />
    </Stack>
  );
}

export default reactModule(ScatterBlock);
