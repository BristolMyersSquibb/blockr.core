// The UI of the subset block: a text field for each of its R expressions,
// one that selects rows and one that selects columns, which the browser does
// not check the spelling of.

import type { ReactElement } from "react";
import { reactModule, Stack, TextField, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function SubsetBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  return (
    <Stack>
      <TextField bound={useValue(host, "subset")} label="Subset" placeholder="Enter a row subsetting expression." spellCheck="false" />
      <TextField bound={useValue(host, "select")} label="Select" placeholder="Enter a column selection expression." spellCheck="false" />
    </Stack>
  );
}

export default reactModule(SubsetBlock);
