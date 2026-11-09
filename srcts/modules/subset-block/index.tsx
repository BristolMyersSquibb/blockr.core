// The UI of the subset block: a text input for each of its R expressions,
// one that selects rows and one that selects columns.

import type { ReactElement } from "react";
import { reactModule, Stack, TextInput, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function SubsetBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  return (
    <Stack>
      <TextInput bound={useValue(host, "subset")} label="Subset" placeholder="Enter a row subsetting expression." />
      <TextInput bound={useValue(host, "select")} label="Select" placeholder="Enter a column selection expression." />
    </Stack>
  );
}

export default reactModule(SubsetBlock);
