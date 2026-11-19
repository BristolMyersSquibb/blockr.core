// The UI of the CSV block: a text field for the field separator and one for
// the quoting characters, which utils::read.table() takes.

import type { ReactElement } from "react";
import { reactModule, Stack, TextField, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function CsvBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  return (
    <Stack>
      <TextField bound={useValue(host, "sep")} label="Field separator" spellCheck="false" />
      <TextField bound={useValue(host, "quote")} label="Quoting characters" spellCheck="false" />
    </Stack>
  );
}

export default reactModule(CsvBlock);
