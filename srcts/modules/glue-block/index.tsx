// The UI of the glue block: a text area for the text that glue::glue() fills
// in, which refers to each input by the name of its link.

import type { ReactElement } from "react";
import { Stack, TextInput } from "../../inputs/index.ts";
import { reactModule, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

function GlueBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  return (
    <Stack>
      <TextInput bound={useValue(host, "text")} label="Text" placeholder="{nrow(data)} rows" lines={4} />
    </Stack>
  );
}

export default reactModule(GlueBlock);
