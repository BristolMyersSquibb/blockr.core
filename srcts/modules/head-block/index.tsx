// The UI of the head block: a number input for the number of rows it keeps,
// whose largest value is the number of rows of its input,
// which the request n_max gives, and a select of the end it keeps them from.
// While the request asks again, as after the input changed,
// and after an ask failed, that number is the one of older data,
// so the number input marks no number above it.

import type { ReactElement } from "react";
import { ComboBox, ComboBoxItem, NumberInput, reactModule, Stack, useRequest, useValue } from "../../react/index.ts";
import type { ModuleProps } from "../../react/index.ts";
import type { BlockHost } from "../../types/hosts.ts";
import type { State } from "./state.ts";

export const api = 1;

const ends = [
  { id: "head", name: "Head" },
  { id: "tail", name: "Tail" },
];

function HeadBlock({ host }: ModuleProps<BlockHost<State>>): ReactElement {
  const rows = useRequest(host, "n_max");
  const max = Array.isArray(rows.answer) && typeof rows.answer[0] === "number" ? rows.answer[0] : undefined;
  const outdated = rows.loading || rows.error !== null;
  return (
    <Stack>
      <NumberInput bound={useValue(host, "n")} label="Number of rows" min={0} max={max} outdated={outdated} />
      <ComboBox bound={useValue(host, "direction")} label="Direction" placeholder="Choose an end" defaultItems={ends} isRequired>
        {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
      </ComboBox>
    </Stack>
  );
}

export default reactModule(HeadBlock);
