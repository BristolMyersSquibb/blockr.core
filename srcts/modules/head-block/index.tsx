// The UI of the head block: a number field for the whole number of rows it
// keeps, whose largest value is the number of rows of its input,
// which the request n_max gives, and toggle buttons of the end it keeps them
// from, which always hold one end.
// While the request asks again, as after the input changed,
// and after an ask failed, that number is the one of older data,
// so the number field marks no number above it.

import type { ReactElement } from "react";
import { NumberField, reactModule, Stack, ToggleButton, ToggleButtonGroup, useRequest, useValue } from "../../react/index.ts";
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
      <NumberField bound={useValue(host, "n")} label="Number of rows" minValue={0} maxValue={max} step={1} outdated={outdated} />
      <ToggleButtonGroup bound={useValue(host, "direction")} label="Direction" disallowEmptySelection>
        {ends.map((end) => (
          <ToggleButton key={end.id} id={end.id}>
            {end.name}
          </ToggleButton>
        ))}
      </ToggleButtonGroup>
    </Stack>
  );
}

export default reactModule(HeadBlock);
