// The UI modules of core's board options, each a React component drawn with
// an input of core's React SDK and bound to the board option's value, so that
// it keeps the contract's rules as core's block UIs do
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).

import type { ReactElement } from "react";
import type { Choice } from "../binding/index.ts";
import { Checklist, NumberInput, Select } from "../inputs/index.ts";
import { reactModule, useValue } from "../react/index.ts";
import type { ModuleProps } from "../react/index.ts";
import type { Json } from "../types/json.ts";
import type { OptionHost, OptionModule } from "../types/hosts.ts";

// A number input for a whole number of at least `min`, such as n_rows.
export function numberUi(label: string, min: number): OptionModule<number> {
  function NumberUi({ host }: ModuleProps<OptionHost<number>>): ReactElement {
    return <NumberInput bound={useValue(host)} label={label} min={min} />;
  }
  return reactModule(NumberUi);
}

// A checkbox for each choice, such as each severity of show_conditions, which
// submits the checked ones in the order of the choices.
export function checklistUi(legend: string, choices: readonly string[]): OptionModule<string[]> {
  function ChecklistUi({ host }: ModuleProps<OptionHost<string[]>>): ReactElement {
    return <Checklist bound={useValue(host)} legend={legend} choices={choices} ordered />;
  }
  return reactModule(ChecklistUi);
}

// A select of one of the choices, such as light or dark for dark_mode.
export function selectUi<V extends Json = string>(
  label: string,
  placeholder: string,
  choices: readonly Choice[],
): OptionModule<V> {
  function SelectUi({ host }: ModuleProps<OptionHost<V>>): ReactElement {
    return <Select bound={useValue(host)} label={label} placeholder={placeholder} choices={choices} />;
  }
  return reactModule(SelectUi);
}
