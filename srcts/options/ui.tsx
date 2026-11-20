// The UI modules of core's board options, each a React component drawn with
// an input of core's React SDK and bound to the board option's value, so that
// it keeps the contract's rules as core's block UIs do
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).

import type { ReactElement } from "react";
import { Checkbox, CheckboxGroup, NumberField, ToggleButton, ToggleButtonGroup } from "../inputs/index.ts";
import { reactModule, useValue } from "../react/index.ts";
import type { ModuleProps } from "../react/index.ts";
import type { Json } from "../types/json.ts";
import type { OptionHost, OptionModule } from "../types/hosts.ts";

// A number field for a whole number of at least `min`, such as n_rows.
export function numberUi(label: string, min: number): OptionModule<number> {
  function NumberUi({ host }: ModuleProps<OptionHost<number>>): ReactElement {
    return <NumberField bound={useValue(host)} label={label} minValue={min} step={1} />;
  }
  return reactModule(NumberUi);
}

// A checkbox for each of the strings given, such as the severities of
// show_conditions, which submits the strings checked in the order of the
// strings.
export function checkboxGroupUi(label: string, values: readonly string[]): OptionModule<string[]> {
  function CheckboxGroupUi({ host }: ModuleProps<OptionHost<string[]>>): ReactElement {
    return (
      <CheckboxGroup bound={useValue(host)} label={label} ordered>
        {values.map((value) => (
          <Checkbox key={value} value={value}>
            {value}
          </Checkbox>
        ))}
      </CheckboxGroup>
    );
  }
  return reactModule(CheckboxGroupUi);
}

// A toggle button for each of the items, each a value by its id and how it
// reads by its name, such as light or dark for dark_mode, of which a board
// option always holds one.
export function toggleButtonGroupUi<V extends Json = string>(
  label: string,
  items: readonly { readonly id: string; readonly name: string }[],
): OptionModule<V> {
  function ToggleButtonGroupUi({ host }: ModuleProps<OptionHost<V>>): ReactElement {
    return (
      <ToggleButtonGroup bound={useValue(host)} label={label} disallowEmptySelection>
        {items.map((item) => (
          <ToggleButton key={item.id} id={item.id}>
            {item.name}
          </ToggleButton>
        ))}
      </ToggleButtonGroup>
    );
  }
  return reactModule(ToggleButtonGroupUi);
}
