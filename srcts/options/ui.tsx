// The UI modules of core's board options, each a React component drawn with
// an input of core's React SDK and bound to the board option's value, so that
// it keeps the contract's rules as core's block UIs do
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).

import type { ReactElement } from "react";
import { ComboBox, ComboBoxItem, NumberInput } from "../inputs/index.ts";
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

// A select of several of the strings given, such as the severities of
// show_conditions, which submits the picks in the order of the strings.
export function multiSelectUi(label: string, values: readonly string[]): OptionModule<string[]> {
  const items = values.map((id) => ({ id }));
  function MultiSelectUi({ host }: ModuleProps<OptionHost<string[]>>): ReactElement {
    return (
      <ComboBox bound={useValue(host)} selectionMode="multiple" label={label} defaultItems={items} ordered>
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
    );
  }
  return reactModule(MultiSelectUi);
}

// A select of one of the items, each a value by its id and how it reads by
// its name, such as light or dark for dark_mode, which a board option always
// holds one of.
export function selectUi<V extends Json = string>(
  label: string,
  placeholder: string,
  items: readonly { readonly id: string; readonly name: string }[],
): OptionModule<V> {
  function SelectUi({ host }: ModuleProps<OptionHost<V>>): ReactElement {
    return (
      <ComboBox bound={useValue(host)} label={label} placeholder={placeholder} defaultItems={items} isRequired>
        {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
      </ComboBox>
    );
  }
  return reactModule(SelectUi);
}
