// The list box of core's React SDK: React Aria's ListBox
// (https://react-aria.adobe.com/ListBox), in core's theme, after the ListBox
// of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ListBox.tsx).
// It takes the props of React Aria's ListBox.
// It shows a list of items in place, in a box like a field's, which picks one
// item, or several, where its selectionMode does.
// Its rows are ListBoxItem below, the rows of the ComboBox's list
// (./combobox.tsx), and its sections those of that list.
// An Autocomplete around it, with a SearchField, narrows its rows to those
// that hold what the user types, as React Aria builds a select's list alone.
// A list box takes no `bound`: a caller that picks with it holds what it
// picks itself.

import type { ReactElement } from "react";
import { composeRenderProps, ListBox as AriaListBox } from "../aria/index.ts";
import type { ListBoxItemProps, ListBoxProps } from "../aria/index.ts";
import { ComboBoxItem } from "./combobox.tsx";

export function ListBox<T extends object>({ className, ...props }: ListBoxProps<T>): ReactElement {
  return <AriaListBox {...props} className={composeRenderProps(className, (given) => `${LISTBOX} ${given ?? ""}`)} />;
}

export function ListBoxItem(props: ListBoxItemProps): ReactElement {
  return <ComboBoxItem {...props} />;
}

// The box of the rows, which scrolls them within its height where the caller
// gives it one.
const LISTBOX =
  "bc:box-border bc:grid bc:min-w-0 bc:content-start bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-empty:px-2.5 bc:data-empty:py-2 bc:data-empty:italic bc:data-empty:text-text-muted";
