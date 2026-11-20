// The checkbox that selects a row of the SDK's Table, GridList or Tree:
// React Aria's CheckboxField and CheckboxButton
// (https://react-aria.adobe.com/Checkbox), in core's theme, after the Checkbox
// of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Checkbox.tsx).
// A row tells it whether it is ticked, through React Aria's slot "selection".
// So does the header of a table where the user selects several rows.
// React Aria names it to assistive technology.
// So it takes neither `bound` nor a label.
// It shows a tick where the row is selected, and a dash in the header where
// some rows are.
// The SDK's own Checkbox, which binds a value, is still a native checkbox.

import type { ReactElement } from "react";
import { CheckboxButton, CheckboxField } from "../aria/index.ts";
import { Tick } from "./combobox.tsx";

export function SelectionCheckbox(): ReactElement {
  return (
    <CheckboxField slot="selection" className="bc:flex">
      <CheckboxButton className={BUTTON}>
        {({ isSelected, isIndeterminate }) => (
          <span className={BOX}>{isIndeterminate ? <Dash /> : isSelected ? <Tick /> : null}</span>
        )}
      </CheckboxButton>
    </CheckboxField>
  );
}

// The dash of a header whose table has some rows selected.
function Dash(): ReactElement {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" aria-hidden="true">
      <path d="M4 8h8" />
    </svg>
  );
}

const BUTTON =
  "bc:group bc:flex bc:cursor-default bc:items-center bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:[-webkit-tap-highlight-color:transparent] bc:data-disabled:opacity-50";
// The box draws the outline of the focus, which its button takes.
// It takes the accent colour where the row is selected.
// The button's attributes say both.
const BOX =
  "bc:box-border bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border bc:border-border-default bc:text-text-on-accent bc:outline-offset-2 bc:group-data-focus-visible:outline-2 bc:group-data-focus-visible:outline-border-accent bc:group-data-selected:border-bg-accent bc:group-data-selected:bg-bg-accent bc:group-data-indeterminate:border-bg-accent bc:group-data-indeterminate:bg-bg-accent bc:forced-colors:group-data-selected:bg-[Highlight]";
