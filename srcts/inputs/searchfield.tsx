// The search field of core's React SDK: React Aria's SearchField
// (https://react-aria.adobe.com/SearchField), in core's theme.
// It is drawn after the SearchField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/SearchField.tsx),
// and takes what that takes: the props of React Aria's SearchField, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// Its field is a box like the select's (./combobox.tsx).
// The box holds a magnifier, the text field and a button at its right.
// The button clears what the user typed, as Escape does, and hides while the
// field is empty.
// Inside React Aria's Autocomplete, what the user types narrows the rows of
// the collection that the Autocomplete holds, such as the SDK's ListBox
// (./listbox.tsx).
// It takes no `bound`, as it searches for its caller.

import type { ReactElement } from "react";
import { Button, composeRenderProps, FieldError, Group, Input, Label, SearchField as AriaSearchField, Text } from "../aria/index.ts";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "../aria/index.ts";
import { styles } from "../styles/index.ts";
import { CLEAR, Cross, FIELD, INPUT } from "./combobox.tsx";

export interface SearchFieldProps extends AriaSearchFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
}

// The label, the field, the description and the error message.
export function SearchField({ label, description, errorMessage, placeholder, className, ...props }: SearchFieldProps): ReactElement {
  return (
    <AriaSearchField {...props} className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <Magnifier />
        <Input placeholder={placeholder} className={`${INPUT} ${SEARCH}`} />
        <Button className={`${CLEAR} bc:group-data-empty:invisible`}>
          <Cross />
        </Button>
      </Group>
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaSearchField>
  );
}

// The magnifier at the field's left.
function Magnifier(): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      aria-hidden="true"
      className="bc:shrink-0 bc:text-text-muted"
    >
      <circle cx="5" cy="5" r="3.5" />
      <path d="M7.75 7.75l2.75 2.75" />
    </svg>
  );
}

// The browser's own button that clears a search input, and its own look of
// one, give way to the field's.
const SEARCH = "bc:appearance-none bc:[&::-webkit-search-cancel-button]:hidden";
