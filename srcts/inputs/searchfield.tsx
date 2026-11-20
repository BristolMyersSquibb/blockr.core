// The search field of core's React SDK: React Aria's SearchField
// (https://react-aria.adobe.com/SearchField), in core's theme, after the
// SearchField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/SearchField.tsx).
// It takes what that takes: the props of React Aria's SearchField, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// Its box shows a magnifier before the text, and after it a FieldButton that
// clears the search, while there is one.
// Escape clears the search too, and goes on to the elements around the field
// where there is none, as a dialog that closes on Escape needs.
// The button is outside the order of Tab, and keeps the focus in the field.
//
// Given `bound`, it shows and edits a string, a block's state arg or a board
// option's value, which useValue() binds, as the SDK's TextField does
// (./textfield.tsx): it hands what the user types to the binding, which
// submits it once the user pauses, and at once as the user leaves the field,
// presses Enter or clears the search.
// It keeps what the user typed until the binding shows the value again.
// It says below itself why the host refused what it submitted.

import type { ReactElement } from "react";
import { composeRenderProps, FieldError, Group, Input, Label, SearchField as AriaSearchField, Text } from "../aria/index.ts";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { FieldButton } from "./button.tsx";
import { Cross, Magnifier } from "./icons.tsx";
import { Refusal, useTyped } from "./inputs.tsx";

export interface SearchFieldProps extends AriaSearchFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function SearchField({ bound, ...props }: SearchFieldProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundSearch {...props} bound={bound} />;
}

type DrawnProps = Omit<SearchFieldProps, "bound">;

function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const [text, typed] = useTyped(bound);
  return (
    <>
      <Drawn
        {...props}
        value={text ?? (typeof bound.value === "string" ? bound.value : "")}
        onChange={(value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          event.continuePropagation();
          onKeyDown?.(event);
        }}
        onBlur={(event) => {
          bound.flush();
          onBlur?.(event);
        }}
        onClear={() => {
          bound.flush();
          onClear?.();
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the box with the magnifier, the text and the button that clears
// it, the description and the error message.
function Drawn({ label, description, errorMessage, placeholder, className, ...props }: DrawnProps): ReactElement {
  return (
    <AriaSearchField {...props} className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <Magnifier />
        <Input placeholder={placeholder} className={INPUT} />
        <FieldButton className="bc:group-data-empty:invisible">
          <Cross />
        </FieldButton>
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

// The classes of the box, which draws the outline of the focus while the
// focus is in it, and of the text, which hides the browser's own button that
// clears a search.
const FIELD =
  "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1.5 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-1 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:opacity-50";
const INPUT =
  "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:[&::-webkit-search-cancel-button]:hidden";
