// The text field of core's React SDK, which core's own UIs, frontends, the
// UIs of plugins and those of blocks all draw with: React Aria's TextField
// (https://react-aria.adobe.com/TextField), in core's theme.
// It is drawn after the TextField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TextField.tsx),
// and takes what that takes: the props of React Aria's TextField, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// React Aria's TextField holds either an Input or a TextArea, and has no prop
// that picks one.
// So where `lines` is above 1, the field holds a TextArea of that many rows,
// for text of several lines.
//
// Given `bound`, it shows and edits a string, a block's state arg or a board
// option's value, which useValue() binds, as the SDK's other inputs do
// (./inputs.tsx).
// It hands what the user types to the binding, which submits it once the user
// pauses, and at once as the user leaves the field or presses Enter.
// In a text area, where Enter starts a new line, Ctrl+Enter or Cmd+Enter
// submits at once.
// It keeps what the user typed until the binding shows the value again.
// It says below itself why the host refused what it submitted.
// A key that it hears goes on to the elements around it, as a dialog that
// closes on Escape needs.

import type { ReactElement } from "react";
import { composeRenderProps, FieldError, Input, Label, Text, TextArea, TextField as AriaTextField } from "../aria/index.ts";
import type { TextFieldProps as AriaTextFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Refusal, useTyped } from "./inputs.tsx";

export interface TextFieldProps extends AriaTextFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  // The rows of a text area, for text of several lines, where above 1.
  readonly lines?: number;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function TextField({ bound, ...props }: TextFieldProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundText {...props} bound={bound} />;
}

type DrawnProps = Omit<TextFieldProps, "bound">;

function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
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
          if (event.key === "Enter" && !event.nativeEvent.isComposing && (lines <= 1 || event.ctrlKey || event.metaKey)) {
            bound.flush();
          }
          event.continuePropagation();
          onKeyDown?.(event);
        }}
        onBlur={(event) => {
          bound.flush();
          onBlur?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field, the description and the error message.
function Drawn({ label, description, errorMessage, placeholder, lines = 1, className, ...props }: DrawnProps): ReactElement {
  return (
    <AriaTextField {...props} className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      {lines > 1 ? (
        <TextArea rows={lines} placeholder={placeholder} className={styles.box} />
      ) : (
        <Input placeholder={placeholder} className={styles.box} />
      )}
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaTextField>
  );
}
