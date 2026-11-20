// The colour field of core's React SDK: React Aria's ColorField
// (https://react-aria.adobe.com/ColorField), in core's theme, which edits a
// colour as the text of its hex, as "#66C2A5".
// It is drawn after the ColorField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ColorField.tsx),
// and takes what that takes: the props of React Aria's ColorField, and the
// label, the description and the error message that the starter kit adds.
// React Aria reports a colour only as the user leaves the field or presses
// Enter, or steps the colour by an arrow key, and writes the hex anew then.
//
// Given `bound`, it shows and edits a colour, a block's state arg or a board
// option's value, which useValue() binds, as the SDK's other inputs do: a
// string of hex (./bound.tsx).
// It submits the colour as React Aria reports it.
// The hex holds no alpha, so the field keeps the alpha of the colour it
// showed.
// Where the user clears the field, or leaves it with no colour that React
// Aria reads, it shows the colour of the state again and submits nothing.
// While the user types, the field keeps showing the colour that it showed as
// the user started, until the user leaves the field or presses Enter.
// React Aria would otherwise write a colour that the host announces meanwhile
// over the text that the user typed.
// It says below itself why the host refused what it submitted.
// Inside a bound ColorPicker, a colour that it reports is one that the picker
// submits.

import { useContext, useState } from "react";
import type { InputEventHandler, ReactElement } from "react";
import { ColorField as AriaColorField, parseColor } from "../aria-colors/index.ts";
import type { ColorFieldProps as AriaColorFieldProps } from "../aria-colors/index.ts";
import { composeRenderProps, FieldError, Input, Label, Text } from "../aria/index.ts";
import type { ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Ended, hex, Refusal, valueColor } from "./bound.tsx";

export interface ColorFieldProps extends AriaColorFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function ColorField({ bound, ...props }: ColorFieldProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundField {...props} bound={bound} />;
}

type DrawnProps = Omit<ColorFieldProps, "bound"> & {
  // What the user typed, as the text input hears each keystroke.
  readonly onInput?: InputEventHandler<HTMLInputElement>;
};

function BoundField({ bound, onChange, onBlur, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const value = valueColor(bound.value);
  // The colour that the field showed as the user started to type.
  const [typing, setTyping] = useState<{ readonly value: string | undefined } | null>(null);
  const shown = typing === null ? value : typing.value;
  return (
    <>
      <Drawn
        {...props}
        value={shown ?? null}
        onInput={() => {
          setTyping((now) => now ?? { value: shown });
        }}
        onChange={(color) => {
          setTyping(null);
          if (color !== null) {
            const alpha = shown === undefined ? 1 : parseColor(shown).getChannelValue("alpha");
            void bound.commit(hex(color.withChannelValue("alpha", alpha)));
          }
          onChange?.(color);
        }}
        onBlur={(event) => {
          setTyping(null);
          onBlur?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field, the description and the error message.
// The field tells a bound ColorPicker around it of each colour it reports.
function Drawn({ label, description, errorMessage, className, onChange, onInput, ...props }: DrawnProps): ReactElement {
  const ended = useContext(Ended);
  return (
    <AriaColorField
      {...props}
      onChange={(color) => {
        onChange?.(color);
        if (color !== null) ended?.(color);
      }}
      className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Input onInput={onInput} className={styles.box} />
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaColorField>
  );
}
