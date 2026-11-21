// The number field of core's React SDK, which core's own UIs, frontends, the
// UIs of plugins and those of blocks all draw with: React Aria's NumberField
// (https://react-aria.adobe.com/NumberField), in core's theme.
// It is drawn after the NumberField of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/NumberField.tsx),
// with its buttons that step the number up and down, and takes what that
// takes: the props of React Aria's NumberField, such as minValue, maxValue,
// step and formatOptions, and the label, the description, the error message
// and the placeholder that the starter kit adds.
// While it is `outdated`, as while the request that gives its maxValue asks
// again, its maxValue may be for older data, so it takes none.
//
// Given `bound`, it shows and edits a number that useValue() binds, a block's
// state arg, a board option's value or a value that a function reads from a
// block's state, as the SDK's other inputs do (./inputs.tsx).
// React Aria reports a number only as the user presses Enter, leaves the field
// or steps the number.
// So the field also hands the binding the number that its text reads as,
// after each keystroke, and the binding submits it once the user pauses.
// It submits at once on Enter and as the user leaves the field.
// A step of the number, by a button of the field or an arrow key, is a pick,
// which it submits at once too, and which gives way to the state again where
// the host refuses it.
// It submits only a number in its range, and on its step where it has one.
// A bound field takes React Aria's commitBehavior "validate" by default, so
// that it keeps a number outside them as it is, rather than pull it in.
// So it shows the state as the host holds it, outside the range too, as
// once the largest number shrank, and marks such a number, as the user types
// it too.
// It says why in the words of the browser's own number input.
// On Enter and as the user leaves the field, a number that does not fit gives
// way to the state again.
// It says below itself why the host refused what it submitted.
//
// React Aria writes the number anew, in its format, whenever the value that
// the field shows changes, which would move the text under the user's cursor,
// as once the binding submitted a number on a pause.
// So while the user types, the field keeps the value that it showed as the
// user started, and shows the state again only once the user ends typing, or
// as the host announces a number other than the one the user typed, such as
// one that the assistant set.

import { useContext, useEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  composeRenderProps,
  FieldError,
  Group,
  Input,
  Label,
  NumberField as AriaNumberField,
  NumberFieldStateContext,
  Text,
} from "../aria/index.ts";
import type { NumberFieldProps as AriaNumberFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Refusal } from "./inputs.tsx";

export interface NumberFieldProps extends AriaNumberFieldProps {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function NumberField({ bound, outdated = false, maxValue, ...props }: NumberFieldProps): ReactElement {
  const largest = outdated ? undefined : maxValue;
  return bound === undefined ? (
    <Drawn {...props} maxValue={largest} />
  ) : (
    <BoundNumber {...props} maxValue={largest} bound={bound} />
  );
}

type DrawnProps = Omit<NumberFieldProps, "bound" | "outdated"> & {
  // What React Aria's state reads as the user types, inside the field.
  readonly watch?: ReactNode;
};

// The value that the field shows in place of the state: the value it showed as
// the user started to type, until the user ends typing, or the number that
// the user ended typing with or stepped to, until the binding shows a value
// again.
interface Kept {
  readonly value: number;
  readonly typing: boolean;
}

function BoundNumber({
  bound,
  onChange,
  onInput,
  onBlur,
  commitBehavior = "validate",
  validationBehavior = "aria",
  ...props
}: DrawnProps & { readonly bound: Bound }): ReactElement {
  const held = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState<Kept | null>(null);
  // The number that the user's text last read as, and the keystrokes so far,
  // by which the field hears the number that React Aria reads after each.
  const [typed, setTyped] = useState(NaN);
  const [keystrokes, setKeystrokes] = useState(0);
  const [seen, setSeen] = useState(held);
  if (!Object.is(seen, held)) {
    setSeen(held);
    if (kept !== null && (!kept.typing || !Object.is(held, typed))) setKept(null);
  }
  const fits = (value: number): boolean => inRange(value, props.minValue, props.maxValue, props.step);

  return (
    <>
      <Drawn
        {...props}
        commitBehavior={commitBehavior}
        validationBehavior={validationBehavior}
        value={kept?.value ?? held}
        onInput={(event) => {
          setKept((now) => (now?.typing === true ? now : { value: now?.value ?? held, typing: true }));
          setKeystrokes((n) => n + 1);
          onInput?.(event);
        }}
        onChange={(value) => {
          // React Aria commits: on Enter or as the user leaves the field,
          // which ends typing, or as the user steps the number.
          if (!fits(value)) {
            bound.type(undefined);
            bound.flush();
            setKept(null);
          } else if (kept?.typing === true) {
            bound.type(value);
            bound.flush();
            setKept(Object.is(value, held) ? null : { value, typing: false });
          } else {
            setKept({ value, typing: false });
            void bound.commit(value).then((took) => {
              if (!took) setKept(null);
            });
          }
          onChange?.(value);
        }}
        onBlur={(event) => {
          // Where the user typed the number that the field showed, React Aria
          // commits nothing, and the user ends typing as the field loses the
          // focus.
          setKept((now) => (now?.typing === true ? null : now));
          onBlur?.(event);
        }}
        watch={
          <Watch
            keystrokes={keystrokes}
            onType={(value) => {
              setTyped(value);
              bound.type(fits(value) ? value : undefined);
            }}
          />
        }
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Hands `onType` the number that React Aria reads from the field's text,
// once after each keystroke, as React Aria has read it.
function Watch({ keystrokes, onType }: { keystrokes: number; onType: (value: number) => void }): null {
  const state = useContext(NumberFieldStateContext);
  const value = state?.numberValue ?? NaN;
  const heard = useRef(0);
  useEffect(() => {
    if (keystrokes === heard.current) return;
    heard.current = keystrokes;
    onType(value);
  }, [keystrokes, value, onType]);
  return null;
}

// Whether the number fits the range, and the step where there is one, which
// counts from the smallest number, or from 0, as the browser's own number
// input counts it.
// The slider takes a number so too (./slider.tsx).
export function inRange(value: number, min: number | undefined, max: number | undefined, step: number | undefined): boolean {
  if (Number.isNaN(value)) return false;
  if (min !== undefined && value < min) return false;
  if (max !== undefined && value > max) return false;
  if (step === undefined || !(step > 0)) return true;
  const steps = (value - (min ?? 0)) / step;
  return Math.abs(steps - Math.round(steps)) < 1e-9;
}

// The label, the field with its buttons, the description and the error
// message.
function Drawn({ label, description, errorMessage, placeholder, watch, className, onInput, ...props }: DrawnProps): ReactElement {
  return (
    <AriaNumberField {...props} className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <Input placeholder={placeholder} onInput={onInput} className={INPUT} />
        <div className={STEPPERS}>
          <Button slot="increment" className={STEPPER}>
            <Chevron up />
          </Button>
          <Button slot="decrement" className={STEPPER}>
            <Chevron up={false} />
          </Button>
        </div>
      </Group>
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      {watch}
    </AriaNumberField>
  );
}

// The chevron of a button that steps the number, up or down.
function Chevron({ up }: { up: boolean }): ReactElement {
  return (
    <svg
      width="10"
      height="10"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <polyline points={up ? "3 7.5 6 4.5 9 7.5" : "3 4.5 6 7.5 9 4.5"} />
    </svg>
  );
}

// The classes of the field and its parts.
// The field is a box like core's other inputs, which holds the text field and
// the buttons, and draws the outline of the focus while the focus is in it.
const FIELD =
  "bc:flex bc:min-w-0 bc:items-stretch bc:justify-self-stretch bc:overflow-hidden bc:rounded-md bc:border bc:border-border-field bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
const INPUT =
  "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted";
const STEPPERS = "bc:flex bc:flex-col bc:border-l bc:border-border-field bc:group-data-invalid:border-border-danger";
const STEPPER =
  "bc:flex bc:flex-1 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:px-1 bc:py-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
