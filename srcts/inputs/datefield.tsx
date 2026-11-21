// The date field and the time field of core's React SDK: React Aria's
// DateField and TimeField (https://react-aria.adobe.com/DateField,
// https://react-aria.adobe.com/TimeField), in core's theme.
// They are drawn after the DateField and the TimeField of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DateField.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/TimeField.tsx),
// and take what those take: the props of their part of React Aria, and the
// label, the description and the error message that the starter kit adds.
// The user types the date or the time segment by segment, as its month, its
// day and its year, and the arrow keys step a segment up and down.
//
// Given `bound`, the date field shows and edits a date, a block's state arg or
// a board option's value, which useValue() binds, as the SDK's other inputs do
// (./inputs.tsx).
// The date is a string, as "2026-11-19", or as "2026-11-19 14:30:00" where
// the field's granularity takes a time (./dates.ts).
// The time field shows and edits a time in the same way, as "14:30:00".
// Each submits what the user typed as the user leaves the field or presses
// Enter (./bounddate.ts).
// It submits only a value from minValue to maxValue, and the date field only
// a date that isDateUnavailable leaves.
// A bound field takes React Aria's validationBehavior "aria" by default, so
// that it marks a value that it does not take as the user types it.
// It says below itself why the host refused what it submitted.

import type { ReactElement } from "react";
import {
  composeRenderProps,
  DateField as AriaDateField,
  DateInput as AriaDateInput,
  DateSegment,
  FieldError,
  Label,
  Text,
  TimeField as AriaTimeField,
} from "../aria/index.ts";
import type {
  DateFieldProps as AriaDateFieldProps,
  DateValue,
  TimeFieldProps as AriaTimeFieldProps,
  TimeValue,
  ValidationResult,
} from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { useBoundDate } from "./bounddate.ts";
import { dateFits, readDate, readTime, takesTime, timeFits, writeDate, writeTime } from "./dates.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";

export interface DateFieldProps<T extends DateValue> extends AriaDateFieldProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export interface TimeFieldProps<T extends TimeValue> extends AriaTimeFieldProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function DateField<T extends DateValue>({ bound, ...props }: DateFieldProps<T>): ReactElement {
  return bound === undefined ? <DrawnDate {...props} /> : <BoundDate {...(props as DrawnDateProps<DateValue>)} bound={bound} />;
}

export function TimeField<T extends TimeValue>({ bound, ...props }: TimeFieldProps<T>): ReactElement {
  return bound === undefined ? <DrawnTime {...props} /> : <BoundTime {...(props as DrawnTimeProps<TimeValue>)} bound={bound} />;
}

// What a field draws with, and the state's value that it shows as missing,
// where it shows one.
type DrawnDateProps<T extends DateValue> = Omit<DateFieldProps<T>, "bound"> & { readonly missing?: string | null };
type DrawnTimeProps<T extends TimeValue> = Omit<TimeFieldProps<T>, "bound"> & { readonly missing?: string | null };

function BoundDate({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnDateProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true,
  });
  return (
    <>
      <DrawnDate
        {...props}
        validationBehavior={validationBehavior}
        value={date.value}
        missing={date.missing}
        onChange={(value) => {
          date.edit(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          date.flush();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter") date.flush();
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundTime({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnTimeProps<TimeValue> & { readonly bound: Bound }): ReactElement {
  const time = useBoundDate<TimeValue>(bound, {
    read: readTime,
    write: writeTime,
    fits: (value) => timeFits(value, props),
    required: props.isRequired === true,
  });
  return (
    <>
      <DrawnTime
        {...props}
        validationBehavior={validationBehavior}
        value={time.value}
        missing={time.missing}
        onChange={(value) => {
          time.edit(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          time.flush();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          if (event.key === "Enter") time.flush();
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field, the description, the error message, and the line that
// names a value of the state that the field shows as missing, as the SDK's
// other inputs name a value that they lack.
function DrawnDate<T extends DateValue>({ label, description, errorMessage, missing = null, isInvalid, className, ...props }: DrawnDateProps<T>): ReactElement {
  return (
    <AriaDateField
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}
    >
      <FieldParts label={label} description={description} errorMessage={errorMessage} missing={missing} />
    </AriaDateField>
  );
}

function DrawnTime<T extends TimeValue>({ label, description, errorMessage, missing = null, isInvalid, className, ...props }: DrawnTimeProps<T>): ReactElement {
  return (
    <AriaTimeField
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}
    >
      <FieldParts label={label} description={description} errorMessage={errorMessage} missing={missing} />
    </AriaTimeField>
  );
}

function FieldParts({
  label,
  description,
  errorMessage,
  missing,
}: Pick<DrawnDateProps<DateValue>, "label" | "description" | "errorMessage" | "missing">): ReactElement {
  return (
    <>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <DateInput className={BOX} />
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      {missing === null || missing === undefined ? null : <NotAvailable values={[missing]} />}
    </>
  );
}

// The segments of a date or a time, which the user types into one by one, in
// the box of a field, or without one inside the box of a picker, which
// `className` gives.
// A segment that the user is in shows in the accent colour, and one without
// a value, as before the user typed it, shows its placeholder muted.
// The slot of a range's field, "start" or "end", picks its half of the range.
export function DateInput({ slot, className }: { readonly slot?: "start" | "end"; readonly className: string }): ReactElement {
  return (
    <AriaDateInput slot={slot} className={className}>
      {(segment) => <DateSegment segment={segment} className={SEGMENT} />}
    </AriaDateInput>
  );
}

// The classes of the field and its parts.
// The field is a box like core's other inputs, which scrolls the segments
// sideways where they are wider than the box, and draws the outline of the
// focus while the focus is in it.
const BOX =
  "bc:flex bc:min-w-0 bc:items-center bc:justify-self-stretch bc:overflow-x-auto bc:whitespace-nowrap bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:[scrollbar-width:none] bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
// The segments of a picker sit in its box, which draws the border and the
// outline of the focus.
export const INLINE =
  "bc:flex bc:min-w-0 bc:items-center bc:whitespace-nowrap bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:data-disabled:cursor-default";
const SEGMENT =
  "bc:rounded-sm bc:px-0.5 bc:tabular-nums bc:caret-transparent bc:outline-none bc:data-[type=literal]:px-0 bc:data-placeholder:text-text-muted bc:data-focused:bg-bg-accent bc:data-focused:text-text-on-accent";
