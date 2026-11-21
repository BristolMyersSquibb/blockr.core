// The date picker and the date range picker of core's React SDK: React Aria's
// DatePicker and DateRangePicker (https://react-aria.adobe.com/DatePicker,
// https://react-aria.adobe.com/DateRangePicker), in core's theme.
// They are drawn after the DatePicker and the DateRangePicker of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DatePicker.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DateRangePicker.tsx),
// and take what those take: the props of their part of React Aria, and the
// label, the description and the error message that the starter kit adds.
// The date picker holds the field of a date, which the user types into as
// into the date field (./field.tsx), and the range picker one such field for
// each end of the range.
// The button at the field's right opens the SDK's calendar, or its range
// calendar (./calendar.tsx), in a popover of the SDK, in which the user picks
// the date, or the first and the last day of the range.
// React Aria draws the popover in a portal at the end of the page's body, as
// every overlay of the SDK (../overlays/index.ts).
//
// Given `bound`, the date picker shows and edits a date, a block's state arg
// or a board option's value, which useValue() binds, as the SDK's other inputs
// do (../inputs/inputs.tsx): a string such as "2026-11-19", or
// "2026-11-19 14:30:00" where its granularity takes a time (./values.ts).
// The range picker shows and edits a range in the same way, as
// { "start": "2026-11-19", "end": "2026-11-26" }.
// Each submits a pick in its calendar at once, and what the user typed into
// its field as the user leaves the picker or presses Enter (./bound.ts).
// The date picker submits only a date from minValue to maxValue that
// isDateUnavailable leaves, and the range picker only a range whose ends lie
// from minValue to maxValue, and whose start comes no later than its end.
// A bound picker takes React Aria's validationBehavior "aria" by default, so
// that it marks a value that it does not take as the user types it.
// It says below itself why the host refused what it submitted.

import { useRef } from "react";
import type { KeyboardEvent, ReactElement } from "react";
import { composeRenderProps, FieldError, Group, Label, Text } from "../aria/index.ts";
import type { ValidationResult } from "../aria/index.ts";
import { DatePicker as AriaDatePicker, DateRangePicker as AriaDateRangePicker } from "./aria.ts";
import type { DatePickerProps as AriaDatePickerProps, DateRange, DateRangePickerProps as AriaDateRangePickerProps, DateValue } from "./aria.ts";
import { FieldButton, NotAvailable, Refusal } from "../inputs/index.ts";
import { Popover } from "../overlays/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { useBoundDate } from "./bound.ts";
import type { BoundDate } from "./bound.ts";
import { Calendar, RangeCalendar } from "./calendar.tsx";
import { DateInput, INLINE } from "./field.tsx";
import { dateFits, rangeFits, readDate, readRange, takesTime, writeDate, writeRange } from "./values.ts";

export interface DatePickerProps<T extends DateValue> extends AriaDatePickerProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export interface DateRangePickerProps<T extends DateValue> extends AriaDateRangePickerProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function DatePicker<T extends DateValue>({ bound, ...props }: DatePickerProps<T>): ReactElement {
  return bound === undefined ? <DrawnPicker {...props} /> : <BoundPicker {...(props as DrawnProps<DateValue>)} bound={bound} />;
}

export function DateRangePicker<T extends DateValue>({ bound, ...props }: DateRangePickerProps<T>): ReactElement {
  return bound === undefined ? (
    <DrawnRangePicker {...props} />
  ) : (
    <BoundRangePicker {...(props as DrawnRangeProps<DateValue>)} bound={bound} />
  );
}

// What a picker draws with, and the state's value that it shows as missing,
// where it shows one.
type DrawnProps<T extends DateValue> = Omit<DatePickerProps<T>, "bound"> & { readonly missing?: string | null };
type DrawnRangeProps<T extends DateValue> = Omit<DateRangePickerProps<T>, "bound"> & { readonly missing?: string | null };

// The handlers through which a bound picker hands its binding what the user
// does: a change while its calendar is open, which is a pick, and any other
// change, which is an edit of its field, which it submits as the focus leaves
// the picker or as the user presses Enter in its field.
// React passes on a key that the user presses in the calendar to the picker,
// whose popover React Aria draws in a portal, so a key counts only where it
// comes from the picker's own element.
function usePicking<V>(value: BoundDate<V>) {
  const open = useRef(false);
  return {
    onOpenChange: (isOpen: boolean): void => {
      open.current = isOpen;
    },
    onChange: (next: V | null): void => {
      if (open.current) value.pick(next);
      else value.edit(next);
    },
    onBlur: (): void => {
      value.flush();
    },
    onKeyDown: (event: KeyboardEvent<HTMLDivElement>): void => {
      if (event.key === "Enter" && event.currentTarget.contains(event.target as Node)) value.flush();
    },
  };
}

function BoundPicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true,
  });
  const picking = usePicking(date);
  return (
    <>
      <DrawnPicker
        {...props}
        validationBehavior={validationBehavior}
        value={date.value}
        missing={date.missing}
        onOpenChange={(isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        }}
        onChange={(value) => {
          picking.onChange(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          picking.onBlur();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundRangePicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}: DrawnRangeProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const time = takesTime(props.granularity);
  const range = useBoundDate<DateRange>(bound, {
    read: (value) => readRange(value, time),
    write: (value) => writeRange(value, time),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: props.isRequired === true,
  });
  const picking = usePicking(range);
  return (
    <>
      <DrawnRangePicker
        {...props}
        validationBehavior={validationBehavior}
        value={range.value}
        missing={range.missing}
        onOpenChange={(isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        }}
        onChange={(value) => {
          picking.onChange(value);
          onChange?.(value);
        }}
        onBlur={(event) => {
          picking.onBlur();
          onBlur?.(event);
        }}
        onKeyDown={(event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the field with its button, the description, the error message,
// which says why the picker is invalid, or which value of the state it shows
// as missing, and the popover with the calendar.
function DrawnPicker<T extends DateValue>({ label, description, errorMessage, missing = null, isInvalid, className, ...props }: DrawnProps<T>): ReactElement {
  return (
    <AriaDatePicker
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <DateInput className={`${INLINE} bc:flex-1`} />
        <Opener />
      </Group>
      <PickerParts description={description} errorMessage={errorMessage} missing={missing} />
      <Popover className={POPOVER}>
        <Calendar />
      </Popover>
    </AriaDatePicker>
  );
}

// The field holds the two fields of the range, with a dash between them.
function DrawnRangePicker<T extends DateValue>({
  label,
  description,
  errorMessage,
  missing = null,
  isInvalid,
  className,
  ...props
}: DrawnRangeProps<T>): ReactElement {
  return (
    <AriaDateRangePicker
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Group className={FIELD}>
        <div className={ENDS}>
          <DateInput slot="start" className={INLINE} />
          <span aria-hidden="true" className="bc:px-1 bc:text-text-muted">
            –
          </span>
          <DateInput slot="end" className={`${INLINE} bc:flex-1`} />
        </div>
        <Opener />
      </Group>
      <PickerParts description={description} errorMessage={errorMessage} missing={missing} />
      <Popover className={POPOVER}>
        <RangeCalendar />
      </Popover>
    </AriaDateRangePicker>
  );
}

function PickerParts({
  description,
  errorMessage,
  missing,
}: Pick<DrawnProps<DateValue>, "description" | "errorMessage" | "missing">): ReactElement {
  return (
    <>
      {/* React Aria names the description of the picker only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      {missing === null || missing === undefined ? null : <NotAvailable values={[missing]} />}
    </>
  );
}

// The button at the field's right, which opens the calendar, the SDK's button
// inside a field, as in the starter kit.
function Opener(): ReactElement {
  return (
    <FieldButton>
      <svg
        width="14"
        height="14"
        viewBox="0 0 16 16"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.4"
        strokeLinecap="round"
        strokeLinejoin="round"
        aria-hidden="true"
      >
        <rect x="2.5" y="3.5" width="11" height="10" rx="1.5" />
        <path d="M2.5 6.5h11M5.5 2v3M10.5 2v3" />
      </svg>
    </FieldButton>
  );
}

// The classes of the picker and its parts.
// The field is a box like core's other inputs, which holds the segments and
// the button, and draws the outline of the focus while the focus is in it.
// The fields of a range scroll sideways where they are wider than the box.
const FIELD =
  "bc:flex bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
const ENDS = "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:overflow-x-auto bc:[scrollbar-width:none]";
const POPOVER = "bc:p-3";
