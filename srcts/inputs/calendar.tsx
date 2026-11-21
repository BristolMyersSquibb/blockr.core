// The calendar and the range calendar of core's React SDK: React Aria's
// Calendar and RangeCalendar (https://react-aria.adobe.com/Calendar,
// https://react-aria.adobe.com/RangeCalendar), in core's theme.
// They are drawn after the Calendar and the RangeCalendar of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Calendar.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/RangeCalendar.tsx),
// and take what those take: the props of their part of React Aria, and the
// error message that the starter kit adds.
// Each shows the months that its visibleDuration gives, one by default, side
// by side, with buttons that page to the months before and after.
// The user picks a day, or the first and the last day of a range, by a press
// or by the arrow keys and Enter.
// The date pickers of the SDK open them (./datepicker.tsx).
//
// Given `bound`, the calendar shows and picks a date, a block's state arg or
// a board option's value, which useValue() binds, as the SDK's other inputs do
// (./inputs.tsx), as a string such as "2026-11-19" (./dates.ts).
// The range calendar picks a range in the same way, as { "start":
// "2026-11-19", "end": "2026-11-26" }.
// Each submits a pick at once (./bounddate.ts).
// It says below itself why the host refused what it submitted.

import type { ReactElement } from "react";
import {
  Calendar as AriaCalendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  composeRenderProps,
  RangeCalendar as AriaRangeCalendar,
  Text,
  useLocale,
} from "../aria/index.ts";
import type {
  CalendarDate,
  CalendarProps as AriaCalendarProps,
  DateRange,
  DateValue,
  RangeCalendarProps as AriaRangeCalendarProps,
} from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { useBoundDate } from "./bounddate.ts";
import { Button } from "./button.tsx";
import { dateFits, rangeFits, readDate, readRange, writeDate, writeRange } from "./dates.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";

export interface CalendarProps<T extends DateValue> extends AriaCalendarProps<T> {
  // What the starter kit adds.
  readonly errorMessage?: string;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export interface RangeCalendarProps<T extends DateValue> extends AriaRangeCalendarProps<T> {
  // What the starter kit adds.
  readonly errorMessage?: string;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function Calendar<T extends DateValue>({ bound, ...props }: CalendarProps<T>): ReactElement {
  return bound === undefined ? <DrawnCalendar {...props} /> : <BoundCalendar {...(props as DrawnProps<DateValue>)} bound={bound} />;
}

export function RangeCalendar<T extends DateValue>({ bound, ...props }: RangeCalendarProps<T>): ReactElement {
  return bound === undefined ? (
    <DrawnRange {...props} />
  ) : (
    <BoundRange {...(props as DrawnRangeProps<DateValue>)} bound={bound} />
  );
}

// What a calendar draws with, and the state's value that it shows as missing,
// where it shows one.
type DrawnProps<T extends DateValue> = Omit<CalendarProps<T>, "bound"> & { readonly missing?: string | null };
type DrawnRangeProps<T extends DateValue> = Omit<RangeCalendarProps<T>, "bound"> & { readonly missing?: string | null };

function BoundCalendar({ bound, onChange, ...props }: DrawnProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const date = useBoundDate<DateValue>(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true,
  });
  return (
    <>
      <DrawnCalendar
        {...props}
        value={date.value}
        missing={date.missing}
        onChange={(value) => {
          date.pick(value);
          onChange?.(value);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundRange({ bound, onChange, ...props }: DrawnRangeProps<DateValue> & { readonly bound: Bound }): ReactElement {
  const range = useBoundDate<DateRange>(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true,
  });
  return (
    <>
      <DrawnRange
        {...props}
        value={range.value}
        missing={range.missing}
        onChange={(value) => {
          range.pick(value);
          onChange?.(value);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The months, each under its heading, the error message, and the line that
// names a value of the state that the calendar shows as missing, as the SDK's
// other inputs name a value that they lack.
function DrawnCalendar<T extends DateValue>({ errorMessage, missing = null, isInvalid, className, ...props }: DrawnProps<T>): ReactElement {
  return (
    <AriaCalendar
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`)}
    >
      <Months count={props.visibleDuration?.months ?? 1} cell={(date) => <CalendarCell date={date} className={CELL} />} />
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
      {missing === null ? null : <NotAvailable values={[missing]} />}
    </AriaCalendar>
  );
}

function DrawnRange<T extends DateValue>({ errorMessage, missing = null, isInvalid, className, ...props }: DrawnRangeProps<T>): ReactElement {
  return (
    <AriaRangeCalendar
      {...props}
      isInvalid={missing === null ? isInvalid : true}
      className={composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`)}
    >
      <Months
        count={props.visibleDuration?.months ?? 1}
        cell={(date) => (
          <CalendarCell date={date} className={RANGE_CELL}>
            {({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => (
              <span className={isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`}>
                {formattedDate}
              </span>
            )}
          </CalendarCell>
        )}
      />
      {errorMessage === undefined ? null : (
        <Text slot="errorMessage" className={styles.alert}>
          {errorMessage}
        </Text>
      )}
      {missing === null ? null : <NotAvailable values={[missing]} />}
    </AriaRangeCalendar>
  );
}

// The months of a calendar, `count` of them side by side, each with its
// heading and its grid of days, which `cell` draws.
// The first month has the button to the months before, and the last the
// button to those after, each with the arrow that points that way in the
// direction of the user's language.
function Months({ count, cell }: { readonly count: number; readonly cell: (date: CalendarDate) => ReactElement }): ReactElement {
  const { direction } = useLocale();
  return (
    <>
      {Array.from({ length: count }, (_, i) => (
        <div key={i} className={MONTH}>
          <header className={HEADER}>
            {i === 0 ? (
              <Button variant="quiet" slot="previous" className={PAGE}>
                <Arrow back={direction !== "rtl"} />
              </Button>
            ) : null}
            <CalendarHeading offset={{ months: i }} className={HEADING} />
            {i === count - 1 ? (
              <Button variant="quiet" slot="next" className={PAGE}>
                <Arrow back={direction === "rtl"} />
              </Button>
            ) : null}
          </header>
          <CalendarGrid offset={{ months: i }} className={GRID}>
            <CalendarGridHeader>{(day) => <CalendarHeaderCell className={WEEKDAY}>{day}</CalendarHeaderCell>}</CalendarGridHeader>
            <CalendarGridBody>{cell}</CalendarGridBody>
          </CalendarGrid>
        </div>
      ))}
    </>
  );
}

// The chevron of a button that pages to the months before, where `back`, or
// to those after.
function Arrow({ back }: { readonly back: boolean }): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <polyline points={back ? "7.5 3 4.5 6 7.5 9" : "4.5 3 7.5 6 4.5 9"} />
    </svg>
  );
}

// The classes of the calendar and its parts.
// A day is a circle, which shows in the accent colour where it is picked, as
// does each end of a range, while the days between the ends of a range show
// on a band in a tint of it.
// A day that the calendar does not take, as one before minValue or one of the
// months around the month shown, shows faded, and one that
// isDateUnavailable takes away shows struck through.
// A picked day of an invalid calendar shows in the colour of danger.
const CALENDAR = "bc:flex bc:w-fit bc:max-w-full bc:gap-3 bc:overflow-auto bc:font-sans bc:text-sm bc:text-text-default";
const MONTH = "bc:flex bc:flex-col bc:gap-2";
const HEADER = "bc:flex bc:items-center bc:gap-1";
const HEADING = "bc:m-0 bc:flex-1 bc:text-center bc:text-sm bc:font-semibold";
const PAGE = "bc:size-7 bc:justify-center bc:px-0 bc:text-text-default";
const GRID = "bc:border-collapse bc:border-spacing-0";
const WEEKDAY = "bc:size-8 bc:p-0 bc:text-xs bc:font-medium bc:text-text-muted";
const CELL =
  "bc:flex bc:size-8 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded-full bc:text-sm bc:tabular-nums bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-today:font-semibold bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-40 bc:data-unavailable:cursor-default bc:data-unavailable:line-through bc:data-unavailable:opacity-40 bc:data-invalid:data-selected:bg-border-danger";
const RANGE_CELL =
  "bc:group bc:size-8 bc:cursor-pointer bc:text-sm bc:tabular-nums bc:outline-none bc:data-selected:bg-bg-accent/15 bc:data-selection-start:rounded-s-full bc:data-selection-end:rounded-e-full bc:[td:first-child_&]:rounded-s-full bc:[td:last-child_&]:rounded-e-full bc:data-disabled:cursor-default bc:data-disabled:opacity-40 bc:data-unavailable:cursor-default bc:data-unavailable:line-through bc:data-unavailable:opacity-40 bc:data-invalid:data-selected:bg-border-danger/15";
const DAY =
  "bc:flex bc:size-full bc:items-center bc:justify-center bc:rounded-full bc:group-data-today:font-semibold bc:group-data-focus-visible:outline-2 bc:group-data-focus-visible:outline-offset-1 bc:group-data-focus-visible:outline-border-accent";
const END = "bc:bg-bg-accent bc:text-text-on-accent bc:group-data-invalid:bg-border-danger";
const UNPICKED = "bc:group-data-hovered:bg-bg-subtle bc:group-data-pressed:bg-bg-subtle";
