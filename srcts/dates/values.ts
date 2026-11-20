// How the SDK's dates and times read the value that a state holds, and write
// the value that they submit, as JSON that R reads:
// - A date is its ISO string, as "2026-11-19", which R reads with as.Date().
// - A date with a time, where the input's granularity takes a time, has a
//   space in place of ISO's "T", as "2026-11-19 14:30:00".
//   R reads that form with as.POSIXct(), and R writes a POSIXct in it, as
//   yyjsonr does.
//   R's as.POSIXct() reads "2026-11-19T14:30:00" as midnight, without a word.
//   So the inputs show such a string as missing.
//   The string holds no time zone.
//   A block reads it in the time zone that it chooses, such as that of its
//   data, so the browser's time zone plays no part, and a board means the same
//   wherever it is opened.
// - A time is "14:30:00".
// - A range is an object of two such strings, as { "start": "2026-11-19",
//   "end": "2026-11-26" }, as React Aria's RangeValue holds them, which R reads
//   as a named list.
// The inputs write the seconds of every time, and read a time without them as
// R does, as "2026-11-19 14:30".
// An input with a time reads a date alone as midnight, as R does.

import { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "./aria.ts";
import type { DateRange, DateValue, TimeValue } from "./aria.ts";
import type { Json } from "../types/json.ts";
import { own } from "../util/index.ts";

// The granularities of React Aria's dates, of which all but "day" take a
// time.
export type Granularity = "day" | "hour" | "minute" | "second";

const DATE = /^\d{4}-\d{2}-\d{2}$/;
const DATE_TIME = /^(\d{4}-\d{2}-\d{2})(?: (\d{2}:\d{2}(?::\d{2})?))?$/;
const TIME = /^\d{2}:\d{2}(?::\d{2})?$/;

// Whether an input of the granularity given takes a time, as one of "minute"
// does, while React Aria's default for a date is "day".
export function takesTime(granularity: Granularity | undefined): boolean {
  return granularity !== undefined && granularity !== "day";
}

// The date that a state's value holds, with its time where `time`, or
// undefined where the value reads as none, as "2026-02-30" or a number.
export function readDate(value: Json, time: boolean): DateValue | undefined {
  if (typeof value !== "string") return undefined;
  const parts = (time ? DATE_TIME : DATE).exec(value);
  if (parts === null) return undefined;
  try {
    return time ? parseDateTime(parts[2] === undefined ? value : `${parts[1] ?? ""}T${parts[2]}`) : parseDate(value);
  } catch {
    // The parts of the date are out of range, as the 30th of February.
    return undefined;
  }
}

export function writeDate(value: DateValue, time: boolean): string {
  return time ? toCalendarDateTime(value).toString().replace("T", " ") : toCalendarDate(value).toString();
}

// The time that a state's value holds, or undefined where it reads as none.
export function readTime(value: Json): TimeValue | undefined {
  if (typeof value !== "string" || !TIME.test(value)) return undefined;
  try {
    return parseTime(value);
  } catch {
    return undefined;
  }
}

export function writeTime(value: TimeValue): string {
  return ("day" in value ? toTime(value) : value).toString();
}

// The range that a state's value holds, of dates with their times where
// `time`, or undefined where it reads as none.
export function readRange(value: Json, time: boolean): DateRange | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return undefined;
  const record = value as Readonly<Record<string, Json>>;
  const start = readDate(own(record, "start") ?? null, time);
  const end = readDate(own(record, "end") ?? null, time);
  return start === undefined || end === undefined ? undefined : { start, end };
}

export function writeRange(value: DateRange, time: boolean): Json {
  return { start: writeDate(value.start, time), end: writeDate(value.end, time) };
}

// The props of an input that say which dates it takes.
export interface DateBounds {
  readonly minValue?: DateValue | null | undefined;
  readonly maxValue?: DateValue | null | undefined;
  readonly isDateUnavailable?: ((date: DateValue) => boolean) | undefined;
}

// Whether the input takes the date: one from minValue to maxValue, which
// isDateUnavailable leaves, as React Aria marks any other as invalid.
export function dateFits(value: DateValue, { minValue, maxValue, isDateUnavailable }: DateBounds): boolean {
  return (
    (minValue === undefined || minValue === null || value.compare(minValue) >= 0) &&
    (maxValue === undefined || maxValue === null || value.compare(maxValue) <= 0) &&
    isDateUnavailable?.(value) !== true
  );
}

// Whether the input takes the range: one whose start comes no later than its
// end, both of which it takes.
export function rangeFits(value: DateRange, bounds: DateBounds): boolean {
  return value.start.compare(value.end) <= 0 && dateFits(value.start, bounds) && dateFits(value.end, bounds);
}

// Whether the input takes the time: one from minValue to maxValue.
export function timeFits(
  value: TimeValue,
  { minValue, maxValue }: { readonly minValue?: TimeValue | null | undefined; readonly maxValue?: TimeValue | null | undefined },
): boolean {
  const time = "day" in value ? toTime(value) : value;
  const at = (bound: TimeValue): number => time.compare("day" in bound ? toTime(bound) : bound);
  return (minValue === undefined || minValue === null || at(minValue) >= 0) && (maxValue === undefined || maxValue === null || at(maxValue) <= 0);
}

// What a state's value reads as, to show as missing where it reads as none:
// the string itself, or the value's JSON.
export function shownValue(value: Json): string {
  return typeof value === "string" ? value : JSON.stringify(value);
}
