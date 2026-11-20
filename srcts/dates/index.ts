// The dates and times of core's React SDK, which core ships to packages as
// `@blockr/core/react/dates`, in a file of their own, beside the SDK
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// They are React Aria's, in core's theme: the date field and the time field,
// the date picker and the date range picker, and the calendar and the range
// calendar.
// No UI of core draws a date, so the page loads them, and the parts of React
// Aria that they draw with, only once a module first imports them.
// Each takes the props of its part of React Aria, and an input takes `bound`
// too, as the SDK's other inputs do, which binds a block's state arg or a
// board option's value to a string, as "2026-11-19", or to a range of two
// (./values.ts).

export { Calendar, RangeCalendar } from "./calendar.tsx";
export type { CalendarProps, RangeCalendarProps } from "./calendar.tsx";
export { DateField, TimeField } from "./field.tsx";
export type { DateFieldProps, TimeFieldProps } from "./field.tsx";
export { DatePicker, DateRangePicker } from "./picker.tsx";
export type { DatePickerProps, DateRangePickerProps } from "./picker.tsx";
