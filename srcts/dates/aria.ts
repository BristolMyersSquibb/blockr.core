// The parts of React Aria (https://react-aria.adobe.com/) that core's dates
// and times draw with (./index.ts), and the functions of
// @internationalized/date (https://react-aria.adobe.com/internationalized/date/)
// with which they read the strings that a state holds and write those that
// they submit.
// The build bundles what this file exports into a file of its own,
// vendor-dates-<hash>.js, which the page loads once a module first imports
// core's dates, rather than into the vendor file, which the page loads as it
// starts (../build/build.ts).
// What they share with the parts that ../aria/index.ts names, such as React
// Aria's Button and its contexts, the file imports from the vendor file's
// code, so that the page runs one copy of React Aria still.
// Each part comes from its own entry point, as in ../aria/index.ts.

export { Calendar, CalendarCell, CalendarGrid, CalendarGridBody, CalendarGridHeader, CalendarHeaderCell, CalendarHeading } from "react-aria-components/Calendar";
export { DateField, DateInput, DateSegment } from "react-aria-components/DateField";
export { DatePicker } from "react-aria-components/DatePicker";
export { DateRangePicker } from "react-aria-components/DateRangePicker";
export { RangeCalendar } from "react-aria-components/RangeCalendar";
export { TimeField } from "react-aria-components/TimeField";
export { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "@internationalized/date";
export type { CalendarDate } from "@internationalized/date";
export type { CalendarProps, DateValue } from "react-aria-components/Calendar";
export type { DateFieldProps } from "react-aria-components/DateField";
export type { DatePickerProps } from "react-aria-components/DatePicker";
export type { DateRange, DateRangePickerProps } from "react-aria-components/DateRangePicker";
export type { RangeCalendarProps } from "react-aria-components/RangeCalendar";
export type { TimeFieldProps, TimeValue } from "react-aria-components/TimeField";
