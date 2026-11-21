import type { ReactElement } from "react";
import type { CalendarProps as AriaCalendarProps, DateValue, RangeCalendarProps as AriaRangeCalendarProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface CalendarProps<T extends DateValue> extends AriaCalendarProps<T> {
    readonly errorMessage?: string;
    readonly bound?: Bound;
}
export interface RangeCalendarProps<T extends DateValue> extends AriaRangeCalendarProps<T> {
    readonly errorMessage?: string;
    readonly bound?: Bound;
}
export declare function Calendar<T extends DateValue>({ bound, ...props }: CalendarProps<T>): ReactElement;
export declare function RangeCalendar<T extends DateValue>({ bound, ...props }: RangeCalendarProps<T>): ReactElement;
