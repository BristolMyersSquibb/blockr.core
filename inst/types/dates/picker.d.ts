import type { ReactElement } from "react";
import type { ValidationResult } from "../aria/index.ts";
import type { DatePickerProps as AriaDatePickerProps, DateRangePickerProps as AriaDateRangePickerProps, DateValue } from "./aria.ts";
import type { Bound } from "../react/index.ts";
export interface DatePickerProps<T extends DateValue> extends AriaDatePickerProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export interface DateRangePickerProps<T extends DateValue> extends AriaDateRangePickerProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function DatePicker<T extends DateValue>({ bound, ...props }: DatePickerProps<T>): ReactElement;
export declare function DateRangePicker<T extends DateValue>({ bound, ...props }: DateRangePickerProps<T>): ReactElement;
