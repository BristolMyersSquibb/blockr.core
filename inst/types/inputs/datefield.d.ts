import type { ReactElement } from "react";
import type { DateFieldProps as AriaDateFieldProps, DateValue, TimeFieldProps as AriaTimeFieldProps, TimeValue, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface DateFieldProps<T extends DateValue> extends AriaDateFieldProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export interface TimeFieldProps<T extends TimeValue> extends AriaTimeFieldProps<T> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function DateField<T extends DateValue>({ bound, ...props }: DateFieldProps<T>): ReactElement;
export declare function TimeField<T extends TimeValue>({ bound, ...props }: TimeFieldProps<T>): ReactElement;
export declare function DateInput({ slot, className }: {
    readonly slot?: "start" | "end";
    readonly className: string;
}): ReactElement;
export declare const INLINE = "bc:flex bc:min-w-0 bc:items-center bc:whitespace-nowrap bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:data-disabled:cursor-default";
