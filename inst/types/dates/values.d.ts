import type { DateRange, DateValue, TimeValue } from "./aria.ts";
import type { Json } from "../types/json.ts";
export type Granularity = "day" | "hour" | "minute" | "second";
export declare function takesTime(granularity: Granularity | undefined): boolean;
export declare function readDate(value: Json, time: boolean): DateValue | undefined;
export declare function writeDate(value: DateValue, time: boolean): string;
export declare function readTime(value: Json): TimeValue | undefined;
export declare function writeTime(value: TimeValue): string;
export declare function readRange(value: Json, time: boolean): DateRange | undefined;
export declare function writeRange(value: DateRange, time: boolean): Json;
export interface DateBounds {
    readonly minValue?: DateValue | null | undefined;
    readonly maxValue?: DateValue | null | undefined;
    readonly isDateUnavailable?: ((date: DateValue) => boolean) | undefined;
}
export declare function dateFits(value: DateValue, { minValue, maxValue, isDateUnavailable }: DateBounds): boolean;
export declare function rangeFits(value: DateRange, bounds: DateBounds): boolean;
export declare function timeFits(value: TimeValue, { minValue, maxValue }: {
    readonly minValue?: TimeValue | null | undefined;
    readonly maxValue?: TimeValue | null | undefined;
}): boolean;
export declare function shownValue(value: Json): string;
