import type { ReactElement } from "react";
import type { SliderProps as AriaSliderProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
type Value = number | number[];
export interface SliderProps<T extends Value = Value> extends AriaSliderProps<T> {
    readonly label?: string;
    readonly thumbLabels?: readonly string[];
    readonly fillOffset?: number;
    readonly bound?: Bound;
    readonly outdated?: boolean;
}
export declare function Slider<T extends Value = Value>({ bound, outdated, ...props }: SliderProps<T>): ReactElement;
export {};
