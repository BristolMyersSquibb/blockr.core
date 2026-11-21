import type { ReactElement } from "react";
import type { ColorAreaProps as AriaColorAreaProps, ColorSliderProps as AriaColorSliderProps, ColorThumbProps, ColorWheelProps as AriaColorWheelProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export declare function ColorThumb({ className, style, ...props }: ColorThumbProps): ReactElement;
export interface ColorAreaProps extends AriaColorAreaProps {
    readonly bound?: Bound;
}
export declare function ColorArea({ bound, ...props }: ColorAreaProps): ReactElement;
export interface ColorSliderProps extends AriaColorSliderProps {
    readonly label?: string;
    readonly bound?: Bound;
}
export declare function ColorSlider({ bound, ...props }: ColorSliderProps): ReactElement;
export interface ColorWheelProps extends Omit<AriaColorWheelProps, "outerRadius" | "innerRadius"> {
    readonly bound?: Bound;
}
export declare function ColorWheel({ bound, ...props }: ColorWheelProps): ReactElement;
