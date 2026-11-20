import type { ReactElement } from "react";
import type { ColorSwatchPickerItemProps, ColorSwatchPickerProps as AriaColorSwatchPickerProps, ColorSwatchProps } from "../aria-colors/index.ts";
import type { Bound } from "../react/index.ts";
export declare function ColorSwatch({ className, style, ...props }: ColorSwatchProps): ReactElement;
export interface ColorSwatchPickerProps extends AriaColorSwatchPickerProps {
    readonly bound?: Bound;
}
export declare function ColorSwatchPicker({ bound, ...props }: ColorSwatchPickerProps): ReactElement;
export declare function ColorSwatchPickerItem({ className, ...props }: ColorSwatchPickerItemProps): ReactElement;
