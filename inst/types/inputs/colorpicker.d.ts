import type { ReactElement, ReactNode } from "react";
import type { ColorPickerProps as AriaColorPickerProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface ColorPickerProps extends Omit<AriaColorPickerProps, "children"> {
    readonly label?: string;
    readonly children?: ReactNode;
    readonly isDisabled?: boolean;
    readonly bound?: Bound;
}
export declare function ColorPicker({ bound, ...props }: ColorPickerProps): ReactElement;
