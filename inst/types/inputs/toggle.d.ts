import type { ReactElement, ReactNode } from "react";
import type { ToggleButtonGroupProps as AriaToggleButtonGroupProps, ToggleButtonProps as AriaToggleButtonProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface ToggleButtonProps extends AriaToggleButtonProps {
    readonly bound?: Bound;
}
export declare function ToggleButton({ bound, onChange, ...props }: ToggleButtonProps): ReactElement;
export interface ToggleButtonGroupProps extends Omit<AriaToggleButtonGroupProps, "children"> {
    readonly label?: string;
    readonly description?: string;
    readonly children?: ReactNode;
    readonly bound?: Bound;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
}
export declare function ToggleButtonGroup({ bound, outdated, ordered, ...props }: ToggleButtonGroupProps): ReactElement;
