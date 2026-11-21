import type { ReactElement, ReactNode } from "react";
import type { CheckboxFieldProps, CheckboxGroupProps as AriaCheckboxGroupProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface CheckboxProps extends Omit<CheckboxFieldProps, "children"> {
    readonly children?: ReactNode;
    readonly description?: string;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function Checkbox({ bound, ...props }: CheckboxProps): ReactElement;
export interface CheckboxGroupProps extends Omit<AriaCheckboxGroupProps, "children"> {
    readonly label?: string;
    readonly description?: string;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly children?: ReactNode;
    readonly bound?: Bound;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
}
export declare function CheckboxGroup({ bound, outdated, ordered, ...props }: CheckboxGroupProps): ReactElement;
