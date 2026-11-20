import type { ReactElement, ReactNode } from "react";
import type { RadioFieldProps, RadioGroupProps as AriaRadioGroupProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface RadioGroupProps extends Omit<AriaRadioGroupProps, "children"> {
    readonly label?: string;
    readonly description?: string;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly children?: ReactNode;
    readonly bound?: Bound;
    readonly outdated?: boolean;
}
export declare function RadioGroup({ bound, outdated, ...props }: RadioGroupProps): ReactElement;
export interface RadioProps extends Omit<RadioFieldProps, "children"> {
    readonly children?: ReactNode;
    readonly description?: string;
}
export declare function Radio(props: RadioProps): ReactElement;
