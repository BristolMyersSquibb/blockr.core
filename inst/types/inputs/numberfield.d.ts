import type { ReactElement } from "react";
import type { NumberFieldProps as AriaNumberFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface NumberFieldProps extends AriaNumberFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
    readonly bound?: Bound;
    readonly outdated?: boolean;
}
export declare function NumberField({ bound, outdated, maxValue, ...props }: NumberFieldProps): ReactElement;
