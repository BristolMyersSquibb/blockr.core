import type { ReactElement } from "react";
import type { ColorFieldProps as AriaColorFieldProps } from "../aria-colors/index.ts";
import type { ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface ColorFieldProps extends AriaColorFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly bound?: Bound;
}
export declare function ColorField({ bound, ...props }: ColorFieldProps): ReactElement;
