import type { ReactElement } from "react";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
export interface SearchFieldProps extends AriaSearchFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
    readonly bound?: Bound;
}
export declare function SearchField({ bound, ...props }: SearchFieldProps): ReactElement;
