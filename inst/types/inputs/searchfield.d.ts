import type { ReactElement } from "react";
import type { SearchFieldProps as AriaSearchFieldProps, ValidationResult } from "../aria/index.ts";
export interface SearchFieldProps extends AriaSearchFieldProps {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
}
export declare function SearchField({ label, description, errorMessage, placeholder, className, ...props }: SearchFieldProps): ReactElement;
