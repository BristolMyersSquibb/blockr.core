import type { ReactElement } from "react";
export interface ComboOption {
    readonly value: string;
    readonly label: string;
    readonly detail?: string;
    readonly badge?: string;
    readonly group?: string;
    readonly keywords?: readonly string[];
    readonly disabled?: boolean;
}
interface Common {
    readonly label: string;
    readonly options: readonly ComboOption[];
    readonly hint?: string;
    readonly error?: string;
    readonly placeholder?: string;
    readonly empty?: string;
    readonly autofocus?: boolean;
}
export interface ComboboxProps extends Common {
    readonly value: string | undefined;
    readonly onChange: (value: string) => void;
}
export interface MultiComboboxProps extends Common {
    readonly values: readonly string[];
    readonly onChange: (values: readonly string[]) => void;
}
export declare function Combobox(props: ComboboxProps): ReactElement;
export declare function MultiCombobox(props: MultiComboboxProps): ReactElement;
export {};
