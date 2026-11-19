import type { ReactElement, ReactNode } from "react";
import type { Bound } from "../react/index.ts";
import type { ComboBoxProps as AriaComboBoxProps, ListBoxItemProps, ListBoxSectionProps, ValidationResult } from "../aria/index.ts";
type Mode = "single" | "multiple";
export interface ComboBoxProps<T extends object, M extends Mode = "single"> extends Omit<AriaComboBoxProps<T, M>, "children"> {
    readonly label?: string;
    readonly description?: string | null;
    readonly errorMessage?: string | ((validation: ValidationResult) => string);
    readonly placeholder?: string;
    readonly children: ReactNode | ((item: T) => ReactNode);
    readonly renderEmptyState?: () => ReactNode;
    readonly bound?: Bound;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
    readonly onSearch?: (text: string) => void;
    readonly total?: number;
}
export declare function ComboBox<T extends object, M extends Mode = "single">(props: ComboBoxProps<T, M>): ReactElement;
export declare function ComboBoxItem(props: ListBoxItemProps): ReactElement;
export interface ComboBoxSectionProps<T> extends ListBoxSectionProps<T> {
    readonly title?: string;
    readonly items?: Iterable<T>;
}
export declare function ComboBoxSection<T extends object>({ title, items, children, ...props }: ComboBoxSectionProps<T>): ReactElement;
export {};
