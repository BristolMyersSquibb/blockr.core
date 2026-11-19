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
}
export declare function ComboBox<T extends object, M extends Mode = "single">(props: ComboBoxProps<T, M>): ReactElement;
export declare function ids(items: Iterable<object>): string[];
export declare function inOrder(values: readonly string[], order: readonly string[]): string[];
export declare function edited(now: readonly string[], began: readonly string[], next: readonly string[]): string[] | null;
export declare function emptyNote(items: Iterable<object> | undefined, children: ReactNode | ((item: never) => ReactNode), outdated: boolean, said: () => ReactNode): ReactElement;
export declare function ComboBoxItem(props: ListBoxItemProps): ReactElement;
export interface ComboBoxSectionProps<T> extends ListBoxSectionProps<T> {
    readonly title?: string;
    readonly items?: Iterable<T>;
}
export declare function ComboBoxSection<T extends object>({ title, items, children, ...props }: ComboBoxSectionProps<T>): ReactElement;
export declare function Cross(): ReactElement;
export declare const FIELD = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
export declare const INPUT = "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
export declare const CLEAR = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:hover:bg-bg-subtle bc:hover:text-text-default";
export declare const LAYOUT: {
    readonly rowHeight: 32;
    readonly headingHeight: 28;
    readonly padding: 4;
};
export {};
