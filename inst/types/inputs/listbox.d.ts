import type { ReactElement } from "react";
import type { Bound } from "../react/index.ts";
import type { ListBoxItemProps, ListBoxProps as AriaListBoxProps } from "../aria/index.ts";
export interface ListBoxProps<T extends object> extends Omit<AriaListBoxProps<T>, "layout" | "orientation"> {
    readonly bound?: Bound;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
}
export declare function ListBox<T extends object>(props: ListBoxProps<T>): ReactElement;
export declare function ListBoxItem(props: ListBoxItemProps): ReactElement;
