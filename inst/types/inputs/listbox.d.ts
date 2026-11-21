import type { ReactElement } from "react";
import type { ListBoxItemProps, ListBoxProps } from "../aria/index.ts";
export declare function ListBox<T extends object>({ className, ...props }: ListBoxProps<T>): ReactElement;
export declare function ListBoxItem(props: ListBoxItemProps): ReactElement;
