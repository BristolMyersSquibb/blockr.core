import type { ReactElement, ReactNode } from "react";
import type { MenuItemProps, MenuProps, MenuSectionProps as AriaMenuSectionProps, MenuTriggerProps as AriaMenuTriggerProps, PopoverProps, SubmenuTriggerProps } from "../aria/index.ts";
export interface MenuTriggerProps extends AriaMenuTriggerProps {
    readonly placement?: PopoverProps["placement"];
}
export declare function MenuTrigger({ placement, children, ...props }: MenuTriggerProps): ReactElement;
export declare function SubmenuTrigger({ children, ...props }: SubmenuTriggerProps): ReactElement;
export declare function Menu<T extends object>({ className, ...props }: MenuProps<T>): ReactElement;
export declare function MenuItem(props: MenuItemProps): ReactElement;
export interface MenuSectionProps<T> extends AriaMenuSectionProps<T> {
    readonly title?: string;
    readonly items?: Iterable<T>;
    readonly children?: ReactNode | ((item: T) => ReactElement);
}
export declare function MenuSection<T extends object>({ title, items, children, ...props }: MenuSectionProps<T>): ReactElement;
