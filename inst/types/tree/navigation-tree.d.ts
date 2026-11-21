import type { ReactElement, ReactNode } from "react";
import type { LinkProps, NavigationTreeHeaderProps, NavigationTreeItemProps, NavigationTreeProps as AriaNavigationTreeProps, NavigationTreeSectionProps } from "../aria/index.ts";
export interface NavigationTreeProps<T> extends AriaNavigationTreeProps<T> {
    readonly renderLink?: LinkProps["render"];
}
export declare function NavigationTree<T extends object>({ renderLink, ...props }: NavigationTreeProps<T>): ReactElement;
export declare function NavigationTreeItem(props: NavigationTreeItemProps): ReactElement;
export declare function NavigationTreeItemContent({ children }: {
    children?: ReactNode;
}): ReactElement;
export declare function NavigationTreeSection<T extends object>(props: NavigationTreeSectionProps<T>): ReactElement;
export declare function NavigationTreeHeader(props: NavigationTreeHeaderProps): ReactElement;
export interface NavigationTreeItemLinkProps extends Omit<LinkProps, "children"> {
    readonly children?: ReactNode;
}
export declare function NavigationTreeItemLink({ render, children, ...props }: NavigationTreeItemLinkProps): ReactElement;
