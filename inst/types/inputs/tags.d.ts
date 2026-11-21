import type { ReactElement, ReactNode } from "react";
import type { TagGroupProps as AriaTagGroupProps, TagListProps, TagProps as AriaTagProps } from "../aria/index.ts";
declare const colors: {
    readonly gray: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default";
    readonly accent: "bc:border-border-accent bc:bg-bg-subtle bc:text-text-default";
};
type Color = keyof typeof colors;
export interface TagGroupProps<T> extends Omit<AriaTagGroupProps, "children">, Pick<TagListProps<T>, "items" | "children" | "renderEmptyState"> {
    readonly color?: Color;
    readonly label?: string;
    readonly description?: string;
    readonly errorMessage?: string;
}
export declare function TagGroup<T extends object>({ label, description, errorMessage, items, children, renderEmptyState, color, className, ...props }: TagGroupProps<T>): ReactElement;
export interface TagProps extends AriaTagProps {
    readonly color?: Color;
    readonly children?: ReactNode;
}
export declare function Tag({ children, color, className, ...props }: TagProps): ReactElement;
export {};
