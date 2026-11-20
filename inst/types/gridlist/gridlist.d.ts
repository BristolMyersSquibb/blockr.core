import type { HTMLAttributes, ReactElement } from "react";
import { GridListSection } from "../aria/index.ts";
import type { GridListItemProps, GridListProps } from "../aria/index.ts";
export { GridListSection };
export declare function GridList<T extends object>(props: GridListProps<T>): ReactElement;
export declare function GridListItem({ children, ...props }: GridListItemProps): ReactElement;
export declare function GridListHeader({ className, ...props }: HTMLAttributes<HTMLElement>): ReactElement;
