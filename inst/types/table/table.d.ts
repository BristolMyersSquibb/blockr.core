import type { ReactElement } from "react";
import type { CellProps, ColumnProps as AriaColumnProps, RowProps, TableBodyProps, TableHeaderProps, TableProps as AriaTableProps } from "../aria/index.ts";
export interface TableProps extends Omit<AriaTableProps, "className"> {
    readonly className?: string;
}
export declare function Table({ className, ...props }: TableProps): ReactElement;
export interface ColumnProps extends AriaColumnProps {
    readonly align?: "start" | "end";
}
export declare function Column({ align, ...props }: ColumnProps): ReactElement;
export declare function TableHeader<T extends object>({ columns, children, ...props }: TableHeaderProps<T>): ReactElement;
export declare function TableBody<T extends object>(props: TableBodyProps<T>): ReactElement;
export declare function Row<T extends object>({ id, columns, children, ...props }: RowProps<T>): ReactElement;
export declare function Cell(props: CellProps): ReactElement;
