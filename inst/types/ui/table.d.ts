import type { ReactElement } from "react";
export declare const PAGE_SIZE = 10;
export interface TableView<T> {
    readonly rows: readonly T[];
    readonly query: string;
    readonly setQuery: (query: string) => void;
    readonly page: number;
    readonly pages: number;
    readonly setPage: (page: number) => void;
    readonly found: number;
    readonly first: number;
}
export declare function useTableView<T>(rows: readonly T[], text: (row: T) => string, size?: number): TableView<T>;
export declare function TableSearch({ label, view }: {
    label: string;
    view: TableView<unknown>;
}): ReactElement;
export declare function Pager({ label, view }: {
    label: string;
    view: TableView<unknown>;
}): ReactElement | null;
