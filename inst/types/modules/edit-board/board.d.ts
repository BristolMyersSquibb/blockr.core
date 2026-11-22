import type { ReactElement, ReactNode } from "react";
import type { Target } from "../../actions/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Menus } from "./menus.tsx";
import type { Kind } from "./selection.ts";
export declare function EditBoard({ core }: {
    core: CoreApi;
}): ReactElement;
export declare function Section({ title, count, children }: {
    title: string;
    count: number;
    children: ReactNode;
}): ReactElement;
export declare function Part({ title, children }: {
    title: string;
    children: ReactNode;
}): ReactElement;
export interface ObjectColumn {
    readonly id: string;
    readonly name: string;
    readonly rowHeader?: boolean;
}
export interface ObjectRow {
    readonly id: string;
    readonly text: string;
    readonly menuLabel: string;
    readonly cells: readonly {
        readonly content: ReactNode;
        readonly className?: string;
    }[];
}
export declare const WRAP = "bc:whitespace-normal! bc:break-words";
export declare function ObjectsTable({ core, menus, kind, label, columns, rows, }: {
    core: CoreApi;
    menus: Menus;
    kind: Kind;
    label: string;
    columns: readonly ObjectColumn[];
    rows: readonly ObjectRow[];
}): ReactElement;
export declare const BOARD: {
    readonly board: true;
};
export declare function offers(core: CoreApi, id: string, target: Target): boolean;
export declare function Advanced({ core, id, target }: {
    core: CoreApi;
    id: string;
    target: Target;
}): ReactElement;
