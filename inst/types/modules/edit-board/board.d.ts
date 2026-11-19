import type { ReactElement, ReactNode, RefObject } from "react";
import type { CoreApi } from "../../types/core-api.ts";
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
export declare const table: {
    readonly table: "bc:w-full bc:table-fixed bc:border-collapse bc:text-sm";
    readonly head: "bc:border-b bc:border-border-default bc:px-1 bc:py-1 bc:text-left bc:text-xs bc:font-semibold bc:text-text-muted";
    readonly row: "bc:border-b bc:border-border-default bc:align-top";
    readonly cell: "bc:px-1 bc:py-1.5";
};
export declare function useRowEditing(): {
    editing: boolean;
    open: () => void;
    close: () => void;
    button: RefObject<HTMLButtonElement | null>;
};
