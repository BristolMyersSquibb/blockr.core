import type { ReactElement, ReactNode, RefObject } from "react";
export declare function Dialog({ title, onClose, children, }: {
    title: string;
    onClose: () => void;
    children: ReactNode;
}): ReactElement;
export interface MenuItem {
    readonly label: string;
    readonly onSelect: () => void;
    readonly disabled?: boolean;
}
export declare const menuClasses: {
    readonly list: "bc:bg-bg-raised bc:text-gray-900 bc:dark:text-gray-100 bc:absolute bc:right-0 bc:z-20 bc:m-0 bc:mt-1 bc:grid bc:min-w-44 bc:list-none bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg";
    readonly item: "bc:w-full bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50";
};
export declare function useDismiss(ref: RefObject<HTMLElement | null>, open: boolean, setOpen: (open: boolean) => void): void;
export declare function Menu({ label, items }: {
    label: string;
    items: readonly MenuItem[];
}): ReactElement;
