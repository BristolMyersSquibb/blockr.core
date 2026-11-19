import type { ReactElement, ReactNode } from "react";
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
export declare function Menu({ label, items }: {
    label: string;
    items: readonly MenuItem[];
}): ReactElement;
