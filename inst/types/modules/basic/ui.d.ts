import type { ReactElement, ReactNode } from "react";
export declare function Dialog({ title, onClose, children, }: {
    title: string;
    onClose: () => void;
    children: ReactNode;
}): ReactElement;
