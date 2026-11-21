import type { ReactElement, ReactNode } from "react";
import type { Bound } from "../react/index.ts";
export declare function Stack({ children }: {
    children: ReactNode;
}): ReactElement;
export declare function Field({ label, children }: {
    label: string;
    children: ReactNode;
}): ReactElement;
export declare function NotAvailable({ values, onRemove, }: {
    readonly values: readonly string[];
    readonly onRemove?: () => void;
}): ReactElement | null;
export declare function Refusal({ error }: {
    error: string | null;
}): ReactElement | null;
export declare function useTyped(bound: Bound): [string | null, (text: string | null) => void];
