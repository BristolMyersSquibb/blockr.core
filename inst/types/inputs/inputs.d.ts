import type { ReactElement, ReactNode } from "react";
import type { Bound } from "../react/index.ts";
export declare function Stack({ children }: {
    children: ReactNode;
}): ReactElement;
export declare function Field({ label, children }: {
    label: string;
    children: ReactNode;
}): ReactElement;
export interface CheckboxProps {
    readonly label: string;
    readonly bound?: Bound;
    readonly isSelected?: boolean;
    readonly onChange?: (isSelected: boolean) => void;
}
export declare function Checkbox({ bound, label, isSelected, onChange }: CheckboxProps): ReactElement;
export declare function missing(value: string): string;
export declare function Refusal({ error }: {
    error: string | null;
}): ReactElement | null;
export declare function useTyped(bound: Bound): [string | null, (text: string | null) => void];
