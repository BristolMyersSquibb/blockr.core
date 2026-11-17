import type { ReactElement, ReactNode } from "react";
import type { Bound } from "../react/index.ts";
export declare function Stack({ children }: {
    children: ReactNode;
}): ReactElement;
export declare function Field({ label, children }: {
    label: string;
    children: ReactNode;
}): ReactElement;
export declare function TextInput({ bound, label, placeholder, lines, }: {
    bound: Bound;
    label: string;
    placeholder?: string;
    lines?: number;
}): ReactElement;
export declare function NumberInput({ bound, label, min, max, outdated, }: {
    bound: Bound;
    label: string;
    min?: number;
    max?: number;
    outdated?: boolean;
}): ReactElement;
export declare function Checkbox({ bound, label }: {
    bound: Bound;
    label: string;
}): ReactElement;
export declare function missing(value: string): string;
export declare function Refusal({ error }: {
    error: string | null;
}): ReactElement | null;
