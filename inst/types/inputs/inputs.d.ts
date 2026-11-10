import type { ReactElement, ReactNode } from "react";
import type { Choice } from "../binding/index.ts";
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
export declare function Select({ bound, label, placeholder, choices, outdated, }: {
    bound: Bound;
    label: string;
    placeholder: string;
    choices?: readonly Choice[];
    outdated?: boolean;
}): ReactElement;
export declare function Checkbox({ bound, label }: {
    bound: Bound;
    label: string;
}): ReactElement;
export declare function Checklist({ bound, legend, choices, empty, ordered, outdated, }: {
    bound: Bound;
    legend: string;
    choices?: readonly string[];
    empty?: string;
    ordered?: boolean;
    outdated?: boolean;
}): ReactElement;
