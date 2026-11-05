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
export declare function TextInput({ bound, label, placeholder, lines, pause, }: {
    bound: Bound;
    label: string;
    placeholder?: string;
    lines?: number;
    pause?: boolean;
}): ReactElement;
export declare function NumberInput({ bound, label, min, max, }: {
    bound: Bound;
    label: string;
    min?: number;
    max?: number;
}): ReactElement;
export declare function Select({ bound, label, placeholder, choices, }: {
    bound: Bound;
    label: string;
    placeholder: string;
    choices: readonly Choice[];
}): ReactElement;
export declare function Checkbox({ bound, label }: {
    bound: Bound;
    label: string;
}): ReactElement;
export declare function Checklist({ bound, legend, choices, empty, ordered, }: {
    bound: Bound;
    legend: string;
    choices?: readonly string[];
    empty?: string;
    ordered?: boolean;
}): ReactElement;
