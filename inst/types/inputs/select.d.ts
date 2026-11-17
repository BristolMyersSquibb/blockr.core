import type { ReactElement } from "react";
import type { Choice } from "../binding/index.ts";
import type { Bound } from "../react/index.ts";
interface Common {
    readonly label: string;
    readonly choices?: readonly Choice[];
    readonly placeholder?: string;
    readonly empty?: string;
    readonly disabled?: boolean;
}
interface Unbound extends Common {
    readonly bound?: undefined;
    readonly hint?: string;
    readonly error?: string;
    readonly autofocus?: boolean;
}
export interface SelectOneProps extends Unbound {
    readonly multiple?: false;
    readonly value: string | undefined;
    readonly onChange: (value: string) => void;
}
export interface SelectSeveralProps extends Unbound {
    readonly multiple: true;
    readonly value: readonly string[];
    readonly onChange: (value: readonly string[]) => void;
    readonly min?: number;
}
export interface SelectBoundProps extends Common {
    readonly bound: Bound;
    readonly multiple?: boolean;
    readonly outdated?: boolean;
    readonly ordered?: boolean;
    readonly min?: number;
}
export type SelectProps = SelectOneProps | SelectSeveralProps | SelectBoundProps;
export declare const MAX_ROWS = 200;
export declare const FILTER_AFTER = 8;
export declare function Select(props: SelectProps): ReactElement;
export declare function matching(choices: readonly Choice[], query: string): Choice[];
export declare function summary(names: readonly string[], total: number | undefined, placeholder: string): {
    lead: string;
    rest: string;
    empty: boolean;
};
export {};
