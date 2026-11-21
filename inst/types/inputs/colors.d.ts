import type { Color } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import type { Json } from "../types/json.ts";
export declare const FALLBACK = "#000000";
export declare function hex(color: Color): string;
export declare function valueColor(value: Json | undefined): string | undefined;
export declare const Ended: import("react").Context<((color: Color) => void) | null>;
export declare function useDragged(bound: Bound): {
    readonly shown: Color | string;
    readonly change: (color: Color) => void;
    readonly end: (color: Color) => void;
};
