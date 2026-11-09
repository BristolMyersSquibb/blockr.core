import type { Patch } from "./delta.ts";
export declare function pointer(path: string): string[] | undefined;
export declare function pluginOf(path: string): string | undefined;
export declare function pathOf(parts: readonly string[]): string;
export declare function overlap(a: readonly string[], b: readonly string[]): boolean;
export declare function malformed(patch: unknown): string[];
export type Applied = {
    readonly next: Readonly<Record<string, unknown>>;
    readonly inverse: Patch | undefined;
    readonly place?: string | null;
} | {
    readonly failure: string;
};
export declare function applyPatch(board: Readonly<Record<string, unknown>>, patch: Patch, parts: readonly string[], place?: string | null): Applied;
