import type { Patch } from "./delta.ts";
export declare function segmentsFromPath(path: string): string[] | undefined;
export declare function pluginIdFromPath(path: string): string | undefined;
export declare function pathFromSegments(segments: readonly string[]): string;
export declare function overlap(a: readonly string[], b: readonly string[], board: unknown): boolean;
export declare function malformed(patch: unknown): string[];
export type Applied = {
    readonly next: Readonly<Record<string, unknown>>;
    readonly inverse: Patch | undefined;
    readonly place?: string | null;
} | {
    readonly failure: string;
};
export declare function applyPatch(board: Readonly<Record<string, unknown>>, patch: Patch, segments: readonly string[], place?: string | null): Applied;
