import type { Json } from "../types/json.ts";
export declare class StoreError extends Error {
    readonly failures: readonly string[];
    constructor(message: string, failures: readonly string[]);
}
export declare function thrown(of: string, error: unknown, attributes: Readonly<Record<string, Json>>): string;
