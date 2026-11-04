export declare class StoreError extends Error {
    readonly failures: readonly string[];
    constructor(message: string, failures: readonly string[]);
}
