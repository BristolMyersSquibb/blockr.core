import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockContext } from "../types/hosts.ts";
export declare const READY: BlockStatus;
export declare function stubBlockContext(given?: Partial<BlockContext>): BlockContext;
export declare function announcer<E extends string>(events: readonly E[]): {
    listen: (event: E, fn: () => void) => () => void;
    announce: (event: E) => void;
};
