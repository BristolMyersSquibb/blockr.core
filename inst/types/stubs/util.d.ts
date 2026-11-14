import type { BlockStatus } from "../scheduler/index.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext } from "../types/hosts.ts";
import type { Log, LogLine } from "../types/log.ts";
export declare const READY: BlockStatus;
export declare function stubBlockContext(given?: Partial<BlockContext>): BlockContext;
export declare function announcer<E extends string>(events: readonly E[]): {
    listen: (event: E, fn: () => void) => () => void;
    announce: (event: E) => void;
};
export declare function notifier(): {
    notified: readonly Raised[];
    notify: (notification: Raised) => () => void;
};
export declare function lineHolder(pkg?: string | null): {
    logged: readonly LogLine[];
    log: Log;
};
