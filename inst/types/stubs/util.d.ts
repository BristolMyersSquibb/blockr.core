import type { BlockStatus } from "../scheduler/index.ts";
import type { OptionId } from "../types/board.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { Log, LogLine } from "../types/log.ts";
export declare const READY: BlockStatus;
export interface StubContext {
    readonly name?: string;
    readonly status?: BlockStatus;
    readonly inputs?: BlockContext["inputs"];
    readonly options?: Readonly<Record<OptionId, Json>>;
}
export declare function stubBlockContext(given?: StubContext): BlockContext;
export declare function optionValue(values: Readonly<Record<OptionId, Json>>, id: OptionId, where: string): Json;
export declare function announcer<E extends string>(events: readonly E[]): {
    listen: (event: E, fn: () => void) => () => void;
    announce: (event: E) => void;
};
export declare function raisedHolder(): {
    notified: readonly Raised[];
    notify: (notification: Raised) => () => void;
};
export declare function lineHolder(pkg?: string | null): {
    logged: readonly LogLine[];
    log: Log;
};
export declare function acting<T>(fn: () => T | Promise<T>): Promise<T>;
export declare function actingNow(fn: () => void): void;
