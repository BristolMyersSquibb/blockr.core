import type { Log, LogLevel, LogLine } from "../types/log.ts";
export type Logger = (line: LogLine) => void;
export declare const THRESHOLDS: readonly ["fatal", "error", "warn", "info", "debug", "trace"];
export type Threshold = (typeof THRESHOLDS)[number];
interface Logging {
    readonly lowest: Threshold;
    readonly logger: Logger;
}
export declare function reaches(level: LogLevel, lowest: Threshold): boolean;
export declare function setLogging(lowest: Threshold, logger: Logger): void;
export declare function awaitLogger(): void;
export declare function fallBack(): void;
export declare function createLog(pkg: string | null): Log;
export declare const log: Log;
export declare const consoleLogger: Logger;
export declare function replaceLogger(logger: Logger, passOn?: boolean): () => void;
export declare function setTestLoggersAside(): () => void;
export declare function currentLogging(): Logging | undefined;
export declare function resetLogging(): void;
export {};
