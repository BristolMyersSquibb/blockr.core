import type { Json } from "./json.ts";
export type LogLevel = "error" | "warn" | "info" | "debug" | "trace";
export interface Log {
    error(message: string | Error, attributes?: Readonly<Record<string, Json>>): void;
    warn(message: string, attributes?: Readonly<Record<string, Json>>): void;
    info(message: string, attributes?: Readonly<Record<string, Json>>): void;
    debug(message: string, attributes?: Readonly<Record<string, Json>>): void;
    trace(message: string, attributes?: Readonly<Record<string, Json>>): void;
}
export interface LogLine {
    readonly level: LogLevel;
    readonly message: string;
    readonly package: string | null;
    readonly error?: Error;
    readonly attributes: Readonly<Record<string, Json>>;
    readonly time: number;
}
