import type { Json } from "../types/json.ts";
import type { Logger, Threshold } from "./log.ts";
export declare const LOGGERS: readonly ["console", "otel", "condition"];
export interface LogSettings {
    readonly lowest: Threshold;
    readonly url?: string;
}
export interface LogOptions {
    readonly logLevel: Json | undefined;
    readonly logger: Json | undefined;
    readonly otelClientEndpoint: Json | undefined;
}
export interface OtelModule {
    otelLogger(options: {
        url: string;
        sessionId: () => string | undefined;
    }): Logger;
}
export declare function logSettings(values: LogOptions, base?: string): LogSettings;
export declare function setUpLogging(settings: LogSettings, sessionId: () => string | undefined, load: () => Promise<OtelModule>): void;
