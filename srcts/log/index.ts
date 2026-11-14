// The page's log lines: the Log that core and the modules of packages write
// through, the logger that writes the lines out, and setting it up as the page
// starts.

export { awaitLogger, consoleLogger, createLog, currentLogging, fallBack, log, reaches, replaceLogger, resetLogging, setLogging, setTestLoggersAside, THRESHOLDS } from "./log.ts";
export type { Logger, Threshold } from "./log.ts";
export { writingLog } from "./line.ts";
export { collectorUrl, LOGGERS, logSettings, setUpLogging } from "./settings.ts";
export type { LogOptions, LogSettings, OtelModule } from "./settings.ts";
