// Setting the page's logger up as the page starts, as Logging has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging),
// from three blockr options, which core reads with those that its scheduler
// reads (../core/start.ts):
// - The blockr option log_level names the lowest level that the page writes,
//   info unless it names another, as in R, which also takes a level by the
//   start of its name.
// - The blockr option logger names the logger, as in R. With "console" or
//   "condition", core writes to the browser's console, since the browser has
//   no conditions. With "otel", it sends each line as a record to an
//   OpenTelemetry collector. Where logger is not set, core sends the records
//   to a collector where otel_client_endpoint names one, and writes to the
//   console otherwise, as R picks OpenTelemetry's logger where OpenTelemetry
//   has set up an exporter.
// - The blockr option otel_client_endpoint gives the URL of that collector,
//   which every user's browser must reach, as OTEL_EXPORTER_OTLP_ENDPOINT
//   gives an SDK the URL of one. Core sends the records to its path v1/logs.
// A value that none of these takes stops the page, as a value of the
// scheduler's blockr options does that the scheduler does not take.
//
// Core sends records through its OpenTelemetry module (../otel/index.ts),
// which the page imports for it only where it sends them (../page/page.ts),
// and holds the lines until the module has loaded. Where the module fails to
// load, core writes to the console instead, and logs why there.

import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";
import { awaitLogger, consoleLogger, log, setLogging, THRESHOLDS } from "./log.ts";
import type { Logger, Threshold } from "./log.ts";

// The names that R's blockr option logger takes.
export const LOGGERS = ["console", "otel", "condition"] as const;

// What core sets up the page's logger with: the lowest level that the page
// writes, and the URL that it sends records to, or none where it writes to the
// console.
export interface LogSettings {
  readonly lowest: Threshold;
  readonly url?: string;
}

// The values of the three blockr options, as R sent them.
export interface LogOptions {
  readonly logLevel: Json | undefined;
  readonly logger: Json | undefined;
  readonly otelClientEndpoint: Json | undefined;
}

// What core's OpenTelemetry module gives: a logger that sends each line as a
// record to the URL, naming the session in which it was logged.
export interface OtelModule {
  otelLogger(options: { url: string; sessionId: () => string | undefined }): Logger;
}

// The settings that the values give, with a relative URL read against `base`,
// the page's. It throws for a value that none of the options takes.
export function logSettings(values: LogOptions, base?: string): LogSettings {
  const lowest = logLevel(values.logLevel);
  const endpoint = values.otelClientEndpoint === undefined ? undefined : logsUrl(values.otelClientEndpoint, base);
  const logger = values.logger;
  if (logger !== undefined && (typeof logger !== "string" || !(LOGGERS as readonly string[]).includes(logger))) {
    throw new TypeError(`The blockr option logger is ${JSON.stringify(logger)}, where it takes "console", "otel" or "condition".`);
  }
  if (logger === "otel" && endpoint === undefined) {
    throw new Error(
      'The blockr option logger is "otel", but the blockr option otel_client_endpoint names no collector to send the log records of the client to.',
    );
  }
  const sends = logger === "otel" || (logger === undefined && endpoint !== undefined);
  return sends && endpoint !== undefined ? { lowest, url: endpoint } : { lowest };
}

// The lowest level that log_level names, info where it is not set. It takes a
// level by its name or the start of it, as R's as_log_level() does through
// match.arg(), so that the page takes what R takes.
function logLevel(value: Json | undefined): Threshold {
  if (value === undefined) return "info";
  const named = typeof value === "string" && value !== "" ? THRESHOLDS.filter((level) => level.startsWith(value)) : [];
  const [level] = named;
  if (named.length === 1 && level !== undefined) return level;
  throw new TypeError(
    `The blockr option log_level is ${JSON.stringify(value)}, where it takes "fatal", "error", "warn", "info", "debug" or "trace".`,
  );
}

// The URL that core sends log records to: the path v1/logs below the
// collector's URL, which may be relative to the page, as an SDK appends
// v1/logs to OTEL_EXPORTER_OTLP_ENDPOINT.
function logsUrl(value: Json, base: string | undefined): string {
  let url: URL | undefined;
  try {
    url = typeof value === "string" && value !== "" ? new URL(value, base) : undefined;
  } catch {
    url = undefined;
  }
  if (url === undefined || (url.protocol !== "http:" && url.protocol !== "https:")) {
    throw new TypeError(
      `The blockr option otel_client_endpoint is ${JSON.stringify(value)}, where it takes the URL of an OpenTelemetry collector, over HTTP or HTTPS.`,
    );
  }
  url.pathname = `${url.pathname.replace(/\/$/, "")}/v1/logs`;
  return url.href;
}

// Sets the page's logger up: the console's at once, or, where the settings
// name a URL, OpenTelemetry's once `load` has imported core's module, which
// names the session of each record by `sessionId`.
export function setUpLogging(settings: LogSettings, sessionId: () => string | undefined, load: () => Promise<OtelModule>): void {
  const { lowest, url } = settings;
  if (url === undefined) {
    setLogging(lowest, consoleLogger);
    return;
  }
  awaitLogger();
  void (async () => {
    let logger: Logger;
    try {
      logger = (await load()).otelLogger({ url, sessionId });
    } catch (error) {
      setLogging(lowest, consoleLogger);
      log.error(
        new Error(`Core could not set up its OpenTelemetry logger, so the page writes its log lines to the console: ${errorMessage(error)}`, {
          cause: error,
        }),
      );
      return;
    }
    setLogging(lowest, logger);
  })();
}
