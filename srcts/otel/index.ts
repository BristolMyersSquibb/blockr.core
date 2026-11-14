// Core's OpenTelemetry module, which the build bundles into blockr-core-otel.js
// on its own, and which the page's import map names `@blockr/core/otel`. Core
// imports it only where the page sends its log records or its spans to an
// OpenTelemetry collector, so that a page that sends none loads none of it
// (../log/settings.ts, ../trace/settings.ts), as Logging and Tracing have it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging).
// The parts of OpenTelemetry's SDK that it uses come from ./sdk.ts, which the
// build bundles, minified, into a file of their own. It imports OpenTelemetry's
// API as the page's import map names it, so that it sets up the one copy of
// the API through which core and the modules of packages record spans.
//
// Its logger sends each line as a log record to the collector, over OTLP's
// HTTP protocol in its JSON encoding, in batches. The SDK's batch processor
// also sends what it holds as the page is hidden, with fetch's keepalive, so
// that the records of a page that closes still go. A record holds:
// - The line's level as its severity, and its message as its body.
// - The time when the line was logged, since core may hand the line over
//   later.
// - As attributes, those of the line, the package as `blockr.package.name`,
//   as R's records name it, and the session by `session.id`, Shiny's session
//   token, as Shiny's own records name it.
// - For an Error, `exception.type`, `exception.message` and
//   `exception.stacktrace`, as OpenTelemetry's conventions for exceptions name
//   them. The stack trace is followed by those of the errors that caused it.
// The attributes that the module sets win over those of the line with the same
// name, so that a line names no other package or session.
//
// Its tracing has every span that core and the modules of packages start on
// the page record, and sends each span to the collector once it ends, over
// OTLP's HTTP protocol in its JSON encoding, in batches, which the SDK's batch
// processor sends as the page is hidden as well. It registers a tracer
// provider and the StackContextManager as the API's globals, so that a span
// stays active through synchronous calls alone. Each span names the session in
// which it started by `session.id`, as each log record does.

import { context, SeverityNumber, trace } from "@opentelemetry/api";
import type { AnyValue, Attributes } from "@opentelemetry/api";
import type { LogRecordExporter } from "@opentelemetry/sdk-logs";
import type { SpanExporter, SpanProcessor } from "@opentelemetry/sdk-trace";
import type { LogLevel, LogLine } from "../types/log.ts";
import {
  BatchLogRecordProcessor,
  BatchSpanProcessor,
  LoggerProvider,
  OTLPLogExporter,
  OTLPTraceExporter,
  StackContextManager,
  TracerProvider,
} from "./sdk.ts";

export interface OtelLoggerOptions {
  // The URL that the records go to, the collector's path v1/logs.
  readonly url: string;
  // The id of the session that runs, or undefined while none runs.
  readonly sessionId: () => string | undefined;
  // What sends the records, OTLP over HTTP to `url` unless given, as a test
  // gives one that holds them.
  readonly exporter?: LogRecordExporter;
}

// A logger that sends each line as a record, and sends what it holds at once
// through flush().
export interface OtelLogger {
  (line: LogLine): void;
  flush(): Promise<void>;
}

const SEVERITIES: Readonly<Record<LogLevel, SeverityNumber>> = {
  error: SeverityNumber.ERROR,
  warn: SeverityNumber.WARN,
  info: SeverityNumber.INFO,
  debug: SeverityNumber.DEBUG,
  trace: SeverityNumber.TRACE,
};

// The most errors that a stack trace follows through their causes.
const CAUSES = 8;

export function otelLogger(options: OtelLoggerOptions): OtelLogger {
  const exporter = options.exporter ?? new OTLPLogExporter({ url: options.url });
  const provider = new LoggerProvider({ processors: [new BatchLogRecordProcessor({ exporter })] });
  const logger = provider.getLogger("blockr.core");
  const write = (line: LogLine): void => {
    const session = options.sessionId();
    logger.emit({
      timestamp: line.time,
      severityNumber: SEVERITIES[line.level],
      severityText: line.level.toUpperCase(),
      body: line.message,
      attributes: {
        ...(line.attributes as Attributes),
        ...(line.package === null ? {} : { "blockr.package.name": line.package }),
        ...(session === undefined ? {} : { "session.id": session }),
        ...(line.error === undefined ? {} : exception(line.error)),
      },
    });
  };
  return Object.assign(write, { flush: () => provider.forceFlush() });
}

export interface OtelTracingOptions {
  // The URL that the spans go to, the collector's path v1/traces.
  readonly url: string;
  // The id of the session that runs, or undefined while none runs.
  readonly sessionId: () => string | undefined;
  // What sends the spans, OTLP over HTTP to `url` unless given, as a test
  // gives one that holds them.
  readonly exporter?: SpanExporter;
}

// The page's tracing, which sends the spans that it holds at once through
// flush().
export interface OtelTracing {
  flush(): Promise<void>;
}

// Sets the page's tracing up, so that every span that starts from now on
// records and goes to the collector. A span that started before records
// nothing.
export function otelTracing(options: OtelTracingOptions): OtelTracing {
  const exporter = options.exporter ?? new OTLPTraceExporter({ url: options.url });
  const provider = new TracerProvider({
    spanProcessors: [sessionNamer(options.sessionId), new BatchSpanProcessor({ exporter })],
  });
  context.setGlobalContextManager(new StackContextManager().enable());
  trace.setGlobalTracerProvider(provider);
  return { flush: () => provider.forceFlush() };
}

// Names the session of each span by `session.id` as the span starts, where a
// session runs, over the attribute of that name that the span started with.
function sessionNamer(sessionId: () => string | undefined): SpanProcessor {
  return {
    onStart(span) {
      const session = sessionId();
      if (session !== undefined) span.setAttribute("session.id", session);
    },
    onEnd: () => undefined,
    forceFlush: () => Promise.resolve(),
    shutdown: () => Promise.resolve(),
  };
}

function exception(error: Error): Readonly<Record<string, AnyValue>> {
  return {
    "exception.type": error.name,
    "exception.message": error.message,
    "exception.stacktrace": stackTrace(error),
  };
}

// The stack trace of an error, and those of the errors that caused it, each
// after "Caused by: ". A stack that leaves out the error's name and message,
// as Firefox's does, gets them in front.
function stackTrace(error: Error): string {
  const traces: string[] = [];
  let at: unknown = error;
  while (at instanceof Error && traces.length < CAUSES) {
    const head = `${at.name}: ${at.message}`;
    const stack = at.stack ?? "";
    traces.push(stack.startsWith(head) ? stack : [head, stack].filter((text) => text !== "").join("\n"));
    at = at.cause;
  }
  return traces.join("\nCaused by: ");
}
