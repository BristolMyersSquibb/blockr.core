// Core's OpenTelemetry module: what each record of its logger holds of the
// line it sends, and what its tracing records, with exporters that hold the
// records and the spans in place of those that send them to the collector.

import { context, SeverityNumber, trace } from "@opentelemetry/api";
import { InMemoryLogRecordExporter } from "@opentelemetry/sdk-logs";
import { InMemorySpanExporter } from "@opentelemetry/sdk-trace";
import { describe, expect, test } from "vitest";
import { stopRecording } from "../testing/spans.ts";
import type { LogLine } from "../types/log.ts";
import { otelLogger, otelTracing } from "./index.ts";

const URL = "https://connect.example.com/otlp/v1/logs";

function line(given: Partial<LogLine>): LogLine {
  return { level: "info", message: "Opened.", package: "blockr.dock", attributes: {}, time: 1_700_000_000_123, ...given };
}

describe("the OpenTelemetry logger", () => {
  test("sends each line as a record with its severity, its message, the time it was logged, its package and its session", async () => {
    const exporter = new InMemoryLogRecordExporter();
    let session: string | undefined = "token-1";
    const logger = otelLogger({ url: URL, sessionId: () => session, exporter });

    logger(line({ level: "warn", message: "Careful.", attributes: { "blockr.block.id": "a" } }));
    session = undefined;
    logger(line({ level: "trace", package: null }));
    await logger.flush();

    const records = exporter.getFinishedLogRecords();
    expect(records.map((record) => [record.severityNumber, record.severityText, record.body])).toEqual([
      [SeverityNumber.WARN, "WARN", "Careful."],
      [SeverityNumber.TRACE, "TRACE", "Opened."],
    ]);
    expect(records[0]?.attributes).toEqual({ "blockr.block.id": "a", "blockr.package.name": "blockr.dock", "session.id": "token-1" });
    // A line of no package, logged while no session runs, names neither.
    expect(records[1]?.attributes).toEqual({});
    expect(records[0]?.hrTime).toEqual([1_700_000_000, 123_000_000]);
    expect(records[0]?.instrumentationScope.name).toBe("blockr.core");
  });

  test("gives an error the type, message and stack trace of its Error, followed by those of its causes", async () => {
    const exporter = new InMemoryLogRecordExporter();
    const logger = otelLogger({ url: URL, sessionId: () => undefined, exporter });
    const cause = new TypeError("No state.");
    const error = new Error("The plugin failed: No state.", { cause });

    logger(line({ level: "error", message: error.message, error }));
    await logger.flush();

    const [record] = exporter.getFinishedLogRecords();
    expect(record?.severityNumber).toBe(SeverityNumber.ERROR);
    expect(record?.attributes["exception.type"]).toBe("Error");
    expect(record?.attributes["exception.message"]).toBe("The plugin failed: No state.");
    expect(record?.attributes["exception.stacktrace"]).toBe(`${error.stack ?? ""}\nCaused by: ${cause.stack ?? ""}`);
  });

  test("names the package and the session itself, whatever the line's attributes say", async () => {
    const exporter = new InMemoryLogRecordExporter();
    const logger = otelLogger({ url: URL, sessionId: () => "token-1", exporter });

    logger(line({ attributes: { "blockr.package.name": "blockr.other", "session.id": "token-2", n: 2 } }));
    await logger.flush();

    expect(exporter.getFinishedLogRecords()[0]?.attributes).toEqual({
      "blockr.package.name": "blockr.dock",
      "session.id": "token-1",
      n: 2,
    });
  });
});

describe("the OpenTelemetry tracing", () => {
  test("has the spans that start from then on record, each naming the session in which it started, and sends them through its exporter", async () => {
    const exporter = new InMemorySpanExporter();
    let session: string | undefined = "token-1";
    const tracer = trace.getTracer("blockr.test");
    const before = tracer.startSpan("before");

    const tracing = otelTracing({ url: "https://connect.example.com/otlp/v1/traces", sessionId: () => session, exporter });
    const named = tracer.startSpan("named", { attributes: { "session.id": "token-2", n: 1 } });
    session = undefined;
    const unnamed = tracer.startSpan("unnamed");
    for (const span of [before, named, unnamed]) span.end();
    await tracing.flush();

    expect(before.isRecording()).toBe(false);
    expect(exporter.getFinishedSpans().map((span) => [span.name, span.attributes])).toEqual([
      ["named", { "session.id": "token-1", n: 1 }],
      ["unnamed", {}],
    ]);
    stopRecording();
  });

  test("keeps a span active through synchronous calls alone, as the StackContextManager does", async () => {
    const exporter = new InMemorySpanExporter();
    const tracing = otelTracing({ url: "https://connect.example.com/otlp/v1/traces", sessionId: () => undefined, exporter });
    const tracer = trace.getTracer("blockr.test");
    const outer = tracer.startSpan("outer");
    const active: (string | undefined)[] = [];

    await context.with(trace.setSpan(context.active(), outer), async () => {
      active.push(trace.getActiveSpan()?.spanContext().spanId);
      await Promise.resolve();
      active.push(trace.getActiveSpan()?.spanContext().spanId);
    });
    outer.end();
    await tracing.flush();

    expect(active).toEqual([outer.spanContext().spanId, undefined]);
    stopRecording();
  });
});
