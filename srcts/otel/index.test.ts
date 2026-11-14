// Core's OpenTelemetry logger: what each record holds of the line it sends,
// with an exporter that holds the records in place of the one that sends them
// to the collector.

import { SeverityNumber } from "@opentelemetry/api";
import { InMemoryLogRecordExporter } from "@opentelemetry/sdk-logs";
import { describe, expect, test } from "vitest";
import type { LogLine } from "../types/log.ts";
import { otelLogger } from "./index.ts";

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
