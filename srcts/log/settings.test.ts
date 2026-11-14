// Setting the page's logger up from the blockr options log_level, logger and
// otel_client_endpoint: what each takes, which logger they pick, and the
// OpenTelemetry module that the page imports for the collector's.

import { afterEach, beforeEach, describe, expect, test } from "vitest";
import { logged } from "../testing/logged.ts";
import type { Json } from "../types/json.ts";
import type { LogLine } from "../types/log.ts";
import { consoleLogger, createLog, currentLogging, fallBack, logSettings, setTestLoggersAside, setUpLogging } from "./index.ts";
import type { OtelModule } from "./index.ts";

const unset = { logLevel: undefined, logger: undefined, otelClientEndpoint: undefined };

describe("the settings of the page's logger", () => {
  test("write at info to the console where no blockr option is set", () => {
    expect(logSettings(unset)).toEqual({ lowest: "info" });
  });

  test("take a level by its name or the start of it, as R's match.arg() does", () => {
    const lowest = (logLevel: Json) => logSettings({ ...unset, logLevel }).lowest;
    expect(lowest("debug")).toBe("debug");
    expect(lowest("deb")).toBe("debug");
    expect(lowest("t")).toBe("trace");
    expect(lowest("fatal")).toBe("fatal");
    for (const value of ["verbose", "", "INFO", 3, null, true]) {
      expect(() => lowest(value)).toThrow(
        `The blockr option log_level is ${JSON.stringify(value)}, where it takes "fatal", "error", "warn", "info", "debug" or "trace".`,
      );
    }
  });

  test("send to the collector that otel_client_endpoint names, at its path v1/logs, where logger is unset or otel", () => {
    const url = (otelClientEndpoint: Json, logger?: Json, base?: string) =>
      logSettings({ ...unset, otelClientEndpoint, logger }, base).url;
    expect(url("https://connect.example.com/otlp/")).toBe("https://connect.example.com/otlp/v1/logs");
    expect(url("https://connect.example.com/otlp")).toBe("https://connect.example.com/otlp/v1/logs");
    expect(url("http://localhost:4318")).toBe("http://localhost:4318/v1/logs");
    expect(url("/otlp/", "otel", "https://connect.example.com/content/1/")).toBe("https://connect.example.com/otlp/v1/logs");
  });

  test("write to the console where logger is console or condition, whatever otel_client_endpoint names", () => {
    for (const logger of ["console", "condition"]) {
      expect(logSettings({ logLevel: "warn", logger, otelClientEndpoint: "https://connect.example.com/otlp/" })).toEqual({ lowest: "warn" });
    }
  });

  test("refuse a logger that is none of the three, and otel where otel_client_endpoint names no collector", () => {
    for (const logger of ["cat_logger", "otel_logger", "", true]) {
      expect(() => logSettings({ ...unset, logger })).toThrow(
        `The blockr option logger is ${JSON.stringify(logger)}, where it takes "console", "otel" or "condition".`,
      );
    }
    expect(() => logSettings({ ...unset, logger: "otel" })).toThrow(
      'The blockr option logger is "otel", but the blockr option otel_client_endpoint names no collector to send the log records of the client to.',
    );
  });

  test("refuse an otel_client_endpoint that is no URL over HTTP or HTTPS", () => {
    for (const value of ["", "javascript:alert(1)", "ftp://example.com/otlp", 4318, false, null]) {
      expect(() => logSettings({ ...unset, otelClientEndpoint: value }, "https://connect.example.com/")).toThrow(
        `The blockr option otel_client_endpoint is ${JSON.stringify(value)}, where it takes the URL of an OpenTelemetry collector, over HTTP or HTTPS.`,
      );
    }
    // A relative URL needs the page's to be read against.
    expect(() => logSettings({ ...unset, otelClientEndpoint: "/otlp/" })).toThrow("The blockr option otel_client_endpoint is");
  });
});

describe("setting the page's logger up", () => {
  let back: () => void;
  beforeEach(() => {
    back = setTestLoggersAside();
  });
  afterEach(() => {
    back();
  });

  const noModule = (): Promise<OtelModule> => Promise.reject(new Error("The test imports no module."));

  test("sets the console's logger at once where the settings name no URL", () => {
    setUpLogging({ lowest: "debug" }, () => undefined, noModule);
    expect(currentLogging()).toEqual({ lowest: "debug", logger: consoleLogger });
  });

  test("holds the lines until the OpenTelemetry module has loaded, though the page fails meanwhile, then hands them to its logger, which names the session", async () => {
    const lines: LogLine[] = [];
    const given: { url: string; session: string | undefined }[] = [];
    let loaded: (module: OtelModule) => void = () => undefined;
    const module = new Promise<OtelModule>((resolve) => {
      loaded = resolve;
    });

    setUpLogging({ lowest: "info", url: "https://connect.example.com/otlp/v1/logs" }, () => "token-1", () => module);
    createLog("blockr.core").warn("Held.");
    // A page that fails meanwhile waits for the module rather than for the
    // console.
    fallBack();
    expect(currentLogging()).toBeUndefined();

    loaded({
      otelLogger({ url, sessionId }) {
        given.push({ url, session: sessionId() });
        return (line) => {
          lines.push(line);
        };
      },
    });
    await module;
    await Promise.resolve();

    expect(given).toEqual([{ url: "https://connect.example.com/otlp/v1/logs", session: "token-1" }]);
    expect(lines.map((line) => line.message)).toEqual(["Held."]);
    createLog("blockr.core").debug("Below.");
    expect(lines).toHaveLength(1);
  });

  test("writes to the console where the module fails to load, and logs why", async () => {
    const reported = logged();
    setUpLogging({ lowest: "info", url: "https://connect.example.com/otlp/v1/logs" }, () => undefined, noModule);
    await new Promise((resolve) => setTimeout(resolve, 0));
    reported.restore();

    expect(currentLogging()).toEqual({ lowest: "info", logger: consoleLogger });
    expect(reported.errors().map((line) => line.message)).toEqual([
      "Core could not set up its OpenTelemetry logger, so the page writes its log lines to the console: The test imports no module.",
    ]);
  });
});
