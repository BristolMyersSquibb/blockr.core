// The page's log lines: a Log that names its package, the lines that core
// holds until it has set the page's logger up, the lowest level that the page
// writes, the console's logger, and the loggers that tests put in place of
// the page's.

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { LogLine } from "../types/log.ts";
import { consoleLogger, createLog, currentLogging, fallBack, replaceLogger, setLogging, setTestLoggersAside } from "./index.ts";

// The page's own logging applies in these tests, with the loggers of tests set
// aside, and starts afresh after each.
let back: () => void;
beforeEach(() => {
  back = setTestLoggersAside();
});
afterEach(() => {
  back();
  vi.restoreAllMocks();
});

// A logger that holds the lines it writes.
function holding(): { lines: LogLine[]; logger: (line: LogLine) => void } {
  const lines: LogLine[] = [];
  return {
    lines,
    logger: (line) => {
      lines.push(line);
    },
  };
}

describe("a log", () => {
  test("writes a line of each level, naming its package, with a copy of its attributes and the time it was logged", () => {
    const { lines, logger } = holding();
    setLogging("trace", logger);
    vi.spyOn(Date, "now").mockReturnValue(1_700_000_000_000);
    const attributes = { "blockr.block.id": "a" };
    const log = createLog("blockr.dock");

    log.error("Failed.", attributes);
    log.warn("Careful.");
    log.info("Opened.");
    log.debug("Dropped.");
    log.trace("Called.");
    attributes["blockr.block.id"] = "b";

    expect(lines).toEqual([
      { level: "error", message: "Failed.", package: "blockr.dock", attributes: { "blockr.block.id": "a" }, time: 1_700_000_000_000 },
      { level: "warn", message: "Careful.", package: "blockr.dock", attributes: {}, time: 1_700_000_000_000 },
      { level: "info", message: "Opened.", package: "blockr.dock", attributes: {}, time: 1_700_000_000_000 },
      { level: "debug", message: "Dropped.", package: "blockr.dock", attributes: {}, time: 1_700_000_000_000 },
      { level: "trace", message: "Called.", package: "blockr.dock", attributes: {}, time: 1_700_000_000_000 },
    ]);
  });

  test("logs an error with the Error it is given, by the Error's message, and names no package for a module defined outside one", () => {
    const { lines, logger } = holding();
    setLogging("trace", logger);
    const error = new TypeError("No state.");

    createLog(null).error(error);

    expect(lines).toEqual([{ level: "error", message: "No state.", package: null, error, attributes: {}, time: expect.any(Number) as number }]);
  });

  test("takes what a module in plain JS gives in place of a message as text, and drops attributes that are no object", () => {
    const { lines, logger } = holding();
    setLogging("trace", logger);
    const log = createLog("blockr.test") as unknown as Record<string, (message: unknown, attributes?: unknown) => void>;

    log.warn?.(new Error("Late."), ["a"]);
    log.info?.(42, "b");

    expect(lines.map(({ message, attributes, error }) => ({ message, attributes, error }))).toEqual([
      { message: "Error: Late.", attributes: {}, error: undefined },
      { message: "42", attributes: {}, error: undefined },
    ]);
  });
});

describe("the page's logger", () => {
  test("holds the lines logged before core sets it, and writes those that reach the level once it does, in the order they came", () => {
    const log = createLog("blockr.core");
    log.debug("first");
    log.warn("second");
    log.error("third");
    expect(currentLogging()).toBeUndefined();

    const { lines, logger } = holding();
    setLogging("warn", logger);
    expect(lines.map((line) => line.message)).toEqual(["second", "third"]);

    // Later lines go to it at once, and those below the level go nowhere.
    log.info("fourth");
    log.warn("fifth");
    expect(lines.map((line) => line.message)).toEqual(["second", "third", "fifth"]);
    expect(currentLogging()).toEqual({ lowest: "warn", logger });
  });

  test("writes no line where the lowest level is fatal, which no line of the client's has", () => {
    const { lines, logger } = holding();
    setLogging("fatal", logger);

    createLog("blockr.core").error("Failed.");

    expect(lines).toEqual([]);
  });

  test("falls back on the console at info where core has not set it, as where the page cannot start, and keeps the one that core set", () => {
    const info = vi.spyOn(console, "info").mockImplementation(() => undefined);
    const debug = vi.spyOn(console, "debug").mockImplementation(() => undefined);
    createLog("blockr.core").info("Held.");
    createLog("blockr.core").debug("Below.");

    fallBack();

    expect(currentLogging()).toEqual({ lowest: "info", logger: consoleLogger });
    expect(info).toHaveBeenCalledWith("%s", "[blockr.core] Held.");
    expect(debug).not.toHaveBeenCalled();

    const { logger } = holding();
    setLogging("trace", logger);
    fallBack();
    expect(currentLogging()?.logger).toBe(logger);
  });
});

describe("the console's logger", () => {
  test("writes each line with the console's function of its level, trace as debug, behind its package, reading no directive in it", () => {
    const spies = {
      error: vi.spyOn(console, "error").mockImplementation(() => undefined),
      warn: vi.spyOn(console, "warn").mockImplementation(() => undefined),
      info: vi.spyOn(console, "info").mockImplementation(() => undefined),
      debug: vi.spyOn(console, "debug").mockImplementation(() => undefined),
    };
    setLogging("trace", consoleLogger);
    const log = createLog("blockr.dock");

    log.error("Failed.");
    log.warn("50%off");
    log.info("Opened.");
    log.debug("Dropped.");
    log.trace("Called.");
    createLog(null).info("No package.");

    expect(spies.error.mock.calls).toEqual([["%s", "[blockr.dock] Failed."]]);
    expect(spies.warn.mock.calls).toEqual([["%s", "[blockr.dock] 50%off"]]);
    expect(spies.info.mock.calls).toEqual([
      ["%s", "[blockr.dock] Opened."],
      ["%s", "No package."],
    ]);
    expect(spies.debug.mock.calls).toEqual([
      ["%s", "[blockr.dock] Dropped."],
      ["%s", "[blockr.dock] Called."],
    ]);
  });

  test("hands the console the Error that an error came with, whose stack the console shows", () => {
    const error = vi.spyOn(console, "error").mockImplementation(() => undefined);
    setLogging("trace", consoleLogger);
    const failure = new Error("Failed.");

    createLog("blockr.dock").error(failure);
    createLog(null).error(failure);

    expect(error.mock.calls).toEqual([["%s", "[blockr.dock]", failure], [failure]]);
  });
});

describe("a test's logger", () => {
  test("takes every line in place of the page's, whatever its level, until it is restored", () => {
    const page = holding();
    setLogging("error", page.logger);
    const test = holding();
    const restore = replaceLogger(test.logger);

    createLog("blockr.core").trace("Called.");
    restore();
    createLog("blockr.core").error("Failed.");

    expect(test.lines.map((line) => line.message)).toEqual(["Called."]);
    expect(page.lines.map((line) => line.message)).toEqual(["Failed."]);
  });

  test("passes each line on to the logger before it, where it is asked to", () => {
    const page = holding();
    setLogging("error", page.logger);
    const below = holding();
    const restoreBelow = replaceLogger(below.logger);
    const passing = holding();
    const restorePassing = replaceLogger(passing.logger, true);

    createLog("blockr.core").info("Opened.");
    restorePassing();
    restoreBelow();

    expect(passing.lines.map((line) => line.message)).toEqual(["Opened."]);
    expect(below.lines.map((line) => line.message)).toEqual(["Opened."]);
    expect(page.lines).toEqual([]);
  });
});

describe("the page's logging", () => {
  test("is held by core's runtime alone, of the files that the build writes into inst/js", () => {
    const js = resolve(dirname(fileURLToPath(import.meta.url)), "../../inst/js");
    const holding = readdirSync(js)
      .filter((file) => file.endsWith(".js"))
      .filter((file) => readFileSync(join(js, file), "utf8").includes("// srcts/log/log.ts\n"));
    expect(holding).toEqual(["blockr-core.js"]);
  });
});
