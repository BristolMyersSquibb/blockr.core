// The log lines of the page, as Logging has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging):
// core's own, and those of the modules of packages, each written through a Log
// that names the package that writes it (../types/log.ts).
//
// Core holds one logger for the page, which writes the lines out, and the
// lowest level that the page writes. Core sets both once, as the page starts,
// from the blockr options log_level, logger and otel_client_endpoint
// (./settings.ts). Until then, it holds every line logged. Once it has set
// them, it writes those held lines that reach the level, so that what fails as
// the page starts reaches the logger too. Where core fails before it has set
// the logger, and sets none up, it writes what it holds to the console, at
// info.
//
// The page runs one copy of this module, in core's runtime. A module of a
// package logs through the Log that CoreApi or its host hands it, so that its
// lines reach this copy too.
// A test puts a logger of its own in place of the page's, which takes every
// line, whatever its level (../testing/logged.ts).

import type { Log, LogLevel, LogLine } from "../types/log.ts";
import { writingLog } from "./line.ts";

// What writes log lines out: the console's, OpenTelemetry's or a test's.
export type Logger = (line: LogLine) => void;

// The levels that the blockr option log_level names, as R orders them, from
// the most severe. A line reaches a level where its level comes no later. So
// fatal, which no line of the client's has, writes none of them.
export const THRESHOLDS = ["fatal", "error", "warn", "info", "debug", "trace"] as const;
export type Threshold = (typeof THRESHOLDS)[number];

// What core set for the page: the lowest level it writes, and its logger.
interface Logging {
  readonly lowest: Threshold;
  readonly logger: Logger;
}

let logging: Logging | undefined;
// Whether core is setting a logger up that it does not have yet, as while its
// OpenTelemetry module loads.
let awaited = false;
// The lines logged before core set the logger, in the order they came.
let held: LogLine[] = [];
// The loggers that tests put in place of the page's, the latest last, each
// with whether it passes each line on to the one before it.
const replaced: { readonly logger: Logger; readonly passOn: boolean }[] = [];

// Whether a line of the level reaches the lowest level that the page writes.
export function reaches(level: LogLevel, lowest: Threshold): boolean {
  return THRESHOLDS.indexOf(level) <= THRESHOLDS.indexOf(lowest);
}

// Writes the line through the latest of the loggers of tests from `at` down,
// or else through the page's logger.
function write(line: LogLine, at = replaced.length - 1): void {
  const test = replaced[at];
  if (test !== undefined) {
    test.logger(line);
    if (test.passOn) write(line, at - 1);
  } else if (logging === undefined) {
    held.push(line);
  } else if (reaches(line.level, logging.lowest)) {
    logging.logger(line);
  }
}

// Sets the lowest level that the page writes and its logger, which then
// writes the lines that core held, those that reach the level.
export function setLogging(lowest: Threshold, logger: Logger): void {
  logging = { lowest, logger };
  awaited = false;
  const lines = held;
  held = [];
  for (const line of lines) write(line);
}

// Says that core sets a logger up that it does not have yet, so that the
// lines wait for it rather than for a fallback.
export function awaitLogger(): void {
  awaited = true;
}

// Has the console write what core holds and every later line, at info, where
// core has not set the logger and sets none up, as where the page cannot
// start before R gives the blockr options that set it up.
export function fallBack(): void {
  if (logging === undefined && !awaited) setLogging("info", consoleLogger);
}

// A Log whose lines name the package, or no package for a module of a block
// class defined outside a package, and go to the page's logger.
export function createLog(pkg: string | null): Log {
  return writingLog(pkg, write);
}

// Core's own log, through which it logs every failure that it catches, at
// error, with the failure's stack. The build leaves it out of a bundle that
// does not use it, so that no module that the build bundles on its own holds a
// copy of the page's logging.
export const log: Log = /* @__PURE__ */ createLog("blockr.core");

// The console's logger, which writes each line with the console's function of
// its level, trace as debug, behind the name of its package, and an error with
// the Error it came with, whose stack the console shows. It hands the console
// the text through "%s", so that the console reads no directive such as "%o"
// in it.
export const consoleLogger: Logger = (line) => {
  const write = consoleWriter(line.level);
  const prefix = line.package === null ? undefined : `[${line.package}]`;
  if (line.error === undefined) {
    write("%s", prefix === undefined ? line.message : `${prefix} ${line.message}`);
  } else if (prefix === undefined) {
    write(line.error);
  } else {
    write("%s", prefix, line.error);
  }
};

function consoleWriter(level: LogLevel): (...data: unknown[]) => void {
  switch (level) {
    case "error":
      return (...data) => {
        console.error(...data);
      };
    case "warn":
      return (...data) => {
        console.warn(...data);
      };
    case "info":
      return (...data) => {
        console.info(...data);
      };
    case "debug":
    case "trace":
      return (...data) => {
        console.debug(...data);
      };
  }
}

// Puts a test's logger in place of the page's, which takes every line,
// whatever its level, until the function that it returns runs. Where `passOn`
// is true, each line goes on to the logger that was in place before it, as
// the stub of CoreApi holds each line and passes it on.
export function replaceLogger(logger: Logger, passOn = false): () => void {
  const entry = { logger, passOn };
  replaced.push(entry);
  return () => {
    const at = replaced.lastIndexOf(entry);
    if (at >= 0) replaced.splice(at, 1);
  };
}

// Sets the loggers of tests aside, so that the page's own logging applies, as
// a test of it needs, until the function that it returns puts them back.
export function setTestLoggersAside(): () => void {
  const saved = replaced.splice(0);
  return () => {
    replaced.unshift(...saved);
  };
}

// What core set for the page, or undefined while it holds the lines, for a
// test to read.
export function currentLogging(): Logging | undefined {
  return logging;
}

// Forgets what core set for the page and the lines it holds, as a test does
// after each test.
export function resetLogging(): void {
  logging = undefined;
  awaited = false;
  held = [];
}
