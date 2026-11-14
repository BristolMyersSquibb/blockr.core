// A Log that hands each line to a function, as the page's logging does
// (./log.ts) and the stubs of the hosts do, which hold what a module logs.
// It holds no state of its own, so that a bundle that imports it, such as the
// stubs, writes no line into the page's logging.

import type { Json } from "../types/json.ts";
import type { Log, LogLevel, LogLine } from "../types/log.ts";
import { isObject } from "../util/index.ts";

// A Log whose lines name the package, or no package for a module of a block
// class defined outside a package, and go to `write`. A module in plain JS may
// give another message than the types allow, which becomes text, and
// attributes that are no object, which it drops.
export function writingLog(pkg: string | null, write: (line: LogLine) => void): Log {
  const line = (level: LogLevel, message: unknown, attributes: unknown): void => {
    const error = level === "error" && message instanceof Error ? message : undefined;
    write({
      level,
      message: error === undefined ? String(message) : error.message,
      package: pkg,
      ...(error === undefined ? {} : { error }),
      attributes: isObject(attributes) ? ({ ...attributes } as Record<string, Json>) : {},
      time: Date.now(),
    });
  };
  return Object.freeze({
    error(message, attributes) {
      line("error", message, attributes);
    },
    warn(message, attributes) {
      line("warn", message, attributes);
    },
    info(message, attributes) {
      line("info", message, attributes);
    },
    debug(message, attributes) {
      line("debug", message, attributes);
    },
    trace(message, attributes) {
      line("trace", message, attributes);
    },
  } satisfies Log);
}
