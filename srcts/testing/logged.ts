// Captures what core and the modules that it mounts log, at every level, in
// place of the page's logger, until restore() runs.

import { replaceLogger } from "../log/index.ts";
import type { LogLine } from "../types/log.ts";

export interface Logged {
  // Every line, in the order they came.
  lines(): readonly LogLine[];
  // The lines at error, in the order they came.
  errors(): readonly LogLine[];
  restore(): void;
}

export function logged(): Logged {
  const lines: LogLine[] = [];
  const restore = replaceLogger((line) => {
    lines.push(line);
  });
  return {
    lines: () => lines,
    errors: () => lines.filter((line) => line.level === "error"),
    restore,
  };
}
