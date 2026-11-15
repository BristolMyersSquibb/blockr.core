// What runs around every test, as vitest.config.ts names this file.
// Each test logs into a logger of its own, which fails the test where it
// logged an error that it did not capture with logged() (./logged.ts), so that
// no failure that core catches goes unseen, and nothing reaches the console.
// After each test, core forgets what it set up for the page's logging, so that
// the next test starts as a page does, and the API records no spans, whatever a
// test set up (./spans.ts).

import { afterEach, beforeEach } from "vitest";
import { replaceLogger, resetLogging } from "../log/index.ts";
import type { LogLine } from "../types/log.ts";
import { stopRecording } from "./spans.ts";

let uncaptured: LogLine[] = [];
let restore: (() => void) | undefined;

beforeEach(() => {
  uncaptured = [];
  restore = replaceLogger((line) => {
    if (line.level === "error") uncaptured.push(line);
  });
});

afterEach(() => {
  restore?.();
  restore = undefined;
  resetLogging();
  stopRecording();
  const errors = uncaptured;
  uncaptured = [];
  if (errors.length > 0) {
    throw new Error(`The test logged errors that it did not capture with logged():\n${errors.map((line) => line.message).join("\n")}`);
  }
});
