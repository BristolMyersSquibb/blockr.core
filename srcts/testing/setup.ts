// What runs around every test, as vitest.config.ts names this file.
// Each test logs into a logger of its own, which fails the test where it
// logged an error that it did not capture with logged() (./logged.ts), so that
// no failure that core catches goes unseen, and nothing reaches the console.
// After each test, core forgets what it set up for the page's logging, so that
// the next test starts as a page does, and the API records no spans, whatever a
// test set up (./spans.ts).
// After each test, this file also unmounts every React root that the test
// made, and then empties the page.
// React runs the cleanups of the effects as a root unmounts.
// These clear the timers that React Aria set, such as the timer of a dialog.
// Left pending, such a timer can run after Vitest has torn down the page, and
// its error fails the run.
// Without the unmount, a tooltip that a test left open would close as one of
// a later test opens.
// A mock of react-dom/client records each root that createRoot() makes, so
// that the roots that core's own code makes unmount too.
// A test file's afterEach() runs before this file's, so a test file must not
// empty the page.
// Once the page is empty, a root whose overlay is still open fails to unmount.
// It makes happy-dom do as a browser does where core's basic frontend needs
// it, as the stubs do for the tests of packages (./dom.ts).
// What blockr.ui's components need of happy-dom, `@blockr/ui/stubs` does as a
// test file loads it, or core's stubs, which load it.
// It loads neither here, since both load React DOM, which reads the hook of
// React's developer tools as it loads, and a test sets the hook first to
// count React's commits.

import "./dom.ts";
import { act } from "react";
import type * as Client from "react-dom/client";
import { afterEach, beforeEach, vi } from "vitest";
import { replaceLogger, resetLogging } from "../log/index.ts";
import type { LogLine } from "../types/log.ts";
import { stopRecording } from "./spans.ts";

// The roots that createRoot() made during the test.
// Vitest moves vi.mock() above the imports.
// Vitest moves vi.hoisted() above vi.mock() as well, so that the mock can reach
// the set.
const roots = vi.hoisted(() => new Set<Client.Root>());

vi.mock("react-dom/client", async (importOriginal) => {
  const client = await importOriginal<typeof Client>();
  return {
    ...client,
    createRoot: (...args: Parameters<typeof client.createRoot>): Client.Root => {
      const root = client.createRoot(...args);
      roots.add(root);
      return root;
    },
  };
});

let uncaptured: LogLine[] = [];
let restore: (() => void) | undefined;

beforeEach(() => {
  uncaptured = [];
  restore = replaceLogger((line) => {
    if (line.level === "error") uncaptured.push(line);
  });
});

afterEach(() => {
  // The roots unmount while the test's logger holds, so that an error logged
  // as a root unmounts fails the test.
  // Unmounting a root that the test unmounted already does nothing.
  // The rest runs even where a root fails to unmount, so that the next test
  // starts afresh.
  try {
    act(() => {
      for (const root of roots) root.unmount();
    });
  } finally {
    roots.clear();
    if (typeof document !== "undefined") document.body.replaceChildren();
    restore?.();
    restore = undefined;
    resetLogging();
    stopRecording();
  }
  const errors = uncaptured;
  uncaptured = [];
  if (errors.length > 0) {
    throw new Error(`The test logged errors that it did not capture with logged():\n${errors.map((line) => line.message).join("\n")}`);
  }
});
