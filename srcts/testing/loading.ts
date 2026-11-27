// Waiting in a test for what core draws once it has loaded the code that it
// imports as it first draws, such as the form of an advanced action of core's
// board actions, core's dialog, or the UI of one of core's board options.
// The first such import in a test file loads the code from its source, with
// blockr.ui's components and React Aria where the code draws with them. That
// takes some seconds, where vi.waitFor() waits one second by default.
// The wait runs inside React's act(), so that what React draws once the code
// has loaded is drawn when the wait ends.

import { act } from "react";
import { vi } from "vitest";

// How long a test waits for such code at most, in milliseconds.
const LOADING = 10_000;

// Waits until `check` returns rather than throws, and returns what it returns.
export async function waitForLoaded<T>(check: () => T): Promise<T> {
  return await act(() => vi.waitFor(check, { timeout: LOADING }));
}
