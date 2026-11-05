// Mounting a module that draws with React in a test, and acting on it, through
// React's act(), which runs what React scheduled before the test reads the
// page.

import { act } from "react";
import { mount } from "../stubs/index.ts";
import type { Mounted } from "../stubs/index.ts";

// React warns where an update in a test runs outside act().
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

export function mountReact(exports: unknown, host: unknown, el?: HTMLElement): Promise<Mounted> {
  return act(() => mount(exports, host, el));
}

// Does what the test does to the page or the host, and then what React
// scheduled in turn.
export async function step(fn: () => void): Promise<void> {
  await act(() => {
    fn();
    return Promise.resolve();
  });
}
