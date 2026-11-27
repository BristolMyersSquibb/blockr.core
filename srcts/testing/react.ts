// Mounting a module that draws with React in a test, through React's act(),
// which runs what React scheduled before the test reads the page.
// What acts on the page and draws a component, step() and draw(), comes from
// `@blockr/ui/stubs`, with the helpers that act on blockr.ui's components.

import { act } from "react";
import { mount } from "../stubs/index.ts";
import type { Mounted } from "../stubs/index.ts";

// React warns where an update in a test runs outside act().
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

// React's act() resolves with what the mount resolved with, but its then()
// chains nothing, so this awaits it and hands back a promise of its own.
export async function mountReact(exports: unknown, host: unknown, el?: HTMLElement): Promise<Mounted> {
  return await act(() => mount(exports, host, el));
}
