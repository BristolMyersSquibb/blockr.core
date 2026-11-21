// Mounting a module that draws with React in a test, drawing a component on
// the page, and acting on either, through React's act(), which runs what
// React scheduled before the test reads the page.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { mount } from "../stubs/index.ts";
import type { Mounted } from "../stubs/index.ts";

// React warns where an update in a test runs outside act().
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

// React's act() resolves with what the mount resolved with, but its then()
// chains nothing, so this awaits it and hands back a promise of its own.
export async function mountReact(exports: unknown, host: unknown, el?: HTMLElement): Promise<Mounted> {
  return await act(() => mount(exports, host, el));
}

// Does what the test does to the page or the host, and then what React
// scheduled in turn.
export async function step(fn: () => void): Promise<void> {
  await act(() => {
    fn();
    return Promise.resolve();
  });
}

// Draws the component in an element of its own on the page, so that its parts
// can take the focus, and gives what draws it again with other props and what
// unmounts it.
export function draw(node: ReactElement): { el: HTMLElement; redraw: (next: ReactElement) => void; unmount: () => void } {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  const render = (next: ReactElement): void => {
    act(() => {
      root.render(next);
    });
  };
  render(node);
  return {
    el,
    redraw: render,
    unmount: () => {
      act(() => {
        root.unmount();
      });
      el.remove();
    },
  };
}
