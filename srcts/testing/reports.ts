// Captures what core reports rather than throws, such as what a caller's
// function threw, by holding what it queues as a microtask.

import { vi } from "vitest";

export function reports(): { errors: () => unknown[]; restore: () => void } {
  const queued: (() => void)[] = [];
  const spy = vi.spyOn(globalThis, "queueMicrotask").mockImplementation((fn) => {
    queued.push(fn);
  });
  return {
    errors: () =>
      queued.map((fn) => {
        try {
          fn();
          return undefined;
        } catch (error) {
          return error;
        }
      }),
    restore: () => {
      spy.mockRestore();
    },
  };
}
