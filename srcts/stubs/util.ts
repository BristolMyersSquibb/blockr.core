// What the stubs of the hosts share.

import type { BlockStatus } from "../scheduler/status.ts";
import type { BlockContext } from "../types/hosts.ts";

// The status of a block that R evaluated at its first revision, with no
// conditions.
export const READY: BlockStatus = Object.freeze({
  status: "ready",
  revision: 1,
  evaluated: 1,
  running: false,
  conditions: Object.freeze([]),
  omitted: Object.freeze({ warning: 0, message: 0 }),
  stdout: "",
  stderr: "",
  summary: null,
});

// A block's context, a ready block named "Block" with no linked inputs and no
// board options, but for what is given.
export function contextOf(given: Partial<BlockContext> = {}): BlockContext {
  return Object.freeze({
    name: given.name ?? "Block",
    status: given.status ?? READY,
    inputs: given.inputs ?? [],
    options: given.options ?? {},
  });
}

// The listeners of a stub's events. A listener that throws throws from what
// announced the event, so that a test sees it.
export function announcer<E extends string>(
  events: readonly E[],
): { listen: (event: E, fn: () => void) => () => void; announce: (event: E) => void } {
  const listeners = new Map<string, Set<() => void>>(events.map((event) => [event, new Set()]));
  return {
    listen: (event, fn) => {
      const heard = listeners.get(event);
      if (heard === undefined) {
        throw new Error(`The host announces ${events.map((name) => `"${name}"`).join(" and ")}, not ${JSON.stringify(event)}.`);
      }
      const wrapped = (): void => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    announce: (event) => {
      for (const fn of [...(listeners.get(event) ?? [])]) fn();
    },
  };
}
