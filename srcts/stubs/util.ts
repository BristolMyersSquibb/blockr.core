// What the stubs of the hosts share.

import { writingLog } from "../log/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext } from "../types/hosts.ts";
import type { Log, LogLine } from "../types/log.ts";
import { checkRaised } from "../util/index.ts";

// The status of a block that R evaluated at its first revision, with no
// conditions.
export const READY: BlockStatus = Object.freeze({
  status: "ready",
  revision: 1,
  evaluated: 1,
  running: false,
  conditions: Object.freeze([]),
  stdout: "",
  stderr: "",
  summary: null,
  reason: null,
});

// A block's context, a ready block named "Block" with no linked inputs and no
// board options, but for what is given.
export function stubBlockContext(given: Partial<BlockContext> = {}): BlockContext {
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

// The notifications that a module raised through a stub's notify(), each
// while it holds, in the order they came, checked as core checks them.
export function notifier(): { notified: readonly Raised[]; notify: (notification: Raised) => () => void } {
  const notified: Raised[] = [];
  return {
    notified,
    notify(notification) {
      const given = checkRaised(notification);
      notified.push(given);
      let withdrawn = false;
      return () => {
        if (withdrawn) return;
        withdrawn = true;
        notified.splice(notified.indexOf(given), 1);
      };
    },
  };
}

// The lines that a module logged through a stub's log, at every level, in the
// order they came, each naming the package given, or none.
export function lineHolder(pkg: string | null = null): { logged: readonly LogLine[]; log: Log } {
  const logged: LogLine[] = [];
  return {
    logged,
    log: writingLog(pkg, (line) => {
      logged.push(line);
    }),
  };
}
