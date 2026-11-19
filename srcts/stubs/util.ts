// What the stubs of the hosts share.

import { writingLog } from "../log/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { OptionId } from "../types/board.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
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
  unsent: null,
});

// What a test gives a stub's block context: the block's name, status and
// linked inputs, and in `options` the values of the board options that the
// block's class reads, by id.
export interface StubContext {
  readonly name?: string;
  readonly status?: BlockStatus;
  readonly inputs?: BlockContext["inputs"];
  readonly options?: Readonly<Record<OptionId, Json>>;
}

// A block's context, a ready block named "Block" with no linked inputs and no
// board options, but for what is given.
// Its option() gives the value of each board option that `options` holds, and
// throws for any other id, as a block's host throws for a board option that
// the block's class does not read.
export function stubBlockContext(given: StubContext = {}): BlockContext {
  const values = given.options ?? {};
  return Object.freeze({
    name: given.name ?? "Block",
    status: given.status ?? READY,
    inputs: given.inputs ?? [],
    option: (id: OptionId): Json => optionValue(values, id, "context"),
  });
}

// The value of a board option that a stub host holds among `values`. It throws
// for any other id, naming `where` in the stub, where a test gives the values.
export function optionValue(values: Readonly<Record<OptionId, Json>>, id: OptionId, where: string): Json {
  if (!Object.hasOwn(values, id)) {
    throw new Error(`The stub's ${where} holds no board option "${id}", whose value a test gives in its \`options\`.`);
  }
  return values[id] as Json;
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
export function raisedHolder(): { notified: readonly Raised[]; notify: (notification: Raised) => () => void } {
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
