// What raises the notifications of the open board, as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications),
// which the code that core runs for the board calls (../core/notifications.ts):
// core's own notifications, where it fails at something that the user asked
// for or that the board needs, the conditions of R's replies to requests, and
// what the modules of packages raise.

import type { Condition } from "../protocol/index.ts";
import type { BoardObject, CoreCode, Notification, Raised, RRequest, SourceModule } from "../types/core-api.ts";
import { isObject } from "./util.ts";

// Raises a notification of the open board, and returns what withdraws it.
export type Raise = (notification: Omit<Notification, "id">) => () => void;

// Holds a notification for each condition of R's latest reply to a request
// that a package's module made, in place of those of the module's reply
// before to a request of the same name, so that a request that fails again
// keeps its notifications and one that succeeds with no condition withdraws
// them. They concern the board object that the module belongs to, for which
// the request was made.
export type Replied = (
  module: SourceModule,
  request: Extract<RRequest, { readonly name: string }>,
  conditions: readonly Condition[],
) => void;

// Raises a notification that a package's module gives, from the module, and
// returns what withdraws the notification.
export type Notify = (module: SourceModule, notification: Raised) => () => void;

// Raise nothing, for code that runs outside an opening, as in a test.
export const raiseNothing: Raise = () => () => undefined;
export const repliedNothing: Replied = () => undefined;
export const notifyNothing: Notify = () => () => undefined;

// A package's module, as a notification's source names it, from the board
// object whose module it is, its package and, where the module is a renderer,
// the renderer's id. A package of null or undefined, as of a block class
// defined outside a package, leaves the module without one.
export function sourceModule(
  object: SourceModule["object"],
  pkg: string | null | undefined,
  renderer?: string,
): SourceModule {
  return {
    object,
    ...(renderer === undefined ? {} : { renderer }),
    ...(pkg === undefined || pkg === null ? {} : { package: pkg }),
  };
}

// What one of core's notifications concerns, none for the board, and the
// module whose code failed, where a module's code failed. A notification with
// a module concerns the module's board object unless `about` names another.
export interface Concerns {
  readonly about?: BoardObject;
  readonly module?: SourceModule;
}

// One of core's notifications, of `code`.
export function coreNotification(
  code: CoreCode,
  severity: "warning" | "error",
  message: string,
  concerns: Concerns = {},
): Omit<Notification, "id"> {
  const { module } = concerns;
  const about = concerns.about ?? module?.object;
  return {
    severity,
    message,
    source: { kind: "core", code, ...(module === undefined ? {} : { module }) },
    ...(about === undefined ? {} : { about }),
  };
}

const SEVERITIES: readonly unknown[] = ["message", "warning", "error"];

// What a board object can be.
const KINDS = ["block", "link", "stack", "option", "plugin"] as const;

// Whether the value names one board object, by its id.
export function isBoardObject(value: unknown): value is BoardObject {
  if (!isObject(value)) return false;
  const keys = Object.keys(value);
  const [kind] = keys;
  if (keys.length !== 1 || kind === undefined || !(KINDS as readonly string[]).includes(kind)) return false;
  const id = value[kind];
  return typeof id === "string" && id !== "";
}

// A notification that a package's module gives, checked, since a module in
// plain JS may give members of the wrong kind, which the types refuse.
export function checkRaised(notification: Raised): Raised {
  const given: unknown = notification;
  if (!isObject(given)) {
    throw new TypeError(`A notification is an object, not ${JSON.stringify(given)}.`);
  }
  if (!SEVERITIES.includes(given.severity)) {
    throw new TypeError(
      `The severity of a notification is "message", "warning" or "error", not ${JSON.stringify(given.severity)}.`,
    );
  }
  if (typeof given.message !== "string") {
    throw new TypeError(`The message of a notification is a string, not ${JSON.stringify(given.message)}.`);
  }
  if (typeof given.code !== "string" || given.code === "") {
    throw new TypeError(
      `The code of a notification is a non-empty string, as "edit_refused", not ${JSON.stringify(given.code)}.`,
    );
  }
  const { about } = given;
  if (about !== undefined && about !== null && !isBoardObject(about)) {
    throw new TypeError(
      `A notification concerns one board object, as { block: "a" }: a block, a link, a stack, a board option or a plugin, or null for the board, not ${JSON.stringify(about)}.`,
    );
  }
  return notification;
}
