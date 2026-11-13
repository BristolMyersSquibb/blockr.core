// What raises the notifications of the open board, as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications),
// which the code that core runs for the board calls (../core/notifications.ts):
// core's own notifications, where it fails at something that the user asked
// for or that the board needs, the conditions of R's replies to requests, and
// what the code of packages raises.

import type { Condition } from "../protocol/index.ts";
import type { CoreCode, Notification, Owner, Part, Raised, RRequest } from "../types/core-api.ts";
import { isObject } from "./util.ts";

// Raises a notification of the open board, and returns what withdraws it.
export type Raise = (notification: Omit<Notification, "id">) => () => void;

// Holds a notification for each condition of R's latest reply to a request,
// which concern the part that the request was made for, in place of those of
// the request's reply before, so that a request that fails again keeps its
// notifications and one that succeeds, with no conditions, withdraws them.
export type Replied = (about: Part, request: Extract<RRequest, { readonly name: string }>, conditions: readonly Condition[]) => void;

// Raises a notification that a package's code gives, from its owner and with
// its package, null where core knows none, and returns what withdraws it.
export type Notify = (owner: Owner, pkg: string | null, notification: Raised) => () => void;

// Raise nothing, for code that runs outside an opening, as in a test.
export const raiseNothing: Raise = () => () => undefined;
export const repliedNothing: Replied = () => undefined;
export const notifyNothing: Notify = () => () => undefined;

// What one of core's notifications concerns, none for the board, and the
// package whose code failed, or null where an id names none, as for a block
// class defined outside a package.
export interface Concerns {
  readonly about?: Part;
  readonly package?: string | null;
}

// One of core's notifications, of `code`.
export function coreNotification(
  code: CoreCode,
  severity: "warning" | "error",
  message: string,
  concerns: Concerns = {},
): Omit<Notification, "id"> {
  const pkg = concerns.package;
  return {
    severity,
    message,
    source: { kind: "core", code, ...(pkg === undefined || pkg === null ? {} : { package: pkg }) },
    ...(concerns.about === undefined ? {} : { about: concerns.about }),
  };
}

const SEVERITIES: readonly unknown[] = ["message", "warning", "error"];

// What a part of the board can be.
const PARTS = ["block", "link", "stack", "option", "plugin"] as const;

// Whether the value names one part of the board, by its id.
export function isPart(value: unknown): value is Part {
  if (!isObject(value)) return false;
  const keys = Object.keys(value);
  const [kind] = keys;
  if (keys.length !== 1 || kind === undefined || !(PARTS as readonly string[]).includes(kind)) return false;
  const id = value[kind];
  return typeof id === "string" && id !== "";
}

// A notification that a package's code gives, checked, since code in plain JS
// may give members of the wrong kind, which the types refuse.
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
  if (about !== undefined && about !== null && !isPart(about)) {
    throw new TypeError(
      `A notification concerns one part of the board, as { block: "a" }, of a block, a link, a stack, a board option or a plugin, or null for the board, not ${JSON.stringify(about)}.`,
    );
  }
  return notification;
}
