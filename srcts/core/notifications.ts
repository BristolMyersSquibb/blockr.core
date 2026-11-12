// The notifications of the open board, which CoreApi's `notifications` holds,
// as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
// Core makes one holder for each board that opens, with the board's store and
// before it imports the modules that R's reply names, so that a failure raised
// before the frontend draws still reaches it. It holds, in the order they came:
// - One notification for each condition of each block's status, which it
//   derives from the scheduler, ahead of any frontend, rather than copies. Its
//   id comes from the block, the severity, the class and the message, so that
//   the same condition in the block's next reply keeps its id, and two that
//   share all four are one. Its origin is "r", or "core" for the two
//   conditions that the scheduler writes itself.
// - Those that core raises, each with an id from what it holds, so that a
//   failure raised twice, as by two mounts of one module, is one notification,
//   which holds until each raise of it is withdrawn.
// - Those that plugins raise through notify(), each with an id of its own.
// It withdraws every notification that concerns a block as the block leaves
// the board, and every one as the board ends, as the next board opens, which
// it announces.

import { announceTo, onAbort } from "../mount/index.ts";
import type { Condition, Phase } from "../protocol/index.ts";
import { writtenByCore } from "../scheduler/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
import { isObject, jsonEqual } from "../util/index.ts";
import type { Raise } from "../util/index.ts";

export interface Holder {
  // CoreApi's `notifications`.
  readonly api: CoreApi["notifications"];
  // Raises one of core's own notifications, and returns what withdraws it.
  readonly raise: Raise;
  // Holds a notification for each condition of each block's status from now
  // on, as the scheduler announces each change of one.
  follow(scheduler: Pick<Scheduler, "status" | "subscribe">): void;
}

const SEVERITIES: readonly unknown[] = ["message", "warning", "error"];

// The phases of the protocol's conditions, which a notification may name.
const PHASES: Readonly<Record<Phase, true>> = {
  expr: true,
  inputs: true,
  block_eval: true,
  result_summary: true,
  output: true,
  request: true,
  status: true,
};

const NONE: readonly Notification[] = Object.freeze([]);

const nothing = (): void => undefined;

export function createNotifications(
  store: Pick<Store, "board" | "hasBoard" | "subscribe">,
  signal?: AbortSignal,
): Holder {
  // Every notification that holds, by id, in the order they came.
  const held = new Map<string, Notification>();
  // The raises that hold each of core's notifications, by its id.
  const raised = new Map<string, Set<object>>();
  // The ids of the notifications of each block's conditions.
  const ofStatus = new Map<BlockId, readonly string[]>();
  const listeners = new Set<(all: readonly Notification[]) => void>();
  let all = NONE;
  let plugins = 0;
  let ended = false;
  const stops: (() => void)[] = [];

  function publish(): void {
    all = Object.freeze([...held.values()]);
    announceTo(listeners, all);
  }

  function holds(block: BlockId): boolean {
    return store.hasBoard() && Object.hasOwn(store.board().blocks, block);
  }

  const offStore = store.subscribe((change) => {
    if ("opened" in change) return;
    const gone = change.delta.blocks?.rm ?? [];
    if (gone.length === 0) return;
    let moved = false;
    for (const [id, notification] of held) {
      if (notification.block === undefined || !gone.includes(notification.block)) continue;
      held.delete(id);
      raised.delete(id);
      moved = true;
    }
    for (const block of gone) ofStatus.delete(block);
    if (moved) publish();
  });
  stops.push(offStore);

  function end(): void {
    if (ended) return;
    ended = true;
    for (const stop of stops) stop();
    const had = held.size > 0;
    held.clear();
    raised.clear();
    ofStatus.clear();
    if (had) publish();
    listeners.clear();
  }
  if (signal?.aborted === true) end();
  else if (signal !== undefined) onAbort(signal, end);

  // Holds the notifications of the conditions of a block's status in place of
  // those it held, and says whether that changed what holds. Those that hold
  // still keep their place.
  function fromStatus(block: BlockId, status: BlockStatus | undefined): boolean {
    const next = new Map<string, Notification>();
    if (status !== undefined && holds(block)) {
      for (const condition of status.conditions) {
        const id = JSON.stringify(["status", block, condition.severity, condition.class, condition.message]);
        if (!next.has(id)) next.set(id, ofCondition(id, block, condition));
      }
    }
    let moved = false;
    for (const id of ofStatus.get(block) ?? []) {
      if (!next.has(id) && held.delete(id)) moved = true;
    }
    for (const [id, notification] of next) {
      const had = held.get(id);
      if (had !== undefined && jsonEqual(had, notification)) continue;
      held.set(id, notification);
      moved = true;
    }
    if (next.size === 0) ofStatus.delete(block);
    else ofStatus.set(block, [...next.keys()]);
    return moved;
  }

  const raise: Raise = (given) => {
    if (ended || (given.block !== undefined && !holds(given.block))) return nothing;
    const id = JSON.stringify([
      "core",
      given.severity,
      given.class,
      given.message,
      given.origin,
      given.phase ?? null,
      given.block ?? null,
      given.option ?? null,
      given.plugin ?? null,
      given.package ?? null,
    ]);
    const token = {};
    const tokens = raised.get(id) ?? new Set<object>();
    tokens.add(token);
    raised.set(id, tokens);
    if (!held.has(id)) {
      held.set(id, frozen(id, given));
      publish();
    }
    return () => {
      const holding = raised.get(id);
      if (holding?.delete(token) !== true || holding.size > 0) return;
      raised.delete(id);
      if (held.delete(id)) publish();
    };
  };

  return {
    api: {
      notify(notification) {
        const given = checked(notification);
        if (ended) return nothing;
        if (given.block !== undefined && !holds(given.block)) {
          throw new Error(`The board holds no block ${JSON.stringify(given.block)} for a notification to concern.`);
        }
        const id = `plugin-${String(++plugins)}`;
        held.set(id, frozen(id, given));
        publish();
        let withdrawn = false;
        return () => {
          if (withdrawn) return;
          withdrawn = true;
          if (held.delete(id)) publish();
        };
      },
      get: () => all,
      subscribe(fn) {
        const wrapped = (notifications: readonly Notification[]): void => {
          fn(notifications);
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
    },
    raise,
    follow(scheduler) {
      if (ended) return;
      const heard = (blocks: readonly BlockId[]): void => {
        let moved = false;
        for (const block of blocks) {
          if (fromStatus(block, scheduler.status(block))) moved = true;
        }
        if (moved) publish();
      };
      stops.push(scheduler.subscribe(heard));
      if (store.hasBoard()) heard(Object.keys(store.board().blocks));
    },
  };
}

// The notification of a condition of a block's status.
function ofCondition(id: string, block: BlockId, condition: Condition): Notification {
  return frozen(id, {
    severity: condition.severity,
    class: condition.class,
    message: condition.message,
    origin: writtenByCore(condition) ? "core" : "r",
    ...(condition.phase === undefined ? {} : { phase: condition.phase }),
    block,
  });
}

// The notification with its id, frozen with its class, holding what it was
// given of its members alone.
function frozen(id: string, given: Omit<Notification, "id">): Notification {
  return Object.freeze({
    id,
    severity: given.severity,
    class: Object.freeze([...given.class]),
    message: given.message,
    origin: given.origin,
    ...(given.phase === undefined ? {} : { phase: given.phase }),
    ...(given.block === undefined ? {} : { block: given.block }),
    ...(given.option === undefined ? {} : { option: given.option }),
    ...(given.plugin === undefined ? {} : { plugin: given.plugin }),
    ...(given.package === undefined ? {} : { package: given.package }),
  });
}

// A notification that a plugin raises, checked, since a plugin in plain JS may
// give members of the wrong kind, which the types refuse.
function checked(notification: Omit<Notification, "id">): Omit<Notification, "id"> {
  const given: unknown = notification;
  if (!isObject(given)) {
    throw new TypeError(`A notification is an object, not ${JSON.stringify(given)}.`);
  }
  if (!SEVERITIES.includes(given.severity)) {
    throw new TypeError(
      `The severity of a notification is "message", "warning" or "error", not ${JSON.stringify(given.severity)}.`,
    );
  }
  const { class: cls } = given;
  if (!Array.isArray(cls) || cls.length === 0 || !cls.every((item) => typeof item === "string" && item !== "")) {
    throw new TypeError(`The class of a notification is a non-empty array of strings, not ${JSON.stringify(cls)}.`);
  }
  if (typeof given.message !== "string") {
    throw new TypeError(`The message of a notification is a string, not ${JSON.stringify(given.message)}.`);
  }
  if (typeof given.origin !== "string" || given.origin === "") {
    throw new TypeError(
      `The origin of a notification is a non-empty string, as the id of the class of the plugin that raises it, not ${JSON.stringify(given.origin)}.`,
    );
  }
  const { phase } = given;
  if (phase !== undefined && !(typeof phase === "string" && Object.hasOwn(PHASES, phase))) {
    throw new TypeError(`The phase of a notification is one of the protocol's, not ${JSON.stringify(phase)}.`);
  }
  for (const member of ["block", "option", "plugin", "package"] as const) {
    const value = given[member];
    if (value !== undefined && (typeof value !== "string" || value === "")) {
      throw new TypeError(`The ${member} that a notification concerns is named by a non-empty string, not ${JSON.stringify(value)}.`);
    }
  }
  return notification;
}
