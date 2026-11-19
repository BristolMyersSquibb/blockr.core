// Reads the notifications of a board in a test, by what each is of and
// without the ids that core gives them, holds notifications that a test sets,
// and records what a request hands on of each of R's replies.

import type { CoreApi, Notification } from "../types/core-api.ts";
import type { Replied } from "../util/index.ts";

// What a notification is of: the first class of R's condition, or the code of
// core's or of a package's.
export function notificationCode(notification: Pick<Notification, "source">): string {
  return notification.source.kind === "r" ? (notification.source.class[0] ?? "") : notification.source.code;
}

// The notifications that hold, without their ids, of those that `keep` keeps.
export function held(core: CoreApi, keep: (notification: Notification) => boolean = () => true): Omit<Notification, "id">[] {
  return core.notifications
    .get()
    .filter(keep)
    .map(({ severity, message, source, about }) => ({ severity, message, source, ...(about === undefined ? {} : { about }) }));
}

// Notifications that hold what the test sets, the same array until it sets
// another, as CoreApi's are.
export function holding(initial: readonly Notification[] = []) {
  let all = initial;
  const listeners = new Set<(all: readonly Notification[]) => void>();
  return {
    notifications: {
      get: () => all,
      subscribe(fn: (all: readonly Notification[]) => void) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    } satisfies Pick<CoreApi["notifications"], "get" | "subscribe">,
    set(next: readonly Notification[]): void {
      all = next;
      for (const fn of [...listeners]) fn(all);
    },
    listening: () => listeners.size,
  };
}

// Whether a notification concerns the block.
export function concerning(block: string): (notification: Notification) => boolean {
  return (notification) => notification.about !== undefined && "block" in notification.about && notification.about.block === block;
}

// What a request handed on of one of R's replies, as replied() got it, with
// the notifier only where the request's caller passed one.
export function handedOn(...args: Parameters<Replied>): Parameters<Replied> {
  const [module, request, conditions, notifier] = args;
  return notifier === undefined ? [module, request, conditions] : [module, request, conditions, notifier];
}
