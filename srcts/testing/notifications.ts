// Reads the notifications of a board in a test, by what each is of and
// without the ids that core gives them.

import type { CoreApi, Notification } from "../types/core-api.ts";

// What a notification is of: the first class of R's condition, or the code of
// core's or of a package's.
export function codeOf(notification: Pick<Notification, "source">): string {
  return notification.source.kind === "r" ? (notification.source.class[0] ?? "") : notification.source.code;
}

// The notifications that hold, without their ids, of those that `keep` keeps.
export function held(core: CoreApi, keep: (notification: Notification) => boolean = () => true): Omit<Notification, "id">[] {
  return core.notifications
    .get()
    .filter(keep)
    .map(({ severity, message, source, about }) => ({ severity, message, source, ...(about === undefined ? {} : { about }) }));
}

// Whether a notification concerns the block.
export function concerning(block: string): (notification: Notification) => boolean {
  return (notification) => notification.about !== undefined && "block" in notification.about && notification.about.block === block;
}
