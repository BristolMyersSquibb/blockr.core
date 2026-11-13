// What the basic frontend tells the user above the board, in the order it
// came, each until it no longer holds or the user dismisses it: the
// notifications of the board that concern no block and those that the code of
// packages raises, and what failed of the frontend's own work, such as an edit
// that the store or the command registry refused. Of the notifications that R
// and core raise about a block, a block's conditions show on its card, the
// failures of its output and its UI in their elements, and those of its
// requests through the module that made them.

import { useSyncExternalStore } from "react";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import { errorMessage } from "../../util/index.ts";

export type Severity = Notification["severity"];

export interface Notice {
  // The same while the notice shows.
  readonly key: string;
  readonly text: string;
  readonly severity: Severity;
  // The notification that it shows, by whose source the list offers what the
  // user can do, such as reloading the page, or none for a failure of the
  // frontend's own.
  readonly notification?: Notification;
}

// The notices that one drawing of the board shows, which components follow as
// React's useSyncExternalStore follows a store, which takes `list` and
// `subscribe` as they are.
export interface Notices {
  // The notices, the same array until one comes or goes.
  readonly list: () => readonly Notice[];
  // Tells the user why something failed, as an error.
  readonly failed: (error: unknown) => void;
  // Hides the notice, which for a notification lasts while it holds.
  readonly dismiss: (key: string) => void;
  readonly subscribe: (fn: () => void) => () => void;
  // Stops following the notifications.
  readonly stop: () => void;
}

// Whether the list shows the notification: one that concerns no block, or that
// the code of a package raised, which nothing else on the page shows.
function listed(notification: Notification): boolean {
  const { about, source } = notification;
  return about === undefined || !("block" in about) || source.kind === "module";
}

// Whether the notification is core's that asks the user to reload the page,
// for which the list offers a button that reloads it.
export function isReload(notification: Notification): boolean {
  return notification.source.kind === "core" && notification.source.code === "page_reload_needed";
}

export function createNotices(notifications: CoreApi["notifications"]): Notices {
  let notices: readonly Notice[] = [];
  // The failures of the frontend's own that show.
  let own: readonly Notice[] = [];
  // The notice of each notification that has shown, while it holds, by the
  // notification's id, with the order in which each notice came, by its key.
  const made = new Map<string, Notice>();
  const order = new Map<string, number>();
  // The notifications that the user dismissed, by id, while they hold.
  const dismissed = new Set<string>();
  let last = 0;
  let heard = notifications.get();
  const listeners = new Set<() => void>();

  function update(): void {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id, notice] of made) {
      if (holding.has(id)) continue;
      made.delete(id);
      order.delete(notice.key);
      dismissed.delete(id);
    }
    const shown: Notice[] = [];
    for (const notification of heard) {
      if (!listed(notification) || dismissed.has(notification.id)) continue;
      let notice = made.get(notification.id);
      if (notice?.notification !== notification) {
        notice = {
          key: notice?.key ?? `notification-${String(++last)}`,
          text: notification.message,
          severity: notification.severity,
          notification,
        };
        made.set(notification.id, notice);
        if (!order.has(notice.key)) order.set(notice.key, last);
      }
      shown.push(notice);
    }
    const at = (notice: Notice): number => order.get(notice.key) ?? 0;
    const next = [...shown, ...own].sort((a, b) => at(a) - at(b));
    if (next.length === notices.length && next.every((notice, i) => notice === notices[i])) return;
    notices = next;
    for (const fn of [...listeners]) fn();
  }

  const off = notifications.subscribe((all) => {
    heard = all;
    update();
  });
  update();

  return {
    list: () => notices,
    failed(error) {
      const key = `own-${String(++last)}`;
      order.set(key, last);
      own = [...own, { key, text: errorMessage(error), severity: "error" }];
      update();
    },
    dismiss(key) {
      const mine = own.find((notice) => notice.key === key);
      if (mine !== undefined) {
        own = own.filter((notice) => notice !== mine);
        order.delete(key);
      } else {
        for (const [id, notice] of made) {
          if (notice.key === key) dismissed.add(id);
        }
      }
      update();
    },
    subscribe(fn) {
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    stop: off,
  };
}

// The notices, which a component follows as React's useSyncExternalStore
// follows a store.
export function useNotices(notices: Notices): readonly Notice[] {
  return useSyncExternalStore(notices.subscribe, notices.list);
}
