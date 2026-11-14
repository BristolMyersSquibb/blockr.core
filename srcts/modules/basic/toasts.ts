// The toasts of core's basic frontend, as Core's basic frontend has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#cores-basic-frontend).
// Every notification of the board that concerns no block shows as a toast,
// whatever its severity and whoever raised it. Each block's card shows those
// about the block.
// - The toasts sit in the window's bottom right corner, one above the other,
//   in the order that their notifications came. The first line of each names
//   what its notification concerns, with the package that its source names.
// - A message's toast fades after five seconds, but not while the pointer
//   rests on it or the keyboard's focus is in it. Its five seconds start again
//   once neither is. A warning's or an error's toast stays until the user
//   closes it.
// - A toast goes as soon as core withdraws its notification. A toast that
//   faded or that the user closed does not come back while its notification
//   holds. The frontend keeps the ids of those notifications for the opening,
//   and forgets each id once core withdraws its notification.
// - A dialog that the frontend opens with showModal() sits in the top layer,
//   and the rest of the page is inert. So while a dialog is open, the toasts
//   show inside it, where the user can still close them.
// - A message's toast is a status, and a warning's or an error's is an alert,
//   which a screen reader reads out at once.

import { useSyncExternalStore } from "react";
import type { Board } from "../../types/board.ts";
import type { BoardObject, CoreApi, Notification } from "../../types/core-api.ts";
import { own } from "../../util/index.ts";

// How long a message's toast shows before it fades, and how long it takes to
// fade, in milliseconds.
export const SHOWN_FOR = 5000;
export const FADES_FOR = 300;

export interface Toast {
  readonly notification: Notification;
  // Whether it is fading, once a message's five seconds are up.
  readonly fading: boolean;
}

// The toasts of one opening, which components follow as React's
// useSyncExternalStore follows a store, which takes `list`, `shownIn` and
// `subscribe` as they are.
export interface Toasts {
  // The toasts that show, the same array until one comes, goes or starts to
  // fade.
  readonly list: () => readonly Toast[];
  // The element inside the open dialog in which the toasts show, or null
  // while no dialog is open.
  readonly shownIn: () => HTMLElement | null;
  readonly subscribe: (fn: () => void) => () => void;
  // Shows the toasts in the element, inside a dialog that is open, until the
  // function that it returns runs.
  readonly showIn: (el: HTMLElement) => () => void;
  // Hides the toast of the notification, as the user closes it.
  readonly close: (id: string) => void;
  // Keeps the toast of a message from fading, as the pointer rests on it or
  // the keyboard's focus moves into it.
  readonly keep: (id: string, by: "pointer" | "focus") => void;
  // Lets the toast fade again, as the pointer or the focus leaves it. Once
  // neither keeps it, its five seconds start again.
  readonly release: (id: string, by: "pointer" | "focus") => void;
  // Stops following the notifications, and stops every toast's timer.
  readonly stop: () => void;
}

// A toast that shows, with the timer of a message's toast, which counts down
// its five seconds or its fade, and what keeps it from fading.
interface Shown {
  toast: Toast;
  timer: ReturnType<typeof setTimeout> | undefined;
  readonly kept: Set<"pointer" | "focus">;
}

export function createToasts(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">): Toasts {
  // The toasts that show, by the ids of their notifications.
  const shown = new Map<string, Shown>();
  // The ids of the notifications whose toasts faded or that the user closed,
  // while the notifications hold.
  const gone = new Set<string>();
  // The elements that the toasts show in, inside the dialogs that are open,
  // the last opened last.
  const inside: HTMLElement[] = [];
  const listeners = new Set<() => void>();
  let heard = notifications.get();
  let list: readonly Toast[] = [];
  let stopped = false;

  function publish(): void {
    const next = heard.flatMap((notification) => {
      const toast = shown.get(notification.id)?.toast;
      return toast === undefined ? [] : [toast];
    });
    if (next.length !== list.length || next.some((toast, i) => toast !== list[i])) list = next;
    for (const fn of [...listeners]) fn();
  }

  // Starts the five seconds of a message's toast, unless something keeps it.
  function countDown(id: string, toast: Shown): void {
    if (stopped || toast.toast.notification.severity !== "message" || toast.kept.size > 0) return;
    clearTimeout(toast.timer);
    toast.timer = setTimeout(() => {
      toast.toast = { ...toast.toast, fading: true };
      toast.timer = setTimeout(() => {
        hide(id);
      }, FADES_FOR);
      publish();
    }, SHOWN_FOR);
  }

  // Hides the toast, which faded or which the user closed, while its
  // notification holds.
  function hide(id: string): void {
    const toast = shown.get(id);
    if (toast === undefined) return;
    clearTimeout(toast.timer);
    shown.delete(id);
    gone.add(id);
    publish();
  }

  function update(): void {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id, toast] of shown) {
      if (holding.has(id)) continue;
      clearTimeout(toast.timer);
      shown.delete(id);
    }
    for (const id of gone) {
      if (!holding.has(id)) gone.delete(id);
    }
    for (const notification of heard) {
      if (aboutBlock(notification) || gone.has(notification.id)) continue;
      const toast = shown.get(notification.id);
      if (toast === undefined) {
        const added: Shown = { toast: { notification, fading: false }, timer: undefined, kept: new Set() };
        shown.set(notification.id, added);
        countDown(notification.id, added);
      } else if (toast.toast.notification !== notification) {
        toast.toast = { ...toast.toast, notification };
      }
    }
    publish();
  }

  const off = notifications.subscribe((all) => {
    heard = all;
    update();
  });
  update();

  return {
    list: () => list,
    shownIn: () => inside.at(-1) ?? null,
    subscribe(fn) {
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    showIn(el) {
      inside.push(el);
      publish();
      let back = false;
      return () => {
        if (back) return;
        back = true;
        const at = inside.lastIndexOf(el);
        if (at >= 0) inside.splice(at, 1);
        publish();
      };
    },
    close: hide,
    keep(id, by) {
      const toast = shown.get(id);
      if (toast === undefined) return;
      toast.kept.add(by);
      clearTimeout(toast.timer);
      toast.timer = undefined;
      if (!toast.toast.fading) return;
      toast.toast = { ...toast.toast, fading: false };
      publish();
    },
    release(id, by) {
      const toast = shown.get(id);
      if (toast?.kept.delete(by) !== true) return;
      countDown(id, toast);
    },
    stop() {
      stopped = true;
      off();
      for (const toast of shown.values()) clearTimeout(toast.timer);
    },
  };
}

// Whether the notification concerns a block, whose card shows it.
function aboutBlock(notification: Notification): boolean {
  const { about } = notification;
  return about !== undefined && "block" in about;
}

// The toasts, which a component follows as React's useSyncExternalStore
// follows a store.
export function useToasts(toasts: Toasts): readonly Toast[] {
  return useSyncExternalStore(toasts.subscribe, toasts.list);
}

// The element inside the open dialog in which the toasts show, or null while
// no dialog is open.
export function useShownIn(toasts: Toasts): HTMLElement | null {
  return useSyncExternalStore(toasts.subscribe, toasts.shownIn);
}

// What the first line of a toast names: the board object that its
// notification concerns, by the name that the page shows for it, or the board
// for none. A board object that core no longer knows goes by its id.
export function concerned(
  about: BoardObject | undefined,
  core: { readonly options: Pick<CoreApi["options"], "registered">; readonly plugins: Pick<CoreApi["plugins"], "appPlugins"> },
  board: Board,
): string {
  if (about === undefined) return "Board";
  if ("option" in about) return `Board option "${own(core.options.registered(), about.option)?.label ?? about.option}"`;
  if ("plugin" in about) return `Plugin "${own(core.plugins.appPlugins(), about.plugin)?.name ?? about.plugin}"`;
  if ("stack" in about) return `Stack "${own(board.stacks, about.stack)?.name ?? about.stack}"`;
  const block = (id: string): string => `"${own(board.blocks, id)?.name ?? id}"`;
  if ("block" in about) return `Block ${block(about.block)}`;
  const link = own(board.links, about.link);
  return link === undefined ? `Link "${about.link}"` : `Link from ${block(link.from)} to ${block(link.to)}`;
}

// The package that the notification's source names, if any: the package of
// the module behind it, which made the request that R replied to, whose code
// failed, or which raised the notification.
export function sourcePackage(notification: Notification): string | undefined {
  return notification.source.module?.package;
}

// Whether the notification is core's request to reload the page, whose toast
// offers a button that reloads it.
export function isReload(notification: Notification): boolean {
  return notification.source.kind === "core" && notification.source.code === "page_reload_needed";
}
