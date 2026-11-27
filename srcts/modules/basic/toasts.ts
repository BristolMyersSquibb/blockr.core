// The toasts of core's basic frontend, as Core's basic frontend has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#cores-basic-frontend).
// Every notification of the board shows as a toast, whatever its severity and
// whoever raised it, but for those that concern a block or a plugin that the
// frontend shows in a section. Each block's card shows those about the block,
// and each plugin's section those about the plugin. So the toasts show those
// about the board, a link, a stack, a board option, and a plugin without a
// section, such as a storage plugin.
// The toasts are those of core's React SDK, React Aria's, in a queue of the
// opening (../../overlays/toast.tsx):
// - The toasts sit in the window's bottom right corner, one above the other,
//   in the order that their notifications came, the latest lowest. The first
//   line of each names what its notification concerns, with the package that
//   its source names.
// - A message's toast fades after five seconds, while a warning's or an
//   error's toast stays until the user closes it. React Aria pauses the five
//   seconds of every message's toast while the pointer rests on the toasts or
//   the keyboard's focus is in them.
// - A toast goes as soon as core withdraws its notification. A toast that
//   faded or that the user closed does not come back while its notification
//   holds. The frontend keeps the ids of those notifications for the opening,
//   and forgets each id once core withdraws its notification.
// - A dialog that the frontend opens, in the modal of core's React SDK, keeps
//   the focus inside it and makes the rest of the page inert, but for the
//   toasts, which React Aria leaves within reach. So while a dialog is open,
//   the user can still close them, and F6 moves the focus to them.
// - A message's toast is a status, and a warning's or an error's is an alert,
//   which a screen reader reads out at once.

import { ToastQueue } from "../../react/index.ts";
import type { Board, PluginClassId } from "../../types/board.ts";
import type { BoardObject, CoreApi, Notification } from "../../types/core-api.ts";
import { idText, internedRecord, own } from "../../util/index.ts";

// How long a message's toast shows before it goes, in milliseconds, its fade
// included.
export const SHOWN_FOR = 5000;

// The toasts of one opening.
export interface Toasts {
  // The queue of the toasts, which the SDK's ToastRegion draws, each holding
  // its notification.
  readonly queue: ToastQueue<Notification>;
  // Stops following the notifications.
  readonly stop: () => void;
}

// The toasts of the notifications, but for those about a block and those
// about a plugin for which `sectioned` holds, whose section shows them.
export function createToasts(
  notifications: Pick<CoreApi["notifications"], "get" | "subscribe">,
  sectioned: (plugin: PluginClassId) => boolean,
): Toasts {
  const queue = new ToastQueue<Notification>();
  // The key of the toast of each notification that shows, by the
  // notification's id.
  const keys = new Map<string, string>();
  // The ids of the notifications whose toasts faded or that the user closed,
  // while the notifications hold.
  const gone = new Set<string>();

  function update(heard: readonly Notification[]): void {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id, key] of keys) {
      if (holding.has(id)) continue;
      keys.delete(id);
      queue.close(key);
    }
    for (const id of gone) {
      if (!holding.has(id)) gone.delete(id);
    }
    for (const notification of heard) {
      const { id } = notification;
      if (shownElsewhere(notification, sectioned) || gone.has(id) || keys.has(id)) continue;
      const key = queue.add(notification, {
        timeout: notification.severity === "message" ? SHOWN_FOR : undefined,
        // A toast that faded or that the user closed, rather than one whose
        // notification core withdrew, which no longer has its key.
        onClose: () => {
          if (keys.get(id) !== key) return;
          keys.delete(id);
          gone.add(id);
        },
      });
      keys.set(id, key);
    }
  }

  const off = notifications.subscribe(update);
  update(notifications.get());

  return {
    queue,
    stop() {
      off();
    },
  };
}

// Whether the frontend shows the notification elsewhere than in a toast: it
// concerns a block, whose card shows it, or a plugin with a section, which
// shows it.
function shownElsewhere(notification: Notification, sectioned: (plugin: PluginClassId) => boolean): boolean {
  const { about } = notification;
  if (about === undefined) return false;
  return "block" in about || ("plugin" in about && sectioned(about.plugin));
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
  if ("plugin" in about) return `Plugin "${core.plugins.appPlugins().get(internedRecord(about.plugin))?.name ?? idText(about.plugin)}"`;
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
