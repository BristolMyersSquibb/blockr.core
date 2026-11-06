// What the basic front-end tells the user above the board, until the user
// dismisses it: what the store dropped as the board opened, and each edit that
// the store or the command registry refused.

import type { Dropped } from "../../store/index.ts";
import { messageOf } from "../../util/index.ts";

export interface Notice {
  readonly id: number;
  // Either what the store dropped, which the user can install, or an edit
  // that was refused.
  readonly kind: "dropped" | "refused";
  readonly text: string;
}

// The notices that one drawing of the board shows, which components follow as
// React's useSyncExternalStore follows a store, which takes `list` and
// `subscribe` as they are.
export interface Notices {
  // The notices, the same array until one comes or goes.
  readonly list: () => readonly Notice[];
  readonly dropped: (item: Dropped) => void;
  readonly refused: (error: unknown) => void;
  readonly dismiss: (id: number) => void;
  readonly subscribe: (fn: () => void) => () => void;
}

export function createNotices(): Notices {
  let notices: readonly Notice[] = [];
  let last = 0;
  const listeners = new Set<() => void>();

  function set(next: readonly Notice[]): void {
    notices = next;
    for (const fn of [...listeners]) fn();
  }

  function add(kind: Notice["kind"], text: string): void {
    set([...notices, { id: ++last, kind, text }]);
  }

  return {
    list: () => notices,
    dropped(item) {
      add("dropped", droppedText(item));
    },
    refused(error) {
      add("refused", messageOf(error));
    },
    dismiss(id) {
      if (notices.some((notice) => notice.id === id)) set(notices.filter((notice) => notice.id !== id));
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
  };
}

// What the store dropped, in a sentence that names what to install, as the
// store names it: a block by its id, since the board no longer holds its
// name, and a package by the version that the board's file records.
export function droppedText(item: Dropped): string {
  if ("block" in item) {
    const install = item.package === null ? "" : ` Install ${item.package} to open it.`;
    return `The board dropped the block "${item.block}", since the app offers no block class ${item.registry_id}.${install}`;
  }
  return `The board was saved with ${item.package} ${item.version}, which the app does not load, so the board dropped what it held of that package. Install ${item.package} to open it in full.`;
}
