// What the basic frontend tells the user above the board, until the user
// dismisses it: each edit that the store or the command registry refused.

import { messageOf } from "../../util/index.ts";

export interface Notice {
  readonly id: number;
  readonly text: string;
}

// The notices that one drawing of the board shows, which components follow as
// React's useSyncExternalStore follows a store, which takes `list` and
// `subscribe` as they are.
export interface Notices {
  // The notices, the same array until one comes or goes.
  readonly list: () => readonly Notice[];
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

  return {
    list: () => notices,
    refused(error) {
      set([...notices, { id: ++last, text: messageOf(error) }]);
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
