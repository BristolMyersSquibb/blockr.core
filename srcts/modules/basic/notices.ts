// What the basic frontend tells the user above the board, each until the user
// dismisses it: what failed, such as an edit that the store or the command
// registry refused, the notes that plugins send, and what the store dropped as
// the board opened.

import { useSyncExternalStore } from "react";
import type { Note } from "../../types/core-api.ts";
import { errorMessage } from "../../util/index.ts";

export type Severity = Note["severity"];

export interface Notice {
  readonly id: number;
  readonly text: string;
  readonly severity: Severity;
}

// The notices that one drawing of the board shows, which components follow as
// React's useSyncExternalStore follows a store, which takes `list` and
// `subscribe` as they are.
export interface Notices {
  // The notices, the same array until one comes or goes.
  readonly list: () => readonly Notice[];
  // Tells the user why something failed, as an error.
  readonly failed: (error: unknown) => void;
  // Tells the user what the note says, in the note's severity.
  readonly add: (note: Note) => void;
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

  function add(note: Note): void {
    set([...notices, { id: ++last, text: note.message, severity: note.severity }]);
  }

  return {
    list: () => notices,
    failed(error) {
      add({ message: errorMessage(error), severity: "error" });
    },
    add,
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

// The notices, which a component follows as React's useSyncExternalStore
// follows a store.
export function useNotices(notices: Notices): readonly Notice[] {
  return useSyncExternalStore(notices.subscribe, notices.list);
}
