// The hooks through which core's basic frontend and its Edit board plugin
// follow the store, as React's useSyncExternalStore follows a store: the
// store's board and its undo history.

import { useCallback, useSyncExternalStore } from "react";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";

// The board, which the store keeps the same object until it announces a
// change.
export function useBoard(core: Pick<CoreApi, "store">): Board {
  const subscribe = useStore(core);
  return useSyncExternalStore(subscribe, () => core.store.board());
}

// Whether the user has a step to undo and one to redo.
export function useHistory(core: Pick<CoreApi, "store">): { canUndo: boolean; canRedo: boolean } {
  const subscribe = useStore(core);
  const canUndo = useSyncExternalStore(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}

function useStore(core: Pick<CoreApi, "store">): (notify: () => void) => () => void {
  return useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
}
