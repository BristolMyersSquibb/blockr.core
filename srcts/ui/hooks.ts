// The hook through which core's basic frontend follows the store's undo
// history, as React's useSyncExternalStore follows a store.
// The frontend and the Edit board plugin follow the board itself through
// useBoard() of core's React SDK.

import { useCallback, useSyncExternalStore } from "react";
import type { CoreApi } from "../types/core-api.ts";

// Whether the store's history holds a step to undo and one to redo, whoever
// took them.
export function useHistory(core: Pick<CoreApi, "store">): { canUndo: boolean; canRedo: boolean } {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
  const canUndo = useSyncExternalStore(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}
