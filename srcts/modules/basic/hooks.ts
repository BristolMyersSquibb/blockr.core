// The hooks through which the basic frontend follows core, as React's
// useSyncExternalStore follows a store: the store's board and undo history,
// a block's status, and the notices.

import { useCallback, useSyncExternalStore } from "react";
import type { BlockStatus } from "../../scheduler/index.ts";
import type { BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Notice, Notices } from "./notices.ts";

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

// The block's status, which the scheduler keeps the same object until it
// announces that it changed.
export function useStatus(core: Pick<CoreApi, "scheduler">, block: BlockId): BlockStatus | undefined {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.scheduler.subscribe((changed) => {
        if (changed.includes(block)) notify();
      }),
    [core, block],
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}

export function useNotices(notices: Notices): readonly Notice[] {
  return useSyncExternalStore(notices.subscribe, notices.list);
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
