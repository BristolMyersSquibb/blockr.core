// The hook through which a card follows its block's status, as React's
// useSyncExternalStore follows a store.

import { useCallback, useSyncExternalStore } from "react";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";

// The block's status, which the scheduler keeps the same object until it
// announces that it changed.
export function useStatus(
  core: { readonly scheduler: Pick<Scheduler, "status" | "subscribe"> },
  block: BlockId,
): BlockStatus | undefined {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.scheduler.subscribe((changed) => {
        if (changed.includes(block)) notify();
      }),
    [core, block],
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}
