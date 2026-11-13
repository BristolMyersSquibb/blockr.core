// The hooks through which a card follows its block's status and the
// notifications about it, as React's useSyncExternalStore follows a store, and
// whether the card gives the block's output a height of its own.

import { useCallback, useMemo, useState, useSyncExternalStore } from "react";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";

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

// The notifications about the block, or every notification of the board given
// null, in the order they came. CoreApi keeps the same array of them until one
// comes or goes, so the card draws them again only then.
export function useNotified(
  core: { readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe"> },
  block: BlockId | null,
): readonly Notification[] {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.notifications.subscribe(() => {
        notify();
      }),
    [core],
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  return useMemo(
    () =>
      block === null
        ? all
        : all.filter((notification) => notification.about !== undefined && "block" in notification.about && notification.about.block === block),
    [all, block],
  );
}

// Whether the card gives the block's output a height of its own: from the
// first time that the block reads ready on, for as long as the card is drawn.
// Core asks R for an output only once the block reads ready and the output's
// element measures more than 0 by 0, so the element measures nothing before,
// and a block that fails or reads invalid at once leaves no empty room.
// Core then keeps drawing the last output whatever the block's status, so the
// output keeps its height as the block fails or waits, and does not jump.
export function useSized(status: BlockStatus | undefined): boolean {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}
