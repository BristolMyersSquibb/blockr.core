// What the page gives end-to-end tests in Shiny's test mode, as shinytest2
// starts an app (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).
// The client holds the board and each block's status, so the page puts
// `blockrTest` on `window`, for AppDriver$get_js() to read, as in
// `app$get_js("blockrTest.statuses()")`.
// R exports each block's latest result beside it, through exportTestValues().
// It reads the board that is open, which core makes anew as another board
// opens, and gives copies, so that nothing changes the board through it.

import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";

export interface BlockrTest {
  // A copy of the board that the store holds now, or null while core starts.
  board(): Board | null;
  // A copy of each block's status now, by block id, for the blocks of the
  // board that the store holds, as the scheduler gives them.
  // It holds none while core starts.
  statuses(): Record<BlockId, BlockStatus>;
}

declare global {
  interface Window {
    blockrTest?: BlockrTest;
  }
}

// What the object reads of the CoreApi of the board that is open.
export interface BlockrTestCore {
  readonly store: Pick<CoreApi["store"], "board" | "hasBoard">;
  readonly scheduler: Pick<CoreApi["scheduler"], "status">;
}

// The object, on the CoreApi of the board that is open, which `core` gives,
// and which is undefined while core starts.
export function blockrTest(core: () => BlockrTestCore | undefined): BlockrTest {
  // The CoreApi of the board that is open, once its store holds the board.
  // The store holds none while the startup modules initialize.
  const open = (): BlockrTestCore | undefined => {
    const now = core();
    return now?.store.hasBoard() === true ? now : undefined;
  };
  return {
    board() {
      const now = open();
      return now === undefined ? null : structuredClone(now.store.board());
    },
    statuses() {
      const now = open();
      if (now === undefined) return {};
      return Object.fromEntries(
        Object.keys(now.store.board().blocks).flatMap((id): [BlockId, BlockStatus][] => {
          const status = now.scheduler.status(id);
          return status === undefined ? [] : [[id, structuredClone(status)]];
        }),
      );
    },
  };
}
