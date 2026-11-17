// What the page gives end-to-end tests in Shiny's test mode, as shinytest2
// starts an app (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).
// The client holds the board and each block's status.
// Core holds, for the page, which block has focus, which blocks and plugins
// are on screen and the notifications.
// These are what a frontend offers other plugins, beside showing a block or a
// plugin (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#what-a-frontend-offers-other-plugins).
// So in test mode, the script that R writes for the page imports
// `blockrTest()` from this module, beside startPage() from core's runtime, and
// puts what it returns on `window` as `blockrTest`, for AppDriver$get_js() to
// read, as in `app$get_js("blockrTest.statuses()")`.
// A browser test reaches core only through this object, so the object gives
// each part of what a frontend offers other plugins.
// A page outside test mode loads none of this module.
// R exports each block's latest result itself, through exportTestValues().
// The object reads the board that is open, which core makes anew as another
// board opens, and gives copies, so that nothing changes the board through it.
// Its show() has the frontend show a block or a plugin, as a plugin asks it
// to.
// Showing changes nothing on the board either.
// The module imports nothing but types, so its bundle holds nothing of core's
// runtime.

import type { BlockStatus } from "../../scheduler/index.ts";
import type { BlockId, Board, PluginClassId } from "../../types/board.ts";
import type { CoreApi, Notification, OnScreen } from "../../types/core-api.ts";

export interface BlockrTest {
  // A copy of the board that the store holds now, or null while core starts.
  board(): Board | null;
  // A copy of each block's status now, by block id, for the blocks of the
  // board that the store holds, as the scheduler gives them.
  // It holds none while core starts.
  statuses(): Record<BlockId, BlockStatus>;
  // The block that has focus now, or null for none, as while core starts.
  focus(): BlockId | null;
  // A copy of the blocks and plugins on screen now, as the frontend set them,
  // or null where the frontend has set none, as while core starts.
  onScreen(): ReturnType<OnScreen["get"]>;
  // A copy of each notification that holds now, in the order they came.
  // It holds none while core starts.
  notifications(): readonly Notification[];
  // Has the frontend show the block or the plugin given, as CoreApi's show()
  // does, and resolves once the frontend has.
  // It rejects while core starts, and where CoreApi's show() refuses the block
  // or the plugin.
  show(object: { readonly block: BlockId } | { readonly plugin: PluginClassId }): Promise<void>;
}

// What the object reads of the CoreApi of the board that is open, and the
// show() that it calls.
export interface BlockrTestCore {
  readonly store: Pick<CoreApi["store"], "board" | "hasBoard">;
  readonly scheduler: Pick<CoreApi["scheduler"], "status">;
  readonly focus: Pick<CoreApi["focus"], "get">;
  readonly onScreen: Pick<OnScreen, "get">;
  readonly notifications: Pick<CoreApi["notifications"], "get">;
  readonly show: CoreApi["show"];
}

// The object, on the CoreApi of the board that is open, which `core` gives,
// and which is undefined while core starts.
export function blockrTest(core: () => BlockrTestCore | undefined): BlockrTest {
  // The CoreApi of the board that is open, once its store holds the board.
  // The store holds none while core registers what packages list and the
  // storage plugin names a board to open.
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
    focus() {
      return open()?.focus.get() ?? null;
    },
    onScreen() {
      const shown = open()?.onScreen.get() ?? null;
      return shown === null ? null : structuredClone(shown);
    },
    notifications() {
      const now = open();
      return now === undefined ? [] : structuredClone(now.notifications.get());
    },
    async show(object) {
      const now = open();
      if (now === undefined) {
        throw new Error("No board is open yet, so the frontend has nothing to show.");
      }
      await now.show(object);
    },
  };
}
