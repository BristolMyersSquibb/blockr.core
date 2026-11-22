// Core's Edit board plugin, the module of the plugin class
// blockr.core-edit_board_plugin, whose default export is the class without its
// id, with the plugin's `ui`, as The Edit board plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15).
// An app runs it unless its rows leave it out, and every frontend places it as
// it places any plugin whose class has a `ui`, where core mounts it with
// mountPlugin().
// It adds the board's blocks, links and stacks through its forms, which are
// the advanced actions of core's board actions Add block, Create link and
// Create stack on the board, which it mounts through mountAdvanced(), so that
// the menus offer the same forms. Each row of its tables opens the menu of its
// block, link or stack through core.boardActions.menu(), so that it offers
// every board action that applies to the object, a package's among them, and
// the bar of the selection opens the menu of the selection. Each edit runs
// commands as one step of the user, as the user submits a form or picks a row
// of a menu, so that undo takes back each one, and the plugin keeps nothing
// staged.
// Where the store or the command registry refuses an edit, the plugin says why
// close to the control that made it: a form below its buttons, and a table or
// the bar of the selection below itself, for a board action that one of its
// menus ran, until the next board action that one of those menus runs. It
// takes the notifications through the notifier of done() or of menu(), so
// that core raises none.
// It keeps no state, so the board holds nothing for it.
// It works on the board through CoreApi alone, and is written in React, on the
// one copy of React that core puts on the page, in a root of its own.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { PluginClass } from "../../store/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { EditBoard } from "./board.tsx";

export const api = 1;

export default {
  ui: {
    render({ el, core }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }): () => void {
      const root = createRoot(el);
      flushSync(() => {
        root.render(<EditBoard core={core} />);
      });
      return () => {
        root.unmount();
      };
    },
  },
} satisfies Omit<PluginClass, "id">;
