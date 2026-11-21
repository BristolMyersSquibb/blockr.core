// Core's Edit board plugin, the module of the plugin class
// blockr.core-edit_board_plugin, whose default export is the class without its
// id, with the plugin's `ui`, as The Edit board plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15).
// An app runs it unless its rows leave it out, and every frontend places it as
// it places any plugin whose class has a `ui`, where core mounts it with
// mountPlugin().
// It adds, renames and removes the board's blocks, and adds, changes and
// removes its links and stacks. Its forms of a link and of a stack are the
// advanced actions of core's board actions Create link, Create stack, Edit
// link and Edit stack, which it mounts through mountAdvanced(), so that the
// menus offer the same forms. Each edit runs core's commands as the user makes
// it, so that undo takes back each one, and the plugin keeps nothing staged.
// Where the store or the command registry refuses an edit, the plugin says why
// close to the control that made it, until the user's next edit there: below
// a form, and below a table for an edit from the buttons of its rows. It takes
// the edit's notification through the edit's notifier, so that core raises
// none.
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
