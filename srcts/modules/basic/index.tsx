// Core's basic frontend, the module of the plugin class
// blockr.core-basic_frontend_plugin, whose default export is the class without
// its id, with the `frontend` that draws the page, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
// An app runs it unless another of its plugins is a frontend, and it draws
// every board that the app opens. Its class requires the severities of the
// conditions that its cards show and the theme that it draws in.
// It does what today's frontend does wherever core's commands and store do
// the work, so it only draws. It works on the board through CoreApi alone,
// and shows each plugin whose class has a `ui` beside the board, reading no
// member of a plugin class beyond core's.
// It is written in React, on the one copy of React that core puts on the
// page.
//
// Its render() takes four steps:
// 1. It demands every block, and the blocks in view higher still, from the
//    board, so that the scheduler evaluates first what the page first shows.
// 2. It shows above the board the notifications of the board that concern no
//    block, such as the one in which core names what the store dropped as the
//    board opened, and those that the code of packages raises, until one is
//    withdrawn or the user dismisses it.
// 3. It draws the board that the store holds, and then demands the blocks in
//    view as the page lays out their cards.
// 4. It binds undo, redo and the buttons among the storage controls to their
//    shortcuts.
// The cleanup that render() returns undoes all four.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { PluginClass } from "../../store/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { BasicBoard } from "./board.tsx";
import type { FrontEnd } from "./context.ts";
import { demandShown } from "./demand.ts";
import { createNotices } from "./notices.ts";
import { bindShortcuts } from "./shortcuts.ts";

export const api = 1;

export default {
  options: ["show_conditions", "dark_mode"],
  frontend: { render },
} satisfies Omit<PluginClass, "id">;

function render({ el, core }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }): () => void {
  const doc = el.ownerDocument;
  const shown = demandShown(core, doc.defaultView ?? window);
  const notices = createNotices(core.notifications);
  const front: FrontEnd = { core, notices, inView: shown };
  const root = createRoot(el);
  flushSync(() => {
    root.render(<BasicBoard front={front} />);
  });
  shown.measure();

  const unbind = bindShortcuts(doc, core, (error) => {
    notices.failed(error);
  });
  return () => {
    unbind();
    notices.stop();
    shown.release();
    root.unmount();
  };
}
