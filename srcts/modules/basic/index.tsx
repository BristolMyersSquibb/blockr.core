// Core's basic frontend, blockr.core-basic, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
// An app runs it, as the plugin of the class
// blockr.core-basic_frontend_plugin, unless another of its plugins is a
// frontend, and it draws every board that the app opens.
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
// 2. It shows each note that a plugin sends, and the one in which core names
//    what the store dropped as the board opened.
// 3. It draws the board that the store holds, and then demands the blocks in
//    view as the page lays out their cards.
// 4. It binds undo, redo and the buttons among the storage controls to their
//    shortcuts.
// The cleanup that render() returns undoes all four.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { CoreApi } from "../../types/core-api.ts";
import { BasicBoard } from "./board.tsx";
import type { FrontEnd } from "./context.ts";
import { demandShown } from "./demand.ts";
import { createNotices } from "./notices.ts";
import { bindShortcuts } from "./shortcuts.ts";

export const api = 1;

export default {
  render({ el, core }: { el: HTMLElement; core: CoreApi; signal: AbortSignal }): () => void {
    const doc = el.ownerDocument;
    const shown = demandShown(core, doc.defaultView ?? window);
    const notices = createNotices();
    const unnotify = core.notifications.subscribe((note) => {
      notices.add(note);
    });
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
      unnotify();
      shown.release();
      root.unmount();
    };
  },
};
