// Core's basic front-end, blockr.core-basic, as Front-ends and extension
// points has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-front-end).
// Core's board class `board` names it, so it draws every board whose class no
// other front-end draws.
// It does what today's front-end does wherever core's commands and store do
// the work, so it only draws. It works on the board through CoreApi alone.
// It is written in React, on the one copy of React that core puts on the
// page, and it takes no extensions.
//
// Its render() takes four steps:
// 1. It demands every block, and the blocks in view higher still, from the
//    board, so that the scheduler evaluates first what the page first shows.
// 2. It tells the user what the store dropped as the board opened.
// 3. It draws the board, and then demands the blocks in view as the page lays
//    out their cards.
// 4. It binds undo and redo to their shortcuts.
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
    for (const item of core.store.dropped()) notices.dropped(item);

    const front: FrontEnd = { core, notices, inView: shown };
    const root = createRoot(el);
    flushSync(() => {
      root.render(<BasicBoard front={front} />);
    });
    shown.measure();

    const unbind = bindShortcuts(doc, core, (error) => {
      notices.refused(error);
    });
    return () => {
      unbind();
      shown.release();
      root.unmount();
    };
  },
};
