// Core's basic frontend, blockr.core-basic, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
// It draws every board that holds no frontend.
// It does what today's frontend does wherever core's commands and store do
// the work, so it only draws. It works on the board through CoreApi alone,
// and places each plugin whose class has a `ui` in a drawer, reading no
// member of a plugin class beyond core's.
// It is written in React, on the one copy of React that core puts on the
// page.
//
// Its render() takes four steps:
// 1. It demands every block, and the blocks in view higher still, from the
//    board, so that the scheduler evaluates first what the page first shows.
// 2. It tells the user what the store dropped as the board opened, which core
//    hands it, and from then on each note that a plugin sends.
// 3. It draws the board that the store holds, and then demands the blocks in
//    view as the page lays out their cards.
// 4. It binds undo and redo to their shortcuts.
// The cleanup that render() returns undoes all four.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { Dropped } from "../../store/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { BasicBoard } from "./board.tsx";
import type { FrontEnd } from "./context.ts";
import { demandShown } from "./demand.ts";
import { droppedNote } from "./dropped.ts";
import { createNotices } from "./notices.ts";
import { bindShortcuts } from "./shortcuts.ts";

export const api = 1;

export default {
  render({
    el,
    core,
    dropped = [],
  }: {
    el: HTMLElement;
    core: CoreApi;
    signal: AbortSignal;
    dropped?: readonly Dropped[];
  }): () => void {
    const doc = el.ownerDocument;
    const shown = demandShown(core, doc.defaultView ?? window);
    const notices = createNotices();
    const told = droppedNote(dropped);
    if (told !== undefined) notices.add(told);
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
