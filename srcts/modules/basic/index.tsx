// Core's basic frontend, the module of the plugin class
// blockr.core-basic_frontend_plugin, whose default export is the class without
// its id, with the `frontend` that draws the page, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
// An app runs it unless another of its plugins is a frontend, and it draws
// every board that the app opens. Its class requires the severities of the
// notifications that its cards show and the theme that it draws in.
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
// 2. It shows as a toast each notification of the board that concerns neither
//    a block nor a plugin that it shows in a section, whoever raised it, such
//    as the one in which core names what the store dropped as the board
//    opened (./toasts.ts). Each block's card shows those about the block, and
//    each plugin's section those about the plugin (./plugins.tsx). The
//    frontend raises none of its own: core tells the user why the store or
//    the command registry refused one of its edits, and a storage plugin why
//    one of its buttons failed.
// 3. It draws the board that the store holds, and then demands the blocks in
//    view as the page lays out their cards.
// 4. It binds undo, redo and the buttons among the storage controls to their
//    shortcuts.
// The cleanup that render() returns undoes all four.
//
// As a plugin asks it to show a block or a plugin, show() opens the group of
// the block's stack, or the plugin's section, where the user collapsed it,
// and scrolls the card or the section into view, leaving the keyboard's focus
// where it is
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { PluginClass } from "../../store/index.ts";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { BasicBoard } from "./board.tsx";
import type { FrontEnd } from "./context.ts";
import { demandShown } from "./demand.ts";
import type { Watched } from "./demand.ts";
import { createSections, hasSection } from "./plugins.tsx";
import { bindShortcuts } from "./shortcuts.ts";
import { createToasts } from "./toasts.ts";

export const api = 1;

export default {
  frontend: { render, show },
} satisfies Omit<PluginClass, "id">;

// What the frontend draws from on each element that it draws in, whose view
// show() finds the cards and the sections by, and whose sections it opens.
const drawn = new WeakMap<HTMLElement, FrontEnd>();

function render({ el, core }: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }): () => void {
  const doc = el.ownerDocument;
  const shown = demandShown(core, doc.defaultView ?? window);
  const toasts = createToasts(core.notifications, (plugin) => hasSection(core, plugin));
  const front: FrontEnd = { core, toasts, inView: shown, sections: createSections(), groups: createSections() };
  const root = createRoot(el);
  flushSync(() => {
    root.render(<BasicBoard front={front} />);
  });
  shown.measure();

  const unbind = bindShortcuts(doc, core);
  drawn.set(el, front);
  return () => {
    if (drawn.get(el) === front) drawn.delete(el);
    unbind();
    toasts.stop();
    shown.release();
    root.unmount();
  };
}

function show({ object, el }: { object: Watched; el: HTMLElement }): void {
  const front = drawn.get(el);
  const target = front?.inView.element(object);
  if (front === undefined || target === undefined) return;
  // A collapsed section of the plugin opens, as does the collapsed group of
  // the stack that holds the block, which the browser lays out before it
  // scrolls.
  flushSync(() => {
    if ("plugin" in object) {
      front.sections.collapse(object.plugin, false);
      return;
    }
    const stack = Object.entries(front.core.store.board().stacks).find(([, held]) => held.blocks.includes(object.block));
    if (stack !== undefined) front.groups.collapse(stack[0], false);
  });
  // A plugin's section scrolls into view whole, with its name, and a block's
  // card alone, rather than the group of its stack.
  const into = "plugin" in object ? (target.closest("section") ?? target) : target;
  into.scrollIntoView({ block: "nearest", inline: "nearest" });
}
