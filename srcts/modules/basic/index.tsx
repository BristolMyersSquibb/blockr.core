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
// 2. It shows as a toast each notification of the board that concerns no
//    block, whoever raised it, such as the one in which core names what the
//    store dropped as the board opened (./toasts.ts). Each block's card shows
//    those about the block. The frontend raises its own notifications under
//    the id of its plugin class, which render() gets: why the store or the
//    command registry refused one of its edits, and why a storage control's
//    button failed.
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
import type { InView, Watched } from "./demand.ts";
import { bindShortcuts } from "./shortcuts.ts";
import { createToasts } from "./toasts.ts";

export const api = 1;

export default {
  options: ["show_conditions", "dark_mode"],
  frontend: { render, show },
} satisfies Omit<PluginClass, "id">;

// What is in view on each element that the frontend draws in, which show()
// finds the cards and the sections by.
const drawn = new WeakMap<HTMLElement, InView>();

function render({ el, core, plugin }: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }): () => void {
  const doc = el.ownerDocument;
  const shown = demandShown(core, doc.defaultView ?? window);
  const toasts = createToasts(core.notifications);
  const front: FrontEnd = { core, plugin, toasts, inView: shown };
  const root = createRoot(el);
  flushSync(() => {
    root.render(<BasicBoard front={front} />);
  });
  shown.measure();

  const unbind = bindShortcuts(doc, front);
  drawn.set(el, shown);
  return () => {
    if (drawn.get(el) === shown) drawn.delete(el);
    unbind();
    toasts.stop();
    shown.release();
    root.unmount();
  };
}

function show({ object, el }: { object: Watched; el: HTMLElement }): void {
  const target = drawn.get(el)?.element(object);
  if (target === undefined) return;
  // Every collapsed group or section that holds it opens, which the browser
  // lays out before it scrolls.
  for (let node = target.parentElement; node !== null && node !== el; node = node.parentElement) {
    if (node.localName === "details" && !node.hasAttribute("open")) node.setAttribute("open", "");
  }
  // A plugin's section scrolls into view whole, with its name, and a block's
  // card alone, rather than the group of its stack.
  const into = "plugin" in object ? (target.closest("details") ?? target) : target;
  into.scrollIntoView({ block: "nearest", inline: "nearest" });
}
