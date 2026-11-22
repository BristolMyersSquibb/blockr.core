// The shortcuts of core's basic frontend, which it listens for on the page's
// document:
// - Undo and redo, as Board updates leaves them to a frontend
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#undo):
//   Mod+Z takes back the board's last step, whoever took it, and Mod+Shift+Z
//   or Mod+Y applies again the step that the last undo took back, with Mod
//   either Ctrl or Cmd.
// - The shortcut of each button among the storage controls, those inside a
//   menu too (./storage.ts). A key press runs the first button, in the order
//   in which the toolbar draws them, whose shortcut it matches, unless that
//   button is disabled. Holding the keys down runs the button once. Undo and
//   redo come first, so that no button takes their keys.
// In a text field, the shortcuts stay the field's own, so that they undo
// typing there.
//
// A shortcut is written as core writes one, such as "Mod+S"
// (../../util/shortcuts.ts). The frontend binds no shortcut that it cannot
// read.

import type { CoreApi } from "../../types/core-api.ts";
import { matchesShortcut } from "../../util/index.ts";
import { buttonsFromControls, drawnControls, runButton } from "./storage.ts";

// Listens for the shortcuts on the document, until the returned cleanup runs.
// A key press runs undo, redo or a button as a click does: core tells the
// user why the store refused an undo or a redo, and the frontend logs a button
// whose run() rejects.
export function bindShortcuts(doc: Document, core: CoreApi): () => void {
  function onKey(event: KeyboardEvent): void {
    if (event.defaultPrevented || editable(event.target)) return;
    const step = undoOrRedo(event);
    if (step !== undefined) {
      event.preventDefault();
      try {
        if (step === "undo") core.store.undo();
        else core.store.redo();
      } catch {
        // Core has told the user why the store refused it.
      }
      return;
    }
    const button = buttonsFromControls(drawnControls(core)?.controls ?? []).find(
      (b) => b.disabled !== true && b.shortcut !== undefined && matchesShortcut(b.shortcut, event),
    );
    if (button === undefined) return;
    event.preventDefault();
    if (!event.repeat) void runButton(core, button);
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}

function undoOrRedo(event: KeyboardEvent): "undo" | "redo" | undefined {
  if (!(event.ctrlKey || event.metaKey) || event.altKey) return undefined;
  const key = event.key.toLowerCase();
  if (key === "z") return event.shiftKey ? "redo" : "undo";
  if (key === "y" && !event.shiftKey) return "redo";
  return undefined;
}

// Whether the element takes text, or is a control whose keys are its own.
function editable(target: EventTarget | null): boolean {
  if (!(target instanceof HTMLElement)) return false;
  return target.isContentEditable || ["INPUT", "TEXTAREA", "SELECT"].includes(target.tagName);
}
