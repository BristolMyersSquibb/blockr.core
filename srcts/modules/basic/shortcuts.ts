// Undo and redo on their shortcuts, as Board updates leaves them to a
// front-end (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#undo):
// Mod+Z takes the user's last step back, and Mod+Shift+Z or Mod+Y applies it
// again, with Mod either Ctrl or Cmd. In a text field, the shortcuts stay the
// field's own, so that they undo typing there.

import type { CoreApi } from "../../types/core-api.ts";

// Listens for the shortcuts on the document, until the returned cleanup runs,
// and hands what the store refused to `refused`.
export function bindShortcuts(
  doc: Document,
  core: Pick<CoreApi, "store">,
  refused: (error: unknown) => void,
): () => void {
  function onKey(event: KeyboardEvent): void {
    const step = stepOf(event);
    if (step === undefined || event.defaultPrevented || editable(event.target)) return;
    event.preventDefault();
    try {
      if (step === "undo") core.store.undo();
      else core.store.redo();
    } catch (error) {
      refused(error);
    }
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}

function stepOf(event: KeyboardEvent): "undo" | "redo" | undefined {
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
