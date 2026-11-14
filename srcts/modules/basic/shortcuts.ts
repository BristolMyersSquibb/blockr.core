// The shortcuts of core's basic frontend, which it listens for on the page's
// document:
// - Undo and redo, as Board updates leaves them to a frontend
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#undo):
//   Mod+Z takes the user's last step back, and Mod+Shift+Z or Mod+Y applies it
//   again, with Mod either Ctrl or Cmd.
// - The shortcut of each button among the storage controls, those inside a
//   menu too (./storage.ts). A key press runs the first button, in the order
//   in which the toolbar draws them, whose shortcut it matches, unless that
//   button is disabled. Holding the keys down runs the button once. Undo and
//   redo come first, so that no button takes their keys.
// In a text field, the shortcuts stay the field's own, so that they undo
// typing there.
//
// A shortcut is written as a command's is, such as "Mod+S": the names of its
// modifiers, then the name of its key, joined by "+". The modifiers are Mod,
// which is Ctrl or Cmd, Ctrl, Alt, Shift and Meta, which is Cmd on a Mac. The
// key is named as the `key` of a key press names it, as "S", "Backspace" or
// "+". Case does not matter in either. A shortcut names each modifier once,
// and names neither Ctrl nor Meta beside Mod. The frontend binds no shortcut
// that it cannot read.

import { refused } from "../../ui/index.ts";
import type { FrontEnd } from "./context.ts";
import { buttonsOf, drawnControls, runButton } from "./storage.ts";

// Listens for the shortcuts on the document, until the returned cleanup runs.
// The frontend tells the user why the store refused an undo or a redo, and
// why a button failed, as it does where the user clicks.
export function bindShortcuts(doc: Document, front: Pick<FrontEnd, "core" | "plugin">): () => void {
  const { core } = front;
  function onKey(event: KeyboardEvent): void {
    if (event.defaultPrevented || editable(event.target)) return;
    const step = undoOrRedo(event);
    if (step !== undefined) {
      event.preventDefault();
      try {
        if (step === "undo") core.store.undo();
        else core.store.redo();
      } catch (error) {
        refused(front, error);
      }
      return;
    }
    const button = buttonsOf(drawnControls(core)?.controls ?? []).find(
      (b) => b.disabled !== true && b.shortcut !== undefined && matchesShortcut(b.shortcut, event),
    );
    if (button === undefined) return;
    event.preventDefault();
    if (!event.repeat) void runButton(button);
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}

// Whether the key press is the one that the shortcut names.
export function matchesShortcut(text: string, event: KeyboardEvent): boolean {
  const shortcut = readShortcut(text);
  // A key press of the browser's autofill names no key.
  const key: unknown = event.key;
  if (shortcut === undefined || typeof key !== "string" || key.toLowerCase() !== shortcut.key.toLowerCase()) {
    return false;
  }
  if (event.altKey !== shortcut.alt || event.shiftKey !== shortcut.shift) return false;
  if (shortcut.mod) return event.ctrlKey || event.metaKey;
  return event.ctrlKey === shortcut.ctrl && event.metaKey === shortcut.meta;
}

// The shortcut as `aria-keyshortcuts` writes it, such as "Control+S Meta+S"
// for "Mod+S", or undefined for no shortcut or one that the frontend cannot
// read.
export function ariaShortcuts(text: string | undefined): string | undefined {
  const shortcut = text === undefined ? undefined : readShortcut(text);
  if (shortcut === undefined) return undefined;
  const rest = [...(shortcut.alt ? ["Alt"] : []), ...(shortcut.shift ? ["Shift"] : []), shortcut.key];
  const firsts = shortcut.mod
    ? [["Control"], ["Meta"]]
    : [[...(shortcut.ctrl ? ["Control"] : []), ...(shortcut.meta ? ["Meta"] : [])]];
  return firsts.map((first) => [...first, ...rest].join("+")).join(" ");
}

interface Shortcut {
  readonly key: string;
  readonly mod: boolean;
  readonly ctrl: boolean;
  readonly alt: boolean;
  readonly shift: boolean;
  readonly meta: boolean;
}

const MODIFIERS: readonly string[] = ["mod", "ctrl", "alt", "shift", "meta"];

// The shortcut that the text names, or undefined where the frontend cannot
// read it.
function readShortcut(text: string): Shortcut | undefined {
  // The key follows the last "+" before the text's last character, so that
  // "Mod++" names the key "+".
  const at = text.length < 2 ? -1 : text.lastIndexOf("+", text.length - 2);
  const key = text.slice(at + 1);
  const names = at < 0 ? [] : text.slice(0, at).toLowerCase().split("+");
  if (key === "" || new Set(names).size < names.length || !names.every((name) => MODIFIERS.includes(name))) {
    return undefined;
  }
  const has = (name: string): boolean => names.includes(name);
  if (has("mod") && (has("ctrl") || has("meta"))) return undefined;
  return { key, mod: has("mod"), ctrl: has("ctrl"), alt: has("alt"), shift: has("shift"), meta: has("meta") };
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
