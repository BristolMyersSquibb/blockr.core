// Keyboard shortcuts as core writes them: as a command's are, such as
// "Mod+S", the names of the modifiers, then the name of the key, joined by
// "+". The modifiers are Mod, which is Ctrl or Cmd, Ctrl, Alt, Shift and Meta,
// which is Cmd on a Mac. The key is named as the `key` of a key press names
// it, as "S", "Backspace" or "+". Case does not matter in either. A shortcut
// names each modifier once, and names neither Ctrl nor Meta beside Mod. Core
// binds no shortcut that it cannot read. Core's basic frontend binds the
// shortcuts of its buttons (../modules/basic/shortcuts.ts), and core's menu
// the key that opens an advanced action (../modules/board-action-menu).

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
// for "Mod+S", or undefined for no shortcut or one that core cannot read.
export function ariaShortcuts(text: string | undefined): string | undefined {
  const shortcut = text === undefined ? undefined : readShortcut(text);
  if (shortcut === undefined) return undefined;
  const rest = [...(shortcut.alt ? ["Alt"] : []), ...(shortcut.shift ? ["Shift"] : []), shortcut.key];
  const firsts = shortcut.mod
    ? [["Control"], ["Meta"]]
    : [[...(shortcut.ctrl ? ["Control"] : []), ...(shortcut.meta ? ["Meta"] : [])]];
  return firsts.map((first) => [...first, ...rest].join("+")).join(" ");
}

// The shortcut as a menu shows it, in the notation of blockr.ui's design
// system, as the dock shows it: glyphs run together on a Mac, as "⌘⇧Z", and
// words joined by "+" elsewhere, as "Ctrl+Shift+Z", where Mod is Command on a
// Mac and Ctrl elsewhere. It gives nothing for a shortcut that core cannot
// read, since core binds no such shortcut.
export function shortcutLabel(text: string | undefined, mac: boolean): string | undefined {
  const shortcut = text === undefined ? undefined : readShortcut(text);
  if (shortcut === undefined) return undefined;
  const key = shortcut.key.length === 1 ? shortcut.key.toUpperCase() : shortcut.key;
  if (mac) {
    const glyphs = [
      ...(shortcut.ctrl ? ["⌃"] : []),
      ...(shortcut.alt ? ["⌥"] : []),
      ...(shortcut.mod || shortcut.meta ? ["⌘"] : []),
      ...(shortcut.shift ? ["⇧"] : []),
    ];
    return [...glyphs, key].join("");
  }
  const words = [
    ...(shortcut.mod || shortcut.ctrl ? ["Ctrl"] : []),
    ...(shortcut.alt ? ["Alt"] : []),
    ...(shortcut.shift ? ["Shift"] : []),
    ...(shortcut.meta ? ["Meta"] : []),
  ];
  return [...words, key].join("+");
}

// Whether the page runs on a Mac, where Mod is Command.
export function onMac(win: Window): boolean {
  const nav = win.navigator as Navigator & { readonly userAgentData?: { readonly platform?: string } };
  return /mac|iphone|ipad/i.test(nav.userAgentData?.platform ?? nav.platform);
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

// The shortcut that the text names, or undefined where core cannot read it.
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
