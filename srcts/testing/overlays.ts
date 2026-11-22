// What a user finds of the overlays of core's React SDK in a test, by what
// the user reads: the dialog that is open, the menu that is open and its
// items, the tooltip that shows, and whether an element is out of reach
// behind a modal.
// React Aria draws each overlay in a portal at the end of the page's body.

import { find } from "./inputs.ts";
import { step } from "./react.ts";

// The dialog that is open.
export function openDialog(): HTMLElement {
  return find(document, '[role="dialog"]');
}

// The menu that is open, by its label.
export function openMenu(label: string): HTMLElement {
  return find(document, `[role="menu"][aria-label="${label}"]`);
}

// The item of the menu, or of one of its groups, whose label is the one given.
export function menuItem(menu: ParentNode, label: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>('[role="menuitem"]')].find((item) => itemLabel(item) === label);
  if (found === undefined) throw new Error(`Found no menu item ${label}.`);
  return found;
}

// The label of each item of the menu, or of one of its groups, in order.
export function menuItems(menu: ParentNode): string[] {
  return [...menu.querySelectorAll('[role="menuitem"]')].map(itemLabel);
}

// What an item's label slot holds, as React Aria names the item by it, where
// the item has one, and otherwise its text.
function itemLabel(item: Element): string {
  const id = (item.getAttribute("aria-labelledby") ?? "").split(" ")[0] ?? "";
  return ((id === "" ? null : item.ownerDocument.getElementById(id)) ?? item).textContent.trim();
}

// The texts that name the item's description and shortcut to assistive
// technology, through its aria-describedby, in order.
export function itemDescribed(item: Element): string[] {
  return (item.getAttribute("aria-describedby") ?? "")
    .split(" ")
    .flatMap((id) => (id === "" ? [] : [item.ownerDocument.getElementById(id)?.textContent.trim() ?? ""]))
    .filter((text) => text !== "");
}

// The group of the menu under the title given.
export function menuGroup(menu: ParentNode, title: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>('[role="group"]')].find((group) => {
    const heading = group.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "");
    return heading?.textContent.trim() === title;
  });
  if (found === undefined) throw new Error(`Found no group ${title}.`);
  return found;
}

// Whether the element sits in a part of the page that a modal made inert.
export function inert(el: Element): boolean {
  for (let node: Element | null = el; node !== null; node = node.parentElement) {
    if (node.hasAttribute("inert") || (node instanceof HTMLElement && node.inert)) return true;
  }
  return false;
}

// The text of the tooltip that shows, or null while none shows.
export function shownTip(): string | null {
  return document.querySelector('[role="tooltip"]')?.textContent ?? null;
}

// The text of the tooltip that shows as the keyboard's focus moves to the
// element, which React Aria shows at once, or null where none shows.
// The keyboard's Tab moves the focus there, so that React Aria counts the
// focus as the keyboard's.
export async function tipOnFocus(el: HTMLElement): Promise<string | null> {
  await step(() => {
    el.ownerDocument.dispatchEvent(new KeyboardEvent("keydown", { key: "Tab", bubbles: true }));
    el.focus();
  });
  return shownTip();
}

// Moves the pointer onto the element, as a mouse does, so that React Aria
// counts what the user does next as the pointer's, and hears the element as
// hovered.
export function pointTo(el: Element): void {
  el.dispatchEvent(new PointerEvent("pointermove", { bubbles: true, pointerType: "mouse" }));
  el.dispatchEvent(new PointerEvent("pointerover", { bubbles: true, pointerType: "mouse" }));
}
