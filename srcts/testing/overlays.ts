// What a user finds of the overlays of core's React SDK in a test, by what
// the user reads: the dialog that is open, the menu that is open and its
// items, and whether an element is out of reach behind a modal.
// React Aria draws each overlay in a portal at the end of the page's body.

import { find } from "./inputs.ts";

// The dialog that is open.
export function openDialog(): HTMLElement {
  return find(document, '[role="dialog"]');
}

// The menu that is open, by its label.
export function openMenu(label: string): HTMLElement {
  return find(document, `[role="menu"][aria-label="${label}"]`);
}

// The item of the menu, or of one of its groups, whose text is the one given.
export function menuItem(menu: ParentNode, text: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>('[role="menuitem"]')].find((item) => item.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no menu item ${text}.`);
  return found;
}

// The text of each item of the menu, or of one of its groups, in order.
export function menuItems(menu: ParentNode): string[] {
  return [...menu.querySelectorAll('[role="menuitem"]')].map((item) => item.textContent.trim());
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
