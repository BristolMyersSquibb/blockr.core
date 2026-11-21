// What a user finds of the rows of the Table, the GridList and the Tree of
// core's React SDK in a test, by what the user reads.
// React Aria marks the row of each item with the item's id, in data-key.
// The row that holds what an empty list says has no data-key.

import { find } from "./inputs.ts";

// The rows of the items in `el`, in order.
export function rows(el: ParentNode): HTMLElement[] {
  return [...el.querySelectorAll<HTMLElement>('[role="row"][data-key]')];
}

// The text of each row of an item in `el`, in order.
export function rowTexts(el: ParentNode): string[] {
  return rows(el).map((row) => row.textContent);
}

// The row of an item in `el` whose text is the one given.
export function rowOf(el: ParentNode, text: string): HTMLElement {
  const found = rows(el).find((row) => row.textContent === text);
  if (found === undefined) throw new Error(`Found no row ${text}.`);
  return found;
}

// The checkbox that selects the row, which React Aria hides but for assistive
// technology.
export function rowCheckbox(row: Element): HTMLInputElement {
  return find(row, 'input[type="checkbox"]', HTMLInputElement);
}
