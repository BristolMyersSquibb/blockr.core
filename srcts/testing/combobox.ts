// What a user does to the searchable dropdowns of core's frontend and its Edit
// board plugin in a test (../ui/combobox.tsx): finds a field by its label,
// types a search into it and picks an option of its list, by what the user
// reads.

import { type } from "./inputs.ts";

// The field that the label of the text given names, through its for
// attribute, in `el`.
export function labelled(el: ParentNode, label: string): HTMLInputElement {
  const found = [...el.querySelectorAll("label")].find((l) => l.textContent.trim() === label);
  const field = found === undefined ? null : (found.ownerDocument.getElementById(found.htmlFor) ?? found.querySelector("input"));
  if (!(field instanceof HTMLInputElement)) throw new Error(`Found no field labelled ${label}.`);
  return field;
}

// The list that the dropdown of the field shows while it is open.
export function listOf(field: HTMLInputElement): HTMLElement | null {
  const id = field.getAttribute("aria-controls");
  return id === null ? null : field.ownerDocument.getElementById(id);
}

// The labels of the options that the open list of the field shows, by group
// where it groups them, as "group: label".
export function optionsOf(field: HTMLInputElement): string[] {
  const list = listOf(field);
  if (list === null) return [];
  return [...list.querySelectorAll('[role="option"]')].map((option) => {
    const group = option.closest('[role="group"]');
    const heading = group === null ? null : field.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "");
    const label = optionLabel(option);
    return heading === null ? label : `${heading.textContent}: ${label}`;
  });
}

// The option of the label given in the open list of the field.
export function optionOf(field: HTMLInputElement, label: string): HTMLElement {
  const found = [...(listOf(field)?.querySelectorAll('[role="option"]') ?? [])].find((option) => optionLabel(option) === label);
  if (!(found instanceof HTMLElement)) throw new Error(`Found no option ${label} of ${field.id}.`);
  return found;
}

// Types the search into the field, which opens its list, and clicks the
// option of the label given, as a user picks one.
export function choose(el: ParentNode, label: string, search: string, option: string = search): void {
  const field = labelled(el, label);
  field.click();
  type(field, search);
  optionOf(field, option).click();
}

// What an option shows as its label, without the mark of a picked one.
function optionLabel(option: Element): string {
  return (option.querySelector("span > span")?.textContent ?? "").replace(/^✓ /, "");
}
