// What a user does to the select of core's React SDK in a test
// (../inputs/select.tsx): finds its field by its label, opens its list, types
// a filter into it and picks a choice of the list, by what the user reads.
// The field of one choice is a text field, into which the user types.
// The field of several is a button, whose list holds the filter box.

import { press } from "./inputs.ts";

// What lets React draw what the user did, as step() of ./react.ts does in a
// test of a React component.
export type Act = (fn: () => void) => Promise<void>;

// The labels of the rows of the select's list, which it opens and closes
// again.
export async function listed(field: HTMLElement, act: Act): Promise<string[]> {
  await act(() => {
    field.click();
  });
  const rows = optionLabels(field);
  await act(() => {
    press(filterOf(field), "Escape");
  });
  return rows;
}

// Opens the select's list and clicks the rows of the labels given, as a user
// picks them, and closes the list of several again, as it reports its picks.
export async function picked(field: HTMLElement, act: Act, ...labels: string[]): Promise<void> {
  await act(() => {
    field.click();
  });
  for (const label of labels) {
    await act(() => {
      optionFromLabel(field, label).click();
    });
  }
  if (field instanceof HTMLButtonElement) {
    await act(() => {
      press(filterOf(field), "Escape");
    });
  }
}

// The text field that the label of the text given names, through its for
// attribute, in `el`, such as the field of a select of one choice.
export function labelled(el: ParentNode, label: string): HTMLInputElement {
  const field = fieldOf(el, label);
  if (!(field instanceof HTMLInputElement)) throw new Error(`Found no text field labelled ${label}.`);
  return field;
}

// The field of the select that the label of the text given names, in `el`:
// the text field of one choice, or the button of several.
export function fieldOf(el: ParentNode, label: string): HTMLInputElement | HTMLButtonElement {
  const found = [...el.querySelectorAll("label")].find((l) => l.textContent.trim() === label);
  const field = found === undefined ? null : (byId(found, found.htmlFor) ?? found.querySelector("input"));
  if (!(field instanceof HTMLInputElement || field instanceof HTMLButtonElement)) throw new Error(`Found no field labelled ${label}.`);
  return field;
}

// The list of the select's rows while its list is open.
export function listFromField(field: HTMLElement): HTMLElement | null {
  const id = field.getAttribute("aria-controls");
  return id === null ? null : byId(field, id);
}

// Where the user types a filter: the field of one choice, or the filter box
// in the list of several.
export function filterOf(field: HTMLElement): HTMLInputElement {
  if (field instanceof HTMLInputElement) return field;
  const filter = listFromField(field)?.parentElement?.querySelector('input[role="combobox"]');
  if (!(filter instanceof HTMLInputElement)) throw new Error(`The list of ${field.id} is not open.`);
  return filter;
}

// The labels of the rows that the open list shows, by group where it groups
// them, as "group: label".
export function optionLabels(field: HTMLElement): string[] {
  const list = listFromField(field);
  if (list === null) return [];
  return [...list.querySelectorAll('[role="option"]')].map((option) => {
    const group = option.closest('[role="group"]');
    const heading = group === null ? null : byId(field, group.getAttribute("aria-labelledby") ?? "");
    const label = optionLabel(option);
    return heading === null ? label : `${heading.textContent}: ${label}`;
  });
}

// The row of the label given in the open list.
export function optionFromLabel(field: HTMLElement, label: string): HTMLElement {
  const found = [...(listFromField(field)?.querySelectorAll('[role="option"]') ?? [])].find((option) => optionLabel(option) === label);
  if (!(found instanceof HTMLElement)) throw new Error(`Found no option ${label} of ${field.id}.`);
  return found;
}

// The labels of the rows that the open list ticks or shows as the pick.
export function pickedLabels(field: HTMLElement): string[] {
  return [...(listFromField(field)?.querySelectorAll('[role="option"][aria-selected="true"]') ?? [])].map(optionLabel);
}

// What the open list says below its rows, such as that no choice matches.
export function listNote(field: HTMLElement): string | null {
  return listFromField(field)?.parentElement?.querySelector(":scope > p")?.textContent ?? null;
}

// The label of the row that the keyboard marks, as the field or the filter
// box names it in aria-activedescendant.
export function markedLabel(field: HTMLElement): string | null {
  const by = field instanceof HTMLInputElement ? field : filterOf(field);
  const id = by.getAttribute("aria-activedescendant");
  const option = id === null ? null : byId(field, id);
  return option === null ? null : optionLabel(option);
}

// The element of the id given in the tree that holds `node`, whether on the
// page or not, as the element of controls() before a module puts it there.
function byId(node: Node, id: string): HTMLElement | null {
  if (id === "") return null;
  const root = node.getRootNode() as Partial<Pick<Document, "getElementById">> & Partial<Pick<Element, "querySelector">>;
  const found = root.getElementById?.(id) ?? root.querySelector?.(`[id="${CSS.escape(id)}"]`) ?? null;
  return found instanceof HTMLElement ? found : null;
}

// What a row shows as its label.
function optionLabel(option: Element): string {
  return option.querySelector("[data-label]")?.textContent ?? "";
}
