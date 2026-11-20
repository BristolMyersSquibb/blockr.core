// What a user does to an input in a test, as a browser has it happen,
// so that an input drawn by React hears it as one drawn without it does:
// the value set through the element's own setter,
// which React does not track as a value it set itself,
// and then the event that the browser fires.
// The browser's file picker, and a file dropped on an element, are played the
// same way.

import { vi } from "vitest";

// The element that the selector finds in `el`, which the test expects there,
// of the class given, or an HTMLElement.
export function find(el: ParentNode, selector: string): HTMLElement;
export function find<E extends Element>(el: ParentNode, selector: string, type: abstract new () => E): E;
export function find(el: ParentNode, selector: string, type: abstract new () => Element = HTMLElement): Element {
  const found = el.querySelector(selector);
  if (!(found instanceof type)) throw new Error(`Found no ${type.name} ${selector}.`);
  return found;
}

// The text of each element that the selector finds in `el`.
export function texts(el: ParentNode, selector: string): string[] {
  return [...el.querySelectorAll(selector)].map((node) => node.textContent);
}

// Types the text into a text or number input as a user does,
// in place of what it held, one keystroke at a time.
// An empty text clears the input.
export function type(input: HTMLInputElement | HTMLTextAreaElement, text: string): void {
  input.focus();
  const steps = text === "" ? [""] : Array.from({ length: text.length }, (_, i) => text.slice(0, i + 1));
  for (const step of steps) {
    setValue(input, step);
    input.dispatchEvent(new Event("input", { bubbles: true }));
  }
}

// The error message below a field of core's React SDK in `el`, or null where
// it shows none.
export function fieldError(el: ParentNode): string | null {
  return el.querySelector('[slot="errorMessage"]')?.textContent ?? null;
}

// What the browser's own number input says of the number in the text, under
// the range and the step given, as the SDK's NumberField says it, or null
// where the number fits them.
export function numberSaid(text: string, { min, max, step }: { min?: number; max?: number; step?: number }): string | null {
  const input = document.createElement("input");
  input.type = "number";
  if (min !== undefined) input.min = String(min);
  if (max !== undefined) input.max = String(max);
  if (step !== undefined) input.step = String(step);
  input.value = text;
  return input.validationMessage === "" ? null : input.validationMessage;
}

export function press(input: HTMLElement, key: string, init: KeyboardEventInit = {}): void {
  input.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, ...init }));
}

// Checks or clears a checkbox as a user does, by clicking it.
export function toggle(checkbox: HTMLInputElement): void {
  checkbox.click();
}

// Picks a file in a file input as a user does.
export function choose(input: HTMLInputElement, file: File): void {
  Object.defineProperty(input, "files", { value: [file], configurable: true });
  input.dispatchEvent(new Event("change", { bubbles: true }));
}

// Drags the file over the element and drops it there, as the browser fires
// the events of a drop.
// The DragEvent of happy-dom leaves out the dataTransfer that it is given, so
// each event carries it as a property of its own.
export function drop(target: HTMLElement, file: File): void {
  for (const name of ["dragenter", "dragover", "drop"]) {
    const transfer = new DataTransfer();
    transfer.items.add(file);
    const event = new DragEvent(name, { bubbles: true, cancelable: true, clientX: 1, clientY: 1 });
    Object.defineProperty(event, "dataTransfer", { value: transfer });
    target.dispatchEvent(event);
  }
}

// The browser's file picker, which shows as a file input is clicked, and in
// which the user picks the file given, or none. It returns what each input
// clicked takes and whether it was on the page then.
export function picking(file: File | undefined): { accept: string; connected: boolean }[] {
  const clicked: { accept: string; connected: boolean }[] = [];
  vi.spyOn(HTMLInputElement.prototype, "click").mockImplementation(function (this: HTMLInputElement) {
    clicked.push({ accept: this.accept, connected: this.isConnected });
    if (file === undefined) this.dispatchEvent(new Event("cancel"));
    else choose(this, file);
  });
  return clicked;
}

// Sets the value through the setter of the element's class rather than of the
// element itself, where React keeps track of the values it sets.
function setValue(element: HTMLInputElement | HTMLTextAreaElement, value: string): void {
  if (!Reflect.set(Object.getPrototypeOf(element) as object, "value", value, element)) {
    throw new Error("The element has no value to set.");
  }
}

// The toggle buttons of the group whose label is the text given, in `el`, by
// their labels, with whether each is picked, as React Aria marks a button of
// a group of one as a radio and of several as a pressed button.
export function toggles(el: ParentNode, label: string): { readonly label: string; readonly picked: boolean }[] {
  return [...toggleGroup(el, label).querySelectorAll("button")].map((button) => ({
    label: button.textContent.trim(),
    picked: button.getAttribute("aria-checked") === "true" || button.getAttribute("aria-pressed") === "true",
  }));
}

// The toggle button of the label given, in the group whose label is the text
// given, in `el`.
export function toggleButton(el: ParentNode, group: string, label: string): HTMLButtonElement {
  const found = [...toggleGroup(el, group).querySelectorAll("button")].find((button) => button.textContent.trim() === label);
  if (found === undefined) throw new Error(`Found no toggle button ${label} in ${group}.`);
  return found;
}

// The group of toggle buttons whose label is the text given, in `el`.
export function toggleGroup(el: ParentNode, label: string): HTMLElement {
  const found = [...el.querySelectorAll<HTMLElement>('[role="radiogroup"], [role="toolbar"]')].find((group) => {
    // The label sits beside the group, on the page or not, as in the element
    // of controls() before a module puts it there.
    const id = group.getAttribute("aria-labelledby") ?? "";
    const root = group.getRootNode() as ParentNode;
    const named = id === "" ? null : root.querySelector(`[id="${CSS.escape(id)}"]`);
    return (named?.textContent.trim() ?? group.getAttribute("aria-label")) === label;
  });
  if (found === undefined) throw new Error(`Found no group of toggle buttons labelled ${label}.`);
  return found;
}
