// What a user does to an input in a test, as a browser has it happen,
// so that an input drawn by React hears it as one drawn without it does:
// the value set through the element's own setter,
// which React does not track as a value it set itself,
// and then the event that the browser fires.
// The browser's file picker is played the same way.

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

export function press(input: HTMLElement, key: string, init: KeyboardEventInit = {}): void {
  input.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, ...init }));
}

// Picks a choice of a select as a user does.
export function pick(select: HTMLSelectElement, value: string): void {
  setValue(select, value);
  select.dispatchEvent(new Event("change", { bubbles: true }));
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
function setValue(element: HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement, value: string): void {
  if (!Reflect.set(Object.getPrototypeOf(element) as object, "value", value, element)) {
    throw new Error("The element has no value to set.");
  }
}
