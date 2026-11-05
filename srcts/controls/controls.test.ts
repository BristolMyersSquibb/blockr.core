// The inputs drawn without React, on the stub of a block's host and of a
// board option's host, as a package that draws with the DOM alone uses them.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { find, pick, press, texts, toggle, type } from "../testing/inputs.ts";
import { controls, PAUSE } from "./index.ts";

type Form = Readonly<{ label: string; direction: string; by?: readonly string[] }>;

// A block whose fixture lists the states that the tests submit.
const form = (state: Partial<Form>): Form => ({ label: "x", direction: "head", ...state });
const fixture: BlockFixture = {
  states: [form({}), form({ label: "yz" }), form({ direction: "tail" }), form({ by: ["b", "a"] })],
  requests: [],
};

let abort: AbortController;

beforeEach(() => {
  vi.useFakeTimers();
  abort = new AbortController();
});

afterEach(() => {
  abort.abort();
  vi.useRealTimers();
  document.body.replaceChildren();
});

function drawn(state: Form = form({})) {
  const host = stubBlockHost<Form>(fixture, { state });
  const ui = controls(host, abort.signal);
  ui.text("label", { label: "Label" });
  ui.select("direction", {
    label: "Direction",
    placeholder: "Choose an end",
    choices: [
      { value: "head", label: "Head" },
      { value: "tail", label: "Tail" },
    ],
  });
  ui.checklist("by", { legend: "By columns", choices: ["a", "b"] });
  document.body.append(ui.el);
  return {
    host,
    ui,
    label: find(ui.el, 'input[type="text"]', HTMLInputElement),
    direction: find(ui.el, "select", HTMLSelectElement),
    by: (value: string) => find(ui.el, `input[value="${value}"]`, HTMLInputElement),
  };
}

describe("the controls of a block's UI", () => {
  test("show the state, and take a state they did not make in the elements they drew", () => {
    const { host, ui, label, direction } = drawn();
    expect([label.value, direction.value]).toEqual(["x", "head"]);

    host.setState(form({ label: "yz", direction: "tail" }));

    expect(find(ui.el, 'input[type="text"]')).toBe(label);
    expect([label.value, direction.value]).toEqual(["yz", "tail"]);
  });

  test("submit typing once the user pauses or presses Enter, and keep it while the state changes", async () => {
    const { host, label, direction } = drawn();

    type(label, "yz");
    host.setState(form({ direction: "tail" }));
    expect([label.value, direction.value]).toEqual(["yz", "tail"]);
    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(host.submitted).toEqual([form({ label: "yz", direction: "tail" })]);

    host.setState(form({}));
    type(label, "yz");
    press(label, "Enter");
    expect(host.submitted).toHaveLength(2);
  });

  test("submit a pick at once, and say why the host refused one, showing the state again", async () => {
    const { host, ui, direction, by } = drawn();

    pick(direction, "tail");
    expect(host.submitted).toEqual([form({ direction: "tail" })]);

    host.setState(form({}));
    toggle(by("a"));
    await vi.advanceTimersByTimeAsync(0);
    expect(by("a").checked).toBe(false);
    expect(texts(ui.el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("submit a checked string after those the state holds, from the state as it is then", () => {
    const { host, by } = drawn();

    host.setState(form({ by: ["b"] }));
    toggle(by("a"));

    expect(host.submitted).toEqual([form({ by: ["b", "a"] })]);
  });

  test("submit typing still pending as the module unmounts", () => {
    const { host, ui, label } = drawn();

    type(label, "yz");
    ui.cleanup();

    expect(host.submitted).toEqual([form({ label: "yz" })]);
  });
});

describe("the controls of a board option's UI", () => {
  test("bind the board option's value under its id, and nothing else", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const ui = controls(host, abort.signal);
    ui.number("n_rows", { label: "Preview rows", min: 1 });
    document.body.append(ui.el);
    const input = find(ui.el, "input", HTMLInputElement);

    type(input, "0");
    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(input.getAttribute("aria-invalid")).toBe("true");
    type(input, "50");
    await vi.advanceTimersByTimeAsync(PAUSE);
    host.setValue(10);

    expect(host.submitted).toEqual([50]);
    expect(input.value).toBe("10");
    expect(() => ui.bind("dark_mode", () => undefined)).toThrow(
      'The host of the board option "n_rows" holds no value "dark_mode".',
    );
  });

  test("submit the checked strings in the order of the choices where ordered", () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error"] });
    const ui = controls(host, abort.signal);
    ui.checklist("show_conditions", { legend: "Message types to display", choices: ["message", "warning", "error"], ordered: true });
    document.body.append(ui.el);

    toggle(find(ui.el, 'input[value="message"]', HTMLInputElement));

    expect(host.submitted).toEqual([["message", "error"]]);
  });
});
