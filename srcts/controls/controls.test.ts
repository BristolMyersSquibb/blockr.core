// Core's inputs drawn through controls(), on the stub of a block's host and of
// a board option's host, as a package that writes its modules without React
// uses them, with no wait for React.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { find, pick, press, texts, toggle, type } from "../testing/inputs.ts";
import { holdRequests } from "../testing/requests.ts";
import { asChoices, controls, PAUSE, strings } from "./index.ts";

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

  test("draw a control before the call that makes or changes it returns, among the module's own elements", () => {
    const host = stubBlockHost<Form>(fixture, { state: form({}) });
    const ui = controls(host, abort.signal);
    ui.text("label", { label: "Label" });
    const own = document.createElement("p");
    own.textContent = "A line of the module's own";
    ui.add(own);
    const by = ui.checklist("by", { legend: "By columns" });
    expect(ui.el.querySelectorAll('input[type="checkbox"]')).toHaveLength(0);

    by.setChoices(["a", "b"]);

    expect(ui.el.querySelectorAll('input[type="checkbox"]')).toHaveLength(2);
    expect(texts(ui.el, "label > span, p, legend")).toEqual(["Label", "A line of the module's own", "By columns"]);
  });

  test("show a value as it is until a select's choices are given, and as missing where they lack it", () => {
    const host = stubBlockHost<Form>(fixture, { state: form({ direction: "middle" }) });
    const ui = controls(host, abort.signal);
    const direction = ui.select("direction", { label: "Direction", placeholder: "Choose an end" });
    const select = find(ui.el, "select", HTMLSelectElement);
    const shown = () => [select.value, texts(select, "option"), select.getAttribute("aria-invalid")];
    expect(shown()).toEqual(["middle", ["middle"], "false"]);

    direction.setChoices([
      { value: "head", label: "Head" },
      { value: "tail", label: "Tail" },
    ]);

    expect(shown()).toEqual(["middle", ["Head", "Tail", "middle (not available)"], "true"]);
    expect(host.submitted).toEqual([]);
  });

  test("keep the choices while the request that gives them asks again, and mark nothing until it answers", async () => {
    const requests = [{ name: "ends", args: {}, options: {}, answer: ["head", "tail"] }];
    const host = stubBlockHost<Form>({ ...fixture, requests }, { state: form({ direction: "middle", by: ["c"] }) });
    const release = holdRequests(host);
    const ui = controls(host, abort.signal);
    const direction = ui.select("direction", { label: "Direction", placeholder: "Choose an end" });
    const by = ui.checklist("by", { legend: "By columns" });
    ui.follow("ends", {
      loading() {
        direction.setOutdated(true);
        by.setOutdated(true);
      },
      answer(answer) {
        direction.setChoices(asChoices(strings(answer)));
        by.setChoices(strings(answer));
        direction.setOutdated(false);
        by.setOutdated(false);
      },
    });
    const select = find(ui.el, "select", HTMLSelectElement);
    const shown = () => [
      texts(select, "option"),
      select.getAttribute("aria-invalid"),
      texts(ui.el, "fieldset label"),
      find(ui.el, 'input[value="c"]').getAttribute("aria-invalid"),
    ];
    expect(shown()).toEqual([["middle"], "false", ["c"], "false"]);

    release();
    await vi.advanceTimersByTimeAsync(0);
    const marked = [["head", "tail", "middle (not available)"], "true", ["head", "tail", "c (not available)"], "true"];
    expect(shown()).toEqual(marked);

    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
    expect(shown()).toEqual([["head", "tail", "middle"], "false", ["head", "tail", "c"], "false"]);

    release();
    await vi.advanceTimersByTimeAsync(0);
    expect(shown()).toEqual(marked);
    expect(host.submitted).toEqual([]);
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

  test("submit typing still pending as the module unmounts, and empty their element", () => {
    const { host, ui, label } = drawn();

    type(label, "yz");
    ui.cleanup();

    expect(host.submitted).toEqual([form({ label: "yz" })]);
    expect(ui.el.childElementCount).toBe(0);
  });

  test("end as core aborts the signal, as they do on cleanup", () => {
    const { host, ui, label } = drawn();

    type(label, "yz");
    abort.abort();

    expect(host.submitted).toEqual([form({ label: "yz" })]);
    expect(ui.el.childElementCount).toBe(0);
  });
});

describe("the reason of a block that does not run", () => {
  test("shows below each control bound to a state arg that it names, in muted text, as the host announces the block's status", () => {
    const { host, ui } = drawn();
    const hints = (): string[] => [...ui.el.querySelectorAll("p")].map((p) => p.textContent);
    expect(hints()).toEqual([]);

    host.setContext({
      status: { ...host.context().status, status: "invalid", reason: { message: "Choose an end.", arg: ["direction"] } },
    });
    expect(hints()).toEqual(["Choose an end."]);
    expect(find(ui.el, "p").className).toContain("bc:text-text-muted");

    host.setContext({ status: { ...host.context().status, status: "ready", reason: null } });
    expect(hints()).toEqual([]);

    // Once the controls end, they no longer follow the host.
    ui.cleanup();
    host.setContext({
      status: { ...host.context().status, status: "invalid", reason: { message: "Choose an end.", arg: ["direction"] } },
    });
    expect(ui.el.children).toHaveLength(0);
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
    expect(() => {
      ui.checkbox("dark_mode", { label: "Dark mode" });
    }).toThrow('The host of the board option "n_rows" holds no value "dark_mode".');
  });

  test("submit a number above the most while setOutdated() says that it may be for older data, and mark none", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const ui = controls(host, abort.signal);
    const rows = ui.number("n_rows", { label: "Preview rows", min: 1 });
    rows.setMax(30);
    rows.setOutdated(true);
    document.body.append(ui.el);
    const input = find(ui.el, "input", HTMLInputElement);
    const shown = () => [input.value, input.max, input.getAttribute("aria-invalid")];

    type(input, "50");
    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(shown()).toEqual(["50", "", "false"]);
    expect(host.submitted).toEqual([50]);

    rows.setOutdated(false);
    expect(shown()).toEqual(["50", "30", "true"]);
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
