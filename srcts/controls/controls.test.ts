// Core's inputs drawn through controls(), on the stub of a block's host and of
// a board option's host, as a package that writes its modules without React
// uses them, with no wait for React.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { find, press, texts, type } from "../testing/inputs.ts";
import { holdRequests } from "../testing/requests.ts";
import { chipLabels, labelled, listed, picked } from "../testing/select.ts";
import { controls, PAUSE, strings } from "./index.ts";

// Items of the ids given, which read as their ids.
const ids = (values: readonly string[]) => values.map((id) => ({ id }));

type Form = Readonly<{ label: string; direction: string; by?: readonly string[] }>;

// A block whose fixture lists the states that the tests submit.
const form = (state: Partial<Form>): Form => ({ label: "x", direction: "head", ...state });
const fixture: BlockFixture = {
  states: [form({}), form({ label: "yz" }), form({ direction: "tail" }), form({ by: ["b", "a"] })],
  requests: [],
};

// Does what the user does, and lets React draw what it changed, which React
// does in a microtask.
async function settled(fn: () => void = () => undefined): Promise<void> {
  fn();
  await vi.advanceTimersByTimeAsync(0);
}

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
  ui.comboBox("direction", {
    label: "Direction",
    placeholder: "Choose an end",
    items: [
      { id: "head", name: "Head" },
      { id: "tail", name: "Tail" },
    ],
  });
  ui.comboBox("by", { label: "By columns", selectionMode: "multiple", items: ids(["a", "b"]) });
  document.body.append(ui.el);
  return {
    host,
    ui,
    label: labelled(ui.el, "Label"),
    direction: labelled(ui.el, "Direction"),
    by: labelled(ui.el, "By columns"),
  };
}

describe("the controls of a block's UI", () => {
  test("show the state, and take a state they did not make in the elements they drew", () => {
    const { host, ui, label, direction } = drawn();
    expect([label.value, direction.value]).toEqual(["x", "Head"]);

    host.setState(form({ label: "yz", direction: "tail" }));

    expect(labelled(ui.el, "Label")).toBe(label);
    expect([label.value, direction.value]).toEqual(["yz", "Tail"]);
  });

  test("draw a control before the call that makes or changes it returns, among the module's own elements", () => {
    const host = stubBlockHost<Form>(fixture, { state: form({ by: ["c"] }) });
    const ui = controls(host, abort.signal);
    ui.text("label", { label: "Label" });
    const own = document.createElement("p");
    own.textContent = "A line of the module's own";
    ui.add(own);
    const by = ui.comboBox("by", { label: "By columns", selectionMode: "multiple" });
    const field = labelled(ui.el, "By columns");
    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["c"], null]);

    by.setItems(ids(["a", "b"]));

    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["c (not available)"], "true"]);
    expect(texts(ui.el, "label > span, p, label[for]")).toEqual(["Label", "A line of the module's own", "By columns"]);
  });

  test("show a value as it is until a select's items are given, and as missing where they lack it", async () => {
    const host = stubBlockHost<Form>(fixture, { state: form({ direction: "middle" }) });
    const ui = controls(host, abort.signal);
    const direction = ui.comboBox("direction", { label: "Direction", placeholder: "Choose an end" });
    const field = labelled(ui.el, "Direction");
    // The field shows a pick that the items lack in place of its placeholder.
    const shown = async () => [field.value || field.placeholder, await listed(field, settled), field.getAttribute("aria-invalid")];
    expect(await shown()).toEqual(["middle", [], null]);

    direction.setItems([
      { id: "head", name: "Head" },
      { id: "tail", name: "Tail" },
    ]);

    expect(await shown()).toEqual(["middle (not available)", ["Head", "Tail"], "true"]);
    expect(host.submitted).toEqual([]);
  });

  test("keep the items while the request that gives them asks again, and mark nothing until it answers", async () => {
    const requests = [{ name: "ends", args: {}, options: {}, answer: ["head", "tail"] }];
    const host = stubBlockHost<Form>({ ...fixture, requests }, { state: form({ direction: "middle", by: ["c"] }) });
    const release = holdRequests(host);
    const ui = controls(host, abort.signal);
    const direction = ui.comboBox("direction", { label: "Direction", placeholder: "Choose an end" });
    const by = ui.comboBox("by", { label: "By columns", selectionMode: "multiple" });
    ui.follow("ends", {
      loading() {
        direction.setOutdated(true);
        by.setOutdated(true);
      },
      answer(answer) {
        direction.setItems(ids(strings(answer)));
        by.setItems(ids(strings(answer)));
        direction.setOutdated(false);
        by.setOutdated(false);
      },
    });
    const one = labelled(ui.el, "Direction");
    const several = labelled(ui.el, "By columns");
    const shown = async () => [
      one.value || one.placeholder,
      one.getAttribute("aria-invalid"),
      chipLabels(several),
      several.getAttribute("aria-invalid"),
      await listed(several, settled),
    ];
    expect(await shown()).toEqual(["middle", null, ["c"], null, []]);

    release();
    await settled();
    const marked = ["middle (not available)", "true", ["c (not available)"], "true", ["head", "tail"]];
    expect(await shown()).toEqual(marked);

    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
    expect(await shown()).toEqual(["middle", null, ["c"], null, ["head", "tail"]]);

    release();
    await settled();
    expect(await shown()).toEqual(marked);
    expect(host.submitted).toEqual([]);
  });

  test("submit typing once the user pauses or presses Enter, and keep it while the state changes", async () => {
    const { host, label, direction } = drawn();

    type(label, "yz");
    host.setState(form({ direction: "tail" }));
    expect([label.value, direction.value]).toEqual(["yz", "Tail"]);
    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(host.submitted).toEqual([form({ label: "yz", direction: "tail" })]);

    host.setState(form({}));
    type(label, "yz");
    press(label, "Enter");
    expect(host.submitted).toHaveLength(2);
  });

  test("submit a pick at once, and say why the host refused one, showing the state again", async () => {
    const { host, ui, direction, by } = drawn();

    await picked(direction, settled, "Tail");
    expect(host.submitted).toEqual([form({ direction: "tail" })]);

    host.setState(form({}));
    await picked(by, settled, "a");
    expect(chipLabels(by)).toEqual([]);
    expect(texts(ui.el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("pass the host a notifier with each submit, their inputs' and a widget's that the module binds, since they say why the host refused it, so that core raises no notification of it", async () => {
    const { host, ui, direction } = drawn();
    const submit = vi.spyOn(host, "submit");

    await picked(direction, settled, "Tail");
    host.setState(form({}));
    await ui.bind("label", () => undefined).commit("yz");

    expect(submit.mock.calls.map(([change, opts]) => [change, typeof opts?.notifier])).toEqual([
      [{ direction: "tail" }, "function"],
      [{ label: "yz" }, "function"],
    ]);
  });

  test("submit the picks of several once, as the list closes, after the strings the state holds then", async () => {
    const { host, by } = drawn();

    host.setState(form({ by: ["b"] }));
    await picked(by, settled, "a");

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

  test("submit the picks of several in the order of the items where ordered", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error"] });
    const ui = controls(host, abort.signal);
    ui.comboBox("show_conditions", {
      label: "Message types to display",
      selectionMode: "multiple",
      items: ids(["message", "warning", "error"]),
      ordered: true,
    });
    document.body.append(ui.el);

    await picked(labelled(ui.el, "Message types to display"), settled, "message");

    expect(host.submitted).toEqual([["message", "error"]]);
  });
});
