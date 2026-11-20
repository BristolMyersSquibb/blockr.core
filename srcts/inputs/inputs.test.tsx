// Core's inputs drawn with React, each bound with useValue(), and requests
// followed with useRequest(), on the stubs of a block's host and of a board
// option's host.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { PAUSE } from "../binding/index.ts";
import { useRequest, useValue } from "../react/index.ts";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture, StubBlockHost } from "../stubs/index.ts";
import { fieldError, find, numberSaid, press, texts, toggle, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { chipLabels, labelled, listed, optionFromLabel, optionLabels, removeChip } from "../testing/select.ts";
import { holdRequests } from "../testing/requests.ts";
import type { BlockHost, OptionHost } from "../types/hosts.ts";
import { Checkbox, ComboBox, ComboBoxItem, NumberField, Stack, TextField } from "./index.ts";

type Form = Readonly<{ n: number; direction: string; label: string; by?: readonly string[]; keep: boolean }>;

// A block whose fixture lists the states that the tests submit.
const form = (state: Partial<Form>): Form => ({ n: 6, direction: "head", label: "x", keep: false, ...state });
const fixture: BlockFixture = {
  states: [
    form({}),
    form({ n: 7 }),
    form({ n: 10 }),
    form({ n: 20, direction: "tail" }),
    form({ direction: "tail" }),
    form({ label: "yz" }),
    form({ by: ["a"] }),
    form({ by: ["b", "a"] }),
    form({ by: ["c", "b"] }),
    form({ keep: true }),
  ],
  requests: [{ name: "n_max", args: {}, options: {}, answer: [150] }],
};

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

// Draws the component on the page, draws it again with other props, and
// unmounts it.
function draw(node: ReactElement): { el: HTMLElement; redraw: (next: ReactElement) => void; unmount: () => void } {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  const render = (next: ReactElement): void => {
    act(() => {
      root.render(next);
    });
  };
  render(node);
  return {
    el,
    redraw: render,
    unmount: () => {
      act(() => {
        root.unmount();
      });
    },
  };
}

async function wait(ms: number): Promise<void> {
  await act(() => vi.advanceTimersByTimeAsync(ms));
}

function BlockForm({ host }: { host: BlockHost<Form> }): ReactElement {
  const max = useRequest(host, "n_max").answer;
  return (
    <Stack>
      <NumberField
        bound={useValue(host, "n")}
        label="Number of rows"
        minValue={0}
        maxValue={Array.isArray(max) && typeof max[0] === "number" ? max[0] : undefined}
        step={1}
      />
      <ComboBox
        bound={useValue(host, "direction")}
        label="Direction"
        placeholder="Choose an end"
        defaultItems={[
          { id: "head", name: "Head" },
          { id: "tail", name: "Tail" },
        ]}
      >
        {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
      </ComboBox>
      <TextField bound={useValue(host, "label")} label="Label" />
      <ComboBox bound={useValue(host, "by")} selectionMode="multiple" label="By columns" defaultItems={ids(["a", "b", "c"])}>
        {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
      </ComboBox>
      <Checkbox bound={useValue(host, "keep")} label="Keep" />
    </Stack>
  );
}

// Items of the ids given, which read as their ids.
const ids = (values: readonly string[]) => values.map((id) => ({ id }));

function block(state: Form = form({})) {
  const host: StubBlockHost<Form> = stubBlockHost<Form>(fixture, { state });
  const drawn = draw(<BlockForm host={host} />);
  const inputs = {
    n: labelled(drawn.el, "Number of rows"),
    direction: labelled(drawn.el, "Direction"),
    label: labelled(drawn.el, "Label"),
    by: labelled(drawn.el, "By columns"),
    keep: find(drawn.el, ":scope > div > label:last-of-type input", HTMLInputElement),
  };
  return { host, ...drawn, inputs };
}

describe("the inputs of a block's UI", () => {
  test("draw their borders in border-field, which stands out more than border-default, the border of a chip", () => {
    const { inputs } = block(form({ by: ["a"] }));
    // The colors of an element's border at rest, those of its states aside.
    const borders = (el: Element | null): string[] => (el?.className ?? "").split(" ").filter((name) => name.startsWith("bc:border-border-"));
    const group = (input: HTMLElement): Element | null => input.closest('[role="group"]');

    // A number field and a select draw the border on the group that holds the
    // text field, and a text field on itself.
    expect(borders(group(inputs.n))).toEqual(["bc:border-border-field"]);
    expect(borders(group(inputs.direction))).toEqual(["bc:border-border-field"]);
    expect(borders(inputs.label)).toEqual(["bc:border-border-field"]);
    // So does the line between a number field's text and its steppers.
    expect(borders(group(inputs.n)?.querySelector(":scope > div") ?? null)).toEqual(["bc:border-border-field"]);
    // A chip of a select of several keeps border-default.
    expect(borders(group(inputs.by)?.querySelector('[role="listitem"], [role="row"]') ?? null)).toEqual(["bc:border-border-default"]);
  });

  test("show the state, and submit nothing until the user acts", async () => {
    const { host, inputs } = block();
    await wait(0);

    expect([inputs.n.value, inputs.direction.value, inputs.label.value]).toEqual(["6", "Head", "x"]);
    expect(host.submitted).toEqual([]);
  });

  test("take a state they did not make in the elements they drew", () => {
    const { host, el, inputs } = block();

    act(() => {
      host.setState(form({ n: 20, direction: "tail" }));
    });

    expect(labelled(el, "Number of rows")).toBe(inputs.n);
    expect([inputs.n.value, inputs.direction.value]).toEqual(["20", "Tail"]);
  });

  test("submit what the user types once the user pauses, and keep it while the state changes", async () => {
    const { host, inputs } = block();

    act(() => {
      type(inputs.n, "20");
    });
    act(() => {
      host.setState(form({ direction: "tail" }));
    });
    expect([inputs.n.value, inputs.direction.value]).toEqual(["20", "Tail"]);

    await wait(PAUSE - 1);
    expect(host.submitted).toEqual([]);
    await wait(1);
    expect(host.submitted).toEqual([form({ n: 20, direction: "tail" })]);
    expect(inputs.n.value).toBe("20");
  });

  test("submit typing at once on Enter and as the user leaves, and nothing above the most that the request gives", async () => {
    const { host, inputs } = block();
    await wait(0);

    act(() => {
      type(inputs.n, "200");
    });
    await wait(PAUSE);
    expect(inputs.n.getAttribute("aria-invalid")).toBe("true");
    expect(host.submitted).toEqual([]);

    act(() => {
      type(inputs.n, "10");
      press(inputs.n, "Enter");
    });
    expect(inputs.n.getAttribute("aria-invalid")).toBeNull();
    act(() => {
      type(inputs.label, "yz");
    });
    act(() => {
      inputs.label.blur();
    });
    expect(host.submitted).toEqual([form({ n: 10 }), form({ n: 10, label: "yz" })]);
  });

  test("submit a pick at once, and show again what the state holds where the host refused it", async () => {
    const { host, el, inputs } = block();

    await step(() => {
      inputs.direction.click();
    });
    await step(() => {
      optionFromLabel(inputs.direction, "Tail").click();
    });
    expect(host.submitted).toEqual([form({ direction: "tail" })]);

    await act(async () => {
      toggle(inputs.keep);
      await Promise.resolve();
    });
    expect(inputs.keep.checked).toBe(false);
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("pass the host a notifier with each submit, since each says below itself why the host refused it, so that core raises no notification of it", async () => {
    const { host, inputs } = block();
    const submit = vi.spyOn(host, "submit");

    await step(() => {
      inputs.direction.click();
    });
    await step(() => {
      optionFromLabel(inputs.direction, "Tail").click();
    });
    await act(async () => {
      toggle(inputs.keep);
      await Promise.resolve();
    });

    expect(submit.mock.calls.map(([change, opts]) => [change, typeof opts?.notifier])).toEqual([
      [{ direction: "tail" }, "function"],
      [{ keep: true }, "function"],
    ]);
  });

  test("submit the strings of several once, as the list closes, built from the state as it is then", async () => {
    const { host, inputs } = block(form({ by: ["a"] }));

    await step(() => {
      inputs.by.click();
    });
    // The assistant adds a string while the list is open.
    act(() => {
      host.setState(form({ by: ["c", "a"] }));
    });
    await step(() => {
      optionFromLabel(inputs.by, "a").click();
    });
    await step(() => {
      optionFromLabel(inputs.by, "b").click();
    });
    expect(host.submitted).toEqual([]);
    await step(() => {
      press(inputs.by, "Escape");
    });

    expect(host.submitted).toEqual([form({ by: ["c", "b"] })]);
    expect(chipLabels(inputs.by)).toEqual(["c", "b"]);
  });

  test("submit typing still pending as the component unmounts", () => {
    const { host, unmount, inputs } = block();

    act(() => {
      type(inputs.label, "yz");
      type(inputs.n, "10");
    });
    unmount();

    expect(host.submitted).toEqual([form({ label: "yz" }), form({ n: 10, label: "yz" })]);
  });

  test("submit a number at once as the user steps it, by a button or an arrow key, and show the state again where the host refused it", async () => {
    const { host, el, inputs } = block();
    const steps = [...el.querySelectorAll<HTMLButtonElement>('button[slot="increment"], button[slot="decrement"]')];
    expect(steps.map((button) => button.getAttribute("aria-label"))).toEqual(["Increase", "Decrease"]);

    await step(() => {
      steps[0]?.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true, pointerId: 1, pointerType: "mouse", button: 0 }));
      steps[0]?.dispatchEvent(new PointerEvent("pointerup", { bubbles: true, pointerId: 1, pointerType: "mouse", button: 0 }));
      steps[0]?.click();
    });
    expect(host.submitted).toEqual([form({ n: 7 })]);
    expect(inputs.n.value).toBe("7");

    await step(() => {
      inputs.n.focus();
      press(inputs.n, "ArrowDown");
    });
    expect(host.submitted).toEqual([form({ n: 7 }), form({})]);
    expect(inputs.n.value).toBe("6");

    // The fixture lists no state of 5 rows, so the host refuses it.
    await step(() => {
      press(inputs.n, "ArrowDown");
    });
    expect(host.submitted).toEqual([form({ n: 7 }), form({}), form({ n: 5 })]);
    expect(inputs.n.value).toBe("6");
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });
});

describe("an input bound with two functions", () => {
  // A block whose state arg `conditions` holds a list, of which an input edits
  // the condition at an index.
  type Conditions = Readonly<{ conditions: readonly string[] }>;

  const conditions: BlockFixture = {
    states: [
      { conditions: ["a", "b"] },
      { conditions: ["a", "b", "c"] },
      { conditions: ["a2", "b", "c"] },
      { conditions: ["a2", "b"] },
      { conditions: ["a2", "b2"] },
    ],
    requests: [],
  };

  // The input binds through functions that the component makes anew each time
  // it draws.
  function Condition({ host, index }: { host: BlockHost<Conditions>; index: number }): ReactElement {
    const bound = useValue(
      host,
      (state) => state.conditions[index],
      (value, state) => ({
        conditions: state.conditions.map((condition, i) => (i === index && typeof value === "string" ? value : condition)),
      }),
    );
    return <TextField bound={bound} label="Condition" />;
  }

  function condition(index = 0) {
    const host = stubBlockHost<Conditions>(conditions);
    const drawn = draw(<Condition host={host} index={index} />);
    return { host, ...drawn, input: labelled(drawn.el, "Condition") };
  }

  test("shows the value that the first reads, and submits the change that the second builds once the user pauses, from the state as it is then", async () => {
    const { host, input } = condition();
    expect(input.value).toBe("a");

    act(() => {
      type(input, "a2");
    });
    // The assistant adds a condition before the pause ends.
    act(() => {
      host.setState({ conditions: ["a", "b", "c"] });
    });
    await wait(PAUSE);

    expect(host.submitted).toEqual([{ conditions: ["a2", "b", "c"] }]);
    expect(input.value).toBe("a2");
  });

  test("says below itself why the host refused a change", async () => {
    const { host, el, input } = condition();

    act(() => {
      type(input, "z");
    });
    await wait(PAUSE);

    expect(host.submitted).toEqual([{ conditions: ["z", "b"] }]);
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("keeps what the user types for one submit while the component draws again, with functions that read the same value", async () => {
    const { host, redraw, input } = condition();

    act(() => {
      type(input, "a2");
    });
    redraw(<Condition host={host} index={0} />);
    expect(host.submitted).toEqual([]);
    await wait(PAUSE);

    expect(host.submitted).toEqual([{ conditions: ["a2", "b"] }]);
  });

  test("binds anew where the functions of a render read another value, and submits what the user typed with those before", async () => {
    const { host, redraw, input } = condition();

    act(() => {
      type(input, "a2");
    });
    redraw(<Condition host={host} index={1} />);
    expect(host.submitted).toEqual([{ conditions: ["a2", "b"] }]);
    expect(input.value).toBe("b");

    act(() => {
      type(input, "b2");
    });
    await wait(PAUSE);
    expect(host.submitted).toEqual([{ conditions: ["a2", "b"] }, { conditions: ["a2", "b2"] }]);
  });
});

describe("a number field's text as the user types", () => {
  test("stays as the user typed it after the binding submits it on a pause, and takes the field's format once the user leaves", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      input.focus();
      type(input, "1000");
    });
    await wait(PAUSE);
    expect(host.submitted).toEqual([1000]);
    expect(input.value).toBe("1000");

    act(() => {
      input.blur();
    });
    expect(input.value).toBe("1,000");
    expect(host.submitted).toEqual([1000]);
  });

  test("gives way to a number that the host announces as the user types, other than the one the user typed", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      input.focus();
      type(input, "40");
    });
    await wait(PAUSE);
    expect(host.submitted).toEqual([40]);
    // The assistant sets another number while the user is still in the field.
    act(() => {
      host.setValue(30);
    });
    expect(input.value).toBe("30");

    act(() => {
      input.blur();
    });
    expect(host.submitted).toEqual([40]);
    expect(input.value).toBe("30");
  });
});

// The UIs of core's board options, each bound to the board option's value.
function Rows({ host, max, outdated }: { host: OptionHost<number>; max?: number; outdated?: boolean }): ReactElement {
  return <NumberField bound={useValue(host)} label="Preview rows" minValue={1} maxValue={max} step={1} outdated={outdated} />;
}

// What the browser's own number input says of the number in the text, with
// the range and the step of the board option's field, as the field says it.
const said = (text: string, max?: number): string | null => numberSaid(text, { min: 1, max, step: 1 });

function Conditions({ host, outdated }: { host: OptionHost<string[]>; outdated?: boolean }): ReactElement {
  return (
    <ComboBox
      bound={useValue(host)}
      selectionMode="multiple"
      label="Message types to display"
      defaultItems={ids(["message", "warning", "error"])}
      ordered
      outdated={outdated}
    >
      {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
    </ComboBox>
  );
}


interface Mode {
  readonly id: string;
  readonly name: string;
}

const modes: readonly Mode[] = [
  { id: "light", name: "Light" },
  { id: "dark", name: "Dark" },
];

function Mode({ host, items, outdated }: { host: OptionHost<string>; items?: readonly Mode[]; outdated?: boolean }): ReactElement {
  return (
    <ComboBox bound={useValue(host)} label="Light/dark mode" placeholder="Choose a mode" defaultItems={items} outdated={outdated}>
      {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
    </ComboBox>
  );
}

describe("the inputs of a board option's UI", () => {
  test("bind the board option's value, a number of at least 1", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} />);
    const input = labelled(el, "Preview rows");
    expect(input.value).toBe("25");

    act(() => {
      type(input, "0");
    });
    await wait(PAUSE);
    act(() => {
      type(input, "50");
    });
    await wait(PAUSE);
    act(() => {
      host.setValue(10);
    });

    expect(host.submitted).toEqual([50]);
    expect(input.value).toBe("10");
  });

  test("submit a number above an outdated most, and mark none until it is current, but hold to the least", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el, redraw } = draw(<Rows host={host} max={30} outdated />);
    const input = labelled(el, "Preview rows");
    const shown = () => [input.value, input.getAttribute("aria-invalid")];

    act(() => {
      type(input, "0");
    });
    await wait(PAUSE);
    expect(shown()).toEqual(["0", "true"]);
    act(() => {
      type(input, "50");
    });
    await wait(PAUSE);
    expect(shown()).toEqual(["50", null]);
    expect(host.submitted).toEqual([50]);

    redraw(<Rows host={host} max={30} />);
    expect(shown()).toEqual(["50", "true"]);
    expect(host.submitted).toEqual([50]);
  });

  test("say why a number does not fit while the user types it, in the words of the browser's own number input, and mark it", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} max={30} />);
    const input = labelled(el, "Preview rows");

    const shown: (string | null)[][] = [];
    for (const text of ["50", "0", "2.5", "", "20"]) {
      act(() => {
        type(input, text);
      });
      shown.push([input.getAttribute("aria-invalid"), fieldError(el)]);
    }
    expect(shown).toEqual([
      ["true", said("50", 30)],
      ["true", said("0", 30)],
      ["true", said("2.5", 30)],
      [null, null],
      [null, null],
    ]);
    expect(said("50", 30)).not.toBeNull();
    await wait(PAUSE);
    expect(host.submitted).toEqual([20]);
  });

  test("say why the state does not fit, as once the most shrank", () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el, redraw } = draw(<Rows host={host} max={30} />);
    expect(fieldError(el)).toBeNull();

    redraw(<Rows host={host} max={20} />);
    expect(fieldError(el)).toBe(said("25", 20));
    expect(labelled(el, "Preview rows").value).toBe("25");
    expect(host.submitted).toEqual([]);
  });

  test("go back to the state in place of a number outside the range on Enter and as the user leaves, which clears what it said", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} max={30} />);
    const input = labelled(el, "Preview rows");
    const shown = () => [input.value, fieldError(el)];

    act(() => {
      type(input, "50");
    });
    expect(shown()).toEqual(["50", said("50", 30)]);
    act(() => {
      press(input, "Enter");
    });
    expect(shown()).toEqual(["25", null]);

    act(() => {
      type(input, "60");
    });
    act(() => {
      input.blur();
    });
    expect(shown()).toEqual(["25", null]);
    await wait(PAUSE);
    expect(host.submitted).toEqual([]);
  });

  test("submit a number that fits once the most grows, as the user presses Enter or leaves", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el, redraw } = draw(<Rows host={host} max={30} />);
    const input = labelled(el, "Preview rows");

    act(() => {
      type(input, "50");
    });
    await wait(PAUSE);
    redraw(<Rows host={host} max={100} />);
    expect(host.submitted).toEqual([]);
    act(() => {
      press(input, "Enter");
    });
    expect(host.submitted).toEqual([50]);

    act(() => {
      type(input, "150");
    });
    await wait(PAUSE);
    redraw(<Rows host={host} max={200} />);
    act(() => {
      input.blur();
    });
    expect(host.submitted).toEqual([50, 150]);
    expect(input.value).toBe("150");
  });

  test("submit the picked severities in the order of the choices", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error"] });
    const { el } = draw(<Conditions host={host} />);
    const field = labelled(el, "Message types to display");

    await step(() => {
      field.click();
    });
    expect(optionLabels(field)).toEqual(["message", "warning", "error"]);
    await step(() => {
      optionFromLabel(field, "message").click();
    });
    await step(() => {
      press(field, "Escape");
    });

    expect(host.submitted).toEqual([["message", "error"]]);
  });

  test("show a string that the items lack as missing, whose chip the user removes", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error", "debug"] });
    const { el } = draw(<Conditions host={host} />);
    const field = labelled(el, "Message types to display");

    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["error", "debug (not available)"], "true"]);
    await removeChip(field, step, "debug (not available)");

    expect(host.submitted).toEqual([["error"]]);
    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["error"], null]);
    expect(await listed(field, step)).toEqual(["message", "warning", "error"]);
  });

  test("keep outdated items on screen, and mark no string that they lack until they are current", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error", "debug"] });
    const { el, redraw } = draw(<Conditions host={host} outdated />);
    const field = labelled(el, "Message types to display");

    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["error", "debug"], null]);
    expect(await listed(field, step)).toEqual(["message", "warning", "error"]);

    redraw(<Conditions host={host} />);
    expect([chipLabels(field), field.getAttribute("aria-invalid")]).toEqual([["error", "debug (not available)"], "true"]);
    expect(host.submitted).toEqual([]);
  });

  // The field shows a pick that the items lack in place of its placeholder.
  const shownMode = async (field: HTMLInputElement) => [field.value || field.placeholder, await listed(field, step), field.getAttribute("aria-invalid")];

  test("show a mode that the items lack as it is until they are known, and as missing while they lack it", async () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "auto" });
    const { el, redraw } = draw(<Mode host={host} />);
    const field = labelled(el, "Light/dark mode");
    expect(await shownMode(field)).toEqual(["auto", [], null]);

    redraw(<Mode host={host} items={modes} />);
    expect(await shownMode(field)).toEqual(["auto (not available)", ["Light", "Dark"], "true"]);

    redraw(<Mode host={host} items={[...modes, { id: "auto", name: "Auto" }]} />);
    expect(await shownMode(field)).toEqual(["Auto", ["Light", "Dark", "Auto"], null]);
    expect(host.submitted).toEqual([]);
  });

  test("keep outdated items on screen, and show a mode that they lack as it is until they are current", async () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "auto" });
    const { el, redraw } = draw(<Mode host={host} items={modes} outdated />);
    const field = labelled(el, "Light/dark mode");
    expect(await shownMode(field)).toEqual(["auto", ["Light", "Dark"], null]);

    redraw(<Mode host={host} items={modes} />);
    expect(await shownMode(field)).toEqual(["auto (not available)", ["Light", "Dark"], "true"]);
    expect(host.submitted).toEqual([]);
  });

  test("submit the mode the user picks", async () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "light" });
    const { el } = draw(<Mode host={host} items={modes} />);
    const field = labelled(el, "Light/dark mode");

    await step(() => {
      field.click();
    });
    await step(() => {
      optionFromLabel(field, "Dark").click();
    });

    expect(host.submitted).toEqual(["dark"]);
  });
});

describe("useRequest()", () => {
  function Names({ host, pattern }: { host: OptionHost<string>; pattern: string }): ReactElement {
    const { answer, error, loading } = useRequest(host, "list", { args: { pattern } });
    return (
      <p title={error ?? ""} aria-busy={loading}>
        {JSON.stringify(answer ?? null)}
      </p>
    );
  }

  test("asks again as its parameters change, and keeps the last answer where a request fails", async () => {
    const host = stubOptionHost<string>({
      option: "data_dir",
      value: "/data",
      requests: [{ name: "list", args: { pattern: "a" }, answer: ["a.csv"] }],
    });
    const { el, redraw } = draw(<Names host={host} pattern="a" />);
    await wait(0);
    expect(find(el, "p").textContent).toBe('["a.csv"]');

    redraw(<Names host={host} pattern="b" />);
    await wait(0);

    expect(host.requested.map((call) => call.args)).toEqual([{ pattern: "a" }, { pattern: "b" }]);
    expect(find(el, "p").textContent).toBe('["a.csv"]');
    expect(find(el, "p").title).toContain('No answer is given for the request "list"');
  });

  test("is loading from each ask until its answer or its failure, and keeps the last answer and error meanwhile", async () => {
    const host = stubOptionHost<string>({
      option: "data_dir",
      value: "/data",
      requests: [{ name: "list", args: { pattern: "a" }, answer: ["a.csv"] }],
    });
    const release = holdRequests(host);
    const { el, redraw } = draw(<Names host={host} pattern="a" />);
    const shown = () => {
      const p = find(el, "p");
      return [p.textContent, p.getAttribute("aria-busy"), p.title === "" ? null : "failed"];
    };
    expect(shown()).toEqual(["null", "true", null]);

    release();
    await wait(0);
    expect(shown()).toEqual(['["a.csv"]', "false", null]);

    redraw(<Names host={host} pattern="b" />);
    expect(shown()).toEqual(['["a.csv"]', "true", null]);

    release();
    await wait(0);
    expect(shown()).toEqual(['["a.csv"]', "false", "failed"]);

    redraw(<Names host={host} pattern="a" />);
    expect(shown()).toEqual(['["a.csv"]', "true", "failed"]);

    release();
    await wait(0);
    expect(shown()).toEqual(['["a.csv"]', "false", null]);
  });
});
