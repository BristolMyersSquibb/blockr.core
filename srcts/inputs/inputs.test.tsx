// Core's inputs drawn with React, each bound with useValue(), and requests
// followed with useRequest(), on the stubs of a block's host and of a board
// option's host.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { PAUSE } from "../binding/index.ts";
import type { Choice } from "../binding/index.ts";
import { useRequest, useValue } from "../react/index.ts";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture, StubBlockHost } from "../stubs/index.ts";
import { find, pick, press, texts, toggle, type } from "../testing/inputs.ts";
import "../testing/react.ts";
import type { BlockHost, OptionHost } from "../types/hosts.ts";
import { Checkbox, Checklist, NumberInput, Select, Stack, TextInput } from "./index.ts";

type Form = Readonly<{ n: number; direction: string; label: string; by?: readonly string[]; keep: boolean }>;

// A block whose fixture lists the states that the tests submit.
const form = (state: Partial<Form>): Form => ({ n: 6, direction: "head", label: "x", keep: false, ...state });
const fixture: BlockFixture = {
  states: [
    form({}),
    form({ n: 10 }),
    form({ n: 20, direction: "tail" }),
    form({ direction: "tail" }),
    form({ label: "yz" }),
    form({ by: ["a"] }),
    form({ by: ["b", "a"] }),
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
      <NumberInput
        bound={useValue(host, "n")}
        label="Number of rows"
        min={0}
        max={Array.isArray(max) && typeof max[0] === "number" ? max[0] : undefined}
      />
      <Select
        bound={useValue(host, "direction")}
        label="Direction"
        placeholder="Choose an end"
        choices={[
          { value: "head", label: "Head" },
          { value: "tail", label: "Tail" },
        ]}
      />
      <TextInput bound={useValue(host, "label")} label="Label" />
      <Checklist bound={useValue(host, "by")} legend="By columns" choices={["a", "b"]} />
      <Checkbox bound={useValue(host, "keep")} label="Keep" />
    </Stack>
  );
}

function block(state: Form = form({})) {
  const host: StubBlockHost<Form> = stubBlockHost<Form>(fixture, { state });
  const drawn = draw(<BlockForm host={host} />);
  const inputs = {
    n: find(drawn.el, 'input[type="number"]', HTMLInputElement),
    direction: find(drawn.el, "select", HTMLSelectElement),
    label: find(drawn.el, 'input[type="text"]', HTMLInputElement),
    by: (value: string) => find(drawn.el, `fieldset input[value="${value}"]`, HTMLInputElement),
    keep: find(drawn.el, ":scope > div > label:last-of-type input", HTMLInputElement),
  };
  return { host, ...drawn, inputs };
}

describe("the inputs of a block's UI", () => {
  test("show the state, take the most the request gives, and submit nothing until the user acts", async () => {
    const { host, inputs } = block();
    await wait(0);

    expect([inputs.n.value, inputs.direction.value, inputs.label.value]).toEqual(["6", "head", "x"]);
    expect(inputs.n.max).toBe("150");
    expect(host.submitted).toEqual([]);
  });

  test("take a state they did not make in the elements they drew", () => {
    const { host, el, inputs } = block();

    act(() => {
      host.setState(form({ n: 20, direction: "tail" }));
    });

    expect(find(el, 'input[type="number"]')).toBe(inputs.n);
    expect([inputs.n.value, inputs.direction.value]).toEqual(["20", "tail"]);
  });

  test("submit what the user types once the user pauses, and keep it while the state changes", async () => {
    const { host, inputs } = block();

    act(() => {
      type(inputs.n, "20");
    });
    act(() => {
      host.setState(form({ direction: "tail" }));
    });
    expect([inputs.n.value, inputs.direction.value]).toEqual(["20", "tail"]);

    await wait(PAUSE - 1);
    expect(host.submitted).toEqual([]);
    await wait(1);
    expect(host.submitted).toEqual([form({ n: 20, direction: "tail" })]);
    expect(inputs.n.value).toBe("20");
  });

  test("submit typing at once on Enter and as the user leaves, and nothing that the input does not take", async () => {
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
    expect(inputs.n.getAttribute("aria-invalid")).toBe("false");
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

    act(() => {
      pick(inputs.direction, "tail");
    });
    expect(host.submitted).toEqual([form({ direction: "tail" })]);

    await act(async () => {
      toggle(inputs.keep);
      await Promise.resolve();
    });
    expect(inputs.keep.checked).toBe(false);
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("submit a checklist's string after those the state holds, from the state as it is then", async () => {
    const { host, inputs } = block(form({ by: ["a"] }));

    act(() => {
      host.setState(form({ by: ["b"] }));
    });
    await act(async () => {
      toggle(inputs.by("a"));
      await Promise.resolve();
    });

    expect(host.submitted).toEqual([form({ by: ["b", "a"] })]);
  });

  test("submit typing still pending as the component unmounts", () => {
    const { host, unmount, inputs } = block();

    act(() => {
      type(inputs.label, "yz");
    });
    unmount();

    expect(host.submitted).toEqual([form({ label: "yz" })]);
  });
});

// The UIs of core's board options, each bound to the board option's value.
function Rows({ host }: { host: OptionHost<number> }): ReactElement {
  return <NumberInput bound={useValue(host)} label="Preview rows" min={1} />;
}

function Conditions({ host }: { host: OptionHost<string[]> }): ReactElement {
  return (
    <Checklist
      bound={useValue(host)}
      legend="Message types to display"
      choices={["message", "warning", "error"]}
      ordered
    />
  );
}

const modes: readonly Choice[] = [
  { value: "light", label: "Light" },
  { value: "dark", label: "Dark" },
];

function Mode({ host, choices }: { host: OptionHost<string>; choices?: readonly Choice[] }): ReactElement {
  return <Select bound={useValue(host)} label="Light/dark mode" placeholder="Choose a mode" choices={choices} />;
}

describe("the inputs of a board option's UI", () => {
  test("bind the board option's value, a number of at least 1", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const { el } = draw(<Rows host={host} />);
    const input = find(el, "input", HTMLInputElement);
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

  test("submit the checked severities in the order of the choices", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error"] });
    const { el } = draw(<Conditions host={host} />);

    await act(async () => {
      toggle(find(el, 'input[value="message"]', HTMLInputElement));
      await Promise.resolve();
    });

    expect(texts(el, "label")).toEqual(["message", "warning", "error"]);
    expect(host.submitted).toEqual([["message", "error"]]);
  });

  test("show a string that the choices lack as missing, and let the user clear it", async () => {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value: ["error", "debug"] });
    const { el } = draw(<Conditions host={host} />);
    const debug = find(el, 'input[value="debug"]', HTMLInputElement);

    expect(texts(el, "label")).toEqual(["message", "warning", "error", "debug (not available)"]);
    expect([debug.checked, debug.getAttribute("aria-invalid")]).toEqual([true, "true"]);
    expect(find(el, 'input[value="error"]').getAttribute("aria-invalid")).toBe("false");

    await act(async () => {
      toggle(debug);
      await Promise.resolve();
    });

    expect(host.submitted).toEqual([["error"]]);
    expect(texts(el, "label")).toEqual(["message", "warning", "error"]);
  });

  test("show a mode that the choices lack as it is until they are known, and as missing while they lack it", () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "auto" });
    const { el, redraw } = draw(<Mode host={host} />);
    const select = find(el, "select", HTMLSelectElement);
    const shown = () => [select.value, texts(select, "option"), select.getAttribute("aria-invalid")];
    expect(shown()).toEqual(["auto", ["auto"], "false"]);

    redraw(<Mode host={host} choices={modes} />);
    expect(shown()).toEqual(["auto", ["Light", "Dark", "auto (not available)"], "true"]);

    redraw(<Mode host={host} choices={[...modes, { value: "auto", label: "Auto" }]} />);
    expect(shown()).toEqual(["auto", ["Light", "Dark", "Auto"], "false"]);
    expect(host.submitted).toEqual([]);
  });

  test("submit the mode the user picks", () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "light" });
    const { el } = draw(<Mode host={host} choices={modes} />);

    act(() => {
      pick(find(el, "select", HTMLSelectElement), "dark");
    });

    expect(host.submitted).toEqual(["dark"]);
  });
});

describe("useRequest()", () => {
  function Names({ host, pattern }: { host: OptionHost<string>; pattern: string }): ReactElement {
    const { answer, error } = useRequest(host, "list", { args: { pattern } });
    return <p title={error ?? ""}>{JSON.stringify(answer ?? null)}</p>;
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
});
