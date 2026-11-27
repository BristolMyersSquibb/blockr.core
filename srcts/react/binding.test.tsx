// The rules that core's binding keeps for blockr.ui's inputs, each bound
// with useValue() on the stub of a block's host or of a board option's host,
// and requests that useRequest() follows
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#testing):
// the pause before a submit of what the user typed, the value that an input
// shows while the user types, and the error of a submit that the host
// refuses, with the notifier that goes with each submit and the typing that
// the binding submits as an input unmounts.
// What an input does with its `bound` is blockr.ui's, which its tests check
// against the stub of Bound.

import { act } from "react";
import type { ReactElement } from "react";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { Checkbox, ComboBox, ComboBoxItem, FormLayout, NumberField, TextField } from "@blockr/ui";
import { draw, find, labelled, optionFromLabel, step, texts, toggle, type } from "@blockr/ui/stubs";
import { PAUSE } from "../binding/index.ts";
import { stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture, StubBlockHost } from "../stubs/index.ts";
import { holdRequests } from "../testing/requests.ts";
import type { BlockHost, OptionHost } from "../types/hosts.ts";
import { useRequest, useValue } from "./index.ts";

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
    form({ label: "yz", direction: "tail" }),
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
});

async function wait(ms: number): Promise<void> {
  await act(() => vi.advanceTimersByTimeAsync(ms));
}

function BlockForm({ host }: { host: BlockHost<Form> }): ReactElement {
  const max = useRequest(host, "n_max").answer;
  return (
    <FormLayout>
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
      <Checkbox bound={useValue(host, "keep")}>Keep</Checkbox>
    </FormLayout>
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
    keep: labelled(drawn.el, "Keep"),
  };
  return { host, ...drawn, inputs };
}

describe("the inputs of a block's UI, bound with useValue()", () => {
  test("submit what the user types into a text field once the user pauses, and keep it while the state changes", async () => {
    const { host, inputs } = block();

    act(() => {
      type(inputs.label, "yz");
    });
    act(() => {
      host.setState(form({ direction: "tail" }));
    });
    expect([inputs.label.value, inputs.direction.value]).toEqual(["yz", "Tail"]);

    await wait(PAUSE - 1);
    expect(host.submitted).toEqual([]);
    await wait(1);
    expect(host.submitted).toEqual([form({ label: "yz", direction: "tail" })]);
    expect(inputs.label.value).toBe("yz");
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
  test("submit typing still pending in a text field as the component unmounts", () => {
    const { host, unmount, inputs } = block();

    act(() => {
      type(inputs.label, "yz");
    });
    unmount();

    expect(host.submitted).toEqual([form({ label: "yz" })]);
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
