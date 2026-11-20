// The inputs of core's React SDK that pick among choices the user sees whole,
// React Aria's in core's theme: the checkbox, the switch and the toggle
// button, each of a boolean, and the groups of several: the checkbox group,
// the radio group and the toggle button group, each bound to a block's state
// arg on the stub of a block's host.
// The rest, such as the keys and the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { StubBlockHost } from "../stubs/index.ts";
import { texts, toggleButton, toggles } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { labelled, notAvailable } from "../testing/select.ts";
import type { BlockHost } from "../types/hosts.ts";
import { Checkbox, CheckboxGroup, Radio, RadioGroup, Switch, ToggleButton, ToggleButtonGroup } from "./index.ts";

type Picks = Readonly<{
  keep: boolean;
  on: boolean;
  bold: boolean;
  end?: string;
  levels?: readonly string[];
  side?: string;
  marks?: readonly string[];
}>;

const picks = (state: Partial<Picks>): Picks => ({ keep: false, on: false, bold: false, ...state });

// A host whose fixture lists the state given and those that a test submits.
function host(state: Picks, ...submits: Picks[]): StubBlockHost<Picks> {
  return stubBlockHost<Picks>({ states: [state, ...submits], requests: [] }, { state });
}

afterEach(() => {
  document.body.replaceChildren();
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () =>
      step(() => {
        root.unmount();
      }),
  };
}

// Clicks the element, and waits for the submit it makes to settle.
async function click(el: HTMLElement): Promise<void> {
  await act(async () => {
    el.click();
    await Promise.resolve();
  });
}

function Picker({ host, outdated = false, ordered = false }: { host: BlockHost<Picks>; outdated?: boolean; ordered?: boolean }): ReactElement {
  return (
    <>
      <Checkbox bound={useValue(host, "keep")} description="Whether it keeps the rows">
        Keep
      </Checkbox>
      <Switch bound={useValue(host, "on")}>On</Switch>
      <ToggleButton bound={useValue(host, "bold")}>Bold</ToggleButton>
      <RadioGroup bound={useValue(host, "end")} label="End" outdated={outdated}>
        <Radio value="head">Head</Radio>
        <Radio value="tail">Tail</Radio>
      </RadioGroup>
      <CheckboxGroup bound={useValue(host, "levels")} label="Levels" outdated={outdated} ordered={ordered}>
        {["message", "warning", "error"].map((level) => (
          <Checkbox key={level} value={level}>
            {level}
          </Checkbox>
        ))}
      </CheckboxGroup>
      <ToggleButtonGroup bound={useValue(host, "side")} label="Side" outdated={outdated}>
        <ToggleButton id="left">Left</ToggleButton>
        <ToggleButton id="right">Right</ToggleButton>
      </ToggleButtonGroup>
      <ToggleButtonGroup bound={useValue(host, "marks")} selectionMode="multiple" label="Marks" outdated={outdated} ordered={ordered}>
        <ToggleButton id="dot">Dot</ToggleButton>
        <ToggleButton id="ring">Ring</ToggleButton>
        <ToggleButton id="star">Star</ToggleButton>
      </ToggleButtonGroup>
    </>
  );
}

// The labels of the checkboxes or radios of the group whose label is the one
// given, with whether each is picked.
function boxes(el: HTMLElement, group: string): { readonly label: string; readonly picked: boolean }[] {
  const found = [...el.querySelectorAll('[role="group"], [role="radiogroup"]')].find(
    (node) => el.ownerDocument.getElementById(node.getAttribute("aria-labelledby") ?? "")?.textContent === group,
  );
  return [...(found?.querySelectorAll("label") ?? [])].map((label) => ({
    label: label.textContent.trim(),
    picked: label.querySelector("input")?.checked === true,
  }));
}

describe("a bound checkbox, switch or toggle button", () => {
  test("shows the boolean that the state holds, and submits the user's pick at once", async () => {
    const start = picks({});
    const stub = host(start, picks({ keep: true }), picks({ keep: true, on: true }), picks({ keep: true, on: true, bold: true }));
    const { el } = await draw(<Picker host={stub} />);
    const bold = (): string | null => [...el.querySelectorAll("button")].find((b) => b.textContent === "Bold")?.getAttribute("aria-pressed") ?? null;
    expect([labelled(el, "Keep").checked, labelled(el, "On").checked, bold()]).toEqual([false, false, "false"]);

    await click(labelled(el, "Keep"));
    await click(labelled(el, "On"));
    await click(find(el, "Bold"));

    expect(stub.submitted).toEqual([picks({ keep: true }), picks({ keep: true, on: true }), picks({ keep: true, on: true, bold: true })]);
    expect([labelled(el, "Keep").checked, labelled(el, "On").checked, bold()]).toEqual([true, true, "true"]);
    expect(labelled(el, "On").getAttribute("role")).toBe("switch");
  });

  test("takes a state it did not make, and shows the state again where the host refused the pick, saying why below itself", async () => {
    const stub = host(picks({}), picks({ on: true }));
    const { el } = await draw(<Picker host={stub} />);
    const keep = labelled(el, "Keep");

    await step(() => {
      stub.setState(picks({ on: true }));
    });
    expect([labelled(el, "Keep"), keep.checked, labelled(el, "On").checked]).toEqual([keep, false, true]);

    await click(keep);
    expect(keep.checked).toBe(false);
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("names its description as the checkbox's", async () => {
    const { el } = await draw(<Picker host={host(picks({}))} />);
    const keep = labelled(el, "Keep");

    expect(el.ownerDocument.getElementById(keep.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Whether it keeps the rows");
  });
});

describe("a bound group", () => {
  test("of radios submits the pick at once", async () => {
    const stub = host(picks({ end: "head" }), picks({ end: "tail" }));
    const { el } = await draw(<Picker host={stub} />);
    expect(boxes(el, "End")).toEqual([
      { label: "Head", picked: true },
      { label: "Tail", picked: false },
    ]);

    await click(labelled(el, "Tail"));

    expect(stub.submitted).toEqual([picks({ end: "tail" })]);
    expect(boxes(el, "End")).toEqual([
      { label: "Head", picked: false },
      { label: "Tail", picked: true },
    ]);
  });

  test("of checkboxes submits each pick at once, built from the state as it is then, and in the order of its checkboxes where ordered", async () => {
    const stub = host(picks({ levels: ["error"] }), picks({ levels: ["error", "message"] }), picks({ levels: ["warning", "error", "message"] }));
    const { el } = await draw(<Picker host={stub} />);

    await click(labelled(el, "message"));
    expect(stub.submitted).toEqual([picks({ levels: ["error", "message"] })]);

    // The assistant adds a level, which the next pick keeps.
    await step(() => {
      stub.setState(picks({ levels: ["warning", "error"] }));
    });
    await click(labelled(el, "message"));
    expect(stub.submitted.at(-1)).toEqual(picks({ levels: ["warning", "error", "message"] }));

    const sorted = host(picks({ levels: ["error"] }), picks({ levels: ["message", "error"] }));
    const { el: other } = await draw(<Picker host={sorted} ordered />);
    await click(labelled(other, "message"));
    expect(sorted.submitted).toEqual([picks({ levels: ["message", "error"] })]);
  });

  test("of toggle buttons of one submits the pick at once, and unsets the state arg as the user unpicks it", async () => {
    const stub = host(picks({ side: "left" }), picks({ side: "right" }), picks({}));
    const { el } = await draw(<Picker host={stub} />);
    expect(toggles(el, "Side")).toEqual([
      { label: "Left", picked: true },
      { label: "Right", picked: false },
    ]);

    await click(toggleButton(el, "Side", "Right"));
    await click(toggleButton(el, "Side", "Right"));

    expect(stub.submitted).toEqual([picks({ side: "right" }), picks({})]);
    expect(Object.hasOwn(stub.state(), "side")).toBe(false);
    expect(toggles(el, "Side").map((side) => side.picked)).toEqual([false, false]);
  });

  test("of toggle buttons of several submits each pick at once, built from the state as it is then, and in the order of its buttons where ordered", async () => {
    const stub = host(picks({ marks: ["star"] }), picks({ marks: ["star", "dot"] }), picks({ marks: ["dot"] }));
    const { el } = await draw(<Picker host={stub} />);

    await click(toggleButton(el, "Marks", "Dot"));
    await click(toggleButton(el, "Marks", "Star"));

    expect(stub.submitted).toEqual([picks({ marks: ["star", "dot"] }), picks({ marks: ["dot"] })]);

    const sorted = host(picks({ marks: ["star"] }), picks({ marks: ["dot", "star"] }));
    const { el: other } = await draw(<Picker host={sorted} ordered />);
    await click(toggleButton(other, "Marks", "Dot"));
    expect(sorted.submitted).toEqual([picks({ marks: ["dot", "star"] })]);
  });

  test("draws no item for a value that none of its items holds, marks itself invalid and says below itself that it is not available, but not while outdated", async () => {
    const state = picks({ end: "middle", levels: ["error", "debug"], side: "up", marks: ["dot", "cross"] });
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Picker host={host(state)} outdated={outdated} />);
      const seen = {
        end: boxes(el, "End"),
        levels: boxes(el, "Levels"),
        side: toggles(el, "Side"),
        marks: toggles(el, "Marks"),
        // The groups that mark themselves invalid, by their roles.
        invalid: [...el.querySelectorAll("[role][data-invalid]")].map((node) => node.getAttribute("role")),
        lines: notAvailable(el),
        // The buttons that drop the values that a group of several lacks.
        removes: [...el.querySelectorAll("button")].flatMap((button) => (button.textContent === "Remove" ? [button.getAttribute("aria-label")] : [])),
      };
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual({
      end: [
        { label: "Head", picked: false },
        { label: "Tail", picked: false },
      ],
      levels: [
        { label: "message", picked: false },
        { label: "warning", picked: false },
        { label: "error", picked: true },
      ],
      side: [
        { label: "Left", picked: false },
        { label: "Right", picked: false },
      ],
      marks: [
        { label: "Dot", picked: true },
        { label: "Ring", picked: false },
        { label: "Star", picked: false },
      ],
      invalid: ["radiogroup", "group", "radiogroup", "toolbar"],
      lines: ["Not available: middle", "Not available: debug", "Not available: up", "Not available: cross"],
      removes: ["Remove debug", "Remove cross"],
    });
    const outdated = await shown(true);
    expect([outdated.invalid, outdated.lines, outdated.removes]).toEqual([[], [], []]);
    expect([outdated.levels.length, outdated.marks.length]).toEqual([3, 3]);
  });

  test("keeps a value that its items lack as the user picks another item of a group of several, and drops it by Remove", async () => {
    const start = picks({ levels: ["error", "debug"], marks: ["dot", "cross"] });
    const stub = host(
      start,
      picks({ levels: ["error", "debug", "message"], marks: ["dot", "cross"] }),
      picks({ levels: ["error", "message"], marks: ["dot", "cross"] }),
      picks({ levels: ["error", "message"], marks: ["dot"] }),
    );
    const { el } = await draw(<Picker host={stub} />);

    await click(labelled(el, "message"));
    expect(notAvailable(el)).toEqual(["Not available: debug", "Not available: cross"]);
    await click(find(el, "Remove", "Remove debug"));
    await click(find(el, "Remove", "Remove cross"));

    expect(stub.submitted).toEqual([
      picks({ levels: ["error", "debug", "message"], marks: ["dot", "cross"] }),
      picks({ levels: ["error", "message"], marks: ["dot", "cross"] }),
      picks({ levels: ["error", "message"], marks: ["dot"] }),
    ]);
    expect([notAvailable(el), el.querySelectorAll("[role][data-invalid]").length]).toEqual([[], 0]);
  });

  test("drops a value that its items lack from a group of one as the user picks an item", async () => {
    const stub = host(picks({ end: "middle", side: "up" }), picks({ end: "head", side: "up" }), picks({ end: "head", side: "left" }));
    const { el } = await draw(<Picker host={stub} />);

    await click(labelled(el, "Head"));
    await click(toggleButton(el, "Side", "Left"));

    expect(stub.submitted).toEqual([picks({ end: "head", side: "up" }), picks({ end: "head", side: "left" })]);
    expect(notAvailable(el)).toEqual([]);
  });
});

// The button of the text given, and of the accessible name given where it has
// one, as a toggle button outside a group or the button that drops a value.
function find(el: HTMLElement, text: string, name?: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find(
    (button) => button.textContent === text && (name === undefined || button.getAttribute("aria-label") === name),
  );
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}
