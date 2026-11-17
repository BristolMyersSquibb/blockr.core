// The select of core's React SDK, in its forms that pick for their caller:
// one choice, and several, which reports the picks as its list closes.
// Its bound form is tested with the other inputs (./inputs.test.tsx), and
// through controls() (../controls/controls.test.ts).

import { act, useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test, vi } from "vitest";
import type { Choice } from "../binding/index.ts";
import { press, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import {
  fieldOf,
  filterOf,
  labelled,
  listFromField,
  listNote,
  markedLabel,
  optionFromLabel,
  optionLabels,
  pickedLabels,
} from "../testing/select.ts";
import { listSpot } from "./place.ts";
import { MAX_ROWS, Select, summary } from "./select.tsx";

const CHOICES: readonly Choice[] = [
  { value: "a", label: "Iris", detail: "dataset block", group: "input", keywords: ["flowers"] },
  { value: "b", label: "Head", detail: "head/tail block", group: "transform" },
  { value: "c", label: "Merge", detail: "merge block", group: "transform", badge: "blockr.core" },
  { value: "d", label: "Gone", detail: "a block that cannot be picked", group: "transform", disabled: true },
];

const COLUMNS: readonly Choice[] = ["mpg", "cyl", "disp", "hp"].map((value) => ({ value, label: value }));

const unmounts: (() => void)[] = [];

afterEach(() => {
  for (const unmount of unmounts.splice(0)) unmount();
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

// Draws the element in an element on the page, as React does on the page.
function drawn(ui: ReactElement): { el: HTMLElement; unmount: () => void } {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(ui);
  });
  let mounted = true;
  const unmount = (): void => {
    if (!mounted) return;
    mounted = false;
    act(() => {
      root.unmount();
    });
  };
  unmounts.push(unmount);
  return { el, unmount };
}

// Clicks the element as a pointer does, which a click that the keyboard
// makes tells apart by its detail.
function click(target: HTMLElement): void {
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
}

function One({ seen, choices = CHOICES, initial }: { seen: string[]; choices?: readonly Choice[]; initial?: string }): ReactElement {
  const [value, setValue] = useState<string | undefined>(initial);
  return (
    <Select
      label="Block"
      choices={choices}
      value={value}
      hint="The block to pick."
      onChange={(next) => {
        seen.push(next);
        setValue(next);
      }}
    />
  );
}

function Several({
  seen,
  choices = CHOICES,
  initial = ["b"],
  min,
}: {
  seen: (readonly string[])[];
  choices?: readonly Choice[];
  initial?: readonly string[];
  min?: number;
}): ReactElement {
  const [value, setValue] = useState<readonly string[]>(initial);
  return (
    <Select
      multiple
      label="Blocks"
      choices={choices}
      value={value}
      min={min}
      onChange={(next) => {
        seen.push(next);
        setValue(next);
      }}
    />
  );
}

describe("a select of one choice", () => {
  test("lists its choices by group as the user opens it, and narrows them to those whose label, detail, value, group, badge or keyword holds what the user types", async () => {
    const { el } = drawn(<One seen={[]} />);
    const field = labelled(el, "Block");
    expect(field.getAttribute("role")).toBe("combobox");
    expect(field.getAttribute("aria-expanded")).toBe("false");
    expect(listFromField(field)).toBeNull();

    await step(() => {
      field.click();
    });
    expect(field.getAttribute("aria-expanded")).toBe("true");
    expect(optionLabels(field)).toEqual(["input: Iris", "transform: Head", "transform: Merge", "transform: Gone"]);

    for (const [search, found] of [
      ["flow", ["input: Iris"]],
      ["tail", ["transform: Head"]],
      ["TRANS", ["transform: Head", "transform: Merge", "transform: Gone"]],
      ["core", ["transform: Merge"]],
      ["merge blo", ["transform: Merge"]],
      // The query is one text, which "block merge" is in no text of a choice.
      ["block merge", []],
    ] as const) {
      await step(() => {
        type(field, search);
      });
      expect(optionLabels(field)).toEqual(found);
    }

    await step(() => {
      type(field, "nothing like it");
    });
    expect(optionLabels(field)).toEqual([]);
    expect(listNote(field)).toBe("No matches");
  });

  test("picks the choice that the user clicks, or marks with Down and Up and picks with Enter, skipping one that cannot be picked", async () => {
    const seen: string[] = [];
    const { el } = drawn(<One seen={seen} />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
    });
    await step(() => {
      optionFromLabel(field, "Head").click();
    });
    expect(seen).toEqual(["b"]);
    // The list closes, and the field shows the label of the pick.
    expect(listFromField(field)).toBeNull();
    expect(field.value).toBe("Head");

    // Down opens the list on the pick, and moves on from it.
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(markedLabel(field)).toBe("Head");
    expect(pickedLabels(field)).toEqual(["Head"]);
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(markedLabel(field)).toBe("Merge");
    // The choice that cannot be picked is passed over, and Down goes round.
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(markedLabel(field)).toBe("Iris");
    await step(() => {
      press(field, "ArrowUp");
    });
    expect(markedLabel(field)).toBe("Merge");
    await step(() => {
      press(field, "Enter");
    });
    expect(seen).toEqual(["b", "c"]);
    expect(field.value).toBe("Merge");

    // A click on the choice that cannot be picked picks nothing, and a click
    // on the pick does not call onChange again.
    await step(() => {
      field.click();
    });
    await step(() => {
      optionFromLabel(field, "Gone").click();
    });
    await step(() => {
      optionFromLabel(field, "Merge").click();
    });
    expect(seen).toEqual(["b", "c"]);
  });

  test("marks no row while the pointer opened the list until the user presses a key, which shows the row the list opened on", async () => {
    const { el } = drawn(<One seen={[]} initial="c" />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
    });
    expect(field.getAttribute("aria-activedescendant")).toBeNull();
    // The field shows the pick, muted, while the user has typed nothing.
    expect([field.value, field.placeholder]).toEqual(["", "Merge"]);
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(markedLabel(field)).toBe("Merge");
    await step(() => {
      press(field, "Enter");
    });
    expect(listFromField(field)).toBeNull();
  });

  test("opens on Enter and on Space, and keeps Enter from submitting the form that holds it", async () => {
    const submitted = vi.fn();
    const { el } = drawn(
      <form
        onSubmit={(event) => {
          event.preventDefault();
          submitted();
        }}
      >
        <One seen={[]} />
      </form>,
    );
    const field = labelled(el, "Block");

    for (const key of ["Enter", " "]) {
      await step(() => {
        press(field, key);
      });
      expect(markedLabel(field)).toBe("Iris");
      await step(() => {
        press(field, "Escape");
      });
      expect(listFromField(field)).toBeNull();
    }
    expect(submitted).not.toHaveBeenCalled();
  });

  test("closes on Escape and as the field loses the focus, picking nothing, and shows the pick again", async () => {
    const seen: string[] = [];
    const { el } = drawn(<One seen={seen} initial="a" />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
      type(field, "He");
    });
    let escape: KeyboardEvent | undefined;
    await step(() => {
      escape = new KeyboardEvent("keydown", { key: "Escape", bubbles: true, cancelable: true });
      field.dispatchEvent(escape);
    });
    expect(listFromField(field)).toBeNull();
    expect(field.value).toBe("Iris");
    // A dialog or a form that holds the field stays as it is.
    expect(escape?.defaultPrevented).toBe(true);

    await step(() => {
      field.click();
      type(field, "Me");
    });
    await step(() => {
      field.dispatchEvent(new FocusEvent("blur"));
      field.dispatchEvent(new FocusEvent("focusout", { bubbles: true }));
    });
    expect(listFromField(field)).toBeNull();
    expect(field.value).toBe("Iris");
    expect(seen).toEqual([]);
  });

  test("names the line below it, a hint or an error, as its description, and marks an error as invalid", () => {
    const { el } = drawn(<Select label="Block" choices={CHOICES} value={undefined} error="Pick a block." onChange={() => undefined} />);
    const field = labelled(el, "Block");
    expect(field.getAttribute("aria-invalid")).toBe("true");
    expect(field.ownerDocument.getElementById(field.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Pick a block.");

    const hinted = drawn(<One seen={[]} />).el;
    const other = labelled(hinted, "Block");
    expect(other.getAttribute("aria-invalid")).toBeNull();
    expect(other.ownerDocument.getElementById(other.getAttribute("aria-describedby") ?? "")?.textContent).toBe("The block to pick.");
  });

  test("says that its choices load while they are undefined, `empty` where there are none, and shows a value meanwhile as it is", async () => {
    const { el } = drawn(<Select label="Column" choices={undefined} value="mpg" empty="The data has no column." onChange={() => undefined} />);
    const field = labelled(el, "Column");
    expect(field.value).toBe("mpg");
    await step(() => {
      field.click();
    });
    expect([optionLabels(field), listNote(field)]).toEqual([[], "Loading…"]);

    const none = labelled(drawn(<Select label="Other" choices={[]} value={undefined} empty="The data has no column." onChange={() => undefined} />).el, "Other");
    await step(() => {
      none.click();
    });
    expect(listNote(none)).toBe("The data has no column.");
  });

  test("shows at most MAX_ROWS rows, and says how many more the filter keeps", async () => {
    const many = Array.from({ length: MAX_ROWS + 50 }, (_, i) => ({ value: `v${String(i)}`, label: `Value ${String(i)}` }));
    const { el } = drawn(<One seen={[]} choices={many} />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
    });
    expect(optionLabels(field)).toHaveLength(MAX_ROWS);
    expect(listNote(field)).toBe("50 more. Type to narrow the list.");

    await step(() => {
      type(field, "Value 24");
    });
    expect(optionLabels(field)).toEqual(["Value 24", ...Array.from({ length: 10 }, (_, i) => `Value 24${String(i)}`)]);
    expect(listNote(field)).toBeNull();
  });

  test("is greyed out while disabled, and never opens", async () => {
    const { el } = drawn(<Select label="Input" choices={COLUMNS} value="mpg" disabled onChange={() => undefined} />);
    const field = labelled(el, "Input");
    expect(field.disabled).toBe(true);

    await step(() => {
      field.click();
      press(field, "ArrowDown");
    });
    expect(listFromField(field)).toBeNull();
  });
});

describe("a select of several choices", () => {
  test("shows the picks as text: one or two by name, more as a count with the names after it, and None for none", () => {
    expect(summary([], 4, "None")).toEqual({ lead: "None", rest: "", empty: true });
    expect(summary(["Head"], 4, "None")).toEqual({ lead: "Head", rest: "", empty: false });
    expect(summary(["Head", "Merge"], 4, "None")).toEqual({ lead: "Head, Merge", rest: "", empty: false });
    expect(summary(["Head", "Merge", "Iris"], 1200, "None")).toEqual({ lead: "3 of 1,200", rest: "Head, Merge, Iris", empty: false });
    // While the choices are unknown, the names alone show.
    expect(summary(["Head", "Merge", "Iris"], undefined, "None")).toEqual({ lead: "Head, Merge, Iris", rest: "", empty: false });

    const { el } = drawn(<Several seen={[]} initial={["b", "c", "a"]} />);
    const field = fieldOf(el, "Blocks");
    expect(field).toBeInstanceOf(HTMLButtonElement);
    expect(field.textContent).toBe("3 of 4Head, Merge, Iris");
    expect(drawn(<Several seen={[]} initial={[]} />).el.querySelector("button")?.textContent).toBe("None");
  });

  test("lists the picks on top under Selected, in the order picked, and then the other choices under Other or under their group", async () => {
    const choices = [...CHOICES, { value: "e", label: "Notes" }];
    const { el } = drawn(<Several seen={[]} choices={choices} initial={["c", "a"]} />);
    const field = fieldOf(el, "Blocks");

    await step(() => {
      field.click();
    });
    expect(field.getAttribute("aria-expanded")).toBe("true");
    expect(optionLabels(field)).toEqual(["Selected: Merge", "Selected: Iris", "Other: Notes", "transform: Head", "transform: Gone"]);
    expect(pickedLabels(field)).toEqual(["Merge", "Iris"]);
    expect(listFromField(field)?.parentElement?.textContent).toContain("2 of 5 selected");
  });

  test("keeps the list open and the filter text on a pick, and reports the picks once, as the list closes", async () => {
    const seen: (readonly string[])[] = [];
    const { el } = drawn(<Several seen={seen} />);
    const field = fieldOf(el, "Blocks");

    await step(() => {
      click(field);
    });
    const filter = filterOf(field);
    expect(document.activeElement).toBe(filter);
    // The pointer opened the list, so it marks no row.
    expect(markedLabel(field)).toBeNull();
    await step(() => {
      type(filter, "merge");
    });
    expect(optionLabels(field)).toEqual(["transform: Merge"]);
    await step(() => {
      optionFromLabel(field, "Merge").click();
    });
    expect([filter.value, optionLabels(field)]).toEqual(["merge", ["Selected: Merge"]]);
    await step(() => {
      type(filter, "");
    });
    expect(optionLabels(field)).toEqual(["Selected: Head", "Selected: Merge", "input: Iris", "transform: Gone"]);
    await step(() => {
      optionFromLabel(field, "Head").click();
    });
    expect(optionLabels(field)).toEqual(["Selected: Merge", "input: Iris", "transform: Head", "transform: Gone"]);
    expect(seen).toEqual([]);

    await step(() => {
      press(filter, "Escape");
    });
    expect(listFromField(field)).toBeNull();
    expect(seen).toEqual([["c"]]);
    expect(document.activeElement).toBe(field);
    expect(field.textContent).toBe("Merge");

    // A list that closes with the picks as they were reports nothing.
    await step(() => {
      click(field);
    });
    await step(() => {
      optionFromLabel(field, "Iris").click();
    });
    await step(() => {
      optionFromLabel(field, "Iris").click();
    });
    await step(() => {
      filterOf(field).blur();
    });
    expect(listFromField(field)).toBeNull();
    expect(seen).toEqual([["c"]]);
  });

  test("moves the row with the arrows, which Enter ticks, as Space does while the filter is empty", async () => {
    const seen: (readonly string[])[] = [];
    const { el } = drawn(<Several seen={seen} initial={[]} />);
    const field = fieldOf(el, "Blocks");

    // A click that the keyboard makes opens the list on its first row.
    await step(() => {
      field.click();
    });
    const filter = filterOf(field);
    expect(markedLabel(field)).toBe("Iris");
    await step(() => {
      press(filter, "Enter");
    });
    // The row moves under Selected, and the keyboard's row follows it.
    expect([optionLabels(field)[0], markedLabel(field)]).toEqual(["Selected: Iris", "Iris"]);
    await step(() => {
      press(filter, "ArrowDown");
    });
    expect(markedLabel(field)).toBe("Head");
    await step(() => {
      press(filter, " ");
    });
    await step(() => {
      press(filter, "Tab");
    });
    expect(listFromField(field)).toBeNull();
    expect(document.activeElement).toBe(field);
    expect(seen).toEqual([["a", "b"]]);
  });

  test("ticks with All and unticks with None the choices that the filter keeps", async () => {
    const seen: (readonly string[])[] = [];
    const { el } = drawn(<Several seen={seen} initial={["c"]} />);
    const field = fieldOf(el, "Blocks");
    const button = (text: string): HTMLButtonElement => {
      const found = [...(listFromField(field)?.parentElement?.querySelectorAll("button") ?? [])].find((b) => b.textContent === text);
      if (found === undefined) throw new Error(`Found no button ${text}.`);
      return found;
    };

    await step(() => {
      click(field);
    });
    await step(() => {
      type(filterOf(field), "block");
    });
    // The choice that cannot be picked stays unticked.
    await step(() => {
      button("All 4 shown").click();
    });
    expect(pickedLabels(field)).toEqual(["Merge", "Iris", "Head"]);
    expect(button("All 4 shown").disabled).toBe(true);
    await step(() => {
      type(filterOf(field), "head");
    });
    await step(() => {
      button("None shown").click();
    });
    expect(pickedLabels(field)).toEqual([]);
    await step(() => {
      type(filterOf(field), "");
    });
    expect(pickedLabels(field)).toEqual(["Merge", "Iris"]);
    await step(() => {
      button("None").click();
    });
    expect(pickedLabels(field)).toEqual([]);
    expect(button("None").disabled).toBe(true);
    await step(() => {
      press(filterOf(field), "Escape");
    });
    expect(seen).toEqual([[]]);
  });

  test("keeps the fewest picks that `min` asks for", async () => {
    const seen: (readonly string[])[] = [];
    const { el } = drawn(<Several seen={seen} initial={["a", "b"]} min={1} />);
    const field = fieldOf(el, "Blocks");
    const none = (): HTMLButtonElement | undefined =>
      [...(listFromField(field)?.parentElement?.querySelectorAll("button") ?? [])].find((b) => b.textContent === "None");

    await step(() => {
      click(field);
    });
    expect(none()?.getAttribute("aria-disabled")).toBe("true");
    await step(() => {
      none()?.click();
    });
    await step(() => {
      optionFromLabel(field, "Iris").click();
    });
    expect(pickedLabels(field)).toEqual(["Head"]);
    // The last pick does not untick.
    expect(optionFromLabel(field, "Head").getAttribute("aria-disabled")).toBe("true");
    await step(() => {
      optionFromLabel(field, "Head").click();
    });
    expect(pickedLabels(field)).toEqual(["Head"]);
    await step(() => {
      press(filterOf(field), "Escape");
    });
    expect(seen).toEqual([["b"]]);
  });

  test("shows its filter box once it holds more than eight choices, and filters as the user types on the closed field", async () => {
    const few = drawn(<Several seen={[]} choices={COLUMNS} initial={[]} />).el;
    const fewField = fieldOf(few, "Blocks");
    await step(() => {
      click(fewField);
    });
    expect(filterOf(fewField).className).toBe("bc:sr-only");

    const nine = Array.from({ length: 9 }, (_, i) => ({ value: `c${String(i)}`, label: `Column ${String(i)}` }));
    const many = drawn(<Several seen={[]} choices={nine} initial={[]} />).el;
    const manyField = fieldOf(many, "Blocks");
    await step(() => {
      manyField.dispatchEvent(new KeyboardEvent("keydown", { key: "8", bubbles: true }));
    });
    expect(filterOf(manyField).className).not.toContain("bc:sr-only");
    expect([filterOf(manyField).value, optionLabels(manyField)]).toEqual(["8", ["Column 8"]]);
  });

  test("reports the picks where it goes with its list open", async () => {
    const seen: (readonly string[])[] = [];
    const { el, unmount } = drawn(<Several seen={seen} />);
    const field = fieldOf(el, "Blocks");

    await step(() => {
      click(field);
    });
    await step(() => {
      optionFromLabel(field, "Iris").click();
    });
    unmount();

    expect(seen).toEqual([["b", "a"]]);
  });
});

describe("the list of a select", () => {
  test("shows in the top layer where the browser has the Popover API, and hides again as it closes", async () => {
    const shown: Element[] = [];
    const hidden: Element[] = [];
    Object.defineProperty(HTMLElement.prototype, "showPopover", {
      configurable: true,
      value(this: HTMLElement) {
        shown.push(this);
      },
    });
    Object.defineProperty(HTMLElement.prototype, "hidePopover", {
      configurable: true,
      value(this: HTMLElement) {
        hidden.push(this);
      },
    });
    try {
      const { el } = drawn(<One seen={[]} />);
      const field = labelled(el, "Block");
      await step(() => {
        field.click();
      });
      const list = listFromField(field)?.parentElement;
      expect([shown, list?.getAttribute("popover")]).toEqual([[list], "manual"]);
      // The list stays inside the select's element in the DOM.
      expect(el.contains(list ?? null)).toBe(true);

      await step(() => {
        press(field, "Escape");
      });
      expect(hidden).toEqual([list]);
    } finally {
      Reflect.deleteProperty(HTMLElement.prototype, "showPopover");
      Reflect.deleteProperty(HTMLElement.prototype, "hidePopover");
    }
  });

  test("names the field by an anchor name of its own, which CSS anchor positioning places the list by", async () => {
    const { el } = drawn(
      <>
        <One seen={[]} />
        <Several seen={[]} />
      </>,
    );
    const one = labelled(el, "Block");
    const several = fieldOf(el, "Blocks");
    const names = [one.parentElement?.style.anchorName, several.style.anchorName];
    expect(names.every((name) => name?.startsWith("--bc-select-"))).toBe(true);
    expect(new Set(names).size).toBe(2);

    await step(() => {
      one.click();
    });
    expect(listFromField(one)?.parentElement?.style.positionAnchor).toBe(names[0]);
  });

  test("is placed by script where the browser has no anchor positioning", async () => {
    // The test DOM, happy-dom, makes the window's CSS anew on each read.
    vi.spyOn(Object.getPrototypeOf(CSS) as { supports: () => boolean }, "supports").mockReturnValue(false);
    const { el } = drawn(<One seen={[]} />);
    const field = labelled(el, "Block");
    vi.spyOn(field.parentElement ?? field, "getBoundingClientRect").mockReturnValue(DOMRect.fromRect({ x: 100, y: 50, width: 240, height: 30 }));

    await step(() => {
      field.click();
    });
    const list = listFromField(field)?.parentElement;
    expect([list?.style.minWidth, list?.style.left, list?.style.top]).toEqual(["240px", "100px", "84px"]);
  });

  test("sits 4px below the field, or above it where it fits there but not below, and 8px inside the window", () => {
    const view = { width: 800, height: 600 };
    const field = (top: number, left = 100) => ({ left, top, bottom: top + 30, width: 200 });
    expect(listSpot(field(50), { width: 200, height: 300 }, view)).toEqual({ left: 100, top: 84 });
    expect(listSpot(field(400), { width: 200, height: 300 }, view)).toEqual({ left: 100, top: 96 });
    // Where it fits on neither side, it stays below, inside the window.
    expect(listSpot(field(200), { width: 200, height: 500 }, view)).toEqual({ left: 100, top: 92 });
    expect(listSpot(field(50, 700), { width: 200, height: 100 }, view)).toEqual({ left: 592, top: 84 });
  });
});
