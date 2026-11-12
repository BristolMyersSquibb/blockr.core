// The pieces that core's basic frontend and its Edit board plugin draw with:
// the searchable dropdown in its single and its multiple form, and the search
// and the pages of a table.

import { act, useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { labelled, listOf, optionOf, optionsOf } from "../testing/combobox.ts";
import { find, press, texts, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { Combobox, MultiCombobox, Pager, TableSearch, useTableView } from "./index.ts";
import type { ComboOption } from "./index.ts";

const OPTIONS: readonly ComboOption[] = [
  { value: "a", label: "Iris", detail: "dataset block", group: "input", keywords: ["flowers"] },
  { value: "b", label: "Head", detail: "head/tail block", group: "transform" },
  { value: "c", label: "Merge", detail: "merge block", group: "transform", badge: "blockr.core" },
  { value: "d", label: "Gone", detail: "a block that cannot be picked", group: "transform", disabled: true },
];

const unmounts: (() => void)[] = [];

afterEach(() => {
  for (const unmount of unmounts.splice(0)) unmount();
  document.body.replaceChildren();
});

// Draws the element in an element on the page, as React does on the page.
function drawn(ui: ReactElement): HTMLElement {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(ui);
  });
  unmounts.push(() => {
    act(() => {
      root.unmount();
    });
  });
  return el;
}

function Single({ seen }: { seen: string[] }): ReactElement {
  const [value, setValue] = useState<string | undefined>(undefined);
  return (
    <Combobox
      label="Block"
      options={OPTIONS}
      value={value}
      hint="The block to pick."
      onChange={(next) => {
        seen.push(next);
        setValue(next);
      }}
    />
  );
}

function Multiple(): ReactElement {
  const [values, setValues] = useState<readonly string[]>(["b"]);
  return <MultiCombobox label="Blocks" options={OPTIONS} values={values} onChange={setValues} />;
}

describe("a searchable dropdown", () => {
  test("lists its options by group as the user opens it, and narrows them by label, detail, group, badge and keywords as the user types", async () => {
    const el = drawn(<Single seen={[]} />);
    const field = labelled(el, "Block");
    expect(field.getAttribute("role")).toBe("combobox");
    expect(field.getAttribute("aria-expanded")).toBe("false");
    expect(listOf(field)).toBeNull();

    await step(() => {
      field.click();
    });
    expect(field.getAttribute("aria-expanded")).toBe("true");
    expect(optionsOf(field)).toEqual(["input: Iris", "transform: Head", "transform: Merge", "transform: Gone"]);

    for (const [search, found] of [
      ["flow", ["input: Iris"]],
      ["tail", ["transform: Head"]],
      ["TRANS", ["transform: Head", "transform: Merge", "transform: Gone"]],
      ["core", ["transform: Merge"]],
    ] as const) {
      await step(() => {
        type(field, search);
      });
      expect(optionsOf(field)).toEqual(found);
    }

    await step(() => {
      type(field, "nothing like it");
    });
    expect(optionsOf(field)).toEqual([]);
    expect(listOf(field)?.textContent).toBe("No option matches.");
  });

  test("picks the option that the user clicks, or marks with Down and Up and picks with Enter, skipping one that cannot be picked", async () => {
    const seen: string[] = [];
    const el = drawn(<Single seen={seen} />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
    });
    await step(() => {
      optionOf(field, "Head").click();
    });
    expect(seen).toEqual(["b"]);
    // The list closes, and the field shows the label of the option picked.
    expect(listOf(field)).toBeNull();
    expect(field.value).toBe("Head");

    // Down opens the list on the option picked, and moves on from it.
    await step(() => {
      press(field, "ArrowDown");
    });
    const active = (): string | null => field.ownerDocument.getElementById(field.getAttribute("aria-activedescendant") ?? "")?.textContent ?? null;
    expect(active()).toContain("Head");
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(active()).toContain("Merge");
    // The option that cannot be picked is passed over, and Down goes round.
    await step(() => {
      press(field, "ArrowDown");
    });
    expect(active()).toContain("Iris");
    await step(() => {
      press(field, "ArrowUp");
    });
    expect(active()).toContain("Merge");
    await step(() => {
      press(field, "Enter");
    });
    expect(seen).toEqual(["b", "c"]);
    expect(field.value).toBe("Merge");

    // A click on the option that cannot be picked picks nothing.
    await step(() => {
      field.click();
    });
    await step(() => {
      optionOf(field, "Gone").click();
    });
    expect(seen).toEqual(["b", "c"]);
  });

  test("closes on Escape and as the field loses the focus, picking nothing, and shows the option picked again", async () => {
    const seen: string[] = [];
    const el = drawn(<Single seen={seen} />);
    const field = labelled(el, "Block");

    await step(() => {
      field.click();
      type(field, "Ir");
    });
    await step(() => {
      press(field, "Escape");
    });
    expect(listOf(field)).toBeNull();
    expect(field.value).toBe("");

    await step(() => {
      field.click();
      type(field, "Me");
    });
    await step(() => {
      field.dispatchEvent(new FocusEvent("blur"));
      field.dispatchEvent(new FocusEvent("focusout", { bubbles: true }));
    });
    expect(listOf(field)).toBeNull();
    expect(seen).toEqual([]);
  });

  test("names the line below it, a hint or an error, as its description, and marks an error as invalid", () => {
    const el = drawn(<Combobox label="Block" options={OPTIONS} value={undefined} error="Pick a block." onChange={() => undefined} />);
    const field = labelled(el, "Block");
    expect(field.getAttribute("aria-invalid")).toBe("true");
    expect(field.ownerDocument.getElementById(field.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Pick a block.");

    const hinted = drawn(<Single seen={[]} />);
    const other = labelled(hinted, "Block");
    expect(other.getAttribute("aria-invalid")).toBeNull();
    expect(other.ownerDocument.getElementById(other.getAttribute("aria-describedby") ?? "")?.textContent).toBe("The block to pick.");
  });
});

describe("a searchable dropdown of several options", () => {
  test("shows each option picked as a chip, which leaves the list, and keeps the list open to pick more", async () => {
    const el = drawn(<Multiple />);
    const field = labelled(el, "Blocks");
    const chips = (): string[] => texts(el, 'ul[aria-label="Blocks, picked"] li > span');
    expect(chips()).toEqual(["Head"]);

    await step(() => {
      field.click();
    });
    expect(optionsOf(field)).toEqual(["input: Iris", "transform: Merge", "transform: Gone"]);
    await step(() => {
      type(field, "mer");
    });
    await step(() => {
      optionOf(field, "Merge").click();
    });
    expect(chips()).toEqual(["Head", "Merge"]);
    // The search clears, and the list stays open on the options left.
    expect(field.value).toBe("");
    expect(optionsOf(field)).toEqual(["input: Iris", "transform: Gone"]);

    await step(() => {
      press(field, "Enter");
    });
    expect(chips()).toEqual(["Head", "Merge", "Iris"]);
  });

  test("takes an option out by its chip's button, or the last one by Backspace in the empty field", async () => {
    const el = drawn(<Multiple />);
    const field = labelled(el, "Blocks");
    await step(() => {
      field.click();
    });
    await step(() => {
      optionOf(field, "Iris").click();
    });
    expect(texts(el, 'ul[aria-label="Blocks, picked"] li > span')).toEqual(["Head", "Iris"]);

    await step(() => {
      find(el, 'button[aria-label="Remove Head"]').click();
    });
    expect(texts(el, 'ul[aria-label="Blocks, picked"] li > span')).toEqual(["Iris"]);
    await step(() => {
      press(field, "Backspace");
    });
    expect(el.querySelector('ul[aria-label="Blocks, picked"]')).toBeNull();
  });
});

function Rows({ rows }: { rows: readonly string[] }): ReactElement {
  const view = useTableView(rows, (row) => row, 3);
  return (
    <div>
      <TableSearch label="Search rows" view={view} />
      <ul aria-label="Rows">
        {view.rows.map((row) => (
          <li key={row}>{row}</li>
        ))}
      </ul>
      <Pager label="Pages of rows" view={view} />
    </div>
  );
}

describe("a table's search and pages", () => {
  const ROWS = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta"];

  test("show a page of rows at a time, which the pager moves between", async () => {
    const el = drawn(<Rows rows={ROWS} />);
    const shown = (): string[] => texts(el, 'ul[aria-label="Rows"] li');
    const pager = (): HTMLElement => find(el, 'nav[aria-label="Pages of rows"]');
    const button = (text: string): HTMLButtonElement => find(pager(), `button:${text === "Previous" ? "first-of-type" : "last-of-type"}`, HTMLButtonElement);

    expect(shown()).toEqual(["alpha", "beta", "gamma"]);
    expect(pager().querySelector("span")?.textContent).toBe("Rows 1–3 of 7");
    expect(button("Previous").disabled).toBe(true);

    await step(() => {
      button("Next").click();
    });
    await step(() => {
      button("Next").click();
    });
    expect(shown()).toEqual(["eta"]);
    expect(pager().querySelector("span")?.textContent).toBe("Rows 7–7 of 7");
    expect(button("Next").disabled).toBe(true);
  });

  test("narrow the rows by the search, from the first page, and draw no pager while one page holds them all", async () => {
    const el = drawn(<Rows rows={ROWS} />);
    await step(() => {
      find(el, 'nav[aria-label="Pages of rows"] button:last-of-type').click();
    });
    await step(() => {
      type(find(el, "search input", HTMLInputElement), "ETA");
    });
    expect(texts(el, 'ul[aria-label="Rows"] li')).toEqual(["beta", "zeta", "eta"]);
    expect(el.querySelector("nav")).toBeNull();
  });

  test("show the last page where the rows of the page shown are gone", async () => {
    const el = document.createElement("div");
    document.body.append(el);
    const root = createRoot(el);
    unmounts.push(() => {
      act(() => {
        root.unmount();
      });
    });
    act(() => {
      root.render(<Rows rows={ROWS} />);
    });
    for (let i = 0; i < 2; i++) {
      await step(() => {
        find(el, 'nav[aria-label="Pages of rows"] button:last-of-type').click();
      });
    }
    await step(() => {
      root.render(<Rows rows={ROWS.slice(0, 4)} />);
    });
    expect(texts(el, 'ul[aria-label="Rows"] li')).toEqual(["delta"]);
  });
});
