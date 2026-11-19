// Core's select, React Aria's ComboBox in core's theme: what core adds to it,
// the chips of the field of several and their order, what the list says where
// it shows no item, the opening by a press in the field, and the binding of a
// block's state arg, on the stub of a block's host.
// The rest, such as the keys, the filter and the drag of a chip by a pointer,
// is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import type { Root } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { press, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import {
  chipLabels,
  cleared,
  clearButton,
  labelled,
  listFromField,
  listNote,
  moveChip,
  optionFromLabel,
  optionLabels,
  picked,
  pickedLabels,
  removeChip,
} from "../testing/select.ts";
import type { BlockHost } from "../types/hosts.ts";
import { ComboBox, ComboBoxItem, ComboBoxSection, Text } from "./index.ts";
import type { ComboBoxProps } from "./index.ts";

interface Fruit {
  readonly id: string;
  readonly name: string;
}

const fruits: readonly Fruit[] = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

const row = (fruit: Fruit): ReactElement => <ComboBoxItem id={fruit.id}>{fruit.name}</ComboBoxItem>;

// The roots that a test drew and has not unmounted, which it unmounts after
// it, so that React Aria forgets their chips, which a later drag would take
// for places to drop on.
const roots = new Set<Root>();

afterEach(async () => {
  for (const root of roots) {
    await step(() => {
      root.unmount();
    });
  }
  roots.clear();
  document.body.replaceChildren();
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  roots.add(root);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () => {
      roots.delete(root);
      return step(() => {
        root.unmount();
      });
    },
  };
}

// Makes every element measure the width and the height given, until the
// function that it returns runs, through getters on the prototype of
// HTMLElement, which React Aria's Virtualizer takes for those of a test that
// sizes the page, where happy-dom measures nothing (../testing/dom.ts).
function measured(width: number, height: number): () => void {
  for (const [name, value] of [
    ["clientWidth", width],
    ["clientHeight", height],
  ] as const) {
    Object.defineProperty(HTMLElement.prototype, name, { configurable: true, get: () => value });
  }
  return () => {
    Reflect.deleteProperty(HTMLElement.prototype, "clientWidth");
    Reflect.deleteProperty(HTMLElement.prototype, "clientHeight");
  };
}

// A select that holds its pick, or its picks, and records each change.
function Held<M extends "single" | "multiple">({
  start,
  seen,
  ...props
}: Omit<ComboBoxProps<Fruit, M>, "value" | "onChange" | "children"> & {
  readonly start: M extends "multiple" ? string[] : string | null;
  readonly seen: unknown[];
}): ReactElement {
  const [value, setValue] = useState<unknown>(start);
  return (
    <ComboBox
      {...(props as ComboBoxProps<Fruit>)}
      value={value as string | null}
      onChange={(next) => {
        seen.push(next);
        setValue(next);
      }}
    >
      {row}
    </ComboBox>
  );
}

describe("a select of one item", () => {
  test("opens its list on a press in the field, as on typing, which narrows the list to the items whose text holds what the user typed", async () => {
    const { el } = await draw(<Held label="Fruit" defaultItems={fruits} start={null} seen={[]} />);
    const field = labelled(el, "Fruit");

    await step(() => {
      field.click();
    });
    expect(optionLabels(field)).toEqual(["Apple", "Banana", "Cherry"]);
    await step(() => {
      press(field, "Escape");
    });
    expect(listFromField(field)).toBeNull();

    await step(() => {
      type(field, "an");
    });
    expect(optionLabels(field)).toEqual(["Banana"]);
  });

  test("calls onChange with the id of the item that the user picks, and shows the item's text", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(<Held label="Fruit" defaultItems={fruits} start="apple" seen={seen} />);
    const field = labelled(el, "Fruit");
    expect(field.value).toBe("Apple");

    await picked(field, step, "Banana");

    expect(seen).toEqual(["banana"]);
    expect([field.value, listFromField(field)]).toEqual(["Banana", null]);
  });

  test("says that it loads while it has no items yet, that nothing matches where the filter keeps none, and what renderEmptyState says where it has none", async () => {
    const shown = async (node: ReactElement, typed?: string): Promise<string | null> => {
      const { el, unmount } = await draw(node);
      const field = labelled(el, "Fruit");
      await step(() => {
        if (typed === undefined) field.click();
        else type(field, typed);
      });
      const note = listNote(field);
      await unmount();
      return note;
    };

    expect(await shown(<ComboBox label="Fruit">{row}</ComboBox>)).toBe("Loading…");
    expect(await shown(<ComboBox label="Fruit" defaultItems={fruits}>{row}</ComboBox>, "kiwi")).toBe("No matches");
    expect(await shown(<ComboBox label="Fruit" defaultItems={[]}>{row}</ComboBox>)).toBe("No options");
    expect(await shown(<ComboBox label="Fruit" defaultItems={[]} renderEmptyState={() => "No fruit grows here."}>{row}</ComboBox>)).toBe(
      "No fruit grows here.",
    );
    expect(await shown(<ComboBox label="Fruit" defaultItems={[]} outdated>{row}</ComboBox>)).toBe("Loading…");
  });

  test("draws only the rows in view, through React Aria's Virtualizer, however many items it holds", async () => {
    const many = Array.from({ length: 500 }, (_, i) => ({ id: `fruit-${String(i)}`, name: `Fruit ${String(i)}` }));
    const restore = measured(300, 200);
    try {
      const { el } = await draw(
        <ComboBox label="Fruit" defaultItems={many}>
          {row}
        </ComboBox>,
      );
      const field = labelled(el, "Fruit");
      await step(() => {
        field.click();
      });

      const drawn = optionLabels(field);
      expect(drawn[0]).toBe("Fruit 0");
      // Rows of 32 pixels, with those that React Aria draws beyond the view.
      expect(drawn.length).toBeGreaterThan(200 / 32);
      expect(drawn.length).toBeLessThan(50);
    } finally {
      restore();
    }
  });

  test("lists its items under the headings of their sections, each named by its label and described by its description", async () => {
    const sections = [
      { id: "sweet", title: "Sweet", items: fruits.slice(0, 2) },
      { id: "sour", title: "Sour", items: [{ id: "lemon", name: "Lemon" }] },
    ];
    const { el } = await draw(
      <ComboBox label="Fruit" defaultItems={sections}>
        {(section) => (
          <ComboBoxSection id={section.id} title={section.title} items={section.items}>
            {(fruit) => (
              <ComboBoxItem id={fruit.id} textValue={fruit.name}>
                <Text slot="label">{fruit.name}</Text>
                <Text slot="description">{`${fruit.id} · a fruit`}</Text>
              </ComboBoxItem>
            )}
          </ComboBoxSection>
        )}
      </ComboBox>,
    );
    const field = labelled(el, "Fruit");

    await step(() => {
      field.click();
    });

    expect(optionLabels(field)).toEqual(["Sweet: Apple", "Sweet: Banana", "Sour: Lemon"]);
    const lemon = optionFromLabel(field, "Lemon");
    expect(document.getElementById(lemon.getAttribute("aria-describedby") ?? "")?.textContent).toBe("lemon · a fruit");
  });

  test("names its description and its error message as the field's, and marks an error as invalid", async () => {
    const { el } = await draw(
      <ComboBox label="Fruit" defaultItems={fruits} description="Pick one." isInvalid errorMessage="Not this one.">
        {row}
      </ComboBox>,
    );
    const field = labelled(el, "Fruit");
    const said = (field.getAttribute("aria-describedby") ?? "").split(" ").map((id) => document.getElementById(id)?.textContent);

    expect([said, field.getAttribute("aria-invalid")]).toEqual([["Pick one.", "Not this one."], "true"]);
  });

  test("keeps an Escape that closes its list from what holds the select, as a dialog that closes on Escape", async () => {
    const heard: boolean[] = [];
    const { el } = await draw(
      <div
        onKeyDown={(event) => {
          if (event.key === "Escape") heard.push(event.defaultPrevented);
        }}
      >
        <Held label="Fruit" defaultItems={fruits} start="apple" seen={[]} />
      </div>,
    );
    const field = labelled(el, "Fruit");

    await step(() => {
      field.click();
    });
    await step(() => {
      press(field, "Escape");
    });
    await step(() => {
      press(field, "Escape");
    });

    expect([listFromField(field), heard]).toEqual([null, [true, false]]);
  });
  test("clears its pick by a button at the right of its field, before the one that opens the list, but where it isRequired", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(
      <>
        <Held label="Fruit" defaultItems={fruits} start="apple" seen={seen} />
        <Held label="Kept" defaultItems={fruits} start="apple" seen={[]} isRequired />
        <Held label="None" defaultItems={fruits} start={null} seen={[]} />
      </>,
    );
    const field = labelled(el, "Fruit");
    const group = field.closest('[role="group"]');
    expect(clearButton(field)?.nextElementSibling).toBe(group?.lastElementChild);

    await cleared(field, step);

    expect([seen, field.value, clearButton(field)]).toEqual([[null], "", null]);
    expect([clearButton(labelled(el, "Kept")), clearButton(labelled(el, "None"))]).toEqual([null, null]);
  });
});

describe("a select of several items", () => {
  test("clears every pick by a button at the right of its field", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(<Held label="Fruits" selectionMode="multiple" defaultItems={fruits} start={["apple", "cherry"]} seen={seen} />);
    const field = labelled(el, "Fruits");

    await cleared(field, step);

    expect([seen, chipLabels(field), clearButton(field)]).toEqual([[[]], [], null]);
  });

  test("shows each pick as a chip in its field, and ticks the picks in its list, which stays open as the user picks", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(<Held label="Fruits" selectionMode="multiple" defaultItems={fruits} start={["apple"]} seen={seen} />);
    const field = labelled(el, "Fruits");
    expect(chipLabels(field)).toEqual(["Apple"]);

    await step(() => {
      field.click();
    });
    expect(pickedLabels(field)).toEqual(["Apple"]);
    await step(() => {
      optionFromLabel(field, "Cherry").click();
    });

    expect(seen).toEqual([["apple", "cherry"]]);
    expect([chipLabels(field), pickedLabels(field)]).toEqual([
      ["Apple", "Cherry"],
      ["Apple", "Cherry"],
    ]);
    expect(listFromField(field)).not.toBeNull();
  });

  test("keeps what the user types into its field, which narrows the list, where its caller draws again as the user types and builds the picks afresh", async () => {
    function Fresh(): ReactElement {
      const [picks, setPicks] = useState<string[]>(["apple"]);
      const [, setTyped] = useState("");
      return (
        <ComboBox
          label="Fruits"
          selectionMode="multiple"
          defaultItems={fruits}
          value={[...picks]}
          onInputChange={setTyped}
          onChange={(keys) => {
            setPicks(keys.map(String));
          }}
        >
          {row}
        </ComboBox>
      );
    }
    const { el } = await draw(<Fresh />);
    const field = labelled(el, "Fruits");

    await step(() => {
      type(field, "an");
    });

    expect([field.value, optionLabels(field), chipLabels(field)]).toEqual(["an", ["Banana"], ["Apple"]]);
  });

  test("drops a pick by its chip's button, and the last one by Backspace in the empty field", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(
      <Held label="Fruits" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana", "cherry"]} seen={seen} />,
    );
    const field = labelled(el, "Fruits");

    await removeChip(field, step, "Banana");
    await step(() => {
      field.focus();
      press(field, "Backspace");
    });

    expect(seen).toEqual([["apple", "cherry"], ["apple"]]);
    expect(chipLabels(field)).toEqual(["Apple"]);
  });

  test("drops the chip that has the focus on Delete or Backspace", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(
      <Held label="Fruits" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana", "cherry"]} seen={seen} />,
    );
    const field = labelled(el, "Fruits");
    const chip = (label: string): HTMLElement => {
      const found = [...el.querySelectorAll<HTMLElement>('[role="row"]')].find((row) => row.textContent.trim() === label);
      if (found === undefined) throw new Error(`Found no chip ${label}.`);
      return found;
    };

    await step(() => {
      chip("Apple").focus();
      press(chip("Apple"), "Delete");
    });
    await step(() => {
      chip("Cherry").focus();
      press(chip("Cherry"), "Backspace");
    });

    expect(seen).toEqual([["banana", "cherry"], ["banana"]]);
    expect(chipLabels(field)).toEqual(["Banana"]);
  });

  test("moves a chip by keyboard, by its grip, the arrow keys and Enter, and calls onChange with the picks in their new order", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(
      <Held label="Fruits" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana", "cherry"]} seen={seen} />,
    );
    const field = labelled(el, "Fruits");

    await moveChip(field, step, "Apple", "ArrowRight", "ArrowRight");

    expect(seen).toEqual([["banana", "cherry", "apple"]]);
    expect(chipLabels(field)).toEqual(["Banana", "Cherry", "Apple"]);
  });

  test("gives no chip a grip, so that none drags, where it is ordered or not reorderable", async () => {
    const { el } = await draw(
      <>
        <Held label="Fixed" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana"]} seen={[]} reorderable={false} />
        <Held label="Ordered" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana"]} seen={[]} ordered />
        <Held label="Free" selectionMode="multiple" defaultItems={fruits} start={["apple", "banana"]} seen={[]} />
      </>,
    );
    const grips = (label: string): number =>
      labelled(el, label).closest('[role="group"]')?.querySelectorAll('[data-allows-dragging] button[aria-label^="Drag"]').length ?? 0;

    expect([grips("Fixed"), grips("Ordered"), grips("Free")]).toEqual([0, 0, 2]);
  });
});

type Basket = Readonly<{ fruit?: string; fruits: readonly string[] }>;

const basket = (state: Partial<Basket>): Basket => ({ fruit: "apple", fruits: ["apple"], ...state });

// A block whose fixture lists the states that the tests submit.
const fixture: BlockFixture = {
  states: [
    basket({}),
    basket({ fruit: "banana" }),
    basket({ fruits: ["cherry", "apple", "banana"] }),
    basket({ fruits: ["apple", "cherry"] }),
    basket({ fruits: ["cherry", "apple"] }),
    basket({ fruits: ["cherry", "banana", "apple"] }),
    basket({ fruits: [] }),
    { fruits: [] },
  ],
  requests: [],
};

function Bound({ host, outdated, ordered }: { host: BlockHost<Basket>; outdated?: boolean; ordered?: boolean }): ReactElement {
  return (
    <>
      <ComboBox bound={useValue(host, "fruit")} label="Fruit" placeholder="Choose a fruit" defaultItems={fruits} outdated={outdated}>
        {row}
      </ComboBox>
      <ComboBox bound={useValue(host, "fruits")} selectionMode="multiple" label="Fruits" defaultItems={fruits} outdated={outdated} ordered={ordered}>
        {row}
      </ComboBox>
    </>
  );
}

describe("a bound select", () => {
  test("submits the pick of one as the user picks it", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);

    await picked(labelled(el, "Fruit"), step, "Banana");

    expect(host.submitted).toEqual([basket({ fruit: "banana" })]);
  });

  test("shows a value that its items lack as missing, in place of the placeholder or as a chip, and marks itself invalid, but not while outdated", async () => {
    const state = basket({ fruit: "durian", fruits: ["apple", "durian"] });
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Bound host={stubBlockHost<Basket>(fixture, { state })} outdated={outdated} />);
      const [one, several] = [labelled(el, "Fruit"), labelled(el, "Fruits")];
      // The field, and the chip of the pick that the items lack, show in the
      // colour of danger by the attributes that mark them.
      const marked = [one, several].map((field) => field.closest('[role="group"]')?.hasAttribute("data-invalid"));
      const missing = [...(several.closest('[role="group"]')?.querySelectorAll("[data-missing]") ?? [])].map((chip) => chip.textContent.trim());
      const seen = [one.value, one.placeholder, one.getAttribute("aria-invalid"), chipLabels(several), several.getAttribute("aria-invalid"), marked, missing];
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual([
      "",
      "durian (not available)",
      "true",
      ["Apple", "durian (not available)"],
      "true",
      [true, true],
      ["durian (not available)"],
    ]);
    expect(await shown(true)).toEqual(["", "durian", null, ["Apple", "durian"], null, [false, false], []]);
  });

  test("submits the picks of several once, as the list closes, after the strings that the state holds then", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);
    const field = labelled(el, "Fruits");

    await step(() => {
      field.click();
    });
    // The assistant adds a string while the list is open.
    await step(() => {
      host.setState(basket({ fruits: ["cherry", "apple"] }));
    });
    await step(() => {
      optionFromLabel(field, "Banana").click();
    });
    expect(host.submitted).toEqual([]);
    await step(() => {
      press(field, "Escape");
    });

    expect(host.submitted).toEqual([basket({ fruits: ["cherry", "apple", "banana"] })]);
    expect(chipLabels(field)).toEqual(["Cherry", "Apple", "Banana"]);
  });

  test("submits a chip that the user removes while the list is closed at once, and the picks of a list that goes while open never", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el, unmount } = await draw(<Bound host={host} />);
    const field = labelled(el, "Fruits");

    await removeChip(field, step, "Apple");
    expect(host.submitted).toEqual([basket({ fruits: [] })]);

    await step(() => {
      field.click();
    });
    await step(() => {
      optionFromLabel(field, "Cherry").click();
    });
    await unmount();
    expect(host.submitted).toEqual([basket({ fruits: [] })]);
  });

  test("submits no pick as the user clears the picks of several, and unsets the state arg of one, as a new block leaves an arg without a default", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["apple", "cherry"] }) });
    const { el } = await draw(<Bound host={host} />);
    const one = labelled(el, "Fruit");

    await cleared(labelled(el, "Fruits"), step);
    await cleared(one, step);

    expect(host.submitted).toEqual([basket({ fruits: [] }), { fruits: [] }]);
    expect([Object.hasOwn(host.state(), "fruit"), one.value, one.placeholder, clearButton(one)]).toEqual([false, "", "Choose a fruit", null]);
  });

  test("submits the picks at once in the order that the user moves their chips into", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["apple", "cherry"] }) });
    const { el } = await draw(<Bound host={host} />);
    const field = labelled(el, "Fruits");

    await moveChip(field, step, "Apple", "ArrowRight");

    expect(host.submitted).toEqual([basket({ fruits: ["cherry", "apple"] })]);
    expect(chipLabels(field)).toEqual(["Cherry", "Apple"]);
  });

  test("submits the order of the chips as the list closes, where the user unticked a pick and ticked it again, and keeps the place of a string that the assistant added in the meantime", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["apple", "banana"] }) });
    const { el } = await draw(<Bound host={host} />);
    const field = labelled(el, "Fruits");

    await step(() => {
      field.click();
    });
    await step(() => {
      host.setState(basket({ fruits: ["cherry", "apple", "banana"] }));
    });
    for (const label of ["Apple", "Apple"]) {
      await step(() => {
        optionFromLabel(field, label).click();
      });
    }
    expect(chipLabels(field)).toEqual(["Banana", "Apple"]);
    await step(() => {
      press(field, "Escape");
    });

    expect(host.submitted).toEqual([basket({ fruits: ["cherry", "banana", "apple"] })]);
  });

  test("submits the picks in the order of its items where ordered", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["cherry"] }) });
    const { el } = await draw(<Bound host={host} ordered />);

    await picked(labelled(el, "Fruits"), step, "Apple");

    expect(host.submitted).toEqual([basket({ fruits: ["apple", "cherry"] })]);
  });
});
