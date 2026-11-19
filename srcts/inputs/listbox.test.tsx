// Core's list in place, React Aria's ListBox in core's theme: what core adds
// to it, what the list says where it shows no row, its search through React
// Aria's Autocomplete and the SDK's SearchField, and the binding of a block's
// state arg, on the stub of a block's host.
// The rest, such as the keys, is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { Selection } from "../aria/index.ts";
import { PAUSE } from "../binding/index.ts";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { type } from "../testing/inputs.ts";
import { wait } from "../testing/modules.ts";
import { step } from "../testing/react.ts";
import { labelled } from "../testing/select.ts";
import type { BlockHost } from "../types/hosts.ts";
import { Autocomplete, ListBox, ListBoxItem, SearchField, useFilter } from "./index.ts";
import type { ListBoxProps } from "./index.ts";

interface Fruit {
  readonly id: string;
  readonly name: string;
}

const fruits: readonly Fruit[] = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

const row = (fruit: Fruit): ReactElement => <ListBoxItem id={fruit.id}>{fruit.name}</ListBoxItem>;

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
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

// The list that the label given names.
function list(el: ParentNode, label: string): HTMLElement {
  const found = el.querySelector(`[role="listbox"][aria-label="${label}"]`);
  if (!(found instanceof HTMLElement)) throw new Error(`Found no list ${label}.`);
  return found;
}

// The labels of the rows that the list shows.
// React Aria holds what an empty list says in a row of its own.
function rowLabels(found: HTMLElement): string[] {
  return found.hasAttribute("data-empty") ? [] : [...found.querySelectorAll('[role="option"]')].map((option) => option.textContent.trim());
}

// The labels of the rows that the list ticks or shows as the pick.
function pickedLabels(found: HTMLElement): string[] {
  return [...found.querySelectorAll('[role="option"][aria-selected="true"]')].map((option) => option.textContent.trim());
}

// Clicks the row of the label given, as a user picks or unpicks it.
async function clicked(found: HTMLElement, label: string): Promise<void> {
  const option = [...found.querySelectorAll('[role="option"]')].find((candidate) => candidate.textContent.trim() === label);
  if (!(option instanceof HTMLElement)) throw new Error(`Found no row ${label}.`);
  await step(() => {
    option.click();
  });
}

// What the list says where it shows no row.
function note(found: HTMLElement): string | null {
  return found.querySelector("p")?.textContent ?? null;
}

// A list of several that holds its picks, and records each change.
function Held({ start, seen }: { readonly start: readonly string[]; readonly seen: unknown[] }): ReactElement {
  const [keys, setKeys] = useState<Selection>(new Set(start));
  return (
    <ListBox
      aria-label="Fruits"
      selectionMode="multiple"
      items={fruits}
      selectedKeys={keys}
      onSelectionChange={(next) => {
        seen.push(next === "all" ? next : [...next]);
        setKeys(next);
      }}
    >
      {row}
    </ListBox>
  );
}

// A list of several in an Autocomplete, which narrows it by what the user
// types into the search field.
function Searched(props: Omit<ListBoxProps<Fruit>, "aria-label" | "children">): ReactElement {
  const { contains } = useFilter({ sensitivity: "base" });
  return (
    <Autocomplete filter={contains}>
      <SearchField label="Search fruits" />
      <ListBox aria-label="Fruits" selectionMode="multiple" items={fruits} {...props}>
        {row}
      </ListBox>
    </Autocomplete>
  );
}

describe("a list in place", () => {
  test("lists its items, always open, ticks the picks of several, and calls onSelectionChange with the picks as the user ticks a row", async () => {
    const seen: unknown[] = [];
    const { el } = await draw(<Held start={["apple"]} seen={seen} />);
    const fruitList = list(el, "Fruits");
    expect([rowLabels(fruitList), pickedLabels(fruitList)]).toEqual([["Apple", "Banana", "Cherry"], ["Apple"]]);

    await clicked(fruitList, "Cherry");

    expect([seen, pickedLabels(fruitList)]).toEqual([[["apple", "cherry"]], ["Apple", "Cherry"]]);
  });

  test("narrows its rows to those whose text holds what the user types into the search field of an Autocomplete around it, and lists them all again as the user clears the field", async () => {
    const { el } = await draw(<Searched />);
    const search = labelled(el, "Search fruits");
    const clear = search.closest('[role="group"]')?.querySelector("button");

    await step(() => {
      type(search, "an");
    });
    expect(rowLabels(list(el, "Fruits"))).toEqual(["Banana"]);
    await step(() => {
      clear?.click();
    });

    expect([search.value, rowLabels(list(el, "Fruits"))]).toEqual(["", ["Apple", "Banana", "Cherry"]]);
  });

  test("says that it loads while it has no items yet, that nothing matches where the search keeps none, and what renderEmptyState says where it has none", async () => {
    const said = async (node: ReactElement, typed?: string): Promise<string | null> => {
      const { el, unmount } = await draw(node);
      if (typed !== undefined) {
        await step(() => {
          type(labelled(el, "Search fruits"), typed);
        });
      }
      const shown = note(list(el, "Fruits"));
      await unmount();
      return shown;
    };

    expect(await said(<ListBox aria-label="Fruits">{row}</ListBox>)).toBe("Loading…");
    expect(await said(<Searched />, "kiwi")).toBe("No matches");
    expect(await said(<ListBox aria-label="Fruits" items={[]}>{row}</ListBox>)).toBe("No options");
    expect(await said(<ListBox aria-label="Fruits" items={[]} renderEmptyState={() => "No fruit grows here."}>{row}</ListBox>)).toBe(
      "No fruit grows here.",
    );
    expect(await said(<ListBox aria-label="Fruits" items={[]} outdated>{row}</ListBox>)).toBe("Loading…");
  });
});

type Basket = Readonly<{ fruit?: string; fruits: readonly string[] }>;

const basket = (state: Partial<Basket>): Basket => ({ fruit: "apple", fruits: ["apple"], ...state });

// A block whose fixture lists the states that the tests submit.
const fixture: BlockFixture = {
  states: [
    basket({}),
    basket({ fruit: "banana" }),
    { fruits: ["apple"] },
    basket({ fruits: ["apple", "cherry"] }),
    basket({ fruits: ["apple", "cherry", "banana"] }),
    basket({ fruits: ["banana", "cherry"] }),
  ],
  requests: [],
};

function Bound({ host, outdated, ordered }: { host: BlockHost<Basket>; outdated?: boolean; ordered?: boolean }): ReactElement {
  return (
    <>
      <ListBox bound={useValue(host, "fruit")} aria-label="Fruit" items={fruits} outdated={outdated}>
        {row}
      </ListBox>
      <ListBox bound={useValue(host, "fruits")} aria-label="Fruits" selectionMode="multiple" items={fruits} outdated={outdated} ordered={ordered}>
        {row}
      </ListBox>
    </>
  );
}

describe("a bound list", () => {
  test("submits the pick of one as the user picks it, and unsets the state arg as the user unpicks it, as a new block leaves an arg without a default", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruit");

    await clicked(fruitList, "Banana");
    expect(host.submitted).toEqual([basket({ fruit: "banana" })]);
    await clicked(fruitList, "Banana");

    expect(host.submitted).toEqual([basket({ fruit: "banana" }), { fruits: ["apple"] }]);
    expect([Object.hasOwn(host.state(), "fruit"), pickedLabels(fruitList)]).toEqual([false, []]);
  });

  test("submits a burst of picks of several once the user pauses, as typing does, in the order the user picked them", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruits");

    await clicked(fruitList, "Cherry");
    await wait(PAUSE - 1);
    await clicked(fruitList, "Banana");
    await wait(PAUSE - 1);
    expect([host.submitted, pickedLabels(fruitList)]).toEqual([[], ["Apple", "Banana", "Cherry"]]);
    await wait(1);

    expect(host.submitted).toEqual([basket({ fruits: ["apple", "cherry", "banana"] })]);
    expect(pickedLabels(fruitList)).toEqual(["Apple", "Banana", "Cherry"]);
  });

  test("builds the value from the one that the host holds as the user picks, so that it keeps a string that the assistant added before the last pick", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruits");

    await clicked(fruitList, "Cherry");
    // The assistant adds a string while the binding waits to submit.
    await step(() => {
      host.setState(basket({ fruits: ["apple", "banana"] }));
    });
    await clicked(fruitList, "Apple");
    await wait(PAUSE);

    expect(host.submitted).toEqual([basket({ fruits: ["banana", "cherry"] })]);
    expect(pickedLabels(fruitList)).toEqual(["Banana", "Cherry"]);
  });

  test("submits the picks still pending as the list goes, as its module unmounts, and nothing where the user unpicked what they picked", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el, unmount } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruits");

    await clicked(fruitList, "Banana");
    await clicked(fruitList, "Banana");
    await wait(PAUSE);
    expect(host.submitted).toEqual([]);
    await clicked(fruitList, "Cherry");
    await unmount();

    expect(host.submitted).toEqual([basket({ fruits: ["apple", "cherry"] })]);
  });

  test("shows a value that its items lack as a row of its own before them, picked and marked as missing, and marks itself invalid, but not while outdated", async () => {
    const state = basket({ fruit: "durian", fruits: ["apple", "durian"] });
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Bound host={stubBlockHost<Basket>(fixture, { state })} outdated={outdated} />);
      const [one, several] = [list(el, "Fruit"), list(el, "Fruits")];
      const seen = [
        rowLabels(one),
        pickedLabels(one),
        pickedLabels(several),
        [one, several].map((found) => found.hasAttribute("data-invalid")),
        [...several.querySelectorAll("[data-missing]")].map((option) => option.textContent.trim()),
      ];
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual([
      ["durian (not available)", "Apple", "Banana", "Cherry"],
      ["durian (not available)"],
      ["durian (not available)", "Apple"],
      [true, true],
      ["durian (not available)"],
    ]);
    expect(await shown(true)).toEqual([["durian", "Apple", "Banana", "Cherry"], ["durian"], ["durian", "Apple"], [false, false], []]);
  });

  test("knows the rows among its children, as for a fixed set of items, and so marks a value that they lack", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["apple", "durian"] }) });
    function Fixed(): ReactElement {
      return (
        <ListBox bound={useValue(host, "fruits")} aria-label="Fruits" selectionMode="multiple">
          <ListBoxItem id="apple">Apple</ListBoxItem>
          <ListBoxItem id="banana">Banana</ListBoxItem>
        </ListBox>
      );
    }
    const { el } = await draw(<Fixed />);
    const fruitList = list(el, "Fruits");

    expect([rowLabels(fruitList), pickedLabels(fruitList)]).toEqual([
      ["durian (not available)", "Apple", "Banana"],
      ["durian (not available)", "Apple"],
    ]);
  });

  test("drops a value that its items lack as the user unpicks its row", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["apple", "durian"] }) });
    const { el } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruits");

    await clicked(fruitList, "durian (not available)");
    await wait(PAUSE);

    expect(host.submitted).toEqual([basket({})]);
    expect([rowLabels(fruitList), fruitList.hasAttribute("data-invalid")]).toEqual([["Apple", "Banana", "Cherry"], false]);
  });

  test("submits the picks in the order of its items where ordered", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({ fruits: ["cherry"] }) });
    const { el } = await draw(<Bound host={host} ordered />);

    await clicked(list(el, "Fruits"), "Apple");
    await wait(PAUSE);

    expect(host.submitted).toEqual([basket({ fruits: ["apple", "cherry"] })]);
  });

  test("says below itself why the host refused what it submitted, and shows the picks of the user meanwhile", async () => {
    const host = stubBlockHost<Basket>(fixture, { state: basket({}) });
    const { el } = await draw(<Bound host={host} />);
    const fruitList = list(el, "Fruits");

    await clicked(fruitList, "Banana");
    await wait(PAUSE);

    expect(el.querySelector('[role="alert"]')?.textContent).toBe(
      'The module submitted the state {"fruit":"apple","fruits":["apple","banana"]}, which the fixture does not list.',
    );
    expect(pickedLabels(fruitList)).toEqual(["Apple", "Banana"]);
  });
});
