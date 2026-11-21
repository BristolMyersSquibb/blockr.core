// The grid list of core's React SDK, drawn on the page: its actions, its
// sections, what it says without items, the checkboxes that select items, and
// dragging an item to another place, from the keyboard.

import { useState } from "react";
import type { ReactElement } from "react";
import { afterEach, expect, test } from "vitest";
import type { Key } from "../aria/index.ts";
import { useDragAndDrop } from "../dnd/index.ts";
import { pressAndRelease } from "../testing/inputs.ts";
import { draw, step } from "../testing/react.ts";
import { rowCheckbox, rowOf, rowTexts } from "../testing/rows.ts";
import { GridList, GridListHeader, GridListItem, GridListSection } from "./index.ts";

afterEach(() => {
  document.body.replaceChildren();
});

const fruits = [
  { id: "apple", name: "Apple" },
  { id: "pear", name: "Pear" },
  { id: "plum", name: "Plum" },
];

test("runs the action of the item that the user presses, or presses Enter on", async () => {
  const ran: Key[] = [];
  const { el } = draw(
    <GridList aria-label="Fruits" items={fruits} onAction={(key) => ran.push(key)}>
      {(fruit) => <GridListItem id={fruit.id}>{fruit.name}</GridListItem>}
    </GridList>,
  );

  expect(rowTexts(el)).toEqual(["Apple", "Pear", "Plum"]);
  await step(() => {
    rowOf(el, "Pear").click();
  });
  await step(() => {
    rowOf(el, "Pear").focus();
  });
  await step(() => {
    pressAndRelease(rowOf(el, "Pear"), "ArrowDown");
  });
  await step(() => {
    pressAndRelease(rowOf(el, "Plum"), "Enter");
  });
  expect(ran).toEqual(["pear", "plum"]);
});

test("groups items under the header of their section, and says what renderEmptyState gives where it has no item", () => {
  const { el } = draw(
    <GridList aria-label="Food">
      <GridListSection>
        <GridListHeader>Fruits</GridListHeader>
        <GridListItem id="apple">Apple</GridListItem>
      </GridListSection>
      <GridListSection>
        <GridListHeader>Nuts</GridListHeader>
        <GridListItem id="hazel">Hazelnut</GridListItem>
      </GridListSection>
    </GridList>,
  );
  const sections = [...el.querySelectorAll('[role="rowgroup"]')].map((group) => [
    group.querySelector('[role="rowheader"], [role="presentation"]')?.textContent ?? group.firstElementChild?.textContent,
    rowTexts(group),
  ]);
  expect(sections).toEqual([
    ["Fruits", ["Apple"]],
    ["Nuts", ["Hazelnut"]],
  ]);

  const empty = draw(
    <GridList aria-label="Fruits" items={[]} renderEmptyState={() => "No fruit"}>
      {(fruit: { id: string; name: string }) => <GridListItem id={fruit.id}>{fruit.name}</GridListItem>}
    </GridList>,
  );
  const grid = empty.el.querySelector('[role="grid"]');
  expect([grid?.textContent, grid?.hasAttribute("data-empty")]).toEqual(["No fruit", true]);
});

test("starts each item with a checkbox that selects it where the list selects items", async () => {
  function Selecting(): ReactElement {
    const [selected, setSelected] = useState<"all" | Iterable<Key>>([]);
    return (
      <>
        <GridList aria-label="Fruits" items={fruits} selectionMode="multiple" selectedKeys={selected} onSelectionChange={setSelected}>
          {(fruit) => <GridListItem id={fruit.id}>{fruit.name}</GridListItem>}
        </GridList>
        <output>{selected === "all" ? "all" : [...selected].join(" ")}</output>
      </>
    );
  }
  const { el } = draw(<Selecting />);

  await step(() => {
    rowCheckbox(rowOf(el, "Plum")).click();
  });
  await step(() => {
    rowCheckbox(rowOf(el, "Apple")).click();
  });
  expect(el.querySelector("output")?.textContent).toBe("plum apple");
  expect(rowOf(el, "Plum").getAttribute("aria-selected")).toBe("true");

  const plain = draw(
    <GridList aria-label="Fruits" items={fruits}>
      {(fruit) => <GridListItem id={fruit.id}>{fruit.name}</GridListItem>}
    </GridList>,
  );
  expect(plain.el.querySelectorAll('input[type="checkbox"]')).toHaveLength(0);
});

test("drags an item to another place from the keyboard, showing where it would land by the SDK's drop indicator", async () => {
  const moves: string[] = [];
  function Reordering(): ReactElement {
    const [items, setItems] = useState(fruits);
    const { dragAndDropHooks } = useDragAndDrop({
      getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
      onReorder: (event) => {
        const [key] = [...event.keys];
        const target = String(event.target.key);
        moves.push(`${String(key)} ${event.target.dropPosition} ${target}`);
        setItems((now) => {
          const moved = now.filter((fruit) => fruit.id !== key);
          const at = moved.findIndex((fruit) => fruit.id === target) + (event.target.dropPosition === "after" ? 1 : 0);
          const fruit = now.find((found) => found.id === key);
          return fruit === undefined ? now : [...moved.slice(0, at), fruit, ...moved.slice(at)];
        });
      },
    });
    return (
      <GridList aria-label="Fruits" items={items} dragAndDropHooks={dragAndDropHooks}>
        {(fruit) => <GridListItem id={fruit.id}>{fruit.name}</GridListItem>}
      </GridList>
    );
  }
  const { el, unmount } = draw(<Reordering />);
  const grip = (text: string): HTMLButtonElement => {
    const found = rowOf(el, text).querySelector<HTMLButtonElement>('button[slot="drag"]');
    if (found === null) throw new Error(`Found no button that drags ${text}.`);
    return found;
  };

  await step(() => {
    grip("Apple").focus();
  });
  await step(() => {
    pressAndRelease(grip("Apple"), "Enter");
  });
  // Each place where the item may land is a row of its own, holding a button
  // that React Aria names a drop indicator.
  const indicators = [...el.querySelectorAll('[role="row"]')].filter((row) => row.querySelector('[aria-roledescription="drop indicator"]') !== null);
  expect(indicators.length).toBeGreaterThan(0);
  expect(indicators.every((indicator) => indicator.classList.contains("bc:data-drop-target:outline-1"))).toBe(true);

  // React Aria starts at the place after the item, between Apple and Pear,
  // and moves on to the place between Pear and Plum.
  expect(document.activeElement?.getAttribute("aria-label")).toBe("Insert between Apple and Pear");
  for (const key of ["ArrowDown", "Enter"]) {
    await step(() => {
      pressAndRelease(document.activeElement as HTMLElement, key);
    });
  }
  expect(moves).toEqual(["apple before plum"]);
  expect(rowTexts(el)).toEqual(["Pear", "Apple", "Plum"]);

  // React Aria moves the focus 50 ms after a drop, on a timer that it clears
  // as the list unmounts.
  // The test unmounts the list, so that the timer cannot fire once the test's
  // page is gone, where React finds no window.
  unmount();
});
