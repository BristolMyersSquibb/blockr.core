// The table of core's React SDK, drawn on the page: the columns that sort and
// resize, the checkboxes that select rows and the buttons that drag them.

import { useState } from "react";
import type { ReactElement } from "react";
import { describe, expect, test } from "vitest";
import type { Key, SortDescriptor } from "../aria/index.ts";
import { useDragAndDrop } from "../dnd/index.ts";
import { draw, step } from "../testing/react.ts";
import { rowCheckbox, rowOf, rowTexts } from "../testing/rows.ts";
import { Cell, Column, Row, Table, TableBody, TableHeader } from "./index.ts";
import type { TableProps } from "./index.ts";

interface Animal {
  readonly id: string;
  readonly name: string;
  readonly legs: number;
}

const animals: readonly Animal[] = [
  { id: "heron", name: "Heron", legs: 2 },
  { id: "ant", name: "Ant", legs: 6 },
  { id: "cat", name: "Cat", legs: 4 },
];

// A table of the animals, sorted as `sort` says, with the props given.
function Animals({ sort, ...props }: Omit<TableProps, "children"> & { sort?: SortDescriptor }): ReactElement {
  const sorted = [...animals].sort((a, b) => {
    if (sort === undefined) return 0;
    const by = sort.column === "legs" ? a.legs - b.legs : a.name.localeCompare(b.name);
    return sort.direction === "descending" ? -by : by;
  });
  return (
    <Table aria-label="Animals" sortDescriptor={sort} {...props}>
      <TableHeader>
        <Column id="name" isRowHeader allowsSorting>
          Name
        </Column>
        <Column id="legs" allowsSorting>
          Legs
        </Column>
        <Column id="note" width={80}>
          Note
        </Column>
      </TableHeader>
      <TableBody items={sorted}>
        {(animal) => (
          <Row id={animal.id}>
            <Cell>{animal.name}</Cell>
            <Cell>{String(animal.legs)}</Cell>
            <Cell>{animal.legs > 4 ? "many" : ""}</Cell>
          </Row>
        )}
      </TableBody>
    </Table>
  );
}

function Sorting(): ReactElement {
  const [sort, setSort] = useState<SortDescriptor>({ column: "name", direction: "ascending" });
  return <Animals sort={sort} onSortChange={setSort} />;
}

function header(el: ParentNode, name: string): HTMLElement {
  const found = [...el.querySelectorAll<HTMLElement>('[role="columnheader"]')].find((th) => th.textContent === name);
  if (found === undefined) throw new Error(`Found no column ${name}.`);
  return found;
}

// The way the arrow of a column's header points, or null where it shows none.
function arrow(th: Element): "up" | "down" | null {
  const svg = th.querySelector("svg");
  if (svg === null) return null;
  return svg.classList.contains("bc:rotate-180") ? "down" : "up";
}

describe("sorting", () => {
  test("sorts by a column that allows sorting as the user presses its header, and shows an arrow on the column it sorts by alone", async () => {
    const { el } = draw(<Sorting />);
    const name = header(el, "Name");
    const legs = header(el, "Legs");

    expect(rowTexts(el)).toEqual(["Ant6many", "Cat4", "Heron2"]);
    expect([name.getAttribute("aria-sort"), legs.getAttribute("aria-sort"), header(el, "Note").getAttribute("aria-sort")]).toEqual([
      "ascending",
      "none",
      null,
    ]);
    expect([arrow(name), arrow(legs)]).toEqual(["up", null]);

    await step(() => {
      legs.click();
    });
    expect(rowTexts(el)).toEqual(["Heron2", "Cat4", "Ant6many"]);
    expect([name.getAttribute("aria-sort"), legs.getAttribute("aria-sort")]).toEqual(["none", "ascending"]);
    expect([arrow(name), arrow(legs)]).toEqual([null, "up"]);

    await step(() => {
      legs.click();
    });
    expect(rowTexts(el)).toEqual(["Ant6many", "Cat4", "Heron2"]);
    expect(legs.getAttribute("aria-sort")).toBe("descending");
    expect(arrow(legs)).toBe("down");
  });
});

test("lines the heading of a column that aligns at its end up at its right edge, with the arrow before it", () => {
  const { el } = draw(
    <Table aria-label="Counts" sortDescriptor={{ column: "n", direction: "ascending" }}>
      <TableHeader>
        <Column id="name" isRowHeader allowsSorting>
          Name
        </Column>
        <Column id="n" align="end" allowsSorting>
          Count
        </Column>
      </TableHeader>
      <TableBody>
        <Row id="a">
          <Cell>a</Cell>
          <Cell className="bc:text-right">1</Cell>
        </Row>
      </TableBody>
    </Table>,
  );

  expect(header(el, "Count").className).toContain("bc:text-end");
  expect(header(el, "Name").className).toContain("bc:text-start");
  // The column of numbers turns its heading round, so that the arrow comes
  // before the name.
  const heading = (name: string): string | undefined => header(el, name).querySelector("span")?.className;
  expect(heading("Count")).toContain("bc:flex-row-reverse");
  expect(heading("Name")).not.toContain("bc:flex-row-reverse");
  expect(arrow(header(el, "Count"))).toBe("up");
});

test("draws no frame, and no bar that resizes a column, since the board would not keep the widths", () => {
  const { el } = draw(<Animals />);

  expect(el.querySelector("[data-resizable-direction]")).toBeNull();
  expect(el.querySelector("table")?.parentElement?.className).not.toContain("bc:border");
});

describe("selecting rows", () => {
  function Selecting({ mode }: { mode: "single" | "multiple" }): ReactElement {
    const [selected, setSelected] = useState<"all" | Iterable<Key>>([]);
    return (
      <>
        <Animals selectionMode={mode} selectedKeys={selected} onSelectionChange={setSelected} />
        <output>{selected === "all" ? "all" : [...selected].join(" ")}</output>
      </>
    );
  }

  test("starts each row with a checkbox that selects it, and the header with one that selects every row where it selects several", async () => {
    const { el } = draw(<Selecting mode="multiple" />);
    const said = (): string | undefined => el.querySelector("output")?.textContent;

    expect(el.querySelectorAll('[role="columnheader"] input[type="checkbox"]')).toHaveLength(1);
    await step(() => {
      rowCheckbox(rowOf(el, "Cat4")).click();
    });
    await step(() => {
      rowCheckbox(rowOf(el, "Ant6many")).click();
    });
    expect(said()).toBe("cat ant");
    expect(rowOf(el, "Cat4").getAttribute("aria-selected")).toBe("true");
    expect(rowOf(el, "Heron2").getAttribute("aria-selected")).toBe("false");

    await step(() => {
      el.querySelector<HTMLInputElement>('[role="columnheader"] input[type="checkbox"]')?.click();
    });
    expect(said()).toBe("all");
  });

  test("draws no checkbox in the header where it selects one row", async () => {
    const { el } = draw(<Selecting mode="single" />);

    expect(el.querySelectorAll('[role="columnheader"] input[type="checkbox"]')).toHaveLength(0);
    await step(() => {
      rowCheckbox(rowOf(el, "Cat4")).click();
    });
    await step(() => {
      rowCheckbox(rowOf(el, "Ant6many")).click();
    });
    expect(el.querySelector("output")?.textContent).toBe("ant");
  });

  test("draws no checkbox where it selects no row", () => {
    const { el } = draw(<Animals />);
    expect(el.querySelectorAll('input[type="checkbox"]')).toHaveLength(0);
  });
});

test("starts each row with a button that drags it where its rows drag", () => {
  function Dragging(): ReactElement {
    const { dragAndDropHooks } = useDragAndDrop({
      getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
    });
    return <Animals dragAndDropHooks={dragAndDropHooks} />;
  }
  const { el } = draw(<Dragging />);

  const buttons = ["Ant6many", "Cat4", "Heron2"].map((text) => rowOf(el, text).querySelector('button[slot="drag"]'));
  expect(buttons.every((button) => button !== null)).toBe(true);
  expect(draw(<Animals />).el.querySelectorAll('button[slot="drag"]')).toHaveLength(0);
});
