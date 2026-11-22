// The trees of core's React SDK, drawn on the page: the tree, whose items
// expand to show the items below them, select and drag, and the navigation
// tree, of links, which marks the item of the page that is current.

import { useState } from "react";
import type { ReactElement } from "react";
import { describe, expect, test } from "vitest";
import type { Key } from "../aria/index.ts";
import { useDragAndDrop } from "../dnd/index.ts";
import { pressAndRelease } from "../testing/inputs.ts";
import { draw, step } from "../testing/react.ts";
import { rowCheckbox, rowOf, rowTexts } from "../testing/rows.ts";
import {
  NavigationTree,
  NavigationTreeHeader,
  NavigationTreeItem,
  NavigationTreeItemContent,
  NavigationTreeItemLink,
  NavigationTreeSection,
  Tree,
  TreeItem,
} from "./index.ts";
import type { NavigationTreeProps } from "./index.ts";
import type { TreeProps } from "../aria/index.ts";

// A tree of a view and a folder that holds two more.
function Views(props: Omit<TreeProps<object>, "children">): ReactElement {
  return (
    <Tree aria-label="Views" {...props}>
      <TreeItem id="overview" title="Overview" />
      <TreeItem id="safety" title="Safety">
        <TreeItem id="events" title="Adverse events" />
        <TreeItem id="labs" title="Labs" />
      </TreeItem>
    </Tree>
  );
}

// The button of a row that expands it, which React Aria names by its slot.
function expander(row: Element): HTMLElement | null {
  return row.querySelector<HTMLElement>('[slot="chevron"]');
}

describe("the tree", () => {
  test("shows the items below an item while it is expanded, by its button or the arrow keys, each indented by its level", async () => {
    const { el } = draw(<Views />);

    expect(rowTexts(el)).toEqual(["Overview", "Safety"]);
    expect(expander(rowOf(el, "Overview"))).toBeNull();
    expect(rowOf(el, "Safety").getAttribute("aria-expanded")).toBe("false");

    await step(() => {
      expander(rowOf(el, "Safety"))?.click();
    });
    expect(rowTexts(el)).toEqual(["Overview", "Safety", "Adverse events", "Labs"]);
    expect(rowOf(el, "Labs").getAttribute("aria-level")).toBe("2");
    expect(rowOf(el, "Labs").style.getPropertyValue("--tree-item-level")).toBe("2");

    await step(() => {
      rowOf(el, "Safety").focus();
    });
    await step(() => {
      pressAndRelease(rowOf(el, "Safety"), "ArrowLeft");
    });
    expect(rowTexts(el)).toEqual(["Overview", "Safety"]);
    await step(() => {
      pressAndRelease(rowOf(el, "Safety"), "ArrowRight");
    });
    expect(rowTexts(el)).toEqual(["Overview", "Safety", "Adverse events", "Labs"]);
  });

  test("starts each item with a checkbox that selects it where the tree selects items", async () => {
    function Selecting(): ReactElement {
      const [selected, setSelected] = useState<"all" | Iterable<Key>>([]);
      return (
        <>
          <Views selectionMode="multiple" selectedKeys={selected} onSelectionChange={setSelected} defaultExpandedKeys={["safety"]} />
          <output>{selected === "all" ? "all" : [...selected].join(" ")}</output>
        </>
      );
    }
    const { el } = draw(<Selecting />);

    await step(() => {
      rowCheckbox(rowOf(el, "Labs")).click();
    });
    await step(() => {
      rowCheckbox(rowOf(el, "Overview")).click();
    });
    expect(el.querySelector("output")?.textContent).toBe("labs overview");
    expect(draw(<Views />).el.querySelectorAll('input[type="checkbox"]')).toHaveLength(0);
  });

  test("starts each item with a button that drags it where its items drag", () => {
    function Dragging(): ReactElement {
      const { dragAndDropHooks } = useDragAndDrop({
        getItems: (keys) => [...keys].map((key) => ({ "text/plain": String(key) })),
      });
      return <Views dragAndDropHooks={dragAndDropHooks} />;
    }
    const { el } = draw(<Dragging />);

    expect(["Overview", "Safety"].map((text) => rowOf(el, text).querySelector('button[slot="drag"]') !== null)).toEqual([true, true]);
    expect(draw(<Views />).el.querySelectorAll('button[slot="drag"]')).toHaveLength(0);
  });
});

describe("the navigation tree", () => {
  // The pages of an app, under two sections.
  function Pages(props: Omit<NavigationTreeProps<object>, "children">): ReactElement {
    return (
      <NavigationTree aria-label="Pages" {...props}>
        <NavigationTreeSection>
          <NavigationTreeHeader>Board</NavigationTreeHeader>
          <NavigationTreeItem id="overview" textValue="Overview" href="/overview">
            <NavigationTreeItemContent>
              <NavigationTreeItemLink>Overview</NavigationTreeItemLink>
            </NavigationTreeItemContent>
          </NavigationTreeItem>
          <NavigationTreeItem id="safety" textValue="Safety">
            <NavigationTreeItemContent>
              <NavigationTreeItemLink>Safety</NavigationTreeItemLink>
            </NavigationTreeItemContent>
            <NavigationTreeItem id="labs" textValue="Labs" href="/safety/labs">
              <NavigationTreeItemContent>
                <NavigationTreeItemLink>Labs</NavigationTreeItemLink>
              </NavigationTreeItemContent>
            </NavigationTreeItem>
          </NavigationTreeItem>
        </NavigationTreeSection>
      </NavigationTree>
    );
  }

  // What the line at the left edge of the row's link shows.
  function indicator(row: Element): string | null {
    return row.querySelector("[data-indicator]")?.getAttribute("data-indicator") ?? null;
  }

  test("marks the item of the current page, and by a dot the collapsed item above it, which its button expands", async () => {
    const { el } = draw(<Pages selectedRoute="/safety/labs" />);

    expect(rowTexts(el)).toEqual(["Overview", "Safety"]);
    expect(el.textContent).toContain("Board");
    expect([indicator(rowOf(el, "Overview")), indicator(rowOf(el, "Safety"))]).toEqual([null, "ancestor"]);
    // An item without items below it holds a button that it hides.
    expect(expander(rowOf(el, "Overview"))?.className).toContain("bc:invisible");

    await step(() => {
      expander(rowOf(el, "Safety"))?.click();
    });
    expect(rowTexts(el)).toEqual(["Overview", "Safety", "Labs"]);
    expect([indicator(rowOf(el, "Safety")), indicator(rowOf(el, "Labs"))]).toEqual([null, "current"]);
    expect(rowOf(el, "Labs").querySelector("a")?.getAttribute("aria-current")).toBe("page");
    expect(rowOf(el, "Overview").querySelector("a")?.getAttribute("href")).toBe("/overview");
  });

  test("draws each link through renderLink", () => {
    const { el } = draw(
      <Pages
        renderLink={(props) =>
          // A router's link, which marks what it draws, for an item with a
          // page, and a span for one without.
          "href" in props ? <a {...props} data-router="" /> : <span {...props} />
        }
      />,
    );

    expect(rowOf(el, "Overview").querySelector("a")?.hasAttribute("data-router")).toBe(true);
  });
});
