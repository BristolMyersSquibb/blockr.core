// The layout of core's React SDK, React Aria's in core's theme: the
// disclosure and its header, the tabs, the toolbar and its separator, the
// breadcrumbs and the link, the meter and the progress bar.
// The rest, such as how a disclosure's panel moves, is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test, vi } from "vitest";
import { Button, ToggleButton, ToggleButtonGroup } from "../inputs/index.ts";
import { find, press, texts } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import {
  Breadcrumb,
  Breadcrumbs,
  Disclosure,
  DisclosureGroup,
  DisclosureHeader,
  DisclosurePanel,
  Link,
  Meter,
  ProgressBar,
  Separator,
  Tab,
  TabList,
  TabPanel,
  Tabs,
  Toolbar,
} from "./index.ts";

// Draws the component on the page, until the test ends.
async function draw(node: ReactElement): Promise<HTMLElement> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return el;
}

async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
}

describe("the disclosure", () => {
  // The header's button, which names the disclosure, and whether it says
  // that the disclosure is expanded, which the test checks against whether
  // the panel shows.
  function state(el: ParentNode, title: string): [string, boolean] {
    const trigger = [...el.querySelectorAll("button")].find((button) => button.textContent === title);
    if (trigger === undefined) throw new Error(`Found no header ${title}.`);
    const expanded = trigger.getAttribute("aria-expanded") === "true";
    const panel = document.getElementById(trigger.getAttribute("aria-controls") ?? "");
    expect(panel?.hidden).toBe(!expanded);
    expect(trigger.querySelector("svg")?.getAttribute("class")?.includes("bc:rotate-90")).toBe(expanded);
    return [title, expanded];
  }

  test("expands and collapses its panel by the button of its header, which a heading holds, and keeps what the panel holds on the page", async () => {
    const el = await draw(
      <Disclosure>
        <DisclosureHeader>Filters</DisclosureHeader>
        <DisclosurePanel>
          <p>Rows where</p>
        </DisclosurePanel>
      </Disclosure>,
    );
    const trigger = find(el, "h3 > button");

    expect(state(el, "Filters")).toEqual(["Filters", false]);
    expect(el.textContent).toContain("Rows where");
    await click(trigger);
    expect(state(el, "Filters")).toEqual(["Filters", true]);
    await click(trigger);
    expect(state(el, "Filters")).toEqual(["Filters", false]);
  });

  test("follows its caller where the caller keeps whether it is expanded, and takes the heading's level and classes", async () => {
    function Kept(): ReactElement {
      const [expanded, setExpanded] = useState(true);
      return (
        <>
          <Disclosure isExpanded={expanded} onExpandedChange={setExpanded}>
            <DisclosureHeader level={2} className="bc:text-sm">
              Sorting
            </DisclosureHeader>
            <DisclosurePanel>
              <p>By name</p>
            </DisclosurePanel>
          </Disclosure>
          <Button
            onPress={() => {
              setExpanded(true);
            }}
          >
            Open
          </Button>
        </>
      );
    }
    const el = await draw(<Kept />);

    // The class given takes the place of the heading's default face.
    expect(find(el, "h2").className).toContain("bc:text-sm");
    expect(find(el, "h2").className).not.toContain("bc:text-base");
    expect(state(el, "Sorting")).toEqual(["Sorting", true]);
    await click(find(el, "h2 > button"));
    expect(state(el, "Sorting")).toEqual(["Sorting", false]);
    await click([...el.querySelectorAll("button")].find((button) => button.textContent === "Open") ?? el);
    expect(state(el, "Sorting")).toEqual(["Sorting", true]);
  });

  test("keeps one of a group expanded at a time, unless the group allows several", async () => {
    const group = (several: boolean): ReactElement => (
      <DisclosureGroup allowsMultipleExpanded={several}>
        {["Rows", "Columns"].map((title) => (
          <Disclosure key={title} id={title}>
            <DisclosureHeader>{title}</DisclosureHeader>
            <DisclosurePanel>
              <p>{title} panel</p>
            </DisclosurePanel>
          </Disclosure>
        ))}
      </DisclosureGroup>
    );
    const one = await draw(group(false));
    await click(find(one, "h3 > button"));
    await click([...one.querySelectorAll("h3 > button")][1] as HTMLElement);
    expect([state(one, "Rows"), state(one, "Columns")]).toEqual([
      ["Rows", false],
      ["Columns", true],
    ]);

    const several = await draw(group(true));
    await click(find(several, "h3 > button"));
    await click([...several.querySelectorAll("h3 > button")][1] as HTMLElement);
    expect([state(several, "Rows"), state(several, "Columns")]).toEqual([
      ["Rows", true],
      ["Columns", true],
    ]);
  });
});

describe("the tabs", () => {
  const tabs = (orientation?: "vertical"): ReactElement => (
    <Tabs orientation={orientation}>
      <TabList aria-label="Views">
        <Tab id="table">Table</Tab>
        <Tab id="plot">Plot</Tab>
      </TabList>
      <TabPanel id="table">The table</TabPanel>
      <TabPanel id="plot">The plot</TabPanel>
    </Tabs>
  );

  test("show the panel of the tab picked, which a press or an arrow key picks, on a pill in the accent colour", async () => {
    const el = await draw(tabs());
    const [table, plot] = [...el.querySelectorAll<HTMLElement>('[role="tab"]')];
    if (table === undefined || plot === undefined) throw new Error("Found no tabs.");

    expect(texts(el, '[role="tabpanel"]')).toEqual(["The table"]);
    expect([table.getAttribute("aria-selected"), plot.getAttribute("aria-selected")]).toEqual(["true", "false"]);
    expect(table.querySelector(".bc\\:bg-bg-accent")).not.toBeNull();
    await click(plot);
    expect(texts(el, '[role="tabpanel"]')).toEqual(["The plot"]);
    await step(() => {
      plot.focus();
      press(plot, "ArrowLeft");
    });
    expect(texts(el, '[role="tabpanel"]')).toEqual(["The table"]);
  });

  test("lay out their list in a row, or in a column where they are vertical", async () => {
    const row = await draw(tabs());
    const column = await draw(tabs("vertical"));

    expect(find(row, '[role="tablist"]').className).toContain("bc:flex-row");
    expect(find(column, '[role="tablist"]').className).toContain("bc:flex-col");
    expect(find(column, '[role="tablist"]').getAttribute("aria-orientation")).toBe("vertical");
  });
});

describe("the toolbar", () => {
  test("moves the focus between its controls by the arrow keys, with an upright separator between its groups", async () => {
    const el = await draw(
      <Toolbar aria-label="Board">
        <Button>Options</Button>
        <Separator orientation="vertical" />
        <Button>Undo</Button>
        <Button>Redo</Button>
      </Toolbar>,
    );
    const bar = find(el, '[role="toolbar"]');
    const buttons = [...bar.querySelectorAll("button")];

    expect(bar.getAttribute("aria-label")).toBe("Board");
    expect(find(bar, '[role="separator"]').getAttribute("aria-orientation")).toBe("vertical");
    expect(find(bar, '[role="separator"]').className).toContain("bc:w-px");
    await step(() => {
      buttons[0]?.focus();
      press(buttons[0] ?? bar, "ArrowRight");
    });
    expect(document.activeElement).toBe(buttons[1]);
  });

  test("lays out a toggle button group in it as it lays out its controls", async () => {
    const el = await draw(
      <Toolbar aria-label="Tools" orientation="vertical">
        <ToggleButtonGroup aria-label="Align">
          <ToggleButton id="left">Left</ToggleButton>
          <ToggleButton id="right">Right</ToggleButton>
        </ToggleButtonGroup>
      </Toolbar>,
    );

    expect(find(el, '[role="toolbar"]').getAttribute("aria-orientation")).toBe("vertical");
    expect(find(el, '[aria-label="Align"]').getAttribute("data-orientation")).toBe("vertical");
  });

  test("lays out a separator across, by default, as a rule", async () => {
    const el = await draw(<Separator />);

    expect(find(el, "hr").className).toContain("bc:h-px");
  });
});

describe("the breadcrumbs", () => {
  test("mark the last as the current one, and act on the others by their ids, with a chevron after each but the last", async () => {
    const onAction = vi.fn();
    const el = await draw(
      <nav aria-label="Directory">
        <Breadcrumbs onAction={onAction}>
          <Breadcrumb id="root">here</Breadcrumb>
          <Breadcrumb id="data">data</Breadcrumb>
        </Breadcrumbs>
      </nav>,
    );
    const crumbs = [...el.querySelectorAll<HTMLElement>('[role="link"]')];

    expect(crumbs.map((crumb) => [crumb.textContent, crumb.getAttribute("aria-current")])).toEqual([
      ["here", null],
      ["data", "page"],
    ]);
    expect([...el.querySelectorAll("li")].map((li) => li.querySelectorAll("svg").length)).toEqual([1, 0]);
    // A breadcrumb's id goes on no element.
    expect(el.querySelector("#root, #data")).toBeNull();
    await click(crumbs[1] ?? el);
    expect(onAction).not.toHaveBeenCalled();
    await click(crumbs[0] ?? el);
    expect(onAction).toHaveBeenCalledWith("root");
  });
});

describe("the link", () => {
  test("goes where its href says, or acts as its onPress says, in the blue of a link or the colour of the text around it", async () => {
    const onPress = vi.fn();
    const el = await draw(
      <>
        <Link href="https://example.com">Docs</Link>
        <Link variant="secondary" onPress={onPress}>
          Details
        </Link>
      </>,
    );

    expect(find(el, "a").getAttribute("href")).toBe("https://example.com");
    expect(find(el, "a").className).toContain("bc:text-blue-700");
    const details = find(el, 'span[role="link"]');
    expect(details.className).toContain("bc:text-text-default");
    await click(details);
    expect(onPress).toHaveBeenCalledTimes(1);
  });
});

describe("the meter and the progress bar", () => {
  test("show their label and their value over a bar that fills as far as the value goes, the meter's in red from 80%", async () => {
    const el = await draw(
      <>
        <Meter label="Storage" value={40} />
        <Meter label="Quota" value={85} />
        <ProgressBar label="Loading" value={30} />
      </>,
    );
    const [storage, quota] = [...el.querySelectorAll<HTMLElement>('[role="meter"]')];
    const loading = find(el, '[role="progressbar"]');
    const fill = (bar: HTMLElement | undefined): HTMLElement | null | undefined => bar?.querySelector<HTMLElement>("[style]");

    expect([storage?.getAttribute("aria-valuenow"), storage?.textContent]).toEqual(["40", "Storage 40%"]);
    expect([fill(storage)?.style.width, fill(storage)?.className.includes("bc:bg-green-600")]).toEqual(["40%", true]);
    expect([fill(quota)?.className.includes("bc:bg-red-600"), quota?.querySelector('[aria-label="Alert"]') !== null]).toEqual([true, true]);
    expect([loading.getAttribute("aria-valuenow"), loading.textContent, fill(loading)?.style.width]).toEqual(["30", "Loading30%", "30%"]);
  });

  test("show a progress bar that cannot tell how far it has come with no value, and a part of its bar that slides across it", async () => {
    const el = await draw(<ProgressBar label="Loading" isIndeterminate />);
    const loading = find(el, '[role="progressbar"]');

    expect(loading.hasAttribute("aria-valuenow")).toBe(false);
    expect(loading.querySelector<HTMLElement>("[style]")?.className).toContain("bc-progress-slide");
  });
});
