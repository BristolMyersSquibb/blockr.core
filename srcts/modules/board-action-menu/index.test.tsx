// Core's menu, the module board_action_menu of blockr.core, as React draws it on
// the page, where core draws each menu with it: the menu of a target with the
// icons, the details and the categories of its rows, the levels that it
// shows in place of each other, a text row, the filter, a multi, the tick of
// an item that gives checked, a level that loads or failed, the open icons of
// a level and of an item, and the keyboard's focus as the menu closes.
// The tests that core ships for a package's module run on it too
// (./module.test.ts).

import { act } from "react";
import { describe, expect, test } from "vitest";
import { SquareArrowUpRightExit } from "@blockr/ui/icons";
import { find, openMenu as shownMenu, pointerPress, press, step, type } from "@blockr/ui/stubs";
import { CORE_MENU, drawWith, openMenu } from "../../actions/index.ts";
import type { BoardAction, CommandCall, DrivenMenu, MenuStart, QuickLevel, QuickRow } from "../../actions/index.ts";
import { shownIcon } from "../../testing/icons.ts";
import { logged } from "../../testing/logged.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { idText, interned } from "../../util/index.ts";
import coreMenu from "./index.tsx";

// A board action of blockr.test on blocks, labelled by its name,
// with the package that core sets as it registers one.
function action(name: string, more: Partial<BoardAction> = {}): BoardAction {
  return {
    package: "blockr.test",
    id: `${name.toLowerCase().replaceAll(" ", "_")}_action`,
    label: name,
    on: ["block"],
    category: "test",
    ...more,
  };
}

const rename = (name: string): CommandCall[] => [{ id: interned("blockr.core", "change_block"), args: { id: "a", name } }];

const item = (label: string, more: Partial<Extract<QuickRow, { kind: "item" }>> = {}): QuickRow => ({
  kind: "item",
  label,
  pick: () => rename(label),
  ...more,
});

// Core's menu, drawn as core draws a menu with it, at a point beside a button
// that the user triggered from, which has the keyboard's focus as the menu
// opens, with parts that record each batch that it runs, a board action's
// run's too, and each advanced action that it opens, with what the open icon
// handed it, and unmount the menu as it closes.
async function drawn(start: MenuStart) {
  const trigger = document.createElement("button");
  trigger.textContent = "Open";
  document.body.append(trigger);
  trigger.focus();
  const ran: [readonly CommandCall[], string][] = [];
  const advanced: [string, { readonly picked?: Json; readonly filter?: string }][] = [];
  let hide: () => void = () => undefined;
  let menu: DrivenMenu | undefined;
  await step(() => {
    menu = openMenu(start, {
      board: () => ({}) as Board,
      target: { block: "a" },
      core: () => ({}) as CoreApi,
      applies: () => true,
      run(batch, by) {
        ran.push([batch, by.id]);
        return Promise.resolve();
      },
      // Runs the board action's run and records its batch, as core's part
      // runs it as trigger() does.
      async trigger(by) {
        const batch = await by.run?.({} as Board, { block: "a" }, {} as CoreApi);
        if (batch !== undefined) ran.push([batch, by.id]);
      },
      advanced(by, opened) {
        advanced.push([by.id, opened]);
        return Promise.resolve();
      },
      failed: () => undefined,
      closed() {
        hide();
      },
    });
    hide = drawWith(coreMenu, idText(CORE_MENU))(menu, { el: trigger, anchor: new DOMRect(10, 20, 0, 0) });
  });
  await settle();
  if (menu === undefined) throw new Error("The menu did not open.");
  return { menu, trigger, ran, advanced };
}

// Waits for what React Aria scheduled, such as moving the focus.
async function settle(): Promise<void> {
  await act(() => new Promise((resolve) => setTimeout(resolve, 50)));
}

async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
  await settle();
}

async function key(el: Element | null, name: string, init: KeyboardEventInit = {}): Promise<void> {
  if (!(el instanceof HTMLElement)) throw new Error(`Found no element to press ${name} in.`);
  await step(() => {
    press(el, name, init);
  });
  await settle();
}

// The rows of the menu, by what assistive technology names each: its label
// with its detail.
function names(menu: ParentNode): string[] {
  return [...menu.querySelectorAll('[role="menuitem"][data-key], [role="menuitemcheckbox"][data-key]')].map(
    (row) => row.getAttribute("aria-label") ?? row.textContent.trim(),
  );
}

function row(menu: ParentNode, name: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>('[role="menuitem"][data-key], [role="menuitemcheckbox"][data-key]')].find(
    (each) => (each.getAttribute("aria-label") ?? each.textContent.trim()) === name,
  );
  if (found === undefined) throw new Error(`Found no row ${name}.`);
  return found;
}

// What assistive technology reads as the row's description.
function description(el: HTMLElement): string {
  const ids = el.getAttribute("aria-describedby")?.split(" ") ?? [];
  return ids.map((id) => document.getElementById(id)?.textContent ?? "").join(" ");
}

// The dialog that holds the menu, which React Aria draws at the end of the
// page's body.
function dialog(): HTMLElement {
  return find(document, '[role="dialog"]');
}

describe("core's menu", () => {
  test("draws the menu of a target at the anchor, each row with its icon, label and detail, with a divider between two categories, and the rows that remove in the danger colour under the pointer", async () => {
    const { trigger } = await drawn({
      actions: [
        action("Remove block", { category: "remove", icon: '<svg data-icon="trash"></svg>', run: () => undefined }),
        action("Rename", { category: "edit", quick: () => ({ rows: [] }) }),
        action("Copy block ID", { category: "clipboard", detail: () => "b", run: () => undefined }),
        action("Add to document", { category: "document", run: () => undefined }),
      ],
    });

    const menu = shownMenu("Board actions");
    expect(names(menu)).toEqual(["Rename", "Copy block ID, b", "Add to document", "Remove block"]);
    expect(menu.querySelectorAll('[role="separator"]')).toHaveLength(3);
    expect(row(menu, "Copy block ID, b").textContent).toBe("Copy block IDb");
    const remove = row(menu, "Remove block");
    expect(remove.querySelector('svg[data-icon="trash"]')?.closest("[aria-hidden=true]")).not.toBeNull();
    expect(remove.querySelector(".bc\\:group-data-focused\\:text-destructive-text")).not.toBeNull();
    expect(remove.classList.contains("bc:data-focused:bg-destructive/10!")).toBe(true);
    expect(remove.classList.contains("bc:data-hovered:bg-destructive/10!")).toBe(true);
    expect(row(menu, "Rename").querySelector(".bc\\:group-data-focused\\:text-destructive-text")).toBeNull();
    expect(row(menu, "Rename").classList.contains("bc:data-focused:bg-destructive/10!")).toBe(false);
    // The popover hangs from the box of the anchor.
    const anchor = [...document.querySelectorAll<HTMLElement>("div[aria-hidden=true]")].find((el) => el.style.position === "fixed");
    expect([anchor?.style.left, anchor?.style.top]).toEqual(["10px", "20px"]);
    // The focus moved into the menu, on its first row.
    expect(document.activeElement).toBe(row(menu, "Rename"));
    expect(trigger.isConnected).toBe(true);
  });

  test("runs a row that the user clicks, closes, and gives the keyboard's focus back to the element that the user triggered from", async () => {
    const copy = action("Copy block ID", { category: "clipboard", run: () => rename("Copied") });
    const { trigger, ran, menu } = await drawn({ actions: [copy] });

    await click(row(shownMenu("Board actions"), "Copy block ID"));

    expect(ran).toEqual([[rename("Copied"), copy.id]]);
    expect(menu.closed()).toBe(true);
    expect(document.querySelector('[role="menu"]')).toBeNull();
    expect(document.activeElement).toBe(trigger);
  });

  test("shows the next level in place of the menu's, from which Escape goes back, and closes from the first level", async () => {
    const asks = action("Insert", { quick: () => ({ title: "Insert after", rows: [item("Head"), item("Filter")] }) });
    const { trigger, menu } = await drawn({ actions: [asks, action("Other", { run: () => undefined })] });

    await click(row(shownMenu("Board actions"), "Insert"));
    expect(dialog().getAttribute("aria-label")).toBe("Insert after");
    expect(find(dialog(), "p").textContent).toBe("Insert after");
    expect(names(shownMenu("Insert after"))).toEqual(["Head", "Filter"]);
    expect(document.activeElement).toBe(row(shownMenu("Insert after"), "Head"));

    await key(document.activeElement, "Escape");
    expect(names(shownMenu("Board actions"))).toEqual(["Insert", "Other"]);
    await key(document.activeElement, "Escape");

    expect(menu.closed()).toBe(true);
    expect(document.querySelector('[role="dialog"]')).toBeNull();
    expect(document.activeElement).toBe(trigger);
  });

  test("draws a text row as a field that holds the value, selected, says below it why its check refuses a value, and submits it on Enter", async () => {
    const asks = action("Rename", {
      quick: () => ({
        rows: [
          {
            kind: "text",
            label: "Name",
            value: "Head",
            check: (value) => (value === "" ? "A name cannot be empty." : undefined),
            submit: (value) => rename(value),
          },
        ],
      }),
    });
    const { ran } = await drawn({ action: asks });

    const input = find(dialog(), "input", HTMLInputElement);
    expect(dialog().getAttribute("aria-label")).toBe("Rename");
    expect(document.activeElement).toBe(input);
    expect([input.value, input.selectionStart, input.selectionEnd]).toEqual(["Head", 0, 4]);
    expect(find(dialog(), "label").textContent).toBe("Name");

    await step(() => {
      type(input, "");
    });
    await key(input, "Enter");
    expect(find(dialog(), '[slot="errorMessage"]').textContent).toBe("A name cannot be empty.");
    expect(input.getAttribute("aria-invalid")).toBe("true");

    await step(() => {
      type(input, "Iris");
    });
    expect(dialog().querySelector('[slot="errorMessage"]')).toBeNull();
    // Enter's default stays undone, so that the button that the focus returns
    // to does not take the key as a click that opens the menu again.
    const enter = new KeyboardEvent("keydown", { key: "Enter", bubbles: true, cancelable: true });
    await step(() => {
      input.dispatchEvent(enter);
    });
    await settle();
    expect(enter.defaultPrevented).toBe(true);
    expect(ran).toEqual([[rename("Iris"), asks.id]]);
    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("draws a filter that has the focus, narrows the rows as the user types, says where it leaves none, and picks the first row left on Enter", async () => {
    const level: QuickLevel = {
      filter: true,
      rows: [item("Head", { heading: "Transform" }), item("Filter", { heading: "Transform" }), item("Scatter", { heading: "Plot" })],
    };
    const adds = action("Add block", { quick: () => level });
    const { ran } = await drawn({ action: adds });

    const filter = find(dialog(), "input[data-filter]", HTMLInputElement);
    expect(document.activeElement).toBe(filter);
    const menu = shownMenu("Add block");
    expect(names(menu)).toEqual(["Head", "Filter", "Scatter"]);
    expect([...menu.querySelectorAll('[role="group"]')].map((group) => group.getAttribute("aria-labelledby") !== null)).toEqual([true, true]);
    expect([...menu.querySelectorAll("header")].map((header) => header.textContent)).toEqual(["Transform", "Plot"]);

    await step(() => {
      type(filter, "zzz");
    });
    expect(names(shownMenu("Add block"))).toEqual([]);
    expect(shownMenu("Add block").textContent).toBe("No matches");

    await step(() => {
      type(filter, "t");
    });
    expect(names(shownMenu("Add block"))).toEqual(["Filter", "Scatter"]);
    await key(filter, "Enter");

    expect(ran).toEqual([[rename("Filter"), adds.id]]);
  });

  test("draws a multi as a group of choices that the user ticks, which apply as Tab closes the menu", async () => {
    const applied: (readonly string[])[] = [];
    const stacks = action("Stacks", {
      quick: () => ({
        rows: [
          {
            kind: "multi",
            label: "Blocks",
            choices: [
              { id: "a", label: "Iris", picked: true },
              { id: "b", label: "Head", picked: false },
            ],
            apply(picked) {
              applied.push(picked);
              return rename("Ticked");
            },
          },
        ],
      }),
    });
    const { ran } = await drawn({ action: stacks });

    const menu = shownMenu("Stacks");
    expect(find(menu, "header").textContent).toBe("Blocks");
    expect([row(menu, "Iris"), row(menu, "Head")].map((choice) => choice.getAttribute("aria-checked"))).toEqual(["true", "false"]);
    await click(row(menu, "Head"));
    expect(row(shownMenu("Stacks"), "Head").getAttribute("aria-checked")).toBe("true");
    expect(ran).toEqual([]);

    await key(document.activeElement, "Tab");
    expect(applied).toEqual([["a", "b"]]);
    expect(ran).toEqual([[rename("Ticked"), stacks.id]]);
    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("draws a tick in a slot at the start of an item that gives checked, while checked is true, which assistive technology hears as a checkbox of the menu, checked or not, and runs the item's pick on a click and on Enter", async () => {
    const layout = action("Layout", {
      quick: () => ({
        rows: [
          item("Lay out again"),
          item("Left to right", { heading: "Direction", checked: true }),
          item("Top to bottom", { heading: "Direction", checked: false }),
          item("Snap to grid", { heading: "Grid", detail: "20 px", checked: true }),
          item("Grid size…", { heading: "Grid" }),
        ],
      }),
    });
    const NAMES = ["Lay out again", "Left to right", "Top to bottom", "Snap to grid, 20 px", "Grid size…"];

    const clicked = await drawn({ action: layout });
    const menu = shownMenu("Layout");
    expect(names(menu)).toEqual(NAMES);
    const rows = NAMES.map((name) => row(menu, name));
    expect(rows.map((each) => [each.getAttribute("role"), each.getAttribute("aria-checked")])).toEqual([
      ["menuitem", null],
      ["menuitemcheckbox", "true"],
      ["menuitemcheckbox", "false"],
      ["menuitemcheckbox", "true"],
      ["menuitem", null],
    ]);
    // Each item that gives checked keeps a slot at its start, before the span
    // of its label, which holds the tick while checked is true, and the other
    // items keep none.
    const slots = rows.map((each) => {
      const first = each.firstElementChild;
      if (first?.textContent !== "") return "none";
      return first.querySelector("svg") === null ? "empty" : "tick";
    });
    expect(slots).toEqual(["none", "tick", "empty", "tick", "none"]);
    expect(row(menu, "Snap to grid, 20 px").textContent).toBe("Snap to grid20 px");
    // Each heading shows once, over all of its items.
    expect([...menu.querySelectorAll("header")].map((header) => header.textContent)).toEqual(["Direction", "Grid"]);
    await click(row(menu, "Top to bottom"));
    expect(clicked.ran).toEqual([[rename("Top to bottom"), layout.id]]);
    document.body.replaceChildren();

    const entered = await drawn({ action: layout });
    const checked = row(shownMenu("Layout"), "Left to right");
    await step(() => {
      checked.focus();
    });
    await key(checked, "Enter");
    expect(entered.ran).toEqual([[rename("Left to right"), layout.id]]);
    expect(entered.menu.closed()).toBe(true);
  });

  test("says that a level loads, shows the message of a promise that rejects, and hears Escape in both", async () => {
    let fail: (error: Error) => void = () => undefined;
    const slow = action("Columns", { quick: () => new Promise<QuickLevel>((_resolve, reject) => (fail = reject)) });
    const { menu } = await drawn({ actions: [slow] });

    await click(row(shownMenu("Board actions"), "Columns"));
    expect(find(dialog(), '[role="status"]').textContent).toBe("Loading…");
    expect(dialog().contains(document.activeElement)).toBe(true);

    const reported = logged();
    await step(() => {
      fail(new Error("R could not read the columns."));
    });
    await settle();
    reported.restore();
    expect(find(dialog(), '[role="alert"]').textContent).toBe("R could not read the columns.");
    expect(reported.errors()).toHaveLength(1);

    await key(document.activeElement, "Escape");
    expect(names(shownMenu("Board actions"))).toEqual(["Columns"]);
    expect(menu.closed()).toBe(false);
  });

  test("draws the open icon of a level at the end of its title's line, named Show all options, and of an item at its end, each with its name and key in its tooltip, which assistive technology hears, and which open the advanced action without picking the item", async () => {
    const adds = action("Append block", {
      advanced: { render: () => undefined },
      quick: () => ({
        title: "Append",
        filter: true,
        picked: { input: "data" },
        rows: [item("Head", { advanced: { class: "head" } }), item("Filter")],
      }),
    });
    const tip = "Show all options (Ctrl+Enter)";
    const keys = "Control+Enter Meta+Enter";

    const level = await drawn({ action: adds });
    const open = find(dialog(), 'button[aria-label="Show all options"]');
    expect(open.closest("div")?.querySelector("p")?.textContent).toBe("Append");
    expect(shownIcon(open)).toEqual(SquareArrowUpRightExit);
    expect([open.title, open.getAttribute("aria-keyshortcuts")]).toEqual([tip, keys]);
    expect(find(dialog(), "input[data-filter]").getAttribute("aria-keyshortcuts")).toBe(keys);
    const menu = shownMenu("Append");
    const head = row(menu, "Head");
    expect(shownIcon(find(head, "[data-open-row]"))).toEqual(SquareArrowUpRightExit);
    expect(head.querySelector<HTMLElement>("[data-open-row]")?.title).toBe(tip);
    expect([head.getAttribute("aria-label"), description(head)]).toEqual(["Head", tip]);
    expect(row(menu, "Filter").querySelector("[data-open-row]")).toBeNull();
    expect(description(row(menu, "Filter"))).toBe("");
    await step(() => {
      type(find(dialog(), "input[data-filter]", HTMLInputElement), "He");
    });
    await click(open);
    expect(level.advanced).toEqual([[adds.id, { picked: { input: "data" }, filter: "He" }]]);
    expect(level.menu.closed()).toBe(true);
    expect(document.querySelector('[role="dialog"]')).toBeNull();
    document.body.replaceChildren();

    // The mouse presses the item's open icon and lets it go, as a user does.
    // React Aria's menu item hears the pointer's events, and click() sends none.
    const onItem = await drawn({ action: adds });
    const icon = row(shownMenu("Append"), "Head").querySelector<HTMLElement>("[data-open-row]");
    if (icon === null) throw new Error("Found no open icon on the row Head.");
    await pointerPress(icon);
    await settle();
    expect(onItem.advanced).toEqual([[adds.id, { picked: { class: "head" }, filter: "" }]]);
    expect(onItem.ran).toEqual([]);
  });

  test("draws the detail and the open icon of an item in one span after its label, which blockr.ui's item holds on the label's line", async () => {
    await drawn({
      action: action("Append block", {
        advanced: { render: () => undefined },
        quick: () => ({
          title: "Append",
          rows: [
            item("Head", { detail: "first rows", advanced: { class: "head" } }),
            item("Filter", { advanced: { class: "filter" } }),
            item("Tail", { detail: "last rows" }),
            item("Arrange"),
          ],
        }),
      }),
    });

    // The item of blockr.ui lays out a grid of two columns: the label's, and
    // one at its right. A third child of the grid would go to a second line.
    // The grid holds the span of the icon and the label first. Each child gives
    // its text and whether it holds the open icon. The description that only
    // assistive technology reads takes no place.
    const menu = shownMenu("Append");
    const cells = (name: string): [string, boolean][] =>
      [...(row(menu, name).querySelector(".bc\\:min-w-0.bc\\:items-center")?.parentElement?.children ?? [])]
        .filter((cell) => cell.getAttribute("slot") !== "description")
        .map((cell) => [cell.textContent, cell.querySelector("[data-open-row]") !== null]);
    expect(cells("Head, first rows")).toEqual([
      ["Head", false],
      ["first rows", true],
    ]);
    expect(cells("Filter")).toEqual([
      ["Filter", false],
      ["", true],
    ]);
    expect(cells("Tail, last rows")).toEqual([
      ["Tail", false],
      ["last rows", false],
    ]);
    expect(cells("Arrange")).toEqual([["Arrange", false]]);
  });

  test("draws no open icon where the board action has no advanced action", async () => {
    await drawn({ action: action("Rename", { quick: () => ({ title: "Rename", rows: [item("Head", { advanced: { class: "head" } })] }) }) });

    expect(dialog().querySelector('button[aria-label="Show all options"]')).toBeNull();
    expect(dialog().querySelector("[data-open-row]")).toBeNull();
  });

  test("opens the advanced action on Mod+Enter, with the item's advanced from a row that shows the open icon, and with the level's picked otherwise", async () => {
    const adds = action("Append block", {
      advanced: { render: () => undefined },
      quick: () => ({ filter: true, picked: { input: "data" }, rows: [item("Head", { advanced: { class: "head" } }), item("Filter")] }),
    });

    const fromRow = await drawn({ action: adds });
    const filter = find(dialog(), "input[data-filter]", HTMLInputElement);
    await key(filter, "ArrowDown");
    await key(filter, "Enter", { ctrlKey: true });
    expect(fromRow.advanced).toEqual([[adds.id, { picked: { class: "head" }, filter: "" }]]);
    expect(fromRow.ran).toEqual([]);
    document.body.replaceChildren();

    const fromLevel = await drawn({ action: adds });
    await key(find(dialog(), "input[data-filter]", HTMLInputElement), "Enter", { metaKey: true });
    expect(fromLevel.advanced).toEqual([[adds.id, { picked: { input: "data" }, filter: "" }]]);
    expect(fromLevel.ran).toEqual([]);
    document.body.replaceChildren();

    // A row that gives checked, with the focus, rather than the filter.
    const spacing = action("Spacing", {
      advanced: { render: () => undefined },
      quick: () => ({ picked: { spacing: "normal" }, rows: [item("Wide", { checked: true, advanced: { spacing: "wide" } })] }),
    });
    const fromChecked = await drawn({ action: spacing });
    const wide = row(shownMenu("Spacing"), "Wide");
    expect(document.activeElement).toBe(wide);
    await key(wide, "Enter", { ctrlKey: true });
    expect(fromChecked.advanced).toEqual([[spacing.id, { picked: { spacing: "wide" } }]]);
    expect(fromChecked.ran).toEqual([]);
  });
});
