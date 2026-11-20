// Core's menu, as React draws it on the page: the menu of a target with the
// icons, the details and the categories of its rows, the levels that it
// shows in place of each other, a text row, the filter, a multi, a level that
// loads or failed, and the keyboard's focus as the menu closes.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { find, press, type } from "../testing/inputs.ts";
import { logged } from "../testing/logged.ts";
import { openMenu as shownMenu } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BoardAction, CommandCall } from "./actions.ts";
import { showMenu } from "./menu.tsx";
import { openMenu } from "./quick.ts";
import type { MenuStart, OpenMenu, QuickLevel, QuickRow } from "./quick.ts";

// A board action of blockr.test on blocks, labelled by its name.
function action(name: string, more: Partial<BoardAction> = {}): BoardAction {
  return { id: `blockr.test-${name.toLowerCase().replaceAll(" ", "_")}_action`, label: name, on: ["block"], category: "test", ...more };
}

const rename = (name: string): CommandCall[] => [{ id: "blockr.core-rename_block", args: { id: "a", name } }];

const item = (label: string, more: Partial<Extract<QuickRow, { kind: "item" }>> = {}): QuickRow => ({
  kind: "item",
  label,
  pick: () => rename(label),
  ...more,
});

// Core's menu, drawn at a point beside a button that the user triggered from,
// which has the keyboard's focus as the menu opens, with parts that record each
// batch that it runs and unmount the menu as it closes.
async function drawn(start: MenuStart) {
  const trigger = document.createElement("button");
  trigger.textContent = "Open";
  document.body.append(trigger);
  trigger.focus();
  const ran: [readonly CommandCall[], string][] = [];
  let hide: () => void = () => undefined;
  let menu: OpenMenu | undefined;
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
      failed: () => undefined,
      closed() {
        hide();
      },
    });
    hide = showMenu(menu, { el: trigger, anchor: new DOMRect(10, 20, 0, 0) });
  });
  await settle();
  if (menu === undefined) throw new Error("The menu did not open.");
  return { menu, trigger, ran };
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

async function key(el: Element | null, name: string): Promise<void> {
  if (!(el instanceof HTMLElement)) throw new Error(`Found no element to press ${name} in.`);
  await step(() => {
    press(el, name);
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

// The dialog that holds the menu, which React Aria draws at the end of the
// page's body.
function dialog(): HTMLElement {
  return find(document, '[role="dialog"]');
}

afterEach(() => {
  document.body.replaceChildren();
});

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
    expect(remove.querySelector(".bc\\:group-data-focused\\:text-text-danger")).not.toBeNull();
    expect(row(menu, "Rename").querySelector(".bc\\:group-data-focused\\:text-text-danger")).toBeNull();
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
});
