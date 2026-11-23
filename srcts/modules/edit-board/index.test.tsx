// Core's Edit board plugin on the CoreApi for tests: the board of the fixture
// hello, whose app runs the plugin, with R played from the protocol's
// fixtures, the plugin's UI mounted as a frontend places it, and the user
// acting on it. The menus that the plugin opens are core's, which the test
// drives as a user would, picking their rows by their labels.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f), and its stack s1, Input, holds a and b.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import type { BoardAction } from "../../actions/index.ts";
import type { HelloReply } from "../../protocol/index.ts";
import type { PluginClass } from "../../store/index.ts";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions, StubMenu } from "../../stubs/index.ts";
import { chipLabels, labelled, listed, listFromField, optionFromLabel, optionLabels, picked, selectButton, selectShows } from "../../testing/select.ts";
import { find, press, texts, type } from "../../testing/inputs.ts";
import { held } from "../../testing/notifications.ts";
import { openDialog } from "../../testing/overlays.ts";
import { listedModules } from "../../testing/plugins.ts";
import { step } from "../../testing/react.ts";
import { rowCheckbox } from "../../testing/rows.ts";
import { withPlugins } from "../../testing/store.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import { editTitle } from "./menus.tsx";
import * as editBoard from "./index.tsx";

// The class of the plugin, which the app of the fixture hello runs.
const EDIT = "blockr.core-edit_board_plugin";

interface Mounted {
  readonly el: HTMLElement;
  readonly core: CoreApi;
  readonly board: () => Board;
  // The notifications that the plugin raised, in order, while they hold.
  readonly notes: readonly Notification[];
  // The menu that is open, as the plugin opened it, which the test drives.
  readonly menu: () => StubMenu;
  // Ends the mount, as a frontend does.
  readonly end: () => void;
}

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) stop();
});

// The plugin's UI on the board of the fixture hello, or of the reply given,
// mounted in an element on the page, as a frontend places it.
async function mounted(options: StubCoreOptions = {}): Promise<Mounted> {
  const el = document.createElement("div");
  document.body.append(el);
  const stubbed = await act(() => stubCore({ ...options, modules: { "blockr.core-edit_board": editBoard, ...options.modules } }));
  stops.push(stubbed.stop);
  const notes: Notification[] = [];
  stubbed.core.notifications.subscribe((all) => {
    notes.splice(
      0,
      notes.length,
      ...all.filter(({ source }) => source.kind === "module" && "plugin" in source.module.object && source.module.object.plugin === EDIT),
    );
  });
  let end = (): void => undefined;
  await act(async () => {
    end = stubbed.core.mountPlugin(el, EDIT);
    await stubbed.settled();
  });
  const menu = (): StubMenu => {
    const open = stubbed.menu();
    if (open === undefined) throw new Error("No menu is open.");
    return open;
  };
  return { el, core: stubbed.core, board: () => stubbed.core.store.board(), notes, menu, end };
}

// A board action of blockr.test on the kinds of target given, labelled by its
// name, which runs the batch that `run` returns.
function action(name: string, on: BoardAction["on"], run: NonNullable<BoardAction["run"]>): BoardAction {
  return { id: `blockr.test-${name.toLowerCase().replaceAll(" ", "_")}_action`, label: name, on, category: "test", run };
}

// The plugin on the board of the fixture hello, or of the reply given, whose
// app runs a plugin of blockr.test too, whose class lists the board actions
// given.
async function withActions(actions: readonly BoardAction[], hello: HelloReply = HELLO): Promise<Mounted> {
  const cls: PluginClass = { id: "blockr.test-doc_plugin", boardActions: actions };
  return mounted({ hello: withPlugins({ [cls.id]: { name: "Document" } }, hello), modules: listedModules([cls]) });
}

// The plugin's UI over a store that refuses every edit: the board of the
// fixture hello under the board lock. The plugin draws no control there, so
// the test hands it a CoreApi that says the board is open, and draws every
// control.
async function refusing(): Promise<Omit<Mounted, "notes" | "menu">> {
  const el = document.createElement("div");
  document.body.append(el);
  const hello: HelloReply = { ...HELLO, lock: { board: true, load: false } };
  const stubbed = await act(() => stubCore({ hello, modules: { "blockr.core-edit_board": editBoard } }));
  const core: CoreApi = { ...stubbed.core, lock: { ...stubbed.core.lock, board: false } };
  let end = (): void => undefined;
  await act(async () => {
    end = editBoard.default.ui.render({ el, core, signal: new AbortController().signal });
    await stubbed.settled();
  });
  stops.push(() => {
    act(() => {
      end();
    });
  }, stubbed.stop);
  return { el, core: stubbed.core, board: () => stubbed.core.store.board(), end };
}

// The plugin's section of the title given.
function section(el: HTMLElement, title: "Blocks" | "Links" | "Stacks"): HTMLElement {
  return find(el, `section[aria-label="${title}"]`);
}

// The bar of the selection, or null where the plugin draws none.
function bar(el: HTMLElement): HTMLElement | null {
  return el.querySelector('[role="region"][aria-label="Selection"]');
}

// The form of the name given, such as "Add block".
function form(el: ParentNode, name: string): HTMLFormElement {
  return find(el, `form[aria-label="${name}"]`, HTMLFormElement);
}

// The button whose label or text is the one given, in `el`.
function button(el: ParentNode, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.getAttribute("aria-label") === text || b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}

// The row of a table of the object with the id given, such as "ab" for the
// link from Iris to Head.
function row(el: ParentNode, id: string): HTMLElement {
  return find(el, `[role="row"][data-key="${id}"]`);
}

// The ids of the objects of the section's table, in the order of its rows.
function rows(el: HTMLElement, title: "Blocks" | "Links" | "Stacks"): string[] {
  return [...section(el, title).querySelectorAll('[role="row"][data-key]')].map((tr) => tr.getAttribute("data-key") ?? "");
}

// The names of the blocks or the stacks of the section's table, in the order
// of its rows, as the cell that names each row shows them.
function names(el: HTMLElement, title: "Blocks" | "Stacks"): string[] {
  return [...section(el, title).querySelectorAll('[role="row"][data-key] [role="rowheader"]')].map((cell) => cell.textContent);
}

// The text of each cell of a row that shows its object, after its checkbox
// and before its "…".
function cells(tr: HTMLElement): string[] {
  return [...tr.querySelectorAll('[role="rowheader"], [role="gridcell"]')].slice(1).flatMap((cell) => (cell.textContent === "⋯" ? [] : [cell.textContent]));
}

// The ticked rows of the section's table, by the ids of their objects.
function ticked(el: HTMLElement, title: "Blocks" | "Links" | "Stacks"): string[] {
  return rows(el, title).filter((id) => rowCheckbox(row(section(el, title), id)).checked);
}

// Opens the menu of the object of a row by the "…" at the row's end, as the
// user does, and returns the menu, which core opened.
async function openMenu(m: Mounted, label: string): Promise<StubMenu> {
  await click(button(m.el, label));
  return m.menu();
}

// The labels of the rows that the menu shows.
function menuLabels(menu: StubMenu): string[] {
  return menu.level()?.rows.map(({ label }) => label) ?? [];
}

// What a banner of the plugin says of why the store or the command registry
// refused an edit: the sentence that names the edit, then each reason.
function said(alert: Element): string[] {
  return [...alert.querySelectorAll("p, li")].map((line) => line.textContent);
}

// What the plugin says in `el` of why the store or the command registry
// refused an edit, banner by banner, in order.
function refusals(el: ParentNode): string[][] {
  return [...el.querySelectorAll('[role="alert"]')].map(said);
}

// What the plugin says below the section's table of why the store or the
// command registry refused a board action that the menu of one of its rows
// ran.
function belowTable(el: HTMLElement): string[][] {
  const banners: string[][] = [];
  const box = find(el, '[role="grid"]').parentElement;
  for (let next = box?.nextElementSibling; next?.getAttribute("role") === "alert"; next = next.nextElementSibling) {
    banners.push(said(next));
  }
  return banners;
}

// The lines below a field, its description and its error, which React Aria
// names in the field's aria-describedby, in that order.
function note(field: HTMLElement): string | undefined {
  const ids = (field.getAttribute("aria-describedby") ?? "").split(" ");
  const texts = ids.flatMap((id) => {
    const text = field.ownerDocument.getElementById(id)?.textContent ?? "";
    return text === "" ? [] : [text];
  });
  return texts.length === 0 ? undefined : texts.join(" ");
}

// Does what the user does, and lets what it set off run, the commands that a
// form runs as the user submits it among them.
async function acting(fn: () => void): Promise<void> {
  await step(fn);
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
}

async function click(el: HTMLElement): Promise<void> {
  await acting(() => {
    el.click();
  });
}

// A click on the element as a browser fires that of element.click(): a
// PointerEvent without a pointer type, which React Aria takes for the click of
// a screen reader, as on a row that it then ticks.
async function clickAsBrowser(el: HTMLElement): Promise<void> {
  await acting(() => {
    el.dispatchEvent(new PointerEvent("click", { bubbles: true, cancelable: true, pointerType: "", detail: 0, button: 0 }));
  });
}

// Picks the row of the menu with the label given, and lets what it set off
// run, as the plugin drawing the board as the batch left it.
async function pick(menu: StubMenu, label?: string): Promise<void> {
  await act(() => menu.pick(label));
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
}

// Clicks the open icon of the menu's level, which opens the board action's
// advanced action where the plugin gives it room, and resolves once the
// plugin has drawn it, while the advanced action runs on.
async function openAdvanced(menu: StubMenu): Promise<void> {
  await acting(() => {
    void menu.openAdvanced();
  });
}

// The button of a stack form's colour picker, which names the colour by its
// hex beside its swatch.
function colorButton(form: HTMLFormElement): HTMLButtonElement {
  return find(form, "fieldset button", HTMLButtonElement);
}

// The name of the colour that a swatch shows, as a screen reader says it.
function swatchName(el: ParentNode): string | null {
  return el.querySelector('[role="img"]')?.getAttribute("aria-label") ?? null;
}

// Picks the option of a searchable dropdown, through its search.
async function chosen(el: ParentNode, label: string, search: string, option?: string): Promise<void> {
  await step(() => {
    labelled(el, label).click();
  });
  await step(() => {
    type(labelled(el, label), search);
  });
  await step(() => {
    optionFromLabel(labelled(el, label), option ?? search).click();
  });
}

// The blocks that the board holds beside those of the fixture's board.
function added(board: Board): [string, Board["blocks"][string]][] {
  return Object.entries(board.blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
}

describe("the blocks", () => {
  test("are added once the user submits the form, of the class that the searchable dropdown picks, under the ID and the name that the user gives", async () => {
    const { el, board, notes } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Blocks"), "Add block");
    const classes = labelled(add(), "Block class");

    // The dropdown lists the block registry's entries by category.
    await step(() => {
      classes.click();
    });
    expect(optionLabels(classes)).toEqual([
      "input: data upload block",
      "input: dataset block",
      "plot: scatter plot block",
      "transform: head/tail block",
      "transform: merge block",
      "transform: rbind block",
      "transform: subset block",
    ]);
    await step(() => {
      type(classes, "scatter");
    });
    expect(optionLabels(classes)).toEqual(["plot: scatter plot block"]);

    // Picking a class adds nothing, but shows the class's description and the
    // name that a block of it takes.
    await step(() => {
      optionFromLabel(classes, "scatter plot block").click();
    });
    expect(added(board())).toEqual([]);
    expect(classes.value).toBe("scatter plot block");
    expect(note(classes)).toBe("Scatter plotting");
    expect(labelled(add(), "Name").placeholder).toBe("Scatter");

    await step(() => {
      type(labelled(add(), "ID"), "petals");
      type(labelled(add(), "Name"), "Petal plot");
    });
    await click(button(add(), "Add block"));

    expect(board().blocks.petals).toMatchObject({ registry_id: "blockr.core-scatter_block", name: "Petal plot" });
    expect(Object.values(board().links).filter((l) => l.to === "petals")).toEqual([]);
    expect(notes).toEqual([]);
    // The form, the advanced action of core's Add block, which core renders
    // again, starts afresh for the next block.
    expect([labelled(add(), "Block class").value, labelled(add(), "ID").value, labelled(add(), "Name").value]).toEqual(["", "", ""]);
    expect(cells(row(section(el, "Blocks"), "petals"))).toEqual(["Petal plot", "petals", "scatter plot block"]);
  });

  test("get an ID that core mints and the name of their class where the user leaves the fields empty", async () => {
    const { el, board } = await mounted();
    const add = form(section(el, "Blocks"), "Add block");

    await chosen(add, "Block class", "dataset", "dataset block");
    await click(button(add, "Add block"));

    const [first] = added(board());
    expect(first?.[0]).toMatch(/^[a-z]+_[a-z]+$/);
    expect(first?.[1]).toMatchObject({ registry_id: "blockr.core-dataset_block", name: "Dataset" });
  });

  test("are not added where the form says, next to the field, what to change: a class that is missing, or an ID of digits alone or that the board holds", async () => {
    const { el, board, notes } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Blocks"), "Add block");

    // Without a class, the form names the field and puts the focus on it.
    await click(button(add(), "Add block"));
    const classes = labelled(add(), "Block class");
    expect(classes.getAttribute("aria-invalid")).toBe("true");
    expect(note(classes)).toBe("Pick the class of the new block.");
    expect(document.activeElement).toBe(classes);

    await chosen(add(), "Block class", "dataset", "dataset block");
    expect(labelled(add(), "Block class").getAttribute("aria-invalid")).toBeNull();

    // An ID is checked as the user leaves its field.
    const id = labelled(add(), "ID");
    await step(() => {
      type(id, "12");
      id.dispatchEvent(new FocusEvent("focusout", { bubbles: true }));
    });
    expect(note(labelled(add(), "ID"))).toBe('Optional. Made up where left empty. An ID needs a character other than a digit, such as "block_12".');
    // Typing takes the error away until the user leaves the field again.
    await step(() => {
      type(labelled(add(), "ID"), "a");
    });
    expect(labelled(add(), "ID").getAttribute("aria-invalid")).toBeNull();

    await click(button(add(), "Add block"));
    expect(note(labelled(add(), "ID"))).toBe('Optional. Made up where left empty. The board holds a block "a" already.');
    expect(document.activeElement).toBe(labelled(add(), "ID"));
    expect(added(board())).toEqual([]);
    expect(notes).toEqual([]);
  });

  test("are listed in a table, which a search narrows and which shows ten at a time", async () => {
    const { el, core } = await mounted();
    for (let i = 1; i <= 8; i++) {
      await act(() => core.commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block", id: `h${String(i)}`, name: `Rows ${String(i)}` }));
    }
    const blocks = section(el, "Blocks");
    expect(names(el, "Blocks")).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Bind", "Scatter", "Rows 1", "Rows 2", "Rows 3", "Rows 4"]);
    expect(find(blocks, 'nav[aria-label="Pages of blocks"] span').textContent).toBe("Rows 1–10 of 14");
    expect(cells(row(blocks, "b"))).toEqual(["Head", "b", "head/tail block"]);

    await click(button(blocks, "Next"));
    expect(names(el, "Blocks")).toEqual(["Rows 5", "Rows 6", "Rows 7", "Rows 8"]);

    // The search finds blocks by name, ID or class, from the first page.
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "merge");
    });
    expect(names(el, "Blocks")).toEqual(["Merge"]);
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "h7");
    });
    expect(names(el, "Blocks")).toEqual(["Rows 7"]);
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "head/tail");
    });
    expect(names(el, "Blocks")).toEqual(["Head", "Rows 1", "Rows 2", "Rows 3", "Rows 4", "Rows 5", "Rows 6", "Rows 7", "Rows 8"]);
    expect(blocks.querySelector("nav")).toBeNull();

    // The button of the search clears it, and so does Escape, each back on
    // the first page.
    const search = find(blocks, "search input", HTMLInputElement);
    await click(find(blocks, 'search button[aria-label="Clear search"]'));
    expect([search.value, names(el, "Blocks")[0], names(el, "Blocks").length]).toEqual(["", "Iris", 10]);
    await step(() => {
      type(search, "block");
    });
    await click(button(blocks, "Next"));
    await step(() => {
      press(search, "Escape");
    });
    expect([search.value, names(el, "Blocks")[0]]).toEqual(["", "Iris"]);
  });

  test("open the menu of their block from the … button at the end of each row, which offers what every menu of the block offers, another package's board actions among them", async () => {
    const documented = action("Add to document", ["block"], (_board, target) => [
      { id: "blockr.core-rename_block", args: { id: "block" in target ? target.block : "", name: "In the document" } },
    ]);
    const m = await withActions([documented]);
    const blocks = section(m.el, "Blocks");

    // The menu offers each board action that on() gives the block, in the
    // order of core's menus, which end with those that remove.
    const menu = await openMenu(m, "Actions of the block Head");
    expect(menuLabels(menu).toSorted()).toEqual(
      m.core.boardActions
        .on({ block: "b" })
        .map(({ label }) => label)
        .toSorted(),
    );
    expect(menuLabels(menu)).toEqual([
      "Append block",
      "Create link",
      "Create stack",
      "Rename",
      "Edit inputs",
      "Copy block ID",
      "Add to document",
      "Remove block",
    ]);
    // A row of the menu runs its board action, which the row draws as the
    // board holds the block then.
    await pick(menu, "Add to document");
    expect(m.board().blocks.b?.name).toBe("In the document");
    expect(cells(row(blocks, "b"))[0]).toBe("In the document");
    // A press on the "…" opens the menu without ticking the row.
    expect(m.core.selection.get()).toEqual([]);
  });

  test("are renamed and removed through the menus of their rows, each with its links and its place in its stack", async () => {
    const m = await mounted();
    const { el, board } = m;
    const blocks = section(el, "Blocks");

    const renaming = await openMenu(m, "Actions of the block Head");
    await pick(renaming, "Rename");
    renaming.type("First rows", "Name");
    await pick(renaming, "Name");
    expect(board().blocks.b?.name).toBe("First rows");
    expect(cells(row(blocks, "b"))).toEqual(["First rows", "b", "head/tail block"]);

    await pick(await openMenu(m, "Actions of the block First rows"), "Remove block");
    expect(Object.keys(board().blocks)).not.toContain("b");
    expect(Object.values(board().links).filter((l) => l.from === "b" || l.to === "b")).toEqual([]);
    expect(board().stacks.s1?.blocks).toEqual(["a"]);
    expect(rows(el, "Blocks")).not.toContain("b");
  });

  test("show in their row the advanced action that the row's menu opens, as the block picker of Append block, until its done() ends it, the focus then back on the row's … button", async () => {
    const m = await mounted();
    const { el, board } = m;
    const blocks = section(el, "Blocks");

    const menu = await openMenu(m, "Actions of the block Scatter");
    await pick(menu, "Append block");
    await openAdvanced(menu);
    // The row turns into the picker, in a cell across the row, which keeps the
    // row's checkbox.
    const picker = form(row(blocks, "f"), "Add block");
    expect(rowCheckbox(row(blocks, "f"))).toBeDefined();
    expect(row(blocks, "f").querySelector('button[aria-label="Actions of the block Scatter"]')).toBeNull();
    expect(find(row(blocks, "f"), 'td[colspan="4"]').contains(picker)).toBe(true);

    await chosen(picker, "Block class", "head", "head/tail block");
    await step(() => {
      type(labelled(picker, "ID"), "rows");
    });
    await click(button(picker, "Add block"));
    expect(board().blocks.rows).toMatchObject({ registry_id: "blockr.core-head_block" });
    expect(Object.values(board().links)).toContainEqual({ from: "f", to: "rows", input: "data" });
    // The picker's done() ended the advanced action, so the row shows its
    // block again, with the focus on its "…".
    expect(row(blocks, "f").querySelector("form")).toBeNull();
    expect(cells(row(blocks, "f"))).toEqual(["Scatter", "f", "scatter plot block"]);
    expect(document.activeElement).toBe(button(blocks, "Actions of the block Scatter"));
  });
});

describe("the links", () => {
  test("are added from a form that offers as a target only a block that takes a link, and no link that closes a cycle", async () => {
    const { el, core, board } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Links"), "Add link");
    const targets = async (): Promise<string[]> => {
      await step(() => {
        labelled(add(), "To").click();
      });
      const found = optionLabels(labelled(add(), "To"));
      await step(() => {
        press(labelled(add(), "To"), "Escape");
      });
      return found;
    };

    // Of the blocks that Head may feed, the dataset block takes no input, and
    // every input of Merge, Wide sepals and Scatter takes a link already,
    // while Bind is variadic.
    await chosen(add(), "From", "Head");
    expect(await targets()).toEqual(["Bind"]);

    // With Head's input free, Bind still may not feed it, since Head feeds
    // Bind, which would close a cycle.
    await act(() => core.commands.run("blockr.core-remove_link", { id: "ab" }));
    await chosen(add(), "From", "Bind");
    expect(await targets()).toEqual([]);
    expect(note(labelled(add(), "To"))).toBe("No block takes a link from this block.");

    await chosen(add(), "From", "Scatter");
    expect(await targets()).toEqual(["Head", "Bind"]);
    await chosen(add(), "To", "Head");
    // Head has one input, which the form shows, with nothing to pick.
    expect([selectShows(selectButton(add(), "Input")), selectButton(add(), "Input").disabled]).toEqual(["data", true]);
    await click(button(add(), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "f", to: "b", input: "data" });
    expect(labelled(add(), "To").value).toBe("");

    // Picking the target first narrows the blocks that may feed it to those
    // not downstream of it.
    await act(() => core.commands.run("blockr.core-remove_link", { id: "bc" }));
    await chosen(add(), "To", "Merge");
    await step(() => {
      labelled(add(), "From").click();
    });
    expect(optionLabels(labelled(add(), "From"))).toEqual(["Iris", "Head", "Wide sepals", "Bind", "Scatter"]);
  });

  test("into a variadic block, are given a name for the input, or none, which the table shows with the link's place among the block's links", async () => {
    const { el, board } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Links"), "Add link");

    await chosen(add(), "From", "Scatter");
    await chosen(add(), "To", "Bind");
    expect(() => selectButton(add(), "Input")).toThrow();
    await step(() => {
      type(labelled(add(), "Input name"), "plotted");
    });
    await click(button(add(), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "f", to: "e", input: "plotted" });

    // A name that another link into the block gives does not do.
    await chosen(add(), "From", "Iris");
    await chosen(add(), "To", "Bind");
    const name = labelled(add(), "Input name");
    await step(() => {
      type(name, "plotted");
      name.dispatchEvent(new FocusEvent("focusout", { bubbles: true }));
    });
    expect(note(labelled(add(), "Input name"))).toBe(
      'Optional. The block "Bind" takes any number of links, each by its name or, where it has none, by its position. Another link gives the input the name "plotted" already.',
    );
    await click(button(add(), "Add link"));
    expect(Object.values(board().links).filter((l) => l.to === "e")).toHaveLength(3);

    // An empty name gives an input without a name.
    await step(() => {
      type(labelled(add(), "Input name"), "");
    });
    await click(button(add(), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "a", to: "e", input: "" });

    const into = Object.entries(board().links)
      .filter(([, link]) => link.to === "e")
      .map(([id]) => cells(row(section(el, "Links"), id))[2]);
    expect(into).toEqual(["No namePosition 1", "No namePosition 2", "plottedPosition 3", "No namePosition 4"]);
  });

  test("open the menu of their link from the … button of each row, which offers Insert block, Edit link and Remove link, and remove the link", async () => {
    const m = await mounted();
    const links = section(m.el, "Links");

    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    expect(menuLabels(menu)).toEqual(["Insert block", "Edit link", "Remove link"]);
    await pick(menu, "Remove link");
    expect(Object.keys(m.board().links)).not.toContain("ac");
    expect(rows(m.el, "Links")).not.toContain("ac");
    expect(links.textContent).not.toContain("Iris → Merge");
  });

  test("are changed in their row, which turns into the form of Edit link that the row's menu opens, until the user saves the edit or closes the form by Close or Escape, the focus then back on the row's … button", async () => {
    const m = await mounted();
    const { el, board } = m;
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const editing = async (): Promise<void> => {
      const menu = await openMenu(m, "Actions of the link from Iris to Merge");
      await pick(menu, "Edit link");
      await openAdvanced(menu);
    };

    await editing();
    expect(row(links, "ac").contains(edit())).toBe(true);
    expect([labelled(edit(), "From").value, labelled(edit(), "To").value]).toEqual(["Iris", "Merge"]);
    // The link feeds x, and the form offers Merge's other input as well, y,
    // which bc feeds.
    expect(await listed(selectButton(edit(), "Input"), step)).toEqual(["x", "y"]);
    await click(button(row(links, "ac"), "Close"));
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(document.activeElement).toBe(button(links, "Actions of the link from Iris to Merge"));

    await editing();
    await chosen(edit(), "To", "Bind");
    await step(() => {
      press(labelled(edit(), "From"), "Escape");
    });
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(document.activeElement).toBe(button(links, "Actions of the link from Iris to Merge"));
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });

    // Moved into the variadic Bind, the link takes a name for its input. Its
    // done() ends the advanced action as the user saves it, and the row shows
    // the link as the board holds it now.
    await editing();
    await chosen(edit(), "To", "Bind");
    await step(() => {
      type(labelled(edit(), "Input name"), "left");
    });
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    await click(button(edit(), "Save"));
    expect(board().links.ac).toEqual({ from: "a", to: "e", input: "left" });
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(cells(row(links, "ac"))).toEqual(["Irisa", "Binde", "leftPosition 1"]);
    expect(document.activeElement).toBe(button(links, "Actions of the link from Iris to Bind"));
  });

  test("into a block that is not variadic, swap their inputs with the link that feeds the input that the user picks, in one step, which the form says first", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const editing = async (label: string): Promise<void> => {
      const menu = await openMenu(m, label);
      await pick(menu, "Edit link");
      await openAdvanced(menu);
    };

    // The link from Iris feeds Merge's x, and the one from Head its y.
    await editing("Actions of the link from Iris to Merge");
    expect(note(selectButton(edit(), "Input"))).toBeUndefined();
    await picked(selectButton(edit(), "Input"), step, "y");
    expect(note(selectButton(edit(), "Input"))).toBe('The link from "Head" feeds "y". Saving gives that link "x", so the two links swap their inputs.');
    await click(button(edit(), "Save"));
    expect([board().links.ac, board().links.bc]).toEqual([
      { from: "a", to: "c", input: "y" },
      { from: "b", to: "c", input: "x" },
    ]);

    await step(() => {
      core.store.undo();
    });
    expect([board().links.ac, board().links.bc]).toEqual([
      { from: "a", to: "c", input: "x" },
      { from: "b", to: "c", input: "y" },
    ]);
    expect(core.store.canUndo()).toBe(false);

    // Into another block, the form offers the free inputs alone, since the
    // link's old input means nothing there.
    await act(() => core.commands.run("blockr.core-remove_link", { id: "bc" }));
    await editing("Actions of the link from Iris to Head");
    await chosen(edit(), "To", "Merge");
    expect([selectShows(selectButton(edit(), "Input")), selectButton(edit(), "Input").disabled]).toEqual(["y", true]);
  });

  test("into a variadic block, move to the place that the user picks in the row's form, keeping their IDs and their ends, as a step of its own", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");
    const ends = board().links.de;

    // The links into Bind, Head's first and Wide sepals' second.
    const menu = await openMenu(m, "Actions of the link from Wide sepals to Bind");
    await pick(menu, "Edit link");
    await openAdvanced(menu);
    expect(await listed(selectButton(edit(), "Position"), step)).toEqual(["1", "2"]);
    expect(selectShows(selectButton(edit(), "Position"))).toBe("2");
    await picked(selectButton(edit(), "Position"), step, "1");
    await click(button(edit(), "Save"));

    expect(into()).toEqual(["de", "be"]);
    expect(board().links.de).toBe(ends);
    expect(cells(row(links, "de"))[2]).toBe("No namePosition 1");

    await step(() => {
      core.store.undo();
    });
    expect(into()).toEqual(["be", "de"]);
    expect(core.store.canUndo()).toBe(false);
  });

  test("into a variadic block from another block, take the place that the user picks there, in the same step that moves the link there", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");

    // Merge's inputs are fixed, so its link's form offers no place.
    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    await pick(menu, "Edit link");
    await openAdvanced(menu);
    expect(selectShows(selectButton(edit(), "Input"))).toBe("x");
    expect(() => selectButton(edit(), "Position")).toThrow();

    // On the board, the link comes before Bind's two, so it would be first.
    await chosen(edit(), "To", "Bind");
    expect(await listed(selectButton(edit(), "Position"), step)).toEqual(["1", "2", "3"]);
    expect(selectShows(selectButton(edit(), "Position"))).toBe("1");
    await picked(selectButton(edit(), "Position"), step, "3");
    await click(button(edit(), "Save"));
    expect(into()).toEqual(["be", "de", "ac"]);
    expect(board().links.ac).toEqual({ from: "a", to: "e", input: "" });

    await step(() => {
      core.store.undo();
    });
    expect(into()).toEqual(["be", "de"]);
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    expect(core.store.canUndo()).toBe(false);
  });

  test("are added from no form where no new link can join two blocks of the board, which the section says, until one can", async () => {
    const { el, core } = await mounted();
    const links = section(el, "Links");

    // Bind alone takes a new link.
    await act(() => core.commands.run("blockr.core-remove_block", { id: "e" }));
    expect(links.querySelector('form[aria-label="Add link"]')).toBeNull();
    expect(links.textContent).toContain("No new link can join two blocks of the board.");

    await act(() => core.commands.run("blockr.core-remove_link", { id: "ab" }));
    expect(form(links, "Add link")).toBeDefined();
    expect(links.textContent).not.toContain("No new link can join two blocks of the board.");
  });

  test("are listed in a table that a search narrows by the blocks' names and IDs and by the input", async () => {
    const { el } = await mounted();
    const links = section(el, "Links");
    expect(rows(el, "Links")).toEqual(["ab", "ac", "bc", "ad", "be", "de", "keen_finch"]);
    expect(cells(row(links, "ab"))).toEqual(["Irisa", "Headb", "data"]);

    for (const [search, found] of [
      ["scatter", ["keen_finch"]],
      ["Merge", ["ac", "bc"]],
      [" y", ["bc"]],
    ] as const) {
      await step(() => {
        type(find(links, "search input", HTMLInputElement), search);
      });
      expect(rows(el, "Links")).toEqual(found);
    }
  });
});

describe("the stacks", () => {
  test("are added with a name, a colour and blocks that the user picks, for which the form suggests a name and a colour of their own", async () => {
    const { el, board } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Stacks"), "Add stack");

    expect(labelled(add(), "Name").placeholder).toBe("Stack 1");
    expect(find(add(), 'input[type="checkbox"]', HTMLInputElement).checked).toBe(true);
    expect(colorButton(add()).textContent).toBe("#66C2A5");
    expect(swatchName(colorButton(add()))).toBe("light pale green cyan");

    // The blocks of no stack come first, and those of another stack apart.
    const blocks = labelled(add(), "Blocks");
    blocks.focus();
    await step(() => {
      blocks.click();
    });
    expect(optionLabels(blocks)).toEqual([
      "In no stack: Merge",
      "In no stack: Wide sepals",
      "In no stack: Bind",
      "In no stack: Scatter",
      "In another stack: Iris",
      "In another stack: Head",
    ]);
    await step(() => {
      optionFromLabel(blocks, "Scatter").click();
    });
    await step(() => {
      optionFromLabel(blocks, "Merge").click();
    });
    // A click on the button takes the focus from the list first, which closes
    // it, and the list reports the picks.
    await step(() => {
      blocks.blur();
    });
    expect([listFromField(blocks), chipLabels(blocks)]).toEqual([null, ["Scatter", "Merge"]]);
    expect(Object.keys(board().stacks)).toEqual(["s1"]);
    await click(button(add(), "Add stack"));

    const [id] = Object.keys(board().stacks).filter((stack) => stack !== "s1");
    expect(board().stacks[id ?? ""]).toEqual({ name: "Stack 1", blocks: ["f", "c"], color: "#66c2a5" });
    // The next stack gets a name and a colour of its own.
    expect(labelled(add(), "Name").placeholder).toBe("Stack 2");
    expect(colorButton(add()).textContent).toBe("#FC8D62");
    expect(texts(row(section(el, "Stacks"), id ?? ""), "li")).toEqual(["Scatter f", "Merge c"]);
    // The table names the stack's colour, as a swatch that a screen reader
    // reads, and draws a ring for a stack with none, which it skips.
    expect(swatchName(row(section(el, "Stacks"), id ?? ""))).toBe("light pale green cyan");
    expect(swatchName(row(section(el, "Stacks"), "s1"))).toBeNull();
    expect(find(row(section(el, "Stacks"), "s1"), 'span[aria-hidden="true"].bc\\:rounded-full').style.backgroundColor).toBe("");
  });

  test("take the colour that the user picks, of the palette or any other, or none", async () => {
    const { el, board } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Stacks"), "Add stack");
    const checkbox = (): HTMLInputElement => find(add(), 'input[type="checkbox"]', HTMLInputElement);

    await click(colorButton(add()));
    const palette = find(openDialog(), '[role="listbox"][aria-label="Palette"]');
    expect([...palette.querySelectorAll('[role="option"]')].map(swatchName)).toEqual([
      "light pale green cyan",
      "light red orange",
      "light pale blue",
      "light magenta pink",
      "light vibrant yellow green",
      "very light vibrant yellow",
      "light pale orange yellow",
      "light gray",
    ]);
    const pick = [...palette.querySelectorAll<HTMLElement>('[role="option"]')].find((option) => swatchName(option) === "light pale blue");
    await click(pick ?? palette);
    expect(colorButton(add()).textContent).toBe("#8DA0CB");

    // Any other colour, as its hex.
    const hex = find(openDialog(), 'input:not([type="range"])', HTMLInputElement);
    await step(() => {
      type(hex, "#123456");
      press(hex, "Enter");
    });
    expect(colorButton(add()).textContent).toBe("#123456");
    await step(() => {
      press(hex, "Escape");
    });
    expect(document.querySelector('[role="dialog"]')).toBeNull();

    // Without a colour of its own, the picker keeps the last colour, which the
    // checkbox gives the stack again.
    await click(checkbox());
    expect([checkbox().checked, colorButton(add()).disabled, colorButton(add()).textContent]).toEqual([false, true, "#123456"]);
    await click(checkbox());
    expect([checkbox().checked, colorButton(add()).disabled]).toEqual([true, false]);
    await click(button(add(), "Add stack"));
    await click(checkbox());
    await click(button(add(), "Add stack"));

    const added = Object.values(board().stacks).filter((stack) => stack.name !== "Input");
    expect(added.map(({ name, color }) => ({ name, color }))).toEqual([
      { name: "Stack 1", color: "#123456" },
      { name: "Stack 2", color: null },
    ]);
  });

  test("open the menu of their stack from the … button of each row, which offers Rename, Edit stack and Remove stack", async () => {
    const m = await mounted();
    const stacks = section(m.el, "Stacks");

    const renaming = await openMenu(m, "Actions of the stack Input");
    expect(menuLabels(renaming)).toEqual(["Rename", "Edit stack", "Remove stack"]);
    await pick(renaming, "Rename");
    renaming.type("Sources", "Name");
    await pick(renaming, "Name");
    expect(m.board().stacks.s1?.name).toBe("Sources");
    expect(names(m.el, "Stacks")).toEqual(["Sources"]);
    expect(button(stacks, "Actions of the stack Sources")).toBeDefined();
  });

  test("are changed in their row, which turns into the form of Edit stack that the row's menu opens, once the user saves it: renamed, coloured and given other blocks, one of which leaves its stack, in one step", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    await act(() => core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"] }));
    const stacks = section(el, "Stacks");

    const menu = await openMenu(m, "Actions of the stack Input");
    await pick(menu, "Edit stack");
    await openAdvanced(menu);
    const edit = (): HTMLFormElement => form(stacks, "Change stack");
    expect(row(stacks, "s1").contains(edit())).toBe(true);
    expect(labelled(edit(), "Name").value).toBe("Input");
    expect(find(edit(), 'input[type="checkbox"]', HTMLInputElement).checked).toBe(false);
    expect(chipLabels(labelled(edit(), "Blocks"))).toEqual(["Iris", "Head"]);

    await step(() => {
      type(labelled(edit(), "Name"), "Sources");
      find(edit(), 'input[type="checkbox"]', HTMLInputElement).click();
    });
    // Escape closes the list, and leaves the form open.
    await picked(labelled(edit(), "Blocks"), step, "Head", "Scatter");
    expect(chipLabels(labelled(edit(), "Blocks"))).toEqual(["Iris", "Scatter"]);
    await click(button(edit(), "Save"));

    expect(board().stacks.s1).toEqual({ name: "Sources", blocks: ["a", "f"], color: "#66c2a5" });
    expect(board().stacks.plots?.blocks).toEqual([]);
    // Saving ends the advanced action, and the row shows the stack again.
    expect(stacks.querySelector('form[aria-label="Change stack"]')).toBeNull();
    expect(texts(row(stacks, "s1"), "li")).toEqual(["Iris a", "Scatter f"]);
    expect(document.activeElement).toBe(button(stacks, "Actions of the stack Sources"));

    // The whole edit is one step.
    await step(() => {
      core.store.undo();
    });
    expect(board().stacks.s1).toEqual({ name: "Input", blocks: ["a", "b"], color: null });
    expect(board().stacks.plots?.blocks).toEqual(["f"]);
  });

  test("are removed through the menus of their rows, leaving their blocks on the board", async () => {
    const m = await mounted();
    await pick(await openMenu(m, "Actions of the stack Input"), "Remove stack");
    expect(m.board().stacks).toEqual({});
    expect(Object.keys(m.board().blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
    expect(section(m.el, "Stacks").textContent).toContain("The board has no stacks yet.");
  });
});

describe("the selection", () => {
  test("takes the object of a row that the user ticks, and lets go of it as the user unticks it, while the rows show what else selected", async () => {
    const { el, core } = await mounted();
    const blocks = section(el, "Blocks");

    await click(rowCheckbox(row(blocks, "b")));
    await click(rowCheckbox(row(section(el, "Links"), "ab")));
    expect(core.selection.get()).toEqual([{ block: "b" }, { link: "ab" }]);
    expect(ticked(el, "Blocks")).toEqual(["b"]);
    expect(ticked(el, "Links")).toEqual(["ab"]);

    await click(rowCheckbox(row(blocks, "b")));
    expect(core.selection.get()).toEqual([{ link: "ab" }]);

    // The rows follow the selection, whatever sets it, as the DAG does.
    await step(() => {
      core.selection.set([{ stack: "s1" }, { block: "d" }]);
    });
    expect([ticked(el, "Blocks"), ticked(el, "Links"), ticked(el, "Stacks")]).toEqual([["d"], [], ["s1"]]);
  });

  test("takes every row of a table's page by the checkbox of the table's header, and keeps the rows of its other pages and of other tables", async () => {
    const { el, core } = await mounted();
    for (let i = 1; i <= 8; i++) {
      await act(() => core.commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block", id: `h${String(i)}`, name: `Rows ${String(i)}` }));
    }
    const blocks = section(el, "Blocks");
    const all = (): HTMLInputElement => find(blocks, '[role="columnheader"] input[type="checkbox"]', HTMLInputElement);
    await step(() => {
      core.selection.set([{ block: "h8" }, { link: "ab" }]);
    });

    await click(all());
    expect(core.selection.get()).toEqual([
      { block: "h8" },
      { link: "ab" },
      ...["a", "b", "c", "d", "e", "f", "h1", "h2", "h3", "h4"].map((block) => ({ block })),
    ]);
    expect(all().checked).toBe(true);

    await click(all());
    expect(core.selection.get()).toEqual([{ block: "h8" }, { link: "ab" }]);
    await click(button(blocks, "Next"));
    expect(ticked(el, "Blocks")).toEqual(["h8"]);
  });

  test("shows a bar above the sections while it holds a board object, which says how many it holds, with the menu of the selection and a button that clears it", async () => {
    const m = await mounted();
    const { el, core } = m;
    expect(bar(el)).toBeNull();

    await click(rowCheckbox(row(section(el, "Blocks"), "c")));
    await click(rowCheckbox(row(section(el, "Links"), "ad")));
    expect(bar(el)?.textContent).toContain("2 selected");
    expect(el.firstElementChild?.firstElementChild).toBe(bar(el));

    const menu = await openMenu(m, "Actions of the selection");
    expect(menuLabels(menu)).toEqual(["Create stack", "Remove"]);
    menu.escape();

    await click(button(bar(el) ?? el, "Clear selection"));
    expect(core.selection.get()).toEqual([]);
    expect(bar(el)).toBeNull();
    expect(ticked(el, "Blocks")).toEqual([]);
  });

  test("is removed through the bar's menu, in one step that undo takes back", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    const before = board();
    await step(() => {
      core.selection.set([{ block: "d" }, { link: "keen_finch" }, { stack: "s1" }]);
    });

    await pick(await openMenu(m, "Actions of the selection"), "Remove");
    expect(Object.keys(board().blocks)).not.toContain("d");
    expect(Object.keys(board().links)).not.toContain("keen_finch");
    expect(board().stacks).toEqual({});
    // What left the board leaves the selection, and the bar goes.
    expect(core.selection.get()).toEqual([]);
    expect(bar(el)).toBeNull();

    await step(() => {
      core.store.undo();
    });
    expect(board()).toEqual(before);
  });

  test("shows below the bar the advanced action that the bar's menu opens, as Create stack's form with the selected blocks given", async () => {
    const m = await mounted();
    const { el, core, board } = m;
    await step(() => {
      core.selection.set([{ block: "c" }, { block: "f" }, { link: "ab" }]);
    });

    const menu = await openMenu(m, "Actions of the selection");
    await pick(menu, "Create stack");
    await openAdvanced(menu);
    expect(chipLabels(labelled(form(bar(el) ?? el, "Add stack"), "Blocks"))).toEqual(["Merge", "Scatter"]);
    expect(section(el, "Stacks").querySelectorAll('form[aria-label="Add stack"]')).toHaveLength(1);
    // Its Close button closes it, and so the form shows until the user closes
    // it or its done() ends it.
    await click(button(bar(el) ?? el, "Close"));
    expect(bar(el)?.querySelector("form")).toBeNull();
    const again = await openMenu(m, "Actions of the selection");
    await pick(again, "Create stack");
    await openAdvanced(again);
    const adding = form(bar(el) ?? el, "Add stack");

    await click(button(adding, "Add stack"));
    const [id] = Object.keys(board().stacks).filter((stack) => stack !== "s1");
    expect(board().stacks[id ?? ""]).toMatchObject({ name: "Stack 1", blocks: ["c", "f"] });
    // The form's done() ended the advanced action, so the bar shows no form.
    expect(bar(el)?.querySelector("form")).toBeNull();
    expect(document.activeElement).toBe(button(bar(el) ?? el, "Actions of the selection"));
  });
});

describe("each edit", () => {
  test("applies as the user makes it, as one step that undo takes back", async () => {
    const m = await mounted();
    const { core, board } = m;

    const renaming = await openMenu(m, "Actions of the block Head");
    await pick(renaming, "Rename");
    renaming.type("First rows", "Name");
    await pick(renaming, "Name");
    await pick(await openMenu(m, "Actions of the link from Iris to Scatter"), "Remove link");
    expect(board().blocks.b?.name).toBe("First rows");
    expect(Object.keys(board().links)).not.toContain("keen_finch");

    await step(() => {
      core.store.undo();
    });
    expect(Object.keys(board().links)).toContain("keen_finch");
    expect(board().blocks.b?.name).toBe("First rows");

    await step(() => {
      core.store.undo();
    });
    expect(board().blocks.b?.name).toBe("Head");
    expect(core.store.canUndo()).toBe(false);
  });

  test("that the command registry or the store refuses, the plugin says why below the form that made it, in a banner in the colour of an error under a sentence that names the edit, until the user submits the form again, and core raises no notification of it", async () => {
    const { el, core, board, notes } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Links"), "Add link");
    await act(() => core.commands.run("blockr.core-remove_link", { id: "ab" }));

    // Another edit feeds Head's one input while the form is filled in.
    await chosen(add(), "From", "Scatter");
    await chosen(add(), "To", "Head");
    await act(() => core.commands.run("blockr.core-add_link", { from: "a", to: "b", input: "data" }));
    await click(button(add(), "Add link"));

    expect(Object.values(board().links).filter((l) => l.to === "b")).toEqual([{ from: "a", to: "b", input: "data" }]);
    expect(refusals(add())).toEqual([["Could not add the link.", 'Every input of the block "b" takes a link already.']]);
    expect(find(add(), '[role="alert"]').className).toContain("bc:border-border-danger");
    // It shows below the form's button.
    expect(find(add(), '[role="alert"]').previousElementSibling?.contains(button(add(), "Add link"))).toBe(true);
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);

    // The reason stays as the user changes the form, and goes as the user
    // submits it again.
    await chosen(add(), "To", "Bind");
    expect(refusals(add())).toHaveLength(1);
    await click(button(add(), "Add link"));
    expect(refusals(add())).toEqual([]);
    expect(Object.values(board().links)).toContainEqual({ from: "f", to: "e", input: "" });
  });

  test("that the store refuses from the menu of a row, the plugin says why below the table, naming the edit in the plugin's words, until the user opens the menu of a row of the table again, and core raises no notification of it", async () => {
    const m = await mounted();
    const { el, core, board, notes } = m;
    const links = section(el, "Links");
    const before = board();

    // Change source… lists Bind as a source of the link from Iris to Merge.
    // Another edit then links Merge to Bind, so that the link from Bind would
    // close a cycle.
    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    await pick(menu, "Edit link");
    await pick(menu, "Change source…");
    await act(() => core.commands.run("blockr.core-add_link", { id: "ce", from: "c", to: "e" }));
    await expect(act(() => menu.pick("Bind"))).rejects.toThrow();
    await acting(() => undefined);

    // The table shows neither id, which the store's reason names, so the
    // sentence before it names the link.
    expect(belowTable(links)).toEqual([['Could not change the link from "Iris" to "Merge".', 'The link would close a cycle through the blocks "c" and "e".']]);
    expect(find(links, '[role="alert"]').className).toContain("bc:border-border-danger");
    expect(belowTable(section(el, "Blocks"))).toEqual([]);
    expect(board().links.ac).toEqual(before.links.ac);
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);

    // The menu of a row of another table leaves the banner.
    (await openMenu(m, "Actions of the block Head")).escape();
    expect(belowTable(links)).toHaveLength(1);
    // The menu of a row of this table takes it down as it opens, even where
    // the user then picks nothing.
    (await openMenu(m, "Actions of the link from Iris to Head")).escape();
    expect(belowTable(links)).toEqual([]);
  });

  test("of another package that the store refuses from the menu of a row, the table names by the board action's label", async () => {
    const breaks = action("Add to document", ["block"], () => [{ id: "blockr.core-remove_link", args: { id: "nowhere" } }]);
    const m = await withActions([breaks]);
    const { el, core, notes } = m;
    const blocks = section(el, "Blocks");

    const menu = await openMenu(m, "Actions of the block Head");
    await expect(act(() => menu.pick("Add to document"))).rejects.toThrow();
    await acting(() => undefined);
    expect(belowTable(blocks)).toEqual([['Add to document failed for the block "Head".', 'It fails its preflight on the board as it is, with the args {"id":"nowhere"}.']]);
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);
  });

  test("that the store refuses from the menu of the selection, the bar says why below itself, naming the selection", async () => {
    const breaks = action("Archive", ["selection"], () => [{ id: "blockr.core-remove_stack", args: { id: "nowhere" } }]);
    const m = await withActions([breaks]);
    const { el, core } = m;
    await step(() => {
      core.selection.set([{ block: "a" }, { stack: "s1" }]);
    });

    const menu = await openMenu(m, "Actions of the selection");
    await expect(act(() => menu.pick("Archive"))).rejects.toThrow();
    await acting(() => undefined);
    expect(refusals(bar(el) ?? el)).toEqual([["Archive failed for the selection.", 'It fails its preflight on the board as it is, with the args {"id":"nowhere"}.']]);
    expect(belowTable(section(el, "Blocks"))).toEqual([]);
    expect(held(core)).toEqual([]);

    // The menu of the selection takes the banner down as it opens again.
    (await openMenu(m, "Actions of the selection")).escape();
    expect(refusals(bar(el) ?? el)).toEqual([]);
  });

  test("that the store would refuse from a form that adds to the board has no form to start from, since core's board actions that add fail their offers under the board lock, whatever the plugin's CoreApi says", async () => {
    const { el } = await refusing();

    // The board lock leaves core no board action that adds, so the plugin
    // mounts no block picker and no form of a link or a stack.
    expect(section(el, "Blocks").querySelector("form")).toBeNull();
    expect(section(el, "Links").querySelector("form")).toBeNull();
    expect(section(el, "Stacks").querySelector("form")).toBeNull();
  });

  test("that the command registry refuses from the block picker, the picker says why below its button, as it stays as the user filled it in", async () => {
    // A class whose new block starts with a state that its schema refuses.
    const head = HELLO.block_registry["blockr.core-head_block"];
    if (head === undefined) throw new Error("The fixture's registry holds no head block.");
    const hello: HelloReply = {
      ...HELLO,
      block_registry: {
        ...HELLO.block_registry,
        "blockr.test-broken_block": { ...head, id: "blockr.test-broken_block", name: "broken block", state: { ...head.state, n: "ten" } },
      },
    };
    const { el, core, board, notes } = await mounted({ hello });
    const before = board();
    const add = (): HTMLFormElement => form(section(el, "Blocks"), "Add block");

    await chosen(add(), "Block class", "broken", "broken block");
    await click(button(add(), "Add block"));
    expect(refusals(add())).toEqual([["Could not add the block.", expect.stringMatching(/^The state of the new block fails its schema at \/n/) as string]]);
    expect(find(add(), '[role="alert"]').previousElementSibling?.contains(button(add(), "Add block"))).toBe(true);
    expect(labelled(add(), "Block class").value).toBe("broken block");

    expect(board()).toBe(before);
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);
  });

  test("that the store refuses from the form that changes a row of a table, the form says why below its button, in the row, naming the link, as the form stays as the user filled it in", async () => {
    const m = await mounted();
    const { el, core, board, notes } = m;
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");

    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    await pick(menu, "Edit link");
    await openAdvanced(menu);
    await chosen(edit(), "From", "Bind");
    // Another edit links Merge to Bind before the user saves, so that the link
    // from Bind would close a cycle.
    await act(() => core.commands.run("blockr.core-add_link", { id: "ce", from: "c", to: "e" }));
    await click(button(edit(), "Save"));

    expect(refusals(edit())).toEqual([['Could not change the link from "Iris" to "Merge".', 'The link would close a cycle through the blocks "c" and "e".']]);
    expect(find(edit(), '[role="alert"]').previousElementSibling?.contains(button(edit(), "Save"))).toBe(true);
    expect(row(links, "ac").contains(edit())).toBe(true);
    expect(labelled(edit(), "From").value).toBe("Bind");
    expect(belowTable(links)).toEqual([]);
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);
  });
});

describe("the sentence that names a refused edit", () => {
  test("names the edit in the plugin's words for core's board actions, and by its label for another package's, with the object as the tables show it", async () => {
    const board = (await mounted()).board();
    const core = (id: string, label = ""): Pick<BoardAction, "id" | "label"> => ({ id: `blockr.core-${id}_action`, label });
    expect(editTitle(board, core("remove_block"), { block: "b" })).toBe('Could not remove the block "Head".');
    expect(editTitle(board, core("edit_link"), { link: "ac" })).toBe('Could not change the link from "Iris" to "Merge".');
    expect(editTitle(board, core("rename"), { stack: "s1" })).toBe('Could not rename the stack "Input".');
    expect(editTitle(board, core("add_stack"), { selection: [{ block: "c" }] })).toBe("Could not put the selection in a stack.");
    expect(editTitle(board, core("append_block"), { block: "f" })).toBe('Could not append a block to the block "Scatter".');
    expect(editTitle(board, core("insert_block"), { link: "ab" })).toBe('Could not insert a block into the link from "Iris" to "Head".');
    expect(editTitle(board, core("prepend_block"), { block: "b" })).toBe('Could not prepend a block to the block "Head".');
    expect(editTitle(board, core("add_link"), { block: "e" })).toBe('Could not add a link for the block "Bind".');
    expect(editTitle(board, core("edit_stack"), { stack: "s1" })).toBe('Could not change the stack "Input".');
    expect(editTitle(board, core("remove_selected"), { selection: [{ block: "c" }, { link: "ab" }] })).toBe("Could not remove the selection.");
    // Another package's, and one of core's that the plugin has no words for,
    // by their labels.
    expect(editTitle(board, { id: "blockr.md-add_to_document_action", label: "Add to document" }, { block: "d" })).toBe(
      'Add to document failed for the block "Wide sepals".',
    );
    expect(editTitle(board, core("copy_block_id", "Copy block ID"), { block: "a" })).toBe('Copy block ID failed for the block "Iris".');
    // An object that the board no longer holds, by its id.
    expect(editTitle(board, core("remove_link"), { link: "gone" })).toBe('Could not remove the link "gone".');
    expect(editTitle(board, core("remove_stack"), { stack: "gone" })).toBe('Could not remove the stack "gone".');
    // The targets of the menus that the plugin does not open, by their ids.
    const other = { id: "blockr.test-check_action", label: "Check" };
    expect([{ board: true } as const, { option: "n_rows" }, { plugin: "blockr.test-doc_plugin" }].map((target) => editTitle(board, other, target))).toEqual([
      "Check failed for the board.",
      'Check failed for the board option "n_rows".',
      'Check failed for the plugin "blockr.test-doc_plugin".',
    ]);
  });
});

describe("a form in a row", () => {
  test("hears the keys of its fields, which the table takes again once the form has closed", async () => {
    const m = await mounted();
    const links = section(m.el, "Links");
    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    await pick(menu, "Edit link");
    await openAdvanced(menu);
    await chosen(form(links, "Change link"), "To", "Bind");
    const name = labelled(form(links, "Change link"), "Input name");

    // React Aria's Table would move the focus to a row whose text starts with
    // a letter that the user types, and keep the arrows from the field.
    await step(() => {
      name.focus();
    });
    const keys = ["H", "ArrowLeft", "ArrowDown"].map((key) => new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
    for (const key of keys) {
      await step(() => {
        name.dispatchEvent(key);
      });
    }
    expect(document.activeElement).toBe(name);
    expect(keys.map((key) => key.defaultPrevented)).toEqual([false, false, false]);

    // Once the form has closed, the arrows move between the rows again.
    await click(button(row(links, "ac"), "Close"));
    const first = row(links, "ab");
    await step(() => {
      first.focus();
    });
    await step(() => {
      press(first, "ArrowDown");
    });
    expect(document.activeElement).toBe(row(links, "ac"));
  });

  test("open beside another in one table each take the table's keys, which the table takes again once both have closed", async () => {
    const m = await mounted();
    const links = section(m.el, "Links");
    for (const label of ["Actions of the link from Iris to Merge", "Actions of the link from Iris to Head"]) {
      const menu = await openMenu(m, label);
      await pick(menu, "Edit link");
      await openAdvanced(menu);
    }
    expect([row(links, "ac"), row(links, "ab")].map((tr) => tr.querySelectorAll('form[aria-label="Change link"]').length)).toEqual([1, 1]);
    const arrow = (): boolean => {
      const key = new KeyboardEvent("keydown", { key: "ArrowLeft", bubbles: true, cancelable: true });
      labelled(find(row(links, "ab"), "form"), "From").dispatchEvent(key);
      return key.defaultPrevented;
    };

    // The first form to close leaves the keys to the other.
    await click(button(row(links, "ac"), "Close"));
    expect(arrow()).toBe(false);
    await click(button(row(links, "ab"), "Close"));
    await step(() => {
      row(links, "ab").focus();
    });
    await step(() => {
      press(row(links, "ab"), "ArrowDown");
    });
    expect(document.activeElement).toBe(row(links, "ac"));
  });

  test("does not tick its row as the user clicks in it or presses Space or Enter in its fields", async () => {
    const m = await mounted();
    const stacks = section(m.el, "Stacks");
    const menu = await openMenu(m, "Actions of the stack Input");
    await pick(menu, "Edit stack");
    await openAdvanced(menu);
    const name = labelled(form(stacks, "Change stack"), "Name");

    await clickAsBrowser(name);
    expect(m.core.selection.get()).toEqual([]);
    for (const key of [" ", "Enter"]) {
      await step(() => {
        name.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
        name.dispatchEvent(new KeyboardEvent("keyup", { key, bubbles: true, cancelable: true }));
      });
      expect(m.core.selection.get()).toEqual([]);
    }
    await clickAsBrowser(labelled(form(stacks, "Change stack"), "Blocks"));
    expect(m.core.selection.get()).toEqual([]);
    expect(row(stacks, "s1").getAttribute("aria-selected")).toBe("false");

    // The row's checkbox still ticks it.
    await click(rowCheckbox(row(stacks, "s1")));
    expect(m.core.selection.get()).toEqual([{ stack: "s1" }]);
  });
});

describe("under the board lock", () => {
  const locked: HelloReply = { ...HELLO, lock: { board: true, load: false } };

  test("the plugin draws no control for what the lock fixes, and lists the board's links and stacks alone, in tables that a search narrows and whose checkboxes select their rows", async () => {
    const { el, core } = await mounted({ hello: locked });

    expect([...el.querySelectorAll("section")].map((s) => s.getAttribute("aria-label"))).toEqual(["Links", "Stacks"]);
    // The only buttons are those of the sections' headers, which collapse a
    // section and edit nothing, and the button of each search that clears
    // it. No row shows a "…", since core's board actions on links and stacks
    // fail their offers under the lock.
    expect(el.querySelectorAll("form, select")).toHaveLength(0);
    expect(
      [...el.querySelectorAll('button:not([aria-label="Clear search"])')].map((button) => [button.parentElement?.tagName, button.textContent]),
    ).toEqual([
      ["H3", "Links7"],
      ["H3", "Stacks1"],
    ]);
    expect([...el.querySelectorAll<HTMLInputElement>('input:not([type="checkbox"])')].map((input) => input.type)).toEqual(["search", "search"]);
    expect(rows(el, "Links")).toHaveLength(7);
    expect(cells(row(section(el, "Links"), "ab"))).toEqual(["Irisa", "Headb", "data"]);
    expect(texts(row(section(el, "Stacks"), "s1"), "li")).toEqual(["Iris a", "Head b"]);

    // The lock fixes no selection, so the rows' checkboxes still select, and
    // the bar shows no "…" either.
    await click(rowCheckbox(row(section(el, "Links"), "ab")));
    expect(core.selection.get()).toEqual([{ link: "ab" }]);
    expect(bar(el)?.textContent).toContain("1 selected");
    expect(bar(el)?.querySelector('button[aria-label="Actions of the selection"]')).toBeNull();
  });

  test("a row shows the … button where another package's board action applies to its object", async () => {
    const counted = action("Count rows", ["link"], () => undefined);
    const m = await withActions([counted], locked);
    const { el } = m;

    expect(rows(el, "Links").filter((id) => row(section(el, "Links"), id).querySelector("button") !== null)).toHaveLength(7);
    expect(section(el, "Stacks").querySelector('[role="grid"] button')).toBeNull();
    expect(menuLabels(await openMenu(m, "Actions of the link from Iris to Head"))).toEqual(["Count rows"]);
  });
});

describe("the cleanup that render() returns", () => {
  test("ends what the plugin drew, so that it draws no more as the board changes", async () => {
    const { el, core, end } = await mounted();
    expect(el.querySelectorAll("section").length).toBeGreaterThan(0);

    await act(() => {
      end();
      return Promise.resolve();
    });
    expect(el.childNodes).toHaveLength(0);

    await act(() => core.commands.run("blockr.core-rename_block", { id: "b", name: "First rows" }));
    expect(el.childNodes).toHaveLength(0);
  });

  test("ends the advanced actions that the plugin's rows and bar show, as the user closes their places", async () => {
    const m = await mounted();
    const menu = await openMenu(m, "Actions of the link from Iris to Merge");
    await pick(menu, "Edit link");
    let ended = false;
    await acting(() => {
      void menu.openAdvanced().then(() => {
        ended = true;
      });
    });
    expect(form(section(m.el, "Links"), "Change link")).toBeDefined();
    expect(ended).toBe(false);

    await acting(() => {
      m.end();
    });
    expect(ended).toBe(true);
  });
});

describe("on a board that holds no blocks", () => {
  test("the forms say what they need, and a link joins the first blocks added", async () => {
    const empty: HelloReply = { ...HELLO, board: { ...HELLO.board, board: { ...HELLO.board.board, blocks: {}, links: {}, stacks: {} } } };
    const { el, board } = await mounted({ hello: empty });
    expect(section(el, "Blocks").textContent).toContain("The board holds no blocks yet.");
    expect(section(el, "Links").textContent).toContain("A link joins two blocks, so the board needs two blocks first.");

    for (const [id, name] of [
      ["iris", "dataset block"],
      ["rows", "head/tail block"],
    ] as const) {
      const add = form(section(el, "Blocks"), "Add block");
      await chosen(add, "Block class", name.split(" ")[0] ?? name, name);
      await step(() => {
        type(labelled(add, "ID"), id);
      });
      await click(button(add, "Add block"));
    }
    expect(Object.keys(board().blocks)).toEqual(["iris", "rows"]);

    const add = form(section(el, "Links"), "Add link");
    await chosen(add, "From", "Dataset");
    await chosen(add, "To", "Head");
    await click(button(add, "Add link"));
    expect(Object.values(board().links)).toEqual([{ from: "iris", to: "rows", input: "data" }]);
  });
});
