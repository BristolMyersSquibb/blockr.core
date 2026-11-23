// Core's Edit board plugin on the CoreApi for tests: the board of the fixture
// hello, whose app runs the plugin, with R played from the protocol's
// fixtures, the plugin's UI mounted as a frontend places it, and the user
// acting on it.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f), and its stack s1, Input, holds a and b.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import type { HelloReply } from "../../protocol/index.ts";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions } from "../../stubs/index.ts";
import { chipLabels, labelled, listed, listFromField, optionFromLabel, optionLabels, picked, selectButton, selectShows } from "../../testing/select.ts";
import { find, press, texts, type } from "../../testing/inputs.ts";
import { held } from "../../testing/notifications.ts";
import { openDialog } from "../../testing/overlays.ts";
import { step } from "../../testing/react.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import * as editBoard from "./index.tsx";

// The class of the plugin, which the app of the fixture hello runs.
const EDIT = "blockr.core-edit_board_plugin";

interface Mounted {
  readonly el: HTMLElement;
  readonly core: CoreApi;
  readonly board: () => Board;
  // The notifications that the plugin raised, in order, while they hold.
  readonly notes: readonly Notification[];
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
  return { el, core: stubbed.core, board: () => stubbed.core.store.board(), notes, end };
}

// The plugin's UI over a store that refuses every edit: the board of the
// fixture hello under the board lock. The plugin draws no control there, so
// the test hands it a CoreApi that says the board is open, and draws every
// control.
async function refusing(): Promise<Omit<Mounted, "notes">> {
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

// The row of a table, by its name, such as "Iris → Head" for a link.
function row(el: ParentNode, name: string): HTMLTableRowElement {
  return find(el, `tr[aria-label="${name}"]`, HTMLTableRowElement);
}

// The names of the rows of the section's table, in order.
function rows(el: HTMLElement, title: "Blocks" | "Links" | "Stacks"): string[] {
  return [...section(el, title).querySelectorAll("tbody tr")].map((tr) => tr.getAttribute("aria-label") ?? "");
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
// command registry refused an edit that the user made in the table's rows.
function belowTable(el: HTMLElement): string[][] {
  const banners: string[][] = [];
  for (let next = find(el, "table").nextElementSibling; next?.getAttribute("role") === "alert"; next = next.nextElementSibling) {
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
    expect(row(section(el, "Blocks"), "Petal plot").textContent).toContain("petals");
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
    expect(rows(el, "Blocks")).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Bind", "Scatter", "Rows 1", "Rows 2", "Rows 3", "Rows 4"]);
    expect(find(blocks, 'nav[aria-label="Pages of blocks"] span').textContent).toBe("Rows 1–10 of 14");

    await click(button(blocks, "Next"));
    expect(rows(el, "Blocks")).toEqual(["Rows 5", "Rows 6", "Rows 7", "Rows 8"]);

    // The search finds blocks by name, ID or class, from the first page.
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "merge");
    });
    expect(rows(el, "Blocks")).toEqual(["Merge"]);
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "h7");
    });
    expect(rows(el, "Blocks")).toEqual(["Rows 7"]);
    await step(() => {
      type(find(blocks, "search input", HTMLInputElement), "head/tail");
    });
    expect(rows(el, "Blocks")).toEqual(["Head", "Rows 1", "Rows 2", "Rows 3", "Rows 4", "Rows 5", "Rows 6", "Rows 7", "Rows 8"]);
    expect(blocks.querySelector("nav")).toBeNull();

    // The button of the search clears it, and so does Escape, each back on
    // the first page.
    const search = find(blocks, "search input", HTMLInputElement);
    await click(find(blocks, 'search button[aria-label="Clear search"]'));
    expect([search.value, rows(el, "Blocks")[0], rows(el, "Blocks").length]).toEqual(["", "Iris", 10]);
    await step(() => {
      type(search, "block");
    });
    await click(button(blocks, "Next"));
    await step(() => {
      press(search, "Escape");
    });
    expect([search.value, rows(el, "Blocks")[0]]).toEqual(["", "Iris"]);
  });

  test("are renamed and removed, each with its links and its place in its stack", async () => {
    const { el, board } = await mounted();
    const blocks = section(el, "Blocks");

    await click(button(blocks, "Rename the block Head"));
    // The field is as wide as the column of names, rather than spilling into
    // the next. Its input stretches across the field.
    const field = find(blocks, 'input[aria-label="Block name"]').parentElement;
    expect(field?.className).toMatch(/(^| )bc:w-full( |$)/);
    expect(field?.className).not.toContain("bc:min-w-40");
    await step(() => {
      const name = find(blocks, 'input[aria-label="Block name"]', HTMLInputElement);
      type(name, "First rows");
      press(name, "Enter");
    });
    expect(board().blocks.b?.name).toBe("First rows");
    expect(row(blocks, "First rows").textContent).toContain("head/tail block");

    await click(button(blocks, "Remove the block First rows"));
    expect(Object.keys(board().blocks)).not.toContain("b");
    expect(Object.values(board().links).filter((l) => l.from === "b" || l.to === "b")).toEqual([]);
    expect(board().stacks.s1?.blocks).toEqual(["a"]);
    expect(rows(el, "Blocks")).not.toContain("First rows");
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
    await click(button(section(el, "Links"), "Remove the link from Iris to Head"));
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

    const into = [...section(el, "Links").querySelectorAll("tbody tr")]
      .filter((tr) => tr.getAttribute("aria-label")?.endsWith("→ Bind") === true)
      .map((tr) => tr.querySelectorAll("td")[2]?.textContent);
    expect(into).toEqual(["No namePosition 1", "No namePosition 2", "plottedPosition 3", "No namePosition 4"]);
  });

  test("are changed in their row, through the form of Edit link, once the user applies the edit, and the row closes on Close or Escape, the focus back on the row's Edit button", async () => {
    const { el, board } = await mounted();
    const links = section(el, "Links");

    await click(button(links, "Edit the link from Iris to Merge"));
    const edit = (): HTMLFormElement => form(links, "Change link");
    expect([labelled(edit(), "From").value, labelled(edit(), "To").value]).toEqual(["Iris", "Merge"]);
    // The link feeds x, and the form offers Merge's other input as well, y,
    // which bc feeds.
    expect(await listed(selectButton(edit(), "Input"), step)).toEqual(["x", "y"]);
    await click(button(row(links, "Iris → Merge"), "Close"));
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(document.activeElement).toBe(button(links, "Edit the link from Iris to Merge"));

    await click(button(links, "Edit the link from Iris to Merge"));
    await chosen(edit(), "To", "Bind");
    await step(() => {
      press(labelled(edit(), "From"), "Escape");
    });
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(document.activeElement).toBe(button(links, "Edit the link from Iris to Merge"));
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });

    // Moved into the variadic Bind, the link takes a name for its input. The
    // form stays open, on the link as the board holds it now.
    await click(button(links, "Edit the link from Iris to Merge"));
    await chosen(edit(), "To", "Bind");
    await step(() => {
      type(labelled(edit(), "Input name"), "left");
    });
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    await click(button(edit(), "Apply"));
    expect(board().links.ac).toEqual({ from: "a", to: "e", input: "left" });
    expect([labelled(edit(), "To").value, labelled(edit(), "Input name").value]).toEqual(["Bind", "left"]);
    await click(button(row(links, "Iris → Bind"), "Close"));
    expect(document.activeElement).toBe(button(links, "Edit the link from Iris to Bind"));
  });

  test("into a block that is not variadic, swap their inputs with the link that feeds the input that the user picks, in one step, which the form says first", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");

    // The link from Iris feeds Merge's x, and the one from Head its y.
    await click(button(links, "Edit the link from Iris to Merge"));
    expect(note(selectButton(edit(), "Input"))).toBeUndefined();
    await picked(selectButton(edit(), "Input"), step, "y");
    expect(note(selectButton(edit(), "Input"))).toBe('The link from "Head" feeds "y". Applying gives that link "x", so the two links swap their inputs.');
    await click(button(edit(), "Apply"));
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
    await click(button(row(links, "Iris → Merge"), "Close"));
    await click(button(links, "Edit the link from Iris to Head"));
    await chosen(edit(), "To", "Merge");
    expect([selectShows(selectButton(edit(), "Input")), selectButton(edit(), "Input").disabled]).toEqual(["y", true]);
  });

  test("into a variadic block, move to the place that the user picks in the row's form, keeping their IDs and their ends, as a step of its own", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");
    const ends = board().links.de;

    // The links into Bind, Head's first and Wide sepals' second.
    await click(button(links, "Edit the link from Wide sepals to Bind"));
    expect(await listed(selectButton(edit(), "Position"), step)).toEqual(["1", "2"]);
    expect(selectShows(selectButton(edit(), "Position"))).toBe("2");
    await picked(selectButton(edit(), "Position"), step, "1");
    await click(button(edit(), "Apply"));

    expect(into()).toEqual(["de", "be"]);
    expect(board().links.de).toBe(ends);
    await click(button(row(links, "Wide sepals → Bind"), "Close"));
    expect(texts(row(links, "Wide sepals → Bind"), "td")[2]).toBe("No namePosition 1");

    await step(() => {
      core.store.undo();
    });
    expect(into()).toEqual(["be", "de"]);
    expect(core.store.canUndo()).toBe(false);
  });

  test("into a variadic block from another block, take the place that the user picks there, in the same step that moves the link there", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");

    // Merge's inputs are fixed, so its link's form offers no place.
    await click(button(links, "Edit the link from Iris to Merge"));
    expect(selectShows(selectButton(edit(), "Input"))).toBe("x");
    expect(() => selectButton(edit(), "Position")).toThrow();

    // On the board, the link comes before Bind's two, so it would be first.
    await chosen(edit(), "To", "Bind");
    expect(await listed(selectButton(edit(), "Position"), step)).toEqual(["1", "2", "3"]);
    expect(selectShows(selectButton(edit(), "Position"))).toBe("1");
    await picked(selectButton(edit(), "Position"), step, "3");
    await click(button(edit(), "Apply"));
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
    expect(rows(el, "Links")).toEqual([
      "Iris → Head",
      "Iris → Merge",
      "Head → Merge",
      "Iris → Wide sepals",
      "Head → Bind",
      "Wide sepals → Bind",
      "Iris → Scatter",
    ]);

    for (const [search, found] of [
      ["scatter", ["Iris → Scatter"]],
      ["Merge", ["Iris → Merge", "Head → Merge"]],
      [" y", ["Head → Merge"]],
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
    expect(texts(row(section(el, "Stacks"), "Stack 1"), "li")).toEqual(["Scatter f", "Merge c"]);
    // The table names the stack's colour, as a swatch that a screen reader
    // reads, and draws a ring for a stack with none, which it skips.
    expect(swatchName(row(section(el, "Stacks"), "Stack 1"))).toBe("light pale green cyan");
    expect(swatchName(row(section(el, "Stacks"), "Input"))).toBeNull();
    expect(find(row(section(el, "Stacks"), "Input"), 'span[aria-hidden="true"]').style.backgroundColor).toBe("");
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

  test("are changed in their row, through the form of Edit stack, once the user applies it: renamed, coloured and given other blocks, one of which leaves its stack, in one step", async () => {
    const { el, core, board } = await mounted();
    await act(() => core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"] }));
    const stacks = section(el, "Stacks");

    await click(button(stacks, "Edit the stack Input"));
    const edit = (): HTMLFormElement => form(stacks, "Change stack");
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
    await click(button(edit(), "Apply"));

    expect(board().stacks.s1).toEqual({ name: "Sources", blocks: ["a", "f"], color: "#66c2a5" });
    expect(board().stacks.plots?.blocks).toEqual([]);
    await click(button(row(stacks, "Sources"), "Close"));
    expect(document.activeElement).toBe(button(stacks, "Edit the stack Sources"));

    // The whole edit is one step.
    await step(() => {
      core.store.undo();
    });
    expect(board().stacks.s1).toEqual({ name: "Input", blocks: ["a", "b"], color: null });
    expect(board().stacks.plots?.blocks).toEqual(["f"]);
  });

  test("are removed, leaving their blocks on the board", async () => {
    const { el, board } = await mounted();
    await click(button(section(el, "Stacks"), "Remove the stack Input"));
    expect(board().stacks).toEqual({});
    expect(Object.keys(board().blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
    expect(section(el, "Stacks").textContent).toContain("The board has no stacks yet.");
  });
});

describe("each edit", () => {
  test("applies as the user makes it, as one step that undo takes back", async () => {
    const { el, core, board } = await mounted();

    await click(button(section(el, "Blocks"), "Rename the block Head"));
    await step(() => {
      const name = find(el, 'input[aria-label="Block name"]', HTMLInputElement);
      type(name, "First rows");
      press(name, "Enter");
    });
    await click(button(section(el, "Links"), "Remove the link from Iris to Scatter"));
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
    await click(button(section(el, "Links"), "Remove the link from Iris to Head"));

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

  test("that the store refuses from the rows of a table, the plugin says why below the table, naming the edit, until the user's next edit in the table", async () => {
    const { el, core, board } = await refusing();
    const before = board();
    const blocks = section(el, "Blocks");
    const stacks = section(el, "Stacks");

    await click(button(blocks, "Remove the block Head"));
    expect(belowTable(blocks)).toEqual([
      [
        'Could not remove the block "Head".',
        'The board lock refuses to remove block "b".',
        'The board lock refuses to remove links "ab", "bc" and "be".',
        'The board lock refuses to change stack "s1".',
      ],
    ]);
    // The board lock refuses each part of the removal, so the reasons show as
    // a list.
    expect(find(blocks, "table").nextElementSibling?.querySelectorAll("ul > li")).toHaveLength(3);

    // The next edit in the table takes the place of the last.
    await click(button(blocks, "Rename the block Iris"));
    await step(() => {
      const name = find(blocks, 'input[aria-label="Block name"]', HTMLInputElement);
      type(name, "Sources");
      press(name, "Enter");
    });
    expect(belowTable(blocks)).toEqual([['Could not rename the block "Iris".', 'The board lock refuses to rename block "a".']]);

    // The tables do not show the ids of links and stacks, which the store's
    // reason names, so the sentence before it names the link or the stack.
    await click(button(section(el, "Links"), "Remove the link from Iris to Head"));
    expect(belowTable(section(el, "Links"))).toEqual([['Could not remove the link from "Iris" to "Head".', 'The board lock refuses to remove link "ab".']]);
    await click(button(stacks, "Remove the stack Input"));
    expect(belowTable(stacks)).toEqual([['Could not remove the stack "Input".', 'The board lock refuses to remove stack "s1".']]);
    // Opening the form that changes a stack is the user's next edit in the
    // table, and the reason goes.
    await click(button(stacks, "Edit the stack Input"));
    expect(refusals(stacks)).toEqual([]);
    await click(button(row(stacks, "Input"), "Close"));
    expect(belowTable(stacks)).toEqual([]);

    expect(board()).toEqual(before);
    expect(held(core)).toEqual([]);
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
    const { el, core, board, notes } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");

    await click(button(links, "Edit the link from Iris to Merge"));
    await chosen(edit(), "From", "Bind");
    // Another edit links Merge to Bind before the user applies the form, so
    // that the link from Bind would close a cycle.
    await act(() => core.commands.run("blockr.core-add_link", { id: "ce", from: "c", to: "e" }));
    await click(button(edit(), "Apply"));

    expect(refusals(edit())).toEqual([['Could not change the link from "Iris" to "Merge".', 'The link would close a cycle through the blocks "c" and "e".']]);
    expect(find(edit(), '[role="alert"]').previousElementSibling?.contains(button(edit(), "Apply"))).toBe(true);
    expect(row(links, "Iris → Merge").contains(edit())).toBe(true);
    expect(labelled(edit(), "From").value).toBe("Bind");
    expect(belowTable(links)).toEqual([]);
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    expect(held(core)).toEqual([]);
    expect(notes).toEqual([]);
  });
});

describe("under the board lock", () => {
  const locked: HelloReply = { ...HELLO, lock: { board: true, load: false } };

  test("the plugin draws no control, and lists the board's links and stacks alone, in tables that a search narrows", async () => {
    const { el } = await mounted({ hello: locked });

    expect([...el.querySelectorAll("section")].map((s) => s.getAttribute("aria-label"))).toEqual(["Links", "Stacks"]);
    // The only buttons are those of the sections' headers, which collapse a
    // section and edit nothing, and the button of each search that clears
    // it.
    expect(el.querySelectorAll("form, select")).toHaveLength(0);
    expect(
      [...el.querySelectorAll('button:not([aria-label="Clear search"])')].map((button) => [button.parentElement?.tagName, button.textContent]),
    ).toEqual([
      ["H3", "Links7"],
      ["H3", "Stacks1"],
    ]);
    expect([...el.querySelectorAll("input")].map((input) => input.type)).toEqual(["search", "search"]);
    expect(rows(el, "Links")).toHaveLength(7);
    expect(texts(row(section(el, "Links"), "Iris → Head"), "td")).toEqual(["Irisa", "Headb", "data"]);
    expect(texts(row(section(el, "Stacks"), "Input"), "li")).toEqual(["Iris a", "Head b"]);
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
