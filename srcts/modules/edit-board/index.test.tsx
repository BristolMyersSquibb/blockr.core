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
import { labelled, optionOf, optionsOf } from "../../testing/combobox.ts";
import { find, pick, press, texts, type } from "../../testing/inputs.ts";
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
  document.body.replaceChildren();
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
      ...all.filter((notification) => notification.source.kind === "module" && "plugin" in notification.source.object && notification.source.object.plugin === EDIT),
    );
  });
  let end = (): void => undefined;
  await act(async () => {
    end = stubbed.core.mountPlugin(el, EDIT);
    await stubbed.settled();
  });
  return { el, core: stubbed.core, board: () => stubbed.core.store.board(), notes, end };
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

// The select of the label given, as a label holds it.
function select(el: ParentNode, label: string): HTMLSelectElement {
  const found = [...el.querySelectorAll("label")].find((l) => l.querySelector("span")?.textContent === label);
  return find(found ?? document.createElement("label"), "select", HTMLSelectElement);
}

// The row of a table, by its name, such as "Iris → Head" for a link.
function row(el: ParentNode, name: string): HTMLTableRowElement {
  return find(el, `tr[aria-label="${name}"]`, HTMLTableRowElement);
}

// The names of the rows of the section's table, in order.
function rows(el: HTMLElement, title: "Blocks" | "Links" | "Stacks"): string[] {
  return [...section(el, title).querySelectorAll("tbody tr")].map((tr) => tr.getAttribute("aria-label") ?? "");
}

// The line below a field, its hint or its error.
function note(field: HTMLElement): string | undefined {
  return field.ownerDocument.getElementById(field.getAttribute("aria-describedby") ?? "")?.textContent;
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

// Picks the option of a searchable dropdown, through its search.
async function chosen(el: ParentNode, label: string, search: string, option?: string): Promise<void> {
  await step(() => {
    labelled(el, label).click();
  });
  await step(() => {
    type(labelled(el, label), search);
  });
  await step(() => {
    optionOf(labelled(el, label), option ?? search).click();
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
    expect(optionsOf(classes)).toEqual([
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
    expect(optionsOf(classes)).toEqual(["plot: scatter plot block"]);

    // Picking a class adds nothing, but shows the class's description and the
    // name that a block of it takes.
    await step(() => {
      optionOf(classes, "scatter plot block").click();
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
    // The form starts again for the next block.
    expect([labelled(add(), "Block class").value, labelled(add(), "ID").value, labelled(add(), "Name").value]).toEqual(["", "", ""]);
    expect(row(section(el, "Blocks"), "Petal plot").textContent).toContain("petals");
  });

  test("get an ID that the command mints and the name of their class where the user leaves the fields empty", async () => {
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
    expect(note(labelled(add(), "ID"))).toBe('An ID needs a character other than a digit, such as "block_12".');
    // Typing takes the error away until the user leaves the field again.
    await step(() => {
      type(labelled(add(), "ID"), "a");
    });
    expect(labelled(add(), "ID").getAttribute("aria-invalid")).toBeNull();

    await click(button(add(), "Add block"));
    expect(note(labelled(add(), "ID"))).toBe('The board holds a block "a" already.');
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
  });

  test("are renamed and removed, each with its links and its place in its stack", async () => {
    const { el, board } = await mounted();
    const blocks = section(el, "Blocks");

    await click(button(blocks, "Rename the block Head"));
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
      const found = optionsOf(labelled(add(), "To"));
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
    // Head has one input, which the form shows.
    expect([...select(add(), "Input").options].map((o) => o.value)).toEqual(["data"]);
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
    expect(optionsOf(labelled(add(), "From"))).toEqual(["Iris", "Head", "Wide sepals", "Bind", "Scatter"]);
  });

  test("into a variadic block, are given a name for the input, or none, which the table shows with the link's place among the block's links", async () => {
    const { el, board } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Links"), "Add link");

    await chosen(add(), "From", "Scatter");
    await chosen(add(), "To", "Bind");
    expect(add().querySelector("select")).toBeNull();
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
    expect(note(labelled(add(), "Input name"))).toBe('Another link gives the input the name "plotted" already.');
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

  test("are changed in their row once the user saves the edit, which Cancel and Escape leave as it was, the focus back on the row's Edit button", async () => {
    const { el, board } = await mounted();
    const links = section(el, "Links");

    await click(button(links, "Edit the link from Iris to Merge"));
    const edit = (): HTMLFormElement => form(links, "Change link");
    expect([labelled(edit(), "From").value, labelled(edit(), "To").value]).toEqual(["Iris", "Merge"]);
    // The link feeds x, and the form offers Merge's other input as well, y,
    // which bc feeds.
    expect([...select(edit(), "Input").options].map((o) => o.value)).toEqual(["x", "y"]);
    await click(button(edit(), "Cancel"));
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(document.activeElement).toBe(button(links, "Edit the link from Iris to Merge"));

    await click(button(links, "Edit the link from Iris to Merge"));
    await step(() => {
      press(labelled(edit(), "From"), "Escape");
    });
    expect(links.querySelector('form[aria-label="Change link"]')).toBeNull();
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });

    // Moved into the variadic Bind, the link takes a name for its input.
    await click(button(links, "Edit the link from Iris to Merge"));
    await chosen(edit(), "To", "Bind");
    await step(() => {
      type(labelled(edit(), "Input name"), "left");
    });
    await click(button(edit(), "Save"));
    expect(board().links.ac).toEqual({ from: "a", to: "e", input: "left" });
    expect(document.activeElement).toBe(button(links, "Edit the link from Iris to Bind"));
  });

  test("into a block that is not variadic, swap their inputs with the link that feeds the input that the user picks, in one step, which the form says first", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");

    // The link from Iris feeds Merge's x, and the one from Head its y.
    await click(button(links, "Edit the link from Iris to Merge"));
    expect(note(select(edit(), "Input"))).toBeUndefined();
    await step(() => {
      pick(select(edit(), "Input"), "y");
    });
    expect(note(select(edit(), "Input"))).toBe('The link from "Head" feeds "y". Saving gives that link "x", so the two links swap their inputs.');
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
    await click(button(links, "Edit the link from Iris to Head"));
    await chosen(edit(), "To", "Merge");
    expect([...select(edit(), "Input").options].map((o) => o.value)).toEqual(["y"]);
  });

  test("into a variadic block, move to the place that the user picks in the row's edit form, keeping their IDs and their ends, as a step of its own", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");
    const ends = board().links.de;

    // The links into Bind, Head's first and Wide sepals' second.
    await click(button(links, "Edit the link from Wide sepals to Bind"));
    expect([...select(edit(), "Position").options].map((o) => o.value)).toEqual(["1", "2"]);
    expect(select(edit(), "Position").value).toBe("2");
    await step(() => {
      pick(select(edit(), "Position"), "1");
    });
    await click(button(edit(), "Save"));

    expect(into()).toEqual(["de", "be"]);
    expect(board().links.de).toBe(ends);
    expect(texts(row(links, "Wide sepals → Bind"), "td")[2]).toBe("No namePosition 1");

    await step(() => {
      core.store.undo();
    });
    expect(into()).toEqual(["be", "de"]);
    expect(core.store.canUndo()).toBe(false);
  });

  test("into a variadic block from another block, take the place that the user picks there, in a step after the one that changes the link", async () => {
    const { el, core, board } = await mounted();
    const links = section(el, "Links");
    const edit = (): HTMLFormElement => form(links, "Change link");
    const into = (): string[] => Object.keys(board().links).filter((id) => board().links[id]?.to === "e");

    // Merge's inputs are fixed, so its link's form offers no place.
    await click(button(links, "Edit the link from Iris to Merge"));
    expect(texts(edit(), "label > span")).toContain("Input");
    expect(texts(edit(), "label > span")).not.toContain("Position");

    // On the board, the link comes before Bind's two, so it would be first.
    await chosen(edit(), "To", "Bind");
    expect([...select(edit(), "Position").options].map((o) => o.value)).toEqual(["1", "2", "3"]);
    expect(select(edit(), "Position").value).toBe("1");
    await step(() => {
      pick(select(edit(), "Position"), "3");
    });
    await click(button(edit(), "Save"));
    expect(into()).toEqual(["be", "de", "ac"]);
    expect(board().links.ac).toEqual({ from: "a", to: "e", input: "" });

    await step(() => {
      core.store.undo();
    });
    expect(into()).toEqual(["ac", "be", "de"]);
    await step(() => {
      core.store.undo();
    });
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
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
    const color = (): HTMLInputElement => find(add(), 'input[type="color"]', HTMLInputElement);

    expect(labelled(add(), "Name").placeholder).toBe("Stack 1");
    expect(find(add(), 'input[type="checkbox"]', HTMLInputElement).checked).toBe(true);
    expect(color().value).toBe("#66c2a5");

    // The blocks of no stack come first, and those of another stack apart.
    const blocks = labelled(add(), "Blocks");
    await step(() => {
      blocks.click();
    });
    expect(optionsOf(blocks)).toEqual([
      "In no stack: Merge",
      "In no stack: Wide sepals",
      "In no stack: Bind",
      "In no stack: Scatter",
      "In another stack: Iris",
      "In another stack: Head",
    ]);
    await step(() => {
      optionOf(blocks, "Scatter").click();
    });
    await step(() => {
      optionOf(blocks, "Merge").click();
    });
    expect(Object.keys(board().stacks)).toEqual(["s1"]);
    await click(button(add(), "Add stack"));

    const [id] = Object.keys(board().stacks).filter((stack) => stack !== "s1");
    expect(board().stacks[id ?? ""]).toEqual({ name: "Stack 1", blocks: ["f", "c"], color: "#66c2a5" });
    // The next stack gets a name and a colour of its own.
    expect(labelled(add(), "Name").placeholder).toBe("Stack 2");
    expect(color().value).toBe("#fc8d62");
    expect(texts(row(section(el, "Stacks"), "Stack 1"), "li")).toEqual(["Scatter f", "Merge c"]);
  });

  test("are changed in their row: renamed, coloured and given other blocks, one of which leaves its stack, each change a step of its own", async () => {
    const { el, core, board } = await mounted();
    await act(() => core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"] }));
    const stacks = section(el, "Stacks");

    await click(button(stacks, "Edit the stack Input"));
    const edit = (): HTMLFormElement => form(stacks, "Change stack");
    expect(labelled(edit(), "Name").value).toBe("Input");
    expect(find(edit(), 'input[type="checkbox"]', HTMLInputElement).checked).toBe(false);
    expect(texts(edit(), 'ul[aria-label="Blocks, picked"] li > span')).toEqual(["Iris", "Head"]);

    await step(() => {
      type(labelled(edit(), "Name"), "Sources");
      find(edit(), 'input[type="checkbox"]', HTMLInputElement).click();
      find(edit(), 'button[aria-label="Remove Head"]').click();
    });
    await chosen(edit(), "Blocks", "Scatter");
    await click(button(edit(), "Save"));

    expect(board().stacks.s1).toEqual({ name: "Sources", blocks: ["a", "f"], color: "#66c2a5" });
    expect(board().stacks.plots?.blocks).toEqual([]);
    expect(document.activeElement).toBe(button(stacks, "Edit the stack Sources"));

    // The name and the rest change in steps of their own.
    await step(() => {
      core.store.undo();
    });
    expect(board().stacks.s1).toEqual({ name: "Sources", blocks: ["a", "b"], color: null });
    await step(() => {
      core.store.undo();
    });
    expect(board().stacks.s1?.name).toBe("Input");
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

  test("that the command registry or the store refuses, the plugin tells the user of through a notification", async () => {
    const { el, core, board, notes } = await mounted();
    const add = (): HTMLFormElement => form(section(el, "Links"), "Add link");
    await click(button(section(el, "Links"), "Remove the link from Iris to Head"));

    // Another edit feeds Head's one input while the form is filled in.
    await chosen(add(), "From", "Scatter");
    await chosen(add(), "To", "Head");
    await act(() => core.commands.run("blockr.core-add_link", { from: "a", to: "b", input: "data" }));
    await click(button(add(), "Add link"));

    expect(Object.values(board().links).filter((l) => l.to === "b")).toEqual([{ from: "a", to: "b", input: "data" }]);
    expect(notes).toHaveLength(1);
    expect(notes[0]?.severity).toBe("error");
    expect(notes[0]?.source).toEqual({ kind: "module", object: { plugin: EDIT }, code: "edit_refused", package: "blockr.core" });
    // It concerns the plugin, whatever block the edit names.
    expect(notes[0]?.about).toEqual({ plugin: EDIT });
    expect(notes[0]?.message).toContain('The command "blockr.core-add_link" refused to run');
  });
});

describe("under the board lock", () => {
  const locked: HelloReply = { ...HELLO, lock: { board: true, load: false } };

  test("the plugin draws no control, and lists the board's links and stacks alone, in tables that a search narrows", async () => {
    const { el } = await mounted({ hello: locked });

    expect([...el.querySelectorAll("section")].map((s) => s.getAttribute("aria-label"))).toEqual(["Links", "Stacks"]);
    expect(el.querySelectorAll("form, button, select")).toHaveLength(0);
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
