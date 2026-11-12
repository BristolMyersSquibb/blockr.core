// Core's Edit board plugin on the CoreApi for tests: the board of the fixture
// hello, whose app runs the plugin, with R played from the protocol's
// fixtures, the plugin's UI mounted as a frontend places it, and the user
// acting on it.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import type { HelloReply } from "../../protocol/index.ts";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions } from "../../stubs/index.ts";
import { find, pick, press, texts, type } from "../../testing/inputs.ts";
import { step } from "../../testing/react.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi, Note } from "../../types/core-api.ts";
import * as editBoard from "./index.tsx";

// The class of the plugin, which the app of the fixture hello runs.
const EDIT = "blockr.core-edit_board_plugin";

interface Mounted {
  readonly el: HTMLElement;
  readonly core: CoreApi;
  readonly board: () => Board;
  // The notes that the plugin sent, in order.
  readonly notes: readonly Note[];
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
  const notes: Note[] = [];
  stubbed.core.notifications.subscribe((note) => {
    notes.push(note);
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

// The button whose label or text is the one given, in `el`.
function button(el: ParentNode, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.getAttribute("aria-label") === text || b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}

// The button of the block registry's entry of the name given, in the picker.
function entry(el: ParentNode, name: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("li button")].find((b) => b.querySelector("span")?.textContent === name);
  if (!(found instanceof HTMLButtonElement)) throw new Error(`Found no entry ${name}.`);
  return found;
}

// The field of the label given, as a label holds it.
function field(el: ParentNode, label: string): HTMLInputElement {
  const found = [...el.querySelectorAll("label")].find((l) => l.querySelector("span")?.textContent === label);
  return find(found ?? document.createElement("label"), "input", HTMLInputElement);
}

// The select of the label given, as a label holds it.
function select(el: ParentNode, label: string): HTMLSelectElement {
  const found = [...el.querySelectorAll("label")].find((l) => l.querySelector("span")?.textContent === label);
  return find(found ?? document.createElement("label"), "select", HTMLSelectElement);
}

// The item of a link, by the names of the blocks it joins.
function link(el: HTMLElement, from: string, to: string): HTMLElement {
  return find(section(el, "Links"), `li[aria-label="${from} → ${to}"]`);
}

async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
}

// The blocks that the board holds beside those of the fixture's board.
function added(board: Board): [string, Board["blocks"][string]][] {
  return Object.entries(board.blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
}

describe("the blocks", () => {
  test("are added from the block registry, which the search narrows, under the ID and the name that the user gives", async () => {
    const { el, board, notes } = await mounted();
    const blocks = section(el, "Blocks");

    // The picker groups the entries by category.
    expect([...blocks.querySelectorAll("section")].map((s) => s.getAttribute("aria-label"))).toEqual(["input", "plot", "transform"]);
    await step(() => {
      type(field(blocks, "Search"), "scatter");
    });
    expect(texts(blocks, "section li button span:first-child")).toEqual(["scatter plot block"]);

    await step(() => {
      type(field(blocks, "ID"), "petals");
      type(field(blocks, "Name"), "Petal plot");
    });
    await click(entry(blocks, "scatter plot block"));

    expect(board().blocks.petals).toMatchObject({ registry_id: "blockr.core-scatter_block", name: "Petal plot" });
    expect(Object.values(board().links).filter((l) => l.to === "petals")).toEqual([]);
    expect(notes).toEqual([]);
    // The next block takes an ID and a name of its own.
    expect([field(blocks, "ID").value, field(blocks, "Name").value]).toEqual(["", ""]);
    expect(texts(find(blocks, 'ul[aria-label="Blocks"]'), "li > span")).toContain("Petal plot (petals)");
  });

  test("get an ID that the command mints and the name of their class where the user leaves the fields empty", async () => {
    const { el, board } = await mounted();

    await click(entry(section(el, "Blocks"), "dataset block"));

    const [first] = added(board());
    expect(first?.[0]).toMatch(/^[a-z]+_[a-z]+$/);
    expect(first?.[1]).toMatchObject({ registry_id: "blockr.core-dataset_block", name: "Dataset" });
  });

  test("keep what the user gave where the store refuses the block, and the user learns why", async () => {
    const { el, board, notes } = await mounted();
    const blocks = section(el, "Blocks");

    // The board holds a block of the ID a already.
    await step(() => {
      type(field(blocks, "ID"), "a");
    });
    await click(entry(blocks, "dataset block"));

    expect(board().blocks.a?.name).toBe("Iris");
    expect(added(board())).toEqual([]);
    expect(notes).toHaveLength(1);
    expect(notes[0]?.severity).toBe("error");
    expect(notes[0]?.message).toContain("The store rejected the delta");
    expect(field(blocks, "ID").value).toBe("a");
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
    expect(find(blocks, 'li[aria-label="First rows"]').textContent).toContain("First rows (b)");

    await click(button(blocks, "Remove the block First rows"));
    expect(Object.keys(board().blocks)).not.toContain("b");
    expect(Object.values(board().links).filter((l) => l.from === "b" || l.to === "b")).toEqual([]);
    expect(board().stacks.s1?.blocks).toEqual(["a"]);
    expect(blocks.querySelector('li[aria-label="First rows"]')).toBeNull();
  });
});

describe("the links", () => {
  test("each show the blocks they join and the input they feed, which the user changes in place", async () => {
    const { el, board } = await mounted();
    expect(find(section(el, "Links"), 'ul[aria-label="Links"]').children).toHaveLength(7);

    // Removing the link from Head frees the input y of Merge.
    await click(button(link(el, "Head", "Merge"), "Remove the link from Head to Merge"));
    expect(Object.keys(board().links)).not.toContain("bc");

    // The input of a link, among the inputs of its target that no other link
    // feeds.
    const input = select(link(el, "Iris", "Merge"), "Input");
    expect([...input.options].map((o) => o.value)).toEqual(["x", "y"]);
    await step(() => {
      pick(input, "y");
    });
    expect(board().links.ac).toEqual({ from: "a", to: "c", input: "y" });

    // The block a link goes to, among those with an input that no other link
    // feeds. The link then feeds the first such input.
    const to = select(link(el, "Iris", "Scatter"), "To");
    const open = [...to.options].filter((o) => !o.disabled).map((o) => o.value);
    expect(open).toEqual(["c", "e", "f"]);
    await step(() => {
      pick(to, "c");
    });
    expect(board().links.keen_finch).toEqual({ from: "a", to: "c", input: "x" });

    // The block a link comes from, which may be any block but its target.
    const from = select(link(el, "Iris", "Head"), "From");
    expect([...from.options].filter((o) => o.disabled).map((o) => o.value)).toEqual(["b"]);
    await step(() => {
      pick(from, "d");
    });
    expect(board().links.ab).toEqual({ from: "d", to: "b", input: "data" });
  });

  test("are added into a free input of their target, which the form offers", async () => {
    const { el, board } = await mounted();
    const form = (): HTMLElement => find(section(el, "Links"), 'form[aria-label="Add link"]');

    await click(button(link(el, "Iris", "Scatter"), "Remove the link from Iris to Scatter"));
    await step(() => {
      pick(select(form(), "From"), "b");
      pick(select(form(), "To"), "f");
    });
    // The scatter block's one input is free again.
    expect(select(form(), "Input").value).toBe("data");
    await click(button(form(), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "b", to: "f", input: "data" });
    // With no input of the scatter block free, the form adds no link to it.
    expect(select(form(), "Input").disabled).toBe(true);
    expect(button(form(), "Add link").disabled).toBe(true);

    // A variadic block takes a link in order, so the form offers no input.
    await step(() => {
      pick(select(form(), "From"), "c");
      pick(select(form(), "To"), "e");
    });
    expect(form().querySelectorAll("select")).toHaveLength(2);
    await click(button(form(), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "c", to: "e", input: "" });
  });

  test("are added between the board's first two blocks unless the user picks others, on a board that held no blocks as well", async () => {
    const empty: HelloReply = { ...HELLO, board: { ...HELLO.board, board: { ...HELLO.board.board, blocks: {}, links: {}, stacks: {} } } };
    const { el, board } = await mounted({ hello: empty });
    const form = (): HTMLElement => find(section(el, "Links"), 'form[aria-label="Add link"]');
    expect(button(form(), "Add link").disabled).toBe(true);

    for (const [id, name] of [
      ["iris", "dataset block"],
      ["rows", "head/tail block"],
    ] as const) {
      await step(() => {
        type(field(section(el, "Blocks"), "ID"), id);
      });
      await click(entry(section(el, "Blocks"), name));
    }

    expect([select(form(), "From").value, select(form(), "To").value, select(form(), "Input").value]).toEqual(["iris", "rows", "data"]);
    await click(button(form(), "Add link"));
    expect(Object.values(board().links)).toEqual([{ from: "iris", to: "rows", input: "data" }]);
  });
});

describe("the stacks", () => {
  test("are added, renamed, coloured, given blocks and removed", async () => {
    const { el, board } = await mounted();
    const stacks = section(el, "Stacks");

    await click(button(stacks, "Add stack"));
    const [id] = Object.keys(board().stacks).filter((stack) => stack !== "s1");
    if (id === undefined) throw new Error("No stack was added.");
    expect(board().stacks[id]).toEqual({ name: "Stack", blocks: [], color: null });
    const item = () => find(stacks, 'li[aria-label="Stack"], li[aria-label="Plots"]');

    await click(button(item(), "Rename the stack Stack"));
    await step(() => {
      const name = find(item(), 'input[aria-label="Stack name"]', HTMLInputElement);
      type(name, "Plots");
      press(name, "Enter");
    });
    expect(board().stacks[id]?.name).toBe("Plots");

    await step(() => {
      const color = find(item(), 'input[type="color"]', HTMLInputElement);
      color.value = "#66c2a5";
      color.dispatchEvent(new Event("change", { bubbles: true }));
    });
    expect(board().stacks[id]?.color).toBe("#66c2a5");

    // A block sits in one stack at most, so Head leaves Input.
    await click(find(item(), 'label:has(input[type="checkbox"]):nth-of-type(2) input', HTMLInputElement));
    expect(board().stacks[id]?.blocks).toEqual(["b"]);
    expect(board().stacks.s1?.blocks).toEqual(["a"]);

    await click(button(item(), "No colour"));
    expect(board().stacks[id]?.color).toBeNull();
    await click(button(item(), "Remove the stack Plots"));
    expect(Object.keys(board().stacks)).toEqual(["s1"]);
    expect(Object.keys(board().blocks)).toContain("b");
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
    await click(button(link(el, "Iris", "Scatter"), "Remove the link from Iris to Scatter"));
    expect(board().blocks.b?.name).toBe("First rows");
    expect(Object.keys(board().links)).not.toContain("keen_finch");

    await step(() => {
      core.store.undo();
    });
    expect(Object.keys(board().links)).toContain("keen_finch");
    expect(board().blocks.b?.name).toBe("First rows");
    expect(link(el, "Iris", "Scatter")).toBeDefined();

    await step(() => {
      core.store.undo();
    });
    expect(board().blocks.b?.name).toBe("Head");
    expect(core.store.canUndo()).toBe(false);
  });

  test("that the store refuses, the plugin tells the user of through a notification", async () => {
    const { el, board, notes } = await mounted();

    // A link from Merge into Head, which feeds Merge, would make a cycle.
    await step(() => {
      pick(select(link(el, "Iris", "Head"), "From"), "c");
    });

    expect(board().links.ab).toEqual({ from: "a", to: "b", input: "data" });
    expect(notes).toHaveLength(1);
    expect(notes[0]?.severity).toBe("error");
    expect(notes[0]?.message).toContain("The store rejected the delta");
    expect(select(link(el, "Iris", "Head"), "From").value).toBe("a");
  });
});

describe("under the board lock", () => {
  const locked: HelloReply = { ...HELLO, lock: { board: true, load: false } };

  test("the plugin draws no control, and lists the board's links and stacks alone", async () => {
    const { el } = await mounted({ hello: locked });

    expect([...el.querySelectorAll("section")].map((s) => s.getAttribute("aria-label"))).toEqual(["Links", "Stacks"]);
    expect(el.querySelectorAll("button, input, select, form")).toHaveLength(0);
    expect(texts(section(el, "Links"), "li")).toEqual([
      "Iris → Head as data",
      "Iris → Merge as x",
      "Head → Merge as y",
      "Iris → Wide sepals as data",
      "Head → Bind",
      "Wide sepals → Bind",
      "Iris → Scatter as data",
    ]);
    expect(find(section(el, "Stacks"), 'li[aria-label="Input"]').textContent).toContain("Iris, Head");
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
