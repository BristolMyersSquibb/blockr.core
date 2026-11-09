// Core's basic front-end on the CoreApi for tests: the board of the fixture
// hello, drawn by the front-end as the page draws it, with R played from the
// protocol's fixtures, every block UI and renderer a stub that names what it
// draws, and the user acting on the page.

import { act } from "react";
import { afterEach, describe, expect, test, vi } from "vitest";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions } from "../../stubs/index.ts";
import { choose, find, pick, press, texts, type } from "../../testing/inputs.ts";
import { step } from "../../testing/react.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import * as basic from "./index.tsx";

// A module that writes what it draws and for which block, as a block's UI or
// an output's renderer.
function stub(what: string) {
  return {
    api: 1,
    default: {
      render({ el, host }: { el: HTMLElement; host: { block: string } }) {
        el.textContent = `${what} of ${host.block}`;
        return () => {
          el.replaceChildren();
        };
      },
    },
  };
}

const entries = Object.values(HELLO.block_registry);
const MODULES: Readonly<Record<string, unknown>> = {
  "blockr.core-basic": basic,
  ...Object.fromEntries(entries.map((entry) => [entry.module, stub("UI")])),
  ...Object.fromEntries(entries.flatMap((entry) => (entry.renderer === undefined ? [] : [[entry.renderer.module, stub("Output")]]))),
};

interface Drawn {
  el: HTMLElement;
  // The CoreApi of the board that is open, which core makes anew for each
  // board that opens.
  readonly core: CoreApi;
  board: () => Board;
  settled: () => Promise<void>;
  stop: () => void;
}

// The board of the fixture hello, or of the reply given, drawn by the basic
// front-end once R has evaluated what it demands.
async function drawn(options: StubCoreOptions = {}): Promise<Drawn> {
  const el = document.createElement("div");
  document.body.append(el);
  const stubbed = await act(() => stubCore({ el, ...options, modules: { ...MODULES, ...options.modules } }));
  const settled = () => act(() => stubbed.settled());
  await settled();
  return {
    el,
    get core() {
      return stubbed.core;
    },
    board: () => stubbed.core.store.board(),
    settled,
    stop: stubbed.stop,
  };
}

// The card of the block of the name given.
function card(el: HTMLElement, name: string): HTMLElement {
  return find(el, `article[aria-label="${name}"]`);
}

// The button whose text is the one given, in `el`.
function button(el: ParentNode, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}

// The button of the block registry's entry of the name given, in the dialog
// that adds a block.
function entry(el: ParentNode, name: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("li button")].find((b) => b.querySelector("span")?.textContent === name);
  if (!(found instanceof HTMLButtonElement)) throw new Error(`Found no entry ${name}.`);
  return found;
}

function buttons(el: ParentNode): string[] {
  return [...el.querySelectorAll("button")].map((b) => b.getAttribute("aria-label") ?? b.textContent.trim());
}

async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
}

// Picks an action from the menu of the card of the block of the name given.
async function menu(el: HTMLElement, name: string, action: string): Promise<void> {
  await click(find(card(el, name), `button[aria-label="Actions of ${name}"]`));
  await click(button(card(el, name), action));
}

function dialog(el: HTMLElement): HTMLDialogElement {
  return find(el, "dialog[open]", HTMLDialogElement);
}

afterEach(() => {
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

describe("the page", () => {
  test("draws the board's name, each stack as a group of its blocks' cards, and the other blocks in the board's order", async () => {
    const { el } = await drawn();

    expect(find(el, "h1").textContent).toBe("Iris review");
    const stack = find(el, 'section[aria-label="Input"]');
    expect(texts(stack, "article h3")).toEqual(["Iris", "Head"]);
    expect(texts(el, "article h3")).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Bind", "Scatter"]);
  });

  test("demands every block, so that R evaluates each, and mounts each block's UI and output", async () => {
    const { el, core } = await drawn();

    for (const id of Object.keys(core.store.board().blocks)) {
      expect(core.scheduler.status(id)?.status).toBe("ready");
    }
    expect(card(el, "Head").textContent).toContain("UI of b");
    expect(card(el, "Head").textContent).toContain("Output of b");
    expect(find(card(el, "Head"), "[data-status]").textContent).toBe("ready");
  });
});

describe("a card", () => {
  test("shows its block's status, its summary and the conditions of the severities that show_conditions names", async () => {
    const { el, core } = await drawn({
      evaluate: (request) =>
        request.block === "b"
          ? {
              summary: "<data.frame> [6 × 5]",
              conditions: [
                { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "expr", message: "Few rows" },
                { severity: "message", class: ["simpleMessage", "message", "condition"], phase: "expr", message: "Took the head" },
              ],
              omitted: { warning: 2, message: 0 },
            }
          : undefined,
    });

    const head = card(el, "Head");
    // The summary is the last line of the output's box, whose end holds the
    // handle that resizes the box.
    expect(find(head, '[aria-label="Output"]').lastElementChild?.textContent).toBe("<data.frame> [6 × 5]");
    // The board holds show_conditions at warnings and errors.
    expect(texts(head, "[data-severity]")).toEqual(["warning: Few rows"]);
    expect(head.textContent).toContain("2 more warnings not kept");

    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["message"] } } });
    });
    expect(texts(card(el, "Head"), "[data-severity]")).toEqual(["message: Took the head"]);
    expect(card(el, "Head").textContent).not.toContain("more warnings");
  });

  test("greys its output out while its block is not ready", async () => {
    const { el, core } = await drawn({
      evaluate: (request) =>
        request.block === "b"
          ? { status: "error", conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "expr", message: "No rows" }] }
          : undefined,
    });

    expect(find(card(el, "Iris"), '[aria-label="Output"]').dataset.ready).toBe("true");
    expect(find(card(el, "Head"), "[data-status]").textContent).toBe("failed");
    expect(find(card(el, "Head"), '[aria-label="Output"]').dataset.ready).toBe("false");
    expect(find(card(el, "Head"), '[aria-label="Output"]').className).toContain("bc:opacity-50");
    expect(texts(card(el, "Head"), "[data-severity]")).toEqual(["error: No rows"]);

    // The blocks downstream of a failed block wait on it.
    expect(core.scheduler.status("c")?.status).toBe("waiting");
    expect(find(card(el, "Merge"), "[data-status]").textContent).toBe("waiting");
  });

  test("renames and removes its block from its menu, through core's commands", async () => {
    const { el, board } = await drawn();

    await menu(el, "Head", "Rename");
    const field = find(card(el, "Head"), 'input[aria-label="Block name"]', HTMLInputElement);
    await step(() => {
      type(field, "First rows");
      press(field, "Enter");
    });
    expect(board().blocks.b?.name).toBe("First rows");
    expect(find(card(el, "First rows"), "h3").textContent).toBe("First rows");

    await menu(el, "Scatter", "Remove");
    expect(Object.keys(board().blocks)).not.toContain("f");
    expect(el.querySelector('article[aria-label="Scatter"]')).toBeNull();
  });

  test("adds a block after its block, which takes its block's result, picking the input where the new block has several", async () => {
    const { el, board, settled } = await drawn();

    await menu(el, "Head", "Add block after");
    expect(find(dialog(el), "h2").textContent).toBe('Add block after "Head"');
    // A block added after another needs an input.
    expect(texts(dialog(el), "li button span:first-child")).not.toContain("dataset block");
    await click(entry(dialog(el), "subset block"));
    await settled();

    const [added] = Object.entries(board().blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
    expect(added?.[1].registry_id).toBe("blockr.core-subset_block");
    expect(Object.values(board().links)).toContainEqual({ from: "b", to: added?.[0], input: "data" });
    expect(el.querySelector("dialog[open]")).toBeNull();

    await menu(el, "Iris", "Add block after");
    await click(entry(dialog(el), "merge block"));
    await step(() => {
      pick(find(dialog(el), "select", HTMLSelectElement), "y");
    });
    await click(button(dialog(el), "Add merge block"));
    const merge = Object.entries(board().blocks).find(([id, block]) => id !== "c" && block.registry_id === "blockr.core-merge_block");
    expect(Object.values(board().links)).toContainEqual({ from: "a", to: merge?.[0], input: "y" });
  });

  test("adds a block before its block, into a free input, and offers it only where an input is free", async () => {
    const { el, board } = await drawn();

    // Every input of the merge block takes a link already.
    await click(find(card(el, "Merge"), 'button[aria-label="Actions of Merge"]'));
    expect(button(card(el, "Merge"), "Add block before").disabled).toBe(true);
    await step(() => {
      press(document.body, "Escape");
    });

    // The bind block is variadic, so it takes another link.
    await menu(el, "Bind", "Add block before");
    await click(entry(dialog(el), "dataset block"));
    const [added] = Object.entries(board().blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
    expect(added?.[1].registry_id).toBe("blockr.core-dataset_block");
    expect(Object.values(board().links)).toContainEqual({ from: added?.[0], to: "e", input: "" });
  });
});

describe("a card's block before", () => {
  test("takes the link into the input the user picks, where several of its block's inputs are free", async () => {
    const { el, core, board } = await drawn();
    await act(() => core.commands.run("blockr.core-remove_link", { id: "ac" }));
    await act(() => core.commands.run("blockr.core-remove_link", { id: "bc" }));

    await menu(el, "Merge", "Add block before");
    const select = find(dialog(el), "select", HTMLSelectElement);
    expect([...select.options].map((option) => option.value)).toEqual(["x", "y"]);
    await step(() => {
      pick(select, "y");
    });
    await click(entry(dialog(el), "dataset block"));

    const [added] = Object.entries(board().blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
    expect(Object.values(board().links)).toContainEqual({ from: added?.[0], to: "c", input: "y" });
  });
});

describe("the toolbar", () => {
  test("adds a block of a class from the block registry, which the search narrows", async () => {
    const { el, board } = await drawn();

    await click(button(el, "Add block"));
    expect(document.activeElement).toBe(find(dialog(el), 'input[type="search"]'));
    expect(texts(dialog(el), "h3")).toEqual(["input", "plot", "transform"]);
    await step(() => {
      type(find(dialog(el), 'input[type="search"]', HTMLInputElement), "scatter");
    });
    expect(texts(dialog(el), "li button span:first-child")).toEqual(["scatter plot block"]);
    await click(entry(dialog(el), "scatter plot block"));

    const [added] = Object.entries(board().blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
    expect(added?.[1]).toMatchObject({ registry_id: "blockr.core-scatter_block", name: "Scatter" });
    expect(Object.values(board().links).filter((link) => link.to === added?.[0])).toEqual([]);
  });

  test("removes a link and adds one into a free input of its target", async () => {
    const { el, board } = await drawn();

    await click(button(el, "Links"));
    expect(find(dialog(el), 'ul[aria-label="Links"]').children).toHaveLength(7);
    await click(find(dialog(el), 'button[aria-label="Remove the link from Iris to Scatter"]'));
    expect(Object.keys(board().links)).not.toContain("keen_finch");

    const selects = () => [...dialog(el).querySelectorAll("form select")] as HTMLSelectElement[];
    const [from, to] = selects();
    if (from === undefined || to === undefined) throw new Error("The form lacks its selects.");
    await step(() => {
      pick(from, "b");
      pick(to, "f");
    });
    // The scatter block's one input is free again.
    expect(selects()[2]?.value).toBe("data");
    await click(button(dialog(el), "Add link"));
    expect(Object.values(board().links)).toContainEqual({ from: "b", to: "f", input: "data" });
    // With no input of the scatter block free, the form adds no link to it.
    expect(button(dialog(el), "Add link").disabled).toBe(true);
  });

  test("adds a stack, renames it, colours it, puts a block in it and removes it", async () => {
    const { el, board } = await drawn();

    await click(button(el, "Stacks"));
    await click(button(dialog(el), "Add stack"));
    const [id] = Object.keys(board().stacks).filter((stack) => stack !== "s1");
    if (id === undefined) throw new Error("No stack was added.");
    expect(board().stacks[id]).toEqual({ name: "Stack", blocks: [], color: null });
    const item = () => find(dialog(el), 'li[aria-label="Stack"], li[aria-label="Plots"]');

    await click(button(item(), "Rename"));
    await step(() => {
      const field = find(item(), 'input[aria-label="Stack name"]', HTMLInputElement);
      type(field, "Plots");
      press(field, "Enter");
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
    expect(texts(find(el, 'section[aria-label="Plots"]'), "article h3")).toEqual(["Head"]);

    await click(button(item(), "No colour"));
    expect(board().stacks[id]?.color).toBeNull();
    await click(find(item(), 'button[aria-label="Remove the stack Plots"]'));
    expect(Object.keys(board().stacks)).toEqual(["s1"]);
    expect(Object.keys(board().blocks)).toContain("b");
  });

  test("sets the board options that something on the page requires or that the board holds, through their UIs, by category", async () => {
    const table = entries.find((entry) => entry.renderer?.id === "blockr.core-table_output")?.renderer;
    if (table === undefined) throw new Error("No block class of the fixtures draws a table.");
    const { el, board, settled } = await drawn({
      hello: { ...HELLO, board: { ...HELLO.board, board: { ...HELLO.board.board, options: {} } } },
      modules: { [table.module]: { api: 1, default: { ...stub("Output").default, options: ["n_rows"] } } },
    });

    await click(button(el, "Options"));
    await settled();
    // The board holds show_conditions and dark_mode, which core's basic
    // front-end requires, while the table renderer requires n_rows, whose
    // default the board does not take.
    expect(Object.keys(board().options).sort()).toEqual(["dark_mode", "show_conditions"]);
    expect(texts(dialog(el), "h3")).toEqual(["Table options", "Board options", "Theme options"]);
    expect(dialog(el).textContent).toContain("Preview rows");
    expect(find(dialog(el), 'section[aria-label="Table options"] input', HTMLInputElement).value).toBe("10");
    expect(dialog(el).textContent).toContain("Message types to display");

    await step(() => {
      pick(find(dialog(el), 'section[aria-label="Theme options"] select', HTMLSelectElement), "dark");
    });
    expect(board().options.dark_mode).toBe("dark");
    expect(document.documentElement.getAttribute("data-bs-theme")).toBe("dark");
  });

  test("renames the board", async () => {
    const { el, board } = await drawn();

    await click(find(el, 'button[aria-label="Rename board"]'));
    await step(() => {
      const field = find(el, 'input[aria-label="Board name"]', HTMLInputElement);
      type(field, "Sepals");
      press(field, "Enter");
    });
    expect(board().name).toBe("Sepals");
    expect(find(el, "h1").textContent).toBe("Sepals");
  });

  test("saves the board as a file that holds the store's text, named by the board's id and the time", async () => {
    const { el, core } = await drawn();
    const blobs: Blob[] = [];
    const names: string[] = [];
    vi.spyOn(URL, "createObjectURL").mockImplementation((blob) => {
      blobs.push(blob as Blob);
      return "blob:board";
    });
    vi.spyOn(URL, "revokeObjectURL").mockImplementation(() => undefined);
    vi.spyOn(HTMLAnchorElement.prototype, "click").mockImplementation(function (this: HTMLAnchorElement) {
      names.push(this.download);
    });

    await click(button(el, "Save"));
    expect(names).toHaveLength(1);
    expect(names[0]).toMatch(/^brisk_otter_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.json$/);
    expect(await blobs[0]?.text()).toBe(core.store.save());
    expect(blobs[0]?.type).toBe("application/json");
  });

  test("opens a file that the user picks, drawn as the page draws it, and says why the store refused one", async () => {
    const page = await drawn();
    const saved = JSON.parse(page.core.store.save()) as { board: Board };
    const text = JSON.stringify({ ...saved, board: { ...saved.board, name: "Opened" } });

    await step(() => {
      choose(find(page.el, 'input[type="file"]', HTMLInputElement), new File([text], "board.json", { type: "application/json" }));
    });
    await page.settled();
    // Core starts again for the board, with a CoreApi of its own.
    expect(page.board().name).toBe("Opened");
    expect(find(page.el, "h1").textContent).toBe("Opened");
    expect(page.el.querySelectorAll("article")).toHaveLength(6);

    await step(() => {
      choose(find(page.el, 'input[type="file"]', HTMLInputElement), new File(["{}"], "board.json"));
    });
    await page.settled();
    expect(page.board().name).toBe("Opened");
    expect(find(page.el, '[role="alert"]').textContent).toContain("The store refused to open the board");
  });
});

describe("what the user learns", () => {
  test("is which edit the store refused", async () => {
    const { el, board } = await drawn();

    // A link from the bind block into itself would make a cycle.
    await click(button(el, "Links"));
    const [from, to] = [...dialog(el).querySelectorAll("form select")] as HTMLSelectElement[];
    if (from === undefined || to === undefined) throw new Error("The form lacks its selects.");
    await step(() => {
      pick(from, "e");
      pick(to, "e");
    });
    await click(button(dialog(el), "Add link"));
    await click(button(dialog(el), "Close"));

    expect(Object.values(board().links)).not.toContainEqual({ from: "e", to: "e", input: "" });
    expect(find(el, '[role="alert"]').textContent).toContain("The store rejected the delta");
  });
});

describe("undo and redo", () => {
  test("are bound to their shortcuts, which a text field keeps for its own typing", async () => {
    const { el, core, board } = await drawn();
    await act(() => core.commands.run("blockr.core-rename_board", { name: "Sepals" }));

    await step(() => {
      press(document.body, "z", { ctrlKey: true });
    });
    expect(board().name).toBe("Iris review");
    await step(() => {
      press(document.body, "z", { metaKey: true, shiftKey: true });
    });
    expect(board().name).toBe("Sepals");
    await step(() => {
      press(document.body, "z", { metaKey: true });
      press(document.body, "y", { ctrlKey: true });
    });
    expect(board().name).toBe("Sepals");

    const field = document.createElement("input");
    el.append(field);
    await step(() => {
      press(field, "z", { ctrlKey: true });
    });
    expect(board().name).toBe("Sepals");
  });

  test("are on the toolbar, each while the user has a step for it", async () => {
    const { el, core, board } = await drawn();
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([true, true]);

    await act(() => core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([false, true]);

    await click(button(el, "Undo"));
    expect(board().name).toBe("Iris review");
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([true, false]);
  });
});

describe("a locked board", () => {
  test("draws no control for what the lock fixes, and the UI only of the blocks it leaves open", async () => {
    const { el } = await drawn({ hello: { ...HELLO, lock: { open: ["b"] } } });

    expect(buttons(find(el, '[role="toolbar"]'))).toEqual(["Undo", "Redo", "Save"]);
    expect(el.querySelector('button[aria-label="Rename board"]')).toBeNull();
    expect(el.querySelector('input[type="file"]')).toBeNull();
    // No card has a menu.
    expect(el.querySelectorAll("article button")).toHaveLength(0);

    expect(card(el, "Head").textContent).toContain("UI of b");
    expect(card(el, "Iris").textContent).not.toContain("UI of a");
    expect(card(el, "Iris").textContent).toContain("Output of a");
  });
});

describe("the cleanup that render() returns", () => {
  test("ends what the front-end drew, demanded and bound", async () => {
    const stubbed = await stubCore({ modules: MODULES });
    const el = document.createElement("div");
    document.body.append(el);
    let cleanup = (): void => undefined;
    act(() => {
      cleanup = basic.default.render({ el, core: stubbed.core, signal: new AbortController().signal });
    });
    await act(() => stubbed.settled());
    expect(el.querySelectorAll("article")).toHaveLength(6);
    await act(() => stubbed.core.commands.run("blockr.core-rename_board", { name: "Sepals" }));

    act(() => {
      cleanup();
    });
    expect(el.childNodes).toHaveLength(0);

    press(document.body, "z", { ctrlKey: true });
    expect(stubbed.core.store.board().name).toBe("Sepals");

    // Nothing demands the blocks any more, so they only turn stale.
    const sent = stubbed.sent.length;
    stubbed.core.scheduler.invalidate("a");
    await stubbed.settled();
    expect(stubbed.sent.slice(sent).filter((request) => request.kind === "evaluate")).toEqual([]);
    expect(stubbed.core.scheduler.status("a")?.status).toBe("stale");
    stubbed.stop();
  });
});
