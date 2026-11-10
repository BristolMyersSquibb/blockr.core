// Core's basic frontend on the CoreApi for tests: the board of the fixture
// hello, drawn by the frontend as the page draws it, with R played from the
// protocol's fixtures, every block UI and renderer a stub that names what it
// draws, the plugins a test needs, and the user acting on the page.

import { act } from "react";
import { afterEach, describe, expect, test, vi } from "vitest";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions } from "../../stubs/index.ts";
import type { HelloReply } from "../../protocol/index.ts";
import type { PluginClass } from "../../store/index.ts";
import { find, pick, picking, press, texts, type } from "../../testing/inputs.ts";
import { pluginUi, registering } from "../../testing/plugins.ts";
import { step } from "../../testing/react.ts";
import { withPlugins } from "../../testing/store.ts";
import type { Board, JsonBoard, Plugin, PluginId } from "../../types/board.ts";
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
// frontend once R has evaluated what it demands.
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

// The status that the badge in a card's header shows.
const badge = (card: HTMLElement): string => find(card, "header [data-status]").textContent;

// The line of a card's footer: the summary, where it shows one, and the text
// of each pill.
const line = (card: HTMLElement): string[] => texts(find(card, "footer"), "span > span, button");

// The severity and the message of each banner that a card shows, in order.
function banners(card: HTMLElement): string[][] {
  return [...card.querySelectorAll('ul[aria-label="Conditions"] li')].map((li) => [
    li.getAttribute("data-severity") ?? "",
    li.querySelector("span > span:last-child")?.textContent ?? "",
  ]);
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

function toolbar(el: HTMLElement): HTMLElement {
  return find(el, '[role="toolbar"]');
}

// The severity and the text of each notice that the page shows.
function notices(el: HTMLElement): [string | null, string][] {
  return [...el.querySelectorAll('ul[aria-label="Notices"] > li')].map((li) => [
    li.getAttribute("data-severity"),
    li.querySelector("span")?.textContent ?? "",
  ]);
}

const plugin = (cls: PluginClass, name: string): Plugin => ({ class: cls.id, name, state: {} });

// What the test gives core for a board that holds the plugins given, of the
// classes given, which blockr.test registers.
function withClasses(classes: readonly PluginClass[], plugins: Readonly<Record<PluginId, Plugin>>, hello = HELLO): StubCoreOptions {
  return { hello: withPlugins(plugins, hello), startup: { "blockr.test": registering(classes) } };
}

// The text of the board that the page saves, as `edit` changes it.
function savedAs(core: CoreApi, edit: (json: JsonBoard) => JsonBoard): string {
  return JSON.stringify(edit(JSON.parse(core.store.save()) as JsonBoard));
}

// Renames the board that a JSON board holds.
const renamed = (name: string) => (json: JsonBoard): JsonBoard => ({ ...json, board: { ...json.board, name } });

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

  test("collapses a stack as the user clicks its name, which hides the cards of its blocks and leaves them mounted", async () => {
    const { el, core, settled } = await drawn();
    const stack = find(el, 'section[aria-label="Input"]');
    const group = find(stack, "details", HTMLDetailsElement);
    expect(group.open).toBe(true);

    await click(find(stack, "summary"));
    await act(() => core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    await settled();
    expect(core.store.board().name).toBe("Sepals");
    // It stays collapsed as the board changes, and its blocks' UIs stay.
    expect(group.open).toBe(false);
    expect(texts(stack, "article h3")).toEqual(["Iris", "Head"]);
    expect(card(el, "Head").textContent).toContain("UI of b");

    await click(find(stack, "summary"));
    expect(group.open).toBe(true);
  });

  test("demands every block, so that R evaluates each, and mounts each block's UI and output", async () => {
    const { el, core } = await drawn();

    for (const id of Object.keys(core.store.board().blocks)) {
      expect(core.scheduler.status(id)?.status).toBe("ready");
    }
    expect(card(el, "Head").textContent).toContain("UI of b");
    expect(card(el, "Head").textContent).toContain("Output of b");
    expect(badge(card(el, "Head"))).toBe("ready");
  });
});

describe("a card", () => {
  test("shows its block's status in its header, and in its footer its summary and a pill for each severity that show_conditions names, which opens its banners", async () => {
    const { el, core } = await drawn({
      evaluate: (request) =>
        request.block === "b"
          ? {
              summary: "<data.frame> [6 × 5]",
              conditions: [
                { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "expr", message: "Few rows" },
                { severity: "message", class: ["simpleMessage", "message", "condition"], phase: "expr", message: "Took the head" },
              ],
            }
          : undefined,
    });

    const head = card(el, "Head");
    expect(badge(head)).toBe("ready");
    // The footer is the last line of the output's box, whose end holds the
    // handle that resizes the box.
    const footer = find(head, "footer");
    expect(footer.parentElement?.firstElementChild).toBe(find(head, '[aria-label="Output"]'));
    expect(footer.parentElement?.lastElementChild).toBe(footer);
    // The board holds show_conditions at warnings and errors, so the footer
    // shows no pill for the message.
    expect(line(head)).toEqual(["<data.frame> [6 × 5]", "1 warning"]);
    expect(banners(head)).toEqual([]);

    await click(button(footer, "1 warning"));
    expect(banners(card(el, "Head"))).toEqual([["warning", "Few rows"]]);

    // The banners of a severity go with its pill, as show_conditions leaves
    // the severity out.
    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["message"] } } });
    });
    expect(line(card(el, "Head"))).toEqual(["<data.frame> [6 × 5]", "1 message"]);
    expect(banners(card(el, "Head"))).toEqual([]);

    await click(button(find(card(el, "Head"), "footer"), "1 message"));
    expect(banners(card(el, "Head"))).toEqual([["message", "Took the head"]]);
  });

  test("greys its output out while its block is not ready, but not its footer, which shows a failed block's error and why a block waits", async () => {
    let fails = false;
    const { el, core, settled } = await drawn({
      evaluate: (request) =>
        request.block === "b" && fails
          ? { status: "error", conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "expr", message: "No rows" }] }
          : undefined,
    });
    expect(badge(card(el, "Head"))).toBe("ready");

    // The user changes the block's state, and R fails to evaluate it.
    fails = true;
    await step(() => {
      core.store.apply({ blocks: { mod: { b: { state: { n: 0, direction: "head" } } } } });
    });
    await settled();

    expect(find(card(el, "Iris"), '[aria-label="Output"]').dataset.ready).toBe("true");
    const head = card(el, "Head");
    expect(badge(head)).toBe("failed");
    expect(find(head, '[aria-label="Output"]').dataset.ready).toBe("false");
    expect(find(head, '[aria-label="Output"]').className).toContain("bc:opacity-50");
    expect(find(head, '[aria-label="Output"]').className).toContain("bc:grayscale");
    // Core keeps drawing the last output, which keeps its height.
    expect(find(head, '[aria-label="Output"]').className).toContain("bc:flex-1");
    expect(find(head, '[aria-label="Output"]').textContent).toBe("Output of b");
    // The pill of the errors keeps the colour of its severity, and its banner
    // is open from the start.
    expect(find(head, "footer").closest('[class*="bc:grayscale"]')).toBeNull();
    expect(line(head)).toEqual(["error"]);
    expect(banners(head)).toEqual([["error", "No rows"]]);

    // The blocks downstream of a failed block wait on it, and their footers
    // say why, whatever show_conditions names.
    expect(core.scheduler.status("c")?.status).toBe("waiting");
    const why = core.scheduler.status("c")?.conditions[0]?.message;
    expect(why).toMatch(/^This block is waiting for/);
    expect(badge(card(el, "Merge"))).toBe("waiting");
    expect(find(card(el, "Merge"), "footer").textContent).toBe(why);

    // Where show_conditions leaves out the errors, the footer shows no pill
    // for the error, and no banner.
    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["warning"] } } });
    });
    expect(line(card(el, "Head"))).toEqual([]);
    expect(banners(card(el, "Head"))).toEqual([]);
  });

  test("leaves no room for an output until its block first reads ready, so that a block that fails at once shows its footer alone, as one that waits does", async () => {
    let fails = true;
    const { el, core, settled } = await drawn({
      evaluate: (request) =>
        request.block === "b" && fails
          ? { status: "error", conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "expr", message: "No rows" }] }
          : undefined,
    });

    const output = (name: string): HTMLElement => find(card(el, name), '[aria-label="Output"]');
    expect(badge(card(el, "Head"))).toBe("failed");
    expect(output("Head").className).toContain("bc:h-0");
    expect(output("Head").parentElement?.className).not.toContain("bc:h-96");
    expect(line(card(el, "Head"))).toEqual(["error"]);
    expect(banners(card(el, "Head"))).toEqual([["error", "No rows"]]);
    expect(badge(card(el, "Merge"))).toBe("waiting");
    expect(output("Merge").className).toContain("bc:h-0");

    // The user changes the block's state, and R evaluates it.
    fails = false;
    await step(() => {
      core.store.apply({ blocks: { mod: { b: { state: { n: 3, direction: "head" } } } } });
    });
    await settled();

    expect(badge(card(el, "Head"))).toBe("ready");
    expect(output("Head").className).toContain("bc:flex-1");
    expect(output("Head").parentElement?.className).toContain("bc:h-96");
    expect(badge(card(el, "Merge"))).toBe("ready");
    expect(output("Merge").className).toContain("bc:flex-1");
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

  test("sets the board options that the board holds through their UIs, by category", async () => {
    const { el, board, settled } = await drawn();

    await click(button(el, "Options"));
    await settled();
    // The board holds n_rows, which the table renderer names, and
    // show_conditions and dark_mode, which core's basic frontend requires.
    expect(Object.keys(board().options).sort()).toEqual(["dark_mode", "n_rows", "show_conditions"]);
    expect(texts(dialog(el), "h3")).toEqual(["Table options", "Board options", "Theme options"]);
    expect(dialog(el).textContent).toContain("Preview rows");
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

  test("saves the board through core's own storage plugin, as a download of the store's text named by the board's id and the time", async () => {
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

  test("opens the file that the user picks through core's own storage plugin, drawn as the page draws it, and says why the store refused one", async () => {
    const page = await drawn();
    const text = savedAs(page.core, renamed("Opened"));

    // Where the user picks no file, no board opens.
    picking(undefined);
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Iris review");

    vi.restoreAllMocks();
    picking(new File([text], "board.json", { type: "application/json" }));
    await click(button(page.el, "Open"));
    await page.settled();
    // Core starts again for the board, with a CoreApi of its own.
    expect(page.board().name).toBe("Opened");
    expect(find(page.el, "h1").textContent).toBe("Opened");
    expect(page.el.querySelectorAll("article")).toHaveLength(6);
    expect(document.querySelectorAll('input[type="file"]')).toHaveLength(0);

    vi.restoreAllMocks();
    picking(new File(["{}"], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Opened");
    expect(notices(page.el)).toEqual([["error", expect.stringContaining("The store refused to open the board")]]);
  });

  test("saves and opens through the board's storage plugin, and says why it failed", async () => {
    const saves: string[] = [];
    let picked: string | null = null;
    let down = false;
    const session: PluginClass = {
      id: "blockr.test-session_plugin",
      storage: {
        save(board) {
          if (down) return Promise.reject(new Error("Connect is down."));
          saves.push(board);
          return Promise.resolve();
        },
        open: () => (down ? Promise.reject(new Error("Connect is down.")) : Promise.resolve(picked)),
      },
    };
    const page = await drawn(withClasses([session], { session: plugin(session, "Session") }));
    // The plugin's class has no ui, so the page shows no section for it.
    expect(page.el.querySelector("aside")).toBeNull();

    await click(button(page.el, "Save"));
    expect(saves).toEqual([page.core.store.save()]);

    // Where the storage gives no board, none opens.
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Iris review");

    picked = savedAs(page.core, renamed("Opened"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Opened");

    down = true;
    await click(button(page.el, "Save"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(notices(page.el)).toEqual([
      ["error", "Connect is down."],
      ["error", "Connect is down."],
    ]);
  });
});

describe("a plugin", () => {
  const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: "blockr.test-notes" };
  // A class whose plugins run from its start alone, with no place on the
  // screen.
  const bridge: PluginClass = { id: "blockr.test-bridge_plugin", start: () => undefined };

  test("whose class has a ui shows beside the board in a section of its own, which the user collapses without core unmounting its UI", async () => {
    const ui = pluginUi();
    const page = await drawn({
      ...withClasses([notes, bridge], { todo: plugin(notes, "To do"), bridge: plugin(bridge, "Bridge"), later: plugin(notes, "Later") }),
      modules: { "blockr.test-notes": ui.exports },
    });
    const sections = (): HTMLDetailsElement[] => [...find(page.el, 'aside[aria-label="Plugins"]').querySelectorAll("details")];
    const shown = () => sections().map((section) => [section.querySelector("summary")?.textContent, section.open]);

    // Each section starts open, in the order of the board's plugins, and core
    // mounts the UI of the plugin's class in it. The board stays beside them.
    expect(shown()).toEqual([
      ["To do", true],
      ["Later", true],
    ]);
    expect(ui.renders.map((render) => render.plugin)).toEqual(["todo", "later"]);
    expect(sections()[0]?.contains(ui.renders[0]?.el ?? null)).toBe(true);
    expect(ui.renders[0]?.el.textContent).toBe("To do");
    expect(page.el.querySelectorAll("article")).toHaveLength(6);
    expect(buttons(toolbar(page.el)).at(-1)).toBe("Open");
    // They come before the board, which a narrow page shows below them.
    expect(page.el.querySelector("aside, article")?.tagName).toBe("ASIDE");

    // A collapsed section stays collapsed as the board changes, and its
    // plugin's UI stays mounted.
    await click(find(page.el, "summary"));
    await act(() => page.core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    await page.settled();
    expect(page.board().name).toBe("Sepals");
    expect(shown()).toEqual([
      ["To do", false],
      ["Later", true],
    ]);
    expect(ui.renders).toHaveLength(2);
    expect(ui.cleanups()).toBe(0);

    await click(find(page.el, "summary"));
    expect(shown()).toEqual([
      ["To do", true],
      ["Later", true],
    ]);
    expect(ui.renders).toHaveLength(2);
  });
});

describe("which block has focus", () => {
  test("is the block whose card the user last clicked into or moved the keyboard's focus into", async () => {
    const { el, core } = await drawn();
    expect(core.focus.get()).toBeNull();

    await step(() => {
      find(card(el, "Head"), '[aria-label="Output"]').dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    });
    expect(core.focus.get()).toBe("b");

    await step(() => {
      find(card(el, "Merge"), 'button[aria-label="Actions of Merge"]').focus();
    });
    expect(core.focus.get()).toBe("c");

    // Hovering over a card gives it no focus.
    await step(() => {
      card(el, "Iris").dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
    });
    expect(core.focus.get()).toBe("c");
  });
});

// R's reply to hello whose opening board the store opens without what the
// app cannot rebuild: the package blockr.dplyr, which R did not load, a block
// of its filter block class, a plugin whose class no package registers, and a
// value of a board option that no package registers.
function droppingHello(): HelloReply {
  const json = HELLO.board;
  const filter = { registry_id: "blockr.dplyr-filter_block", version: 1, state: {}, name: "Filter", locked: null };
  return withPlugins(
    { gone: { class: "blockr.gone-gone_plugin", name: "Gone", state: {} } },
    {
      ...HELLO,
      board: {
        ...json,
        packages: { ...json.packages, "blockr.dplyr": "0.3.0" },
        board: {
          ...json.board,
          blocks: { ...json.board.blocks, x: filter },
          options: { ...json.board.options, zoom: { value: 2, version: 1 } },
        },
      },
    },
  );
}

describe("what the user learns", () => {
  test("is what the store dropped as the board opened, the opening board included", async () => {
    const page = await drawn({ hello: droppingHello() });

    expect(notices(page.el)).toEqual([
      [
        "warning",
        [
          "The board opened without:",
          "- package blockr.dplyr 0.3.0",
          '- block "x" (blockr.dplyr-filter_block)',
          '- plugin "gone" (blockr.gone-gone_plugin)',
          '- board option "zoom"',
        ].join("\n"),
      ],
    ]);
    expect(find(page.el, '[data-severity="warning"]').getAttribute("role")).toBe("alert");

    // A board that opens later starts core and the frontend again, which shows
    // only the note of what that board dropped.
    picking(new File([savedAs(page.core, (json) => ({ ...json, packages: { ...json.packages, "blockr.io": "0.1.0" } }))], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(notices(page.el)).toEqual([["warning", "The board opened without:\n- package blockr.io 0.1.0"]]);

    vi.restoreAllMocks();
    picking(new File([savedAs(page.core, renamed("Clean"))], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Clean");
    expect(notices(page.el)).toEqual([]);
  });

  test("is each note that a plugin sends, in the colour of its severity, until the user dismisses it", async () => {
    const { el, core } = await drawn();

    await step(() => {
      core.notifications.notify({ message: "Saved to Connect.", severity: "message" });
      core.notifications.notify({ message: "The token expires soon.", severity: "warning" });
      core.notifications.notify({ message: "Not shared.", severity: "error" });
    });
    expect(notices(el)).toEqual([
      ["message", "Saved to Connect."],
      ["warning", "The token expires soon."],
      ["error", "Not shared."],
    ]);
    expect(texts(el, 'ul[aria-label="Notices"] > li[role="alert"] span')).toEqual(["The token expires soon.", "Not shared."]);
    expect(find(el, '[data-severity="message"]').getAttribute("role")).toBe("status");

    await click(button(find(el, '[data-severity="message"]'), "Dismiss"));
    expect(notices(el)).toEqual([
      ["warning", "The token expires soon."],
      ["error", "Not shared."],
    ]);
  });

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
    expect(notices(el)).toEqual([["error", expect.stringContaining("The store rejected the delta")]]);
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

// R's reply to hello with the locks given, whose opening board gives the head
// block b a lock of false, so that the board lock leaves b open, while every
// other block's lock is null, which follows the board lock.
function lockedHello(lock: HelloReply["lock"]): HelloReply {
  const { blocks } = HELLO.board.board;
  const b = blocks.b;
  if (b === undefined) throw new Error("The fixture's board has no block b.");
  return {
    ...HELLO,
    lock,
    board: { ...HELLO.board, board: { ...HELLO.board.board, blocks: { ...blocks, b: { ...b, locked: false } } } },
  };
}

describe("a locked board", () => {
  const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: "blockr.test-notes" };

  test("draws no control for what the board lock fixes, and the UI only of the blocks it leaves open", async () => {
    const { el } = await drawn({
      ...withClasses([notes], { todo: plugin(notes, "To do") }, lockedHello({ board: true, load: true })),
      modules: { "blockr.test-notes": pluginUi().exports },
    });

    // The board options, the history, saving and the plugins stay, since no
    // lock fixes them, and the load lock takes opening another board.
    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save"]);
    expect(find(el, 'aside[aria-label="Plugins"] summary').textContent).toBe("To do");
    expect(el.querySelector('button[aria-label="Rename board"]')).toBeNull();
    // No card has a menu.
    expect(el.querySelectorAll("article button")).toHaveLength(0);

    expect(card(el, "Head").textContent).toContain("UI of b");
    expect(card(el, "Iris").textContent).not.toContain("UI of a");
    expect(card(el, "Iris").textContent).toContain("Output of a");
  });

  test("under the board lock alone, keeps opening another board, and sets the board options", async () => {
    const { el, board, settled } = await drawn({ hello: lockedHello({ board: true, load: false }) });
    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save", "Open"]);

    await click(button(el, "Options"));
    await settled();
    await step(() => {
      pick(find(dialog(el), 'section[aria-label="Theme options"] select', HTMLSelectElement), "dark");
    });
    expect(board().options.dark_mode).toBe("dark");
  });

  test("under the load lock alone, draws every control but the one for opening a board", async () => {
    const { el } = await drawn({ hello: lockedHello({ board: false, load: true }) });

    expect(buttons(toolbar(el))).toEqual(["Add block", "Links", "Stacks", "Options", "Undo", "Redo", "Save"]);
    expect(el.querySelector('button[aria-label="Rename board"]')).not.toBeNull();
    expect(card(el, "Iris").querySelector("button")).not.toBeNull();
    expect(card(el, "Iris").textContent).toContain("UI of a");
  });
});

describe("the cleanup that render() returns", () => {
  test("ends what the frontend drew, demanded and bound", async () => {
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
