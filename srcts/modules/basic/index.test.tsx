// Core's basic frontend on the CoreApi for tests: the board of the fixture
// hello, drawn by the frontend as the page draws it, from the module of the
// class of core's basic frontend, with R played from the protocol's fixtures,
// every block UI and renderer a stub that names what it draws, the ui of core's
// Edit board plugin a stub that names its plugin, the plugins a test needs, and
// the user acting on the page.

import { act } from "react";
import { afterEach, describe, expect, test, vi } from "vitest";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubCoreOptions } from "../../stubs/index.ts";
import type { HelloReply } from "../../protocol/index.ts";
import type { PluginClass, StorageControl } from "../../store/index.ts";
import { labelled, listOf, optionOf, optionsOf } from "../../testing/combobox.ts";
import { find, pick, picking, press, texts, type } from "../../testing/inputs.ts";
import { classModule, listedModules, pluginUi } from "../../testing/plugins.ts";
import { step } from "../../testing/react.ts";
import { reports } from "../../testing/reports.ts";
import { withoutPlugins, withPlugins } from "../../testing/store.ts";
import type { GivenPlugin } from "../../testing/store.ts";
import type { Board, JsonBoard, PluginClassId } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
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
// The basic frontend is core's own, which the CoreApi for tests serves, while
// core's Edit board plugin draws with a stub of its ui.
const MODULES: Readonly<Record<string, unknown>> = {
  "blockr.core-edit_board_plugin": classModule({ ui: pluginUi().ui }),
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

// What stops each page that a test drew, which runs as the test ends, so that
// no page of an earlier test hears the keys that a later test presses.
const stops: (() => void)[] = [];

// The board of the fixture hello, or of the reply given, drawn by the basic
// frontend once R has evaluated what it demands.
async function drawn(options: StubCoreOptions = {}): Promise<Drawn> {
  const el = document.createElement("div");
  document.body.append(el);
  const stubbed = await act(() => stubCore({ el, ...options, modules: { ...MODULES, ...options.modules } }));
  stops.push(stubbed.stop);
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

// The line of a card's footer, a ready block's summary, or null where the card
// draws no footer.
const summary = (card: HTMLElement): string | null => card.querySelector("footer")?.textContent ?? null;

// The text of each pill below a card's output.
const pills = (card: HTMLElement): string[] => texts(card, '[aria-label="Status"] button');

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

// Picks the option of a searchable dropdown, through its search.
async function chosen(el: ParentNode, label: string, search: string, option: string): Promise<void> {
  await step(() => {
    labelled(el, label).click();
  });
  await step(() => {
    type(labelled(el, label), search);
  });
  await step(() => {
    optionOf(labelled(el, label), option).click();
  });
}

// The blocks that the board holds beside those of the fixture's board.
function added(board: Board): [string, Board["blocks"][string]][] {
  return Object.entries(board.blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
}

function buttons(el: ParentNode): string[] {
  return [...el.querySelectorAll("button")].map((b) => b.getAttribute("aria-label") ?? b.textContent.trim());
}

// Clicks the element, and lets what the click set off run, such as the
// command that a form runs as the user submits it.
async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
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

// The toasts that the page shows, inside the dialog that is open or not.
const toastsIn = (el: ParentNode): HTMLElement[] => [
  ...el.querySelectorAll<HTMLElement>('section[aria-label="Notifications"] > [data-severity]'),
];

// The severity, the first line and the message of each toast that the page
// shows, in order.
function toasts(el: ParentNode): [string | null, string, string][] {
  return toastsIn(el).map((toast) => {
    const [title, message] = [...toast.querySelectorAll("p")].map((p) => p.textContent);
    return [toast.getAttribute("data-severity"), title ?? "", message ?? ""];
  });
}

// The toast whose message is the one given.
function toast(el: ParentNode, message: string): HTMLElement {
  const found = toastsIn(el).find((t) => t.querySelectorAll("p")[1]?.textContent === message);
  if (found === undefined) throw new Error(`Found no toast "${message}".`);
  return found;
}

// The class of core's basic frontend, which raises its own notifications.
const BASIC = "blockr.core-basic_frontend_plugin";

// The class of core's own storage plugin, which the app of the fixture hello
// runs.
const FILE_STORAGE = "blockr.core-file_storage_plugin";

// The app's plugin of the class given, with the name given, as the reply names
// it.
const plugin = (cls: PluginClass, name: string): Readonly<Record<PluginClassId, GivenPlugin>> => ({ [cls.id]: { name } });

// What the test gives core for an app that runs the plugins given, of the
// classes given, each the default export of its class's module.
function withClasses(
  classes: readonly PluginClass[],
  plugins: Readonly<Record<PluginClassId, GivenPlugin>>,
  hello = HELLO,
): StubCoreOptions {
  return { hello: withPlugins(plugins, hello), modules: listedModules(classes) };
}

// The text of the board that the page saves, as `edit` changes it.
function savedAs(core: CoreApi, edit: (json: JsonBoard) => JsonBoard): string {
  return JSON.stringify(edit(JSON.parse(core.store.save()) as JsonBoard));
}

// Renames the board that a JSON board holds.
const renamed = (name: string) => (json: JsonBoard): JsonBoard => ({ ...json, board: { ...json.board, name } });

afterEach(() => {
  act(() => {
    for (const stop of stops.splice(0)) stop();
  });
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
  test("shows its block's status in its header, its summary in its footer, and below its output a pill for each severity that show_conditions names, which opens its banners", async () => {
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
    // handle that resizes the box, and the pills sit below the box.
    const footer = find(head, "footer");
    const box = footer.parentElement;
    expect(box?.firstElementChild).toBe(find(head, '[aria-label="Output"]'));
    expect(box?.lastElementChild).toBe(footer);
    expect(box?.nextElementSibling).toBe(find(head, '[aria-label="Status"]'));
    expect(summary(head)).toBe("<data.frame> [6 × 5]");
    // The board holds show_conditions at warnings and errors, so no pill
    // shows for the message.
    expect(pills(head)).toEqual(["1 warning"]);
    expect(banners(head)).toEqual([]);

    await click(button(head, "1 warning"));
    expect(banners(card(el, "Head"))).toEqual([["warning", "Few rows"]]);

    // The banners of a severity go with its pill, as show_conditions leaves
    // the severity out.
    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["message"] } } });
    });
    expect(pills(card(el, "Head"))).toEqual(["1 message"]);
    expect(banners(card(el, "Head"))).toEqual([]);

    await click(button(card(el, "Head"), "1 message"));
    expect(banners(card(el, "Head"))).toEqual([["message", "Took the head"]]);
  });

  test("counts why an invalid block does not run among its messages, whatever show_conditions names", async () => {
    const { el, core } = await drawn({
      evaluate: (request) =>
        request.block === "c"
          ? { status: "invalid", reason: "Choose at least one column to join by." }
          : undefined,
    });

    const merge = card(el, "Merge");
    expect(badge(merge)).toBe("invalid");
    expect(summary(merge)).toBeNull();
    // The board holds show_conditions at warnings and errors, and the pill of
    // the messages counts the reason all the same.
    expect(pills(merge)).toEqual(["1 message"]);
    expect(banners(merge)).toEqual([]);

    await click(button(merge, "1 message"));
    expect(banners(card(el, "Merge"))).toEqual([["message", "Choose at least one column to join by."]]);

    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["error"] } } });
    });
    expect(pills(card(el, "Merge"))).toEqual(["1 message"]);
    expect(banners(card(el, "Merge"))).toEqual([["message", "Choose at least one column to join by."]]);
  });

  test("greys its output out while its block is not ready, but not its pills, which show a failed block's error and why a block waits", async () => {
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
    // is open from the start. The footer has no summary to show.
    expect(find(head, '[aria-label="Status"]').closest('[class*="bc:grayscale"]')).toBeNull();
    expect(summary(head)).toBe("");
    expect(pills(head)).toEqual(["error"]);
    expect(banners(head)).toEqual([["error", "No rows"]]);

    // The blocks downstream of a failed block wait on it, and the pills of
    // their messages say why, whatever show_conditions names.
    expect(core.scheduler.status("c")?.status).toBe("waiting");
    const why = core.scheduler.status("c")?.reason;
    expect(why).toMatch(/^This block is waiting for/);
    expect(badge(card(el, "Merge"))).toBe("waiting");
    expect(pills(card(el, "Merge"))).toEqual(["1 message"]);
    await click(button(card(el, "Merge"), "1 message"));
    expect(banners(card(el, "Merge"))).toEqual([["message", why]]);

    // Where show_conditions leaves out the errors, no pill shows for the
    // error, and no banner.
    await step(() => {
      core.store.apply({ options: { set: { show_conditions: ["warning"] } } });
    });
    expect(pills(card(el, "Head"))).toEqual([]);
    expect(banners(card(el, "Head"))).toEqual([]);
  });

  test("leaves no room for an output, and draws no footer, until its block first reads ready, so that a block that fails at once shows its pills alone, as one that waits does", async () => {
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
    expect(summary(card(el, "Head"))).toBeNull();
    expect(pills(card(el, "Head"))).toEqual(["error"]);
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
    expect(summary(card(el, "Head"))).not.toBeNull();
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

  test("adds a block after its block once the user submits the form, which takes its block's result, picking the input where the new block has several", async () => {
    const { el, board, settled } = await drawn();

    await menu(el, "Head", "Add block after");
    expect(find(dialog(el), "h2").textContent).toBe('Add block after "Head"');
    const classes = labelled(dialog(el), "Block class");
    expect(document.activeElement).toBe(classes);
    // A block added after another needs an input.
    await step(() => {
      classes.click();
    });
    expect(optionsOf(classes)).not.toContain("input: dataset block");
    await step(() => {
      optionOf(classes, "subset block").click();
    });
    expect(added(board())).toEqual([]);
    await click(button(dialog(el), "Add block"));
    await settled();

    const [subset] = added(board());
    expect(subset?.[1].registry_id).toBe("blockr.core-subset_block");
    expect(Object.values(board().links)).toContainEqual({ from: "b", to: subset?.[0], input: "data" });
    expect(el.querySelector("dialog[open]")).toBeNull();

    await menu(el, "Iris", "Add block after");
    await chosen(dialog(el), "Block class", "merge", "merge block");
    await step(() => {
      pick(find(dialog(el), "select", HTMLSelectElement), "y");
    });
    await step(() => {
      type(labelled(dialog(el), "ID"), "joined");
    });
    await click(button(dialog(el), "Add block"));
    expect(board().blocks.joined?.registry_id).toBe("blockr.core-merge_block");
    expect(Object.values(board().links)).toContainEqual({ from: "a", to: "joined", input: "y" });

    // A variadic block takes the link under the name that the user gives.
    await menu(el, "Iris", "Add block after");
    await chosen(dialog(el), "Block class", "rbind", "rbind block");
    await step(() => {
      type(labelled(dialog(el), "Input name of the new rbind block"), "first");
      type(labelled(dialog(el), "ID"), "bound");
    });
    await click(button(dialog(el), "Add block"));
    expect(Object.values(board().links)).toContainEqual({ from: "a", to: "bound", input: "first" });
  });

  test("adds a block before its block, into a free input, and offers it only where an input is free", async () => {
    const { el, board } = await drawn();

    // Every input of the merge block takes a link already.
    await click(find(card(el, "Merge"), 'button[aria-label="Actions of Merge"]'));
    expect(button(card(el, "Merge"), "Add block before").disabled).toBe(true);
    await step(() => {
      press(document.body, "Escape");
    });

    // The bind block is variadic, so it takes another link, under a name that
    // the user may give, which none of its links may give already.
    await menu(el, "Bind", "Add block before");
    await chosen(dialog(el), "Block class", "dataset", "dataset block");
    await click(button(dialog(el), "Add block"));
    const [first] = added(board());
    expect(first?.[1].registry_id).toBe("blockr.core-dataset_block");
    expect(Object.values(board().links)).toContainEqual({ from: first?.[0], to: "e", input: "" });

    await menu(el, "Bind", "Add block before");
    await chosen(dialog(el), "Block class", "dataset", "dataset block");
    await step(() => {
      type(labelled(dialog(el), 'Input name of "Bind"'), "extra");
      type(labelled(dialog(el), "ID"), "more");
    });
    await click(button(dialog(el), "Add block"));
    expect(Object.values(board().links)).toContainEqual({ from: "more", to: "e", input: "extra" });
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
    await chosen(dialog(el), "Block class", "dataset", "dataset block");
    await click(button(dialog(el), "Add block"));

    const [first] = added(board());
    expect(Object.values(board().links)).toContainEqual({ from: first?.[0], to: "c", input: "y" });
  });

  test("says why it adds none where another edit feeds the block's last free input meanwhile", async () => {
    const { el, core, board } = await drawn();
    await act(() => core.commands.run("blockr.core-remove_link", { id: "bc" }));

    await menu(el, "Merge", "Add block before");
    await chosen(dialog(el), "Block class", "dataset", "dataset block");
    await act(() => core.commands.run("blockr.core-add_link", { from: "b", to: "c", input: "y" }));

    expect(dialog(el).textContent).toContain('Every input of "Merge" takes a link already.');
    expect(button(dialog(el), "Add block").disabled).toBe(true);
    expect(added(board())).toEqual([]);
  });
});

describe("what floats over the board", () => {
  // The backgrounds that an element draws in the page's theme and under the
  // dark theme, those of its hover and of a dialog's backdrop aside.
  const backgrounds = (el: Element): string[] => el.className.split(" ").filter((name) => /^bc:(dark:)?bg-/.test(name));

  test("is drawn on the background that core's theme gives it: a card's menu, the dialogs and the list of a searchable dropdown", async () => {
    const { el, settled } = await drawn();

    await click(find(card(el, "Head"), 'button[aria-label="Actions of Head"]'));
    expect(backgrounds(find(card(el, "Head"), '[role="menu"]'))).toEqual(["bc:bg-bg-raised"]);

    await click(button(card(el, "Head"), "Add block after"));
    expect(backgrounds(dialog(el))).toEqual(["bc:bg-bg-raised"]);
    const classes = labelled(dialog(el), "Block class");
    await step(() => {
      classes.click();
    });
    const list = listOf(classes);
    expect(list).not.toBeNull();
    expect(list === null ? [] : backgrounds(list)).toEqual(["bc:bg-bg-raised"]);
    await click(button(dialog(el), "Close"));

    await click(button(el, "Options"));
    await settled();
    expect(find(dialog(el), "h2").textContent).toBe("Board options");
    expect(backgrounds(dialog(el))).toEqual(["bc:bg-bg-raised"]);
  });
});

describe("the toolbar", () => {
  test("draws no control for adding a block or for links and stacks, which the Edit board plugin edits in a section of its own", async () => {
    const { el } = await drawn();

    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save", "Open"]);
    expect(texts(find(el, 'aside[aria-label="Plugins"]'), "summary")).toEqual(["Edit board"]);
    expect(find(el, 'aside[aria-label="Plugins"] details').textContent).toContain("Edit board");
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
    expect(document.documentElement.getAttribute("data-bc-theme")).toBe("dark");
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
    expect(toasts(page.el)).toEqual([["error", 'Plugin "File storage"', expect.stringContaining("The store refused to open the board")]]);
  });

  test("draws no storage control where the app runs no storage plugin", async () => {
    const page = await drawn({ hello: withoutPlugins(HELLO, FILE_STORAGE) });

    expect(page.core.plugins.storage()).toBeUndefined();
    expect(buttons(toolbar(page.el))).toEqual(["Options", "Undo", "Redo"]);
  });
});

// The class of a storage plugin whose controls the test sets, which calls back
// through subscribe() whenever the test changes them, with what the test gives
// core for an app that runs it in place of core's own storage plugin.
function storagePlugin(controls: () => readonly StorageControl[]) {
  let current = controls;
  const listeners = new Set<() => void>();
  const cls: PluginClass = {
    id: "blockr.test-session_plugin",
    storage: {
      controls: () => current(),
      subscribe(listener) {
        listeners.add(listener);
        return () => {
          listeners.delete(listener);
        };
      },
    },
  };
  return {
    options: withClasses([cls], plugin(cls, "Session"), withoutPlugins(HELLO, FILE_STORAGE)),
    set(next: () => readonly StorageControl[]): void {
      current = next;
      for (const listener of [...listeners]) listener();
    },
    listening: () => listeners.size,
  };
}

// The text of each part of the toolbar, in order.
const parts = (el: HTMLElement): string[] => [...toolbar(el).children].map((part) => part.textContent.trim());

// The runs of storage buttons, by their ids, and what records the run of one.
function running() {
  const runs: string[] = [];
  return {
    runs,
    ran: (id: string) => (): Promise<void> => {
      runs.push(id);
      return Promise.resolve();
    },
  };
}

describe("the storage controls", () => {
  test("show at the end of the toolbar in the plugin's order, each kind in its own look, those that ask for the end of the bar last", async () => {
    const { ran } = running();
    const storage = storagePlugin(() => [
      { kind: "status", id: "user", text: "NS", tooltip: "Signed in as Nina", placement: "end" },
      { kind: "button", id: "save", label: "Save", tooltip: "Save to Connect", shortcut: "Mod+S", run: ran("save") },
      { kind: "menu", id: "versions", label: "Versions", items: [] },
      { kind: "status", id: "saved", text: "Saved" },
      { kind: "button", id: "share", label: "Share", disabled: true, run: ran("share") },
      // A hint that the frontend does not know draws as if it were not there.
      { kind: "button", id: "help", label: "Help", placement: "start" as "end", run: ran("help") },
    ]);
    const { el } = await drawn(storage.options);

    expect(parts(el)).toEqual(["Options", "Undo", "Redo", "Save", "Versions▾", "Saved", "Share", "Help", "NS"]);
    // A button looks like the toolbar's own, and is greyed out while it is
    // disabled.
    const save = button(toolbar(el), "Save");
    expect(save.className).toBe(button(toolbar(el), "Options").className);
    expect(save.title).toBe("Save to Connect");
    expect(save.getAttribute("aria-keyshortcuts")).toBe("Control+S Meta+S");
    expect(button(toolbar(el), "Share").disabled).toBe(true);
    // A menu is a button that opens it, and a status is text.
    expect(find(toolbar(el), 'button[aria-haspopup="menu"]').getAttribute("aria-expanded")).toBe("false");
    const statuses = [...toolbar(el).querySelectorAll('[role="status"]')];
    expect(statuses.map((status) => [status.tagName, status.textContent, status.getAttribute("title")])).toEqual([
      ["SPAN", "Saved", null],
      ["SPAN", "NS", "Signed in as Nina"],
    ]);
  });

  test("open a menu of what it holds, a menu inside it as a group under its label, and close it as one of its buttons runs", async () => {
    const { runs, ran } = running();
    const storage = storagePlugin(() => [
      {
        kind: "menu",
        id: "versions",
        label: "Versions",
        items: [
          { kind: "button", id: "restore", label: "Restore", run: ran("restore") },
          // The frontend ignores hints inside a menu.
          { kind: "status", id: "latest", text: "Version 3", tooltip: "Saved at noon", placement: "end" },
          {
            kind: "menu",
            id: "older",
            label: "Older",
            items: [{ kind: "button", id: "v1", label: "Version 1", tooltip: "Saved in May", run: ran("v1") }],
          },
        ],
      },
    ]);
    const { el } = await drawn(storage.options);
    const opener = find(toolbar(el), 'button[aria-haspopup="menu"]');
    const list = () => el.querySelector<HTMLElement>('ul[role="menu"][aria-label="Versions"]');

    await click(opener);
    expect(opener.getAttribute("aria-expanded")).toBe("true");
    const shown = list();
    if (shown === null) throw new Error("The menu did not open.");
    expect(texts(shown, ":scope > li")).toEqual(["Restore", "Version 3", "OlderVersion 1"]);
    expect(find(shown, ":scope > li:nth-child(2)").title).toBe("Saved at noon");
    const older = find(shown, '[role="group"][aria-label="Older"]');
    expect(texts(older, 'button[role="menuitem"]')).toEqual(["Version 1"]);
    expect(button(older, "Version 1").title).toBe("Saved in May");

    await click(button(older, "Version 1"));
    expect(runs).toEqual(["v1"]);
    expect(list()).toBeNull();

    // It closes on Escape too.
    await click(opener);
    await step(() => {
      press(document.body, "Escape");
    });
    expect(list()).toBeNull();
    expect(runs).toEqual(["v1"]);
  });

  test("show again whenever the plugin's subscribe() calls back, and as the board changes, until the frontend ends", async () => {
    const storage = storagePlugin(() => [{ kind: "status", id: "saved", text: "Unsaved" }]);
    const page = await drawn(storage.options);
    expect(parts(page.el).at(-1)).toBe("Unsaved");
    expect(storage.listening()).toBe(1);

    await step(() => {
      storage.set(() => [
        { kind: "status", id: "saved", text: "Saved" },
        { kind: "button", id: "share", label: "Share", run: () => Promise.resolve() },
      ]);
    });
    expect(parts(page.el).slice(-2)).toEqual(["Saved", "Share"]);

    // The controls read the board, as a status may show the board's name.
    storage.set(() => [{ kind: "status", id: "saved", text: `Unsaved ${page.board().name}` }]);
    await act(() => page.core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    expect(parts(page.el).at(-1)).toBe("Unsaved Sepals");

    page.stop();
    expect(storage.listening()).toBe(0);
  });

  test("bind the shortcut of each button, those in menus too, but not of a disabled one, nor in a text field, and leave undo its keys", async () => {
    const { runs, ran } = running();
    const storage = storagePlugin(() => [
      { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: ran("save") },
      {
        kind: "menu",
        id: "more",
        label: "More",
        items: [{ kind: "button", id: "publish", label: "Publish", shortcut: "Mod+Shift+P", run: ran("publish") }],
      },
      { kind: "button", id: "lock", label: "Lock", shortcut: "Mod+L", disabled: true, run: ran("lock") },
      { kind: "button", id: "back", label: "Back", shortcut: "Mod+Z", run: ran("back") },
      { kind: "button", id: "odd", label: "Odd", shortcut: "Hyper+O", run: ran("odd") },
    ]);
    const { el, core, board } = await drawn(storage.options);
    await act(() => core.commands.run("blockr.core-rename_board", { name: "Sepals" }));

    const save = new KeyboardEvent("keydown", { key: "s", ctrlKey: true, bubbles: true, cancelable: true });
    await step(() => {
      document.body.dispatchEvent(save);
      press(document.body, "s", { metaKey: true });
      press(document.body, "P", { metaKey: true, shiftKey: true });
      press(document.body, "p", { metaKey: true });
      press(document.body, "l", { ctrlKey: true });
      press(document.body, "o", { ctrlKey: true });
      // Holding the keys down runs the button once.
      press(document.body, "s", { ctrlKey: true, repeat: true });
    });
    // The page saves rather than the browser.
    expect(save.defaultPrevented).toBe(true);
    expect(runs).toEqual(["save", "save", "publish"]);

    // Undo comes first.
    await step(() => {
      press(document.body, "z", { ctrlKey: true });
    });
    expect(board().name).toBe("Iris review");
    expect(runs).toEqual(["save", "save", "publish"]);

    const field = document.createElement("input");
    el.append(field);
    await step(() => {
      press(field, "s", { ctrlKey: true });
    });
    expect(runs).toEqual(["save", "save", "publish"]);
  });

  test("tell the user why a button failed, run by a click or by its shortcut, through an error of the code storage_control_failed about the storage plugin", async () => {
    const storage = storagePlugin(() => [
      { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: () => Promise.reject(new Error("Connect is down.")) },
    ]);
    const { el, core } = await drawn(storage.options);

    await click(button(toolbar(el), "Save"));
    await step(() => {
      press(document.body, "s", { ctrlKey: true });
    });
    await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
    // The frontend raised them, so the toasts name no package beside the
    // storage plugin, which may be of another.
    expect(toasts(el)).toEqual([
      ["error", 'Plugin "Session"', "Connect is down."],
      ["error", 'Plugin "Session"', "Connect is down."],
    ]);
    expect(core.notifications.get().map(({ source, about }) => [source, about])).toEqual(
      Array(2).fill([
        { kind: "module", object: { plugin: BASIC }, code: "storage_control_failed", package: "blockr.core" },
        { plugin: "blockr.test-session_plugin" },
      ]),
    );
  });

  test("draw the first of the controls of one list that share an id, and a warning in their place where controls() fails, of which the page hears once", async () => {
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { runs, ran } = running();
    const storage = storagePlugin(() => [
      { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: ran("save") },
      { kind: "button", id: "save", label: "Save again", shortcut: "Mod+E", run: ran("again") },
      { kind: "menu", id: "more", label: "More", items: [{ kind: "status", id: "a", text: "A" }, { kind: "status", id: "a", text: "B" }] },
    ]);
    const reported = reports();
    let errors: unknown[];
    try {
      const page = await drawn(storage.options);
      expect(parts(page.el)).toEqual(["Options", "Undo", "Redo", "Save", "More▾"]);
      await click(find(toolbar(page.el), 'button[aria-haspopup="menu"]'));
      expect(texts(find(page.el, 'ul[role="menu"]'), ":scope > li")).toEqual(["A"]);
      await step(() => {
        press(document.body, "e", { ctrlKey: true });
      });
      expect(runs).toEqual([]);

      await step(() => {
        storage.set(() => {
          throw new Error("No session.");
        });
      });
      const warning = find(toolbar(page.el), '[role="alert"]');
      expect(warning.textContent).toBe("Storage failed");
      expect(warning.title).toBe("The storage controls failed: No session.");
      expect(parts(page.el)).toEqual(["Options", "Undo", "Redo", "Storage failed"]);
      // It draws again with the same failure, which the page has heard of.
      await act(() => page.core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
      await step(() => {
        press(document.body, "s", { ctrlKey: true });
      });
      expect(runs).toEqual([]);
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    expect(errors.filter((error) => error instanceof Error).map((error) => error.message)).toEqual([
      'Storage controls of one list share the ids "save" and "a", so only the first of each draws.',
      "The storage controls failed: No session.",
    ]);
  });
});

describe("a plugin", () => {
  // A class whose plugin runs from its start alone, with no place on the
  // screen.
  const bridge: PluginClass = { id: "blockr.test-bridge_plugin", start: () => undefined };

  test("whose class has a ui shows beside the board in a section of its own, which the user collapses without core unmounting its UI", async () => {
    const ui = pluginUi();
    const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: ui.ui };
    // A class whose plugin draws with the ui of the notes' class.
    const later: PluginClass = { id: "blockr.test-later_plugin", ui: ui.ui };
    const page = await drawn(
      withClasses([notes, bridge, later], {
        ...plugin(notes, "To do"),
        ...plugin(bridge, "Bridge"),
        ...plugin(later, "Later"),
      }),
    );
    const sections = (): HTMLDetailsElement[] => [...find(page.el, 'aside[aria-label="Plugins"]').querySelectorAll("details")];
    const shown = () => sections().map((section) => [section.querySelector("summary")?.textContent, section.open]);
    const summary = (name: string): HTMLElement => {
      const found = sections().find((section) => section.querySelector("summary")?.textContent === name);
      return find(found ?? document.createElement("details"), "summary");
    };

    // Each section starts open, in the order of the app's plugins, titled by
    // the name of the plugin's class, and core mounts the UI of the class in
    // it. Of core's own plugins, the Edit board plugin alone has a UI, and so
    // a section. The board stays beside them.
    expect(shown()).toEqual([
      ["Edit board", true],
      ["To do", true],
      ["Later", true],
    ]);
    expect(ui.renders.map((render) => render.plugin)).toEqual([notes.id, later.id]);
    expect(sections()[1]?.contains(ui.renders[0]?.el ?? null)).toBe(true);
    expect(ui.renders[0]?.el.textContent).toBe("To do");
    expect(page.el.querySelectorAll("article")).toHaveLength(6);
    expect(buttons(toolbar(page.el)).at(-1)).toBe("Open");
    // They come before the board, which a narrow page shows below them.
    expect(page.el.querySelector("aside, article")?.tagName).toBe("ASIDE");

    // A collapsed section stays collapsed as the board changes, and its
    // plugin's UI stays mounted.
    await click(summary("To do"));
    await act(() => page.core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    await page.settled();
    expect(page.board().name).toBe("Sepals");
    expect(shown()).toEqual([
      ["Edit board", true],
      ["To do", false],
      ["Later", true],
    ]);
    expect(ui.renders).toHaveLength(2);
    expect(ui.cleanups()).toBe(0);

    await click(summary("To do"));
    expect(shown()).toEqual([
      ["Edit board", true],
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
// of its filter block class, the state of a plugin that the app does not run,
// and a value of a board option that no package registers.
function droppingHello(): HelloReply {
  const json = HELLO.board;
  const filter = { registry_id: "blockr.dplyr-filter_block", version: 1, state: {}, name: "Filter", locked: null };
  return {
    ...HELLO,
    board: {
      ...json,
      packages: { ...json.packages, "blockr.dplyr": "0.3.0" },
      board: {
        ...json.board,
        blocks: { ...json.board.blocks, x: filter },
        versions: { ...json.board.versions, "blockr.gone-gone_plugin": 1 },
        plugins: { ...json.board.plugins, "blockr.gone-gone_plugin": {} },
        options: { ...json.board.options, zoom: { value: 2, version: 1 } },
      },
    },
  };
}

describe("what the user learns", () => {
  test("is what the store dropped as the board opened, the opening board included, in a toast about the board", async () => {
    const page = await drawn({ hello: droppingHello() });

    expect(toasts(page.el)).toEqual([
      [
        "warning",
        "Board",
        [
          "The board opened without:",
          "- package blockr.dplyr 0.3.0",
          '- block "x" (blockr.dplyr-filter_block)',
          '- plugin "blockr.gone-gone_plugin"',
          '- board option "zoom"',
        ].join("\n"),
      ],
    ]);
    expect(toastsIn(page.el)[0]?.getAttribute("role")).toBe("alert");

    // A board that opens later starts core and the frontend again, which shows
    // only the toast of what that board dropped.
    picking(new File([savedAs(page.core, (json) => ({ ...json, packages: { ...json.packages, "blockr.io": "0.1.0" } }))], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(toasts(page.el)).toEqual([["warning", "Board", "The board opened without:\n- package blockr.io 0.1.0"]]);

    vi.restoreAllMocks();
    picking(new File([savedAs(page.core, renamed("Clean"))], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Clean");
    expect(toasts(page.el)).toEqual([]);
  });

  test("is each notification of the board that concerns no block, whoever raised it, as a toast in the colour of its severity, until it is withdrawn or the user closes it, while a block's card shows those about the block", async () => {
    const { el, core } = await drawn({
      evaluate: (request) =>
        request.block === "b"
          ? { conditions: [{ severity: "warning", class: ["warning", "condition"], phase: "block_eval", message: "Few rows." }] }
          : undefined,
    });

    let shared: () => void = () => undefined;
    let expiring: () => void = () => undefined;
    await step(() => {
      core.notifications.notify(FILE_STORAGE, { severity: "message", code: "saved", message: "Saved to Connect." });
      expiring = core.notifications.notify(FILE_STORAGE, { severity: "warning", code: "token_expiring", message: "The token expires soon." });
      shared = core.notifications.notify(FILE_STORAGE, { severity: "error", code: "share_failed", message: "Not shared.", about: { block: "b" } });
      core.notifications.notify(FILE_STORAGE, { severity: "message", code: "shown", message: "Shown on the board.", about: null });
    });
    // The condition of block b and what the plugin raised about b show on b's
    // card, whoever raised them, and in no toast.
    expect(core.notifications.get().some((n) => n.message === "Few rows." && n.source.kind === "r")).toBe(true);
    expect(toasts(el)).toEqual([
      ["message", 'Plugin "File storage" · blockr.core', "Saved to Connect."],
      ["warning", 'Plugin "File storage" · blockr.core', "The token expires soon."],
      ["message", "Board · blockr.core", "Shown on the board."],
    ]);
    expect(toast(el, "Saved to Connect.").getAttribute("role")).toBe("status");
    expect(toast(el, "The token expires soon.").getAttribute("role")).toBe("alert");
    const head = card(el, "Head");
    expect(pills(head)).toEqual(["error", "1 warning"]);
    expect(banners(head)).toEqual([["error", "Not shared."]]);

    await click(find(toast(el, "Saved to Connect."), 'button[aria-label="Close"]'));
    expect(toasts(el).map(([, , message]) => message)).toEqual(["The token expires soon.", "Shown on the board."]);
    await step(() => {
      shared();
      expiring();
    });
    expect(pills(head)).toEqual(["1 warning"]);
    expect(toasts(el).map(([, , message]) => message)).toEqual(["Shown on the board."]);
    expect(core.notifications.get().some((n) => n.message === "Saved to Connect.")).toBe(true);
  });

  test("shows its toasts inside the dialog that is open, the rest of the page being inert, and in the page once the dialog closes", async () => {
    const { el, core, settled } = await drawn();
    await step(() => {
      core.notifications.notify(FILE_STORAGE, { severity: "error", code: "share_failed", message: "Not shared." });
    });
    expect(toastsIn(el).map((t) => t.closest("dialog"))).toEqual([null]);

    await click(button(el, "Options"));
    await settled();
    await step(() => {
      core.notifications.notify(FILE_STORAGE, { severity: "warning", code: "token_expiring", message: "The token expires soon." });
    });
    // The toasts come after what the dialog holds.
    expect(toasts(dialog(el)).map(([, , message]) => message)).toEqual(["Not shared.", "The token expires soon."]);
    expect(dialog(el).lastElementChild?.contains(toast(el, "Not shared."))).toBe(true);
    await click(find(toast(dialog(el), "Not shared."), 'button[aria-label="Close"]'));

    await click(button(dialog(el), "Close"));
    expect(el.querySelector("dialog[open]")).toBeNull();
    expect(toasts(el).map(([, , message]) => message)).toEqual(["The token expires soon."]);
    expect(toastsIn(el).map((t) => t.closest("dialog"))).toEqual([null]);
  });

  test("is why the store or the command registry refused an edit of the frontend's, an error of the code edit_refused about the frontend's plugin, whatever block the edit names", async () => {
    const { el, core } = await drawn();
    vi.spyOn(core.commands, "run").mockRejectedValueOnce(new Error("The store refused the edit."));
    await menu(el, "Head", "Remove");
    await act(() => core.commands.run("blockr.core-rename_board", { name: "Sepals" }));
    vi.spyOn(core.store, "undo").mockImplementation(() => {
      throw new Error("The store refused to undo the step.");
    });
    await click(button(el, "Undo"));
    await step(() => {
      press(document.body, "z", { ctrlKey: true });
    });

    const title = 'Plugin "Basic frontend" · blockr.core';
    expect(toasts(el)).toEqual([
      ["error", title, "The store refused the edit."],
      ["error", title, "The store refused to undo the step."],
      ["error", title, "The store refused to undo the step."],
    ]);
    expect(new Set(core.notifications.get().map(({ source, about }) => JSON.stringify([source, about])))).toEqual(
      new Set([JSON.stringify([{ kind: "module", object: { plugin: BASIC }, code: "edit_refused", package: "blockr.core" }, { plugin: BASIC }])]),
    );
    expect(pills(card(el, "Head"))).toEqual([]);
  });

  test("fades from a message's toast after five seconds, but not while the pointer rests on it or the keyboard's focus is in it", async () => {
    const { el, core } = await drawn();
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      await step(() => {
        core.notifications.notify(FILE_STORAGE, { severity: "message", code: "saved", message: "Saved." });
        core.notifications.notify(FILE_STORAGE, { severity: "error", code: "share_failed", message: "Not shared." });
      });
      const saved = toast(el, "Saved.");
      await step(() => {
        saved.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
        vi.advanceTimersByTime(60_000);
      });
      await step(() => {
        find(saved, 'button[aria-label="Close"]').focus();
        saved.dispatchEvent(new PointerEvent("pointerout", { bubbles: true }));
        vi.advanceTimersByTime(60_000);
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Saved.", "Not shared."]);

      await step(() => {
        find(saved, 'button[aria-label="Close"]').blur();
        vi.advanceTimersByTime(5000);
      });
      expect(toast(el, "Saved.").className).toContain("bc:opacity-0");
      await step(() => {
        vi.advanceTimersByTime(300);
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Not shared."]);
    } finally {
      vi.useRealTimers();
    }
  });

  test("fades from a message's toast that moves out of a dialog as it closes, though the pointer rested on it there", async () => {
    const { el, core, settled } = await drawn();
    await click(button(el, "Options"));
    await settled();
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      await step(() => {
        core.notifications.notify(FILE_STORAGE, { severity: "message", code: "saved", message: "Saved." });
      });
      await step(() => {
        toast(dialog(el), "Saved.").dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
        vi.advanceTimersByTime(60_000);
      });
      expect(toasts(dialog(el)).map(([, , message]) => message)).toEqual(["Saved."]);

      await step(() => {
        button(dialog(el), "Close").click();
      });
      expect(el.querySelector("dialog[open]")).toBeNull();
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Saved."]);
      await step(() => {
        vi.advanceTimersByTime(5300);
      });
      expect(toasts(el)).toEqual([]);
    } finally {
      vi.useRealTimers();
    }
  });

  test("that core raises to ask for the page to be reloaded offers to reload it, which none of a plugin's does, whatever its code", async () => {
    const { core } = await drawn();
    const message = "R now runs other versions of packages than this page loaded (blockr.core 0.2.0 to 0.2.1). Save the board, then reload the page.";
    const reloading: Notification = { id: "reload", severity: "warning", message, source: { kind: "core", code: "page_reload_needed" } };
    const posing: Notification = {
      id: "posing",
      severity: "error",
      message: "Reload, please.",
      source: { kind: "module", object: { plugin: FILE_STORAGE }, code: "page_reload_needed" },
    };
    // The frontend drawn with notifications that hold core's and a plugin's,
    // the same array while they hold, as CoreApi's are.
    const holding = [reloading, posing];
    const notifications: CoreApi["notifications"] = {
      notify: () => () => undefined,
      get: () => holding,
      subscribe: () => () => undefined,
    };
    const el = document.createElement("div");
    document.body.append(el);
    const controller = new AbortController();
    let cleanup: () => void = () => undefined;
    act(() => {
      cleanup = basic.default.frontend.render({ el, core: { ...core, notifications }, plugin: BASIC, signal: controller.signal });
    });

    expect(buttons(toast(el, message))).toEqual(["Reload", "Close"]);
    expect(buttons(toast(el, "Reload, please."))).toEqual(["Close"]);
    const reload = vi.spyOn(window.location, "reload").mockImplementation(() => undefined);
    await click(button(toast(el, message), "Reload"));
    expect(reload).toHaveBeenCalledTimes(1);
    reload.mockRestore();
    act(() => {
      cleanup();
    });
    el.remove();
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
  test("draws no control for what the board lock fixes, and the UI only of the blocks it leaves open", async () => {
    const notes: PluginClass = { id: "blockr.test-notes_plugin", ui: pluginUi().ui };
    const { el } = await drawn(withClasses([notes], plugin(notes, "To do"), lockedHello({ board: true, load: true })));

    // The board options, the history, saving and the plugins stay, since no
    // lock fixes them, and the load lock takes opening another board.
    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save"]);
    expect(texts(find(el, 'aside[aria-label="Plugins"]'), "summary")).toEqual(["Edit board", "To do"]);
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

    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save"]);
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
      cleanup = basic.default.frontend.render({ el, core: stubbed.core, plugin: BASIC, signal: new AbortController().signal });
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
