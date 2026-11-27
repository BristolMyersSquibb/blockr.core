// Core's basic frontend on the CoreApi for tests: the board of the fixture
// hello, drawn by the frontend as the page draws it, from the module of the
// class of core's basic frontend, with R played from the protocol's fixtures,
// every block UI and renderer a stub that names what it draws, the ui of core's
// Edit board plugin a stub that names its plugin, the plugins a test needs, and
// the user acting on the page.

import { act } from "react";
import { afterEach, describe, expect, test, vi } from "vitest";
import {
  find,
  inert,
  itemDescribed,
  labelled,
  menuItem,
  menuItems,
  openDialog,
  openMenu,
  optionFromLabel,
  picking,
  press,
  step,
  texts,
  tipOnFocus,
  toggleButton,
  type,
} from "@blockr/ui/stubs";
import { HELLO, stubCore } from "../../stubs/index.ts";
import type { StubAdvanced, StubCoreOptions, StubMenu } from "../../stubs/index.ts";
import type { HelloReply, Request } from "../../protocol/index.ts";
import type { PluginClass, StorageControl } from "../../store/index.ts";
import { classModule, listedModules, pluginUi } from "../../testing/plugins.ts";
import { logged } from "../../testing/logged.ts";
import { withoutPlugins, withPlugins } from "../../testing/store.ts";
import type { GivenPlugin } from "../../testing/store.ts";
import type { Board, JsonBoard, PluginClassId } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import type { LogLine } from "../../types/log.ts";
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
  ...Object.fromEntries(entries.flatMap((entry) => (entry.renderers ?? []).map((renderer) => [renderer.module, stub("Output")]))),
};

interface Drawn {
  el: HTMLElement;
  // The CoreApi of the board that is open, which core makes anew for each
  // board that opens.
  readonly core: CoreApi;
  board: () => Board;
  settled: () => Promise<void>;
  stop: () => void;
  // The blocks whose outputs core asked R for, in order.
  asked: () => string[];
  // The menu of board actions that is open, which core runs and the CoreApi
  // for tests does not draw, and the advanced action that core's dialog
  // shows.
  menu: () => StubMenu | undefined;
  advanced: () => StubAdvanced | undefined;
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
    asked: () => stubbed.sent.flatMap((request: Request) => (request.kind === "output" ? [request.block] : [])),
    menu: () => stubbed.menu(),
    advanced: () => stubbed.advanced(),
  };
}

// An IntersectionObserver in place of the one that the tests run with, which
// reports an element in view or out of view only as the test says.
function viewing() {
  const observers = new Set<{ readonly callback: IntersectionObserverCallback; readonly observed: Set<Element>; readonly observer: IntersectionObserver }>();
  class Observer {
    readonly #observed = new Set<Element>();
    constructor(callback: IntersectionObserverCallback) {
      observers.add({ callback, observed: this.#observed, observer: this as unknown as IntersectionObserver });
    }
    observe(target: Element): void {
      this.#observed.add(target);
    }
    unobserve(target: Element): void {
      this.#observed.delete(target);
    }
    disconnect(): void {
      this.#observed.clear();
    }
    takeRecords(): IntersectionObserverEntry[] {
      return [];
    }
  }
  vi.stubGlobal("IntersectionObserver", Observer);
  return {
    report(target: Element, isIntersecting: boolean): void {
      for (const { callback, observed, observer } of observers) {
        if (observed.has(target)) callback([{ target, isIntersecting } as IntersectionObserverEntry], observer);
      }
    },
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
    optionFromLabel(labelled(el, label), option).click();
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

// Opens the menu of the card of the block of the name given, through its
// button, and returns the menu of board actions that core opened, which the
// CoreApi for tests drives.
async function menu(drawn: Drawn, name: string): Promise<StubMenu> {
  await click(find(card(drawn.el, name), `button[aria-label="Actions of ${name}"]`));
  const opened = drawn.menu();
  if (opened === undefined) throw new Error(`The card "${name}" opened no menu.`);
  return opened;
}

// The labels of the rows that the menu shows.
const labels = (menu: StubMenu): string[] => menu.level()?.rows.map((row) => row.label) ?? [];

// Does what the user does in a menu of board actions, and lets what it set
// off run, such as the batch of a row.
async function inMenu(fn: () => Promise<void> | void): Promise<void> {
  await act(async () => {
    await fn();
  });
}

// The dialog that is open, which React Aria draws at the end of the page's
// body.
const dialog = (): HTMLElement => openDialog();

// The element that has the focus, where a key that the user presses lands.
const focused = (): HTMLElement => (document.activeElement instanceof HTMLElement ? document.activeElement : document.body);

function toolbar(el: HTMLElement): HTMLElement {
  return find(el, '[role="toolbar"]');
}

// The toasts that the page of the element shows, top to bottom, in the
// region of blockr.ui's toasts at the end of the page's body, which holds the
// latest first and lays it out lowest.
function toastsIn(el: ParentNode): HTMLElement[] {
  const doc = el instanceof Document ? el : (el.ownerDocument ?? document);
  return [...doc.querySelectorAll<HTMLElement>('[role="region"] [role="alertdialog"][data-severity]')].reverse();
}

// The role of what a toast says, which a screen reader reads out: "status"
// or "alert".
const said = (toast: HTMLElement): string | null => find(toast, '[aria-atomic="true"]').getAttribute("role");

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

// The section of each plugin that the page shows, in order.
function sections(el: HTMLElement): HTMLElement[] {
  return [...el.querySelectorAll<HTMLElement>('aside[aria-label="Plugins"] > section')];
}

// The section of the plugin of the name given.
function section(el: HTMLElement, name: string): HTMLElement {
  return find(el, `aside[aria-label="Plugins"] > section[aria-label="${name}"]`);
}

// The button of a section's title, which holds the plugin's name and
// collapses and expands the section, or that of a stack's group, which holds
// the stack's name.
const titled = (section: HTMLElement): HTMLButtonElement => find(section, "h3 > button", HTMLButtonElement);

// The element of a section that holds its plugin's UI, which the section's
// button shows and hides.
function bodyOf(section: HTMLElement): HTMLElement {
  const id = titled(section).getAttribute("aria-controls") ?? "";
  const body = section.ownerDocument.getElementById(id);
  if (body === null || !section.contains(body)) throw new Error(`Found no element "${id}" in the section.`);
  return body;
}

// Whether the section is open, as its button says, which the test checks
// against whether the section shows its plugin's UI.
function isOpen(section: HTMLElement): boolean {
  const open = titled(section).getAttribute("aria-expanded") === "true";
  expect(bodyOf(section).hidden).toBe(!open);
  return open;
}

// Whether the group of a stack is open, as the button of its name says, which
// the test checks against whether the group shows the cards of its blocks.
function groupOpen(group: HTMLElement): boolean {
  const open = titled(group).getAttribute("aria-expanded") === "true";
  expect(bodyOf(group).hidden).toBe(!open);
  expect([...group.querySelectorAll("article")].every((card) => card.closest("[hidden]") === null)).toBe(open);
  return open;
}

// The class of core's basic frontend, which raises its own notifications.
const BASIC = "blockr.core-basic_frontend_plugin";

// The class of core's Edit board plugin, which the page shows in a section.
const EDIT_BOARD = "blockr.core-edit_board_plugin";

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
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
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
    expect(groupOpen(stack)).toBe(true);

    await click(titled(stack));
    await act(() => core.commands.run("blockr.core-change_board", { name: "Sepals" }));
    await settled();
    expect(core.store.board().name).toBe("Sepals");
    // It stays collapsed as the board changes, and its blocks' UIs stay.
    expect(groupOpen(stack)).toBe(false);
    expect(texts(stack, "article h3")).toEqual(["Iris", "Head"]);
    expect(card(el, "Head").textContent).toContain("UI of b");

    await click(titled(stack));
    expect(groupOpen(stack)).toBe(true);
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
  test("shows its block's status in its header, its summary in its footer, and below its output a pill for each severity of the notifications about the block, which opens its banners", async () => {
    const { el } = await drawn({
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
    // The banners of the warnings and of the messages start closed.
    expect(pills(head)).toEqual(["1 warning", "1 message"]);
    expect(banners(head)).toEqual([]);

    await click(button(head, "1 warning"));
    expect(banners(card(el, "Head"))).toEqual([["warning", "Few rows"]]);

    // The open banners show in the order their notifications came, and each
    // pill closes its own.
    await click(button(card(el, "Head"), "1 message"));
    expect(banners(card(el, "Head"))).toEqual([
      ["warning", "Few rows"],
      ["message", "Took the head"],
    ]);
    await click(button(card(el, "Head"), "1 warning"));
    expect(banners(card(el, "Head"))).toEqual([["message", "Took the head"]]);
  });

  test("counts why an invalid block does not run among its messages", async () => {
    const { el } = await drawn({
      evaluate: (request) =>
        request.block === "c"
          ? { status: "invalid", reason: "Choose at least one column to join by." }
          : undefined,
    });

    const merge = card(el, "Merge");
    expect(badge(merge)).toBe("invalid");
    expect(summary(merge)).toBeNull();
    expect(pills(merge)).toEqual(["1 message"]);
    expect(banners(merge)).toEqual([]);

    await click(button(merge, "1 message"));
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
    // their messages say why.
    expect(core.scheduler.status("c")?.status).toBe("waiting");
    const why = core.scheduler.status("c")?.reason;
    expect(why).toMatch(/^This block is waiting for/);
    expect(badge(card(el, "Merge"))).toBe("waiting");
    expect(pills(card(el, "Merge"))).toEqual(["1 message"]);
    await click(button(card(el, "Merge"), "1 message"));
    expect(banners(card(el, "Merge"))).toEqual([["message", why]]);
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
    expect(output("Head").parentElement?.className).not.toContain("bc:h-104");
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
    expect(output("Head").parentElement?.className).toContain("bc:h-104");
    expect(summary(card(el, "Head"))).not.toBeNull();
    expect(badge(card(el, "Merge"))).toBe("ready");
    expect(output("Merge").className).toContain("bc:flex-1");
  });

  test("draws the dot of its block's status in the colour that core's stylesheet holds for the status, which every plugin draws it in, and the word in the muted text colour", async () => {
    const { el } = await drawn({
      evaluate: (request) =>
        request.block === "b"
          ? { status: "error", conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "expr", message: "No rows" }] }
          : undefined,
    });

    // The colours of the badge's dot and of its word.
    // The border takes a quarter of the word's colour.
    const colours = (name: string): { dot: string[]; word: string[] } => {
      const shown = find(card(el, name), "header [data-status]");
      const of = (part: Element): string[] =>
        part.className.split(" ").filter((cls) => /^bc:(bg|text)-/.test(cls) && cls !== "bc:text-xs");
      return { dot: of(find(shown, '[aria-hidden="true"]')), word: of(shown) };
    };
    expect(badge(card(el, "Iris"))).toBe("ready");
    expect(colours("Iris")).toEqual({ dot: ["bc:bg-status-ready"], word: ["bc:text-muted-foreground"] });
    expect(badge(card(el, "Head"))).toBe("failed");
    expect(colours("Head")).toEqual({ dot: ["bc:bg-status-failed"], word: ["bc:text-muted-foreground"] });
    expect(badge(card(el, "Merge"))).toBe("waiting");
    expect(colours("Merge")).toEqual({ dot: ["bc:bg-status-waiting"], word: ["bc:text-muted-foreground"] });
  });

  test("opens its block's menu of board actions, which core draws, from its button, so that every board action on the block shows, those of packages too", async () => {
    const page = await drawn();
    const opening = vi.spyOn(page.core.boardActions, "menu");

    const head = await menu(page, "Head");
    const actions = find(card(page.el, "Head"), 'button[aria-label="Actions of Head"]');
    expect(opening).toHaveBeenCalledWith({ block: "b" }, { el: actions, anchor: actions.getBoundingClientRect() });
    expect(actions.getAttribute("aria-haspopup")).toBe("menu");
    // Links feed every input of the head block, so it offers no Prepend block.
    expect(labels(head)).toEqual(["Append block", "Create link", "Create stack", "Rename", "Edit inputs", "Copy block ID", "Remove block"]);
    head.escape();
    // The bind block is variadic, so it takes one more link, and no stack
    // holds it, as the stack Input holds the head block, so it joins one.
    expect(labels(await menu(page, "Bind"))).toEqual([
      "Append block",
      "Prepend block",
      "Create link",
      "Create stack",
      "Rename",
      "Edit inputs",
      "Add to stack",
      "Copy block ID",
      "Remove block",
    ]);
  });

  test("renames and removes its block through its menu, each in one step of the user", async () => {
    const page = await drawn();
    const { el, board } = page;

    const renaming = await menu(page, "Head");
    await inMenu(() => renaming.pick("Rename"));
    renaming.type("First rows", "Name");
    await inMenu(() => renaming.pick("Name"));
    expect(board().blocks.b?.name).toBe("First rows");
    expect(find(card(el, "First rows"), "h3").textContent).toBe("First rows");

    const removing = await menu(page, "Scatter");
    await inMenu(() => removing.pick("Remove block"));
    expect(Object.keys(board().blocks)).not.toContain("f");
    expect(el.querySelector('article[aria-label="Scatter"]')).toBeNull();
  });

  test("appends and prepends a block through its menu, by a row of the board action's quick action or through the block picker, which core shows in its dialog, each in one step of the user", async () => {
    const page = await drawn();
    const { el, core, board, settled } = page;
    const opened = board();

    // A row of Append block's quick action appends a block of its class,
    // which takes the block's result.
    const appending = await menu(page, "Head");
    await inMenu(() => appending.pick("Append block"));
    await inMenu(() => appending.pick("subset block"));
    await settled();
    const [subset] = added(board());
    expect(subset?.[1].registry_id).toBe("blockr.core-subset_block");
    expect(Object.values(board().links)).toContainEqual({ from: "b", to: subset?.[0], input: "data" });
    expect(card(el, "Subset").textContent).toContain(`UI of ${subset?.[0] ?? ""}`);
    await step(() => {
      core.store.undo();
    });
    expect(board()).toEqual(opened);

    // The open icon of Prepend block's quick action opens the picker, which
    // core shows in its dialog, since the frontend gives no place of its own.
    const prepending = await menu(page, "Bind");
    await inMenu(() => prepending.pick("Prepend block"));
    prepending.type("data");
    await step(() => {
      void prepending.openAdvanced();
    });
    const shown = page.advanced();
    expect(shown).toMatchObject({ title: "Prepend block", target: { block: "e" } });
    const form = shown?.el ?? document.body;
    expect(labelled(form, "Block class").value).toBe("data");
    await chosen(form, "Block class", "dataset", "dataset block");
    await step(() => {
      type(labelled(form, 'Input name of "Bind"'), "extra");
      type(labelled(form, "ID"), "more");
    });
    await click(button(form, "Add block"));
    expect(Object.values(board().links)).toContainEqual({ from: "more", to: "e", input: "extra" });
    expect(page.advanced()).toBeUndefined();
    await step(() => {
      core.store.undo();
    });
    expect(board()).toEqual(opened);
  });
});

describe("the toolbar", () => {
  test("draws no control for adding a block or for links and stacks, which the Edit board plugin edits in a section of its own", async () => {
    const { el } = await drawn();

    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save", "Open"]);
    expect(sections(el).map((shown) => titled(shown).textContent)).toEqual(["Edit board"]);
    expect(bodyOf(section(el, "Edit board")).textContent).toBe("Edit board");
  });

  test("sets the board options that the board holds through their UIs, by category", async () => {
    const { el, board, settled } = await drawn();

    await click(button(el, "Options"));
    await settled();
    // The board holds n_rows, which the table renderer names, and dark_mode,
    // which core's basic frontend requires.
    expect(Object.keys(board().options).sort()).toEqual(["dark_mode", "n_rows"]);
    expect(texts(dialog(), "h3")).toEqual(["Table options", "Theme options"]);
    // The categories sit in one box, which draws a line between each two.
    const boxes = new Set([...dialog().querySelectorAll("section")].map((section) => section.parentElement));
    expect([...boxes].map((box) => box?.className)).toEqual([expect.stringMatching(/(^| )bc:divide-y( |$)/)]);
    expect(dialog().textContent).toContain("Preview rows");

    await click(toggleButton(find(dialog(), 'section[aria-label="Theme options"]'), "Light/dark mode", "Dark"));
    expect(board().options.dark_mode).toBe("dark");
    expect(document.documentElement.getAttribute("data-bk-mode")).toBe("dark");
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

    // Where the store refuses the board in the file, core tells the user why,
    // and the frontend logs the Open that rejects.
    vi.restoreAllMocks();
    picking(new File(["{}"], "board.json"));
    const reported = logged();
    let errors: readonly LogLine[];
    try {
      await click(button(page.el, "Open"));
      await page.settled();
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    expect(page.board().name).toBe("Opened");
    expect(toasts(page.el)).toEqual([["error", "Board", expect.stringContaining("The store refused to open the board")]]);
    expect(errors.map((line) => [line.package, line.error?.message])).toEqual([
      ["blockr.core", expect.stringContaining("The store refused to open the board")],
    ]);
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
function storagePlugin(controls: (args: { core: CoreApi; plugin: PluginClassId }) => readonly StorageControl[]) {
  let current = controls;
  const listeners = new Set<() => void>();
  const cls: PluginClass = {
    id: "blockr.test-session_plugin",
    storage: {
      controls: (args) => current(args),
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
    set(next: (args: { core: CoreApi; plugin: PluginClassId }) => readonly StorageControl[]): void {
      current = next;
      for (const listener of [...listeners]) listener();
    },
    listening: () => listeners.size,
  };
}

// The text of each part of the toolbar, in order.
// The text of each part of the toolbar, but for the separators between its
// groups.
const parts = (el: HTMLElement): string[] =>
  [...toolbar(el).children].filter((part) => part.getAttribute("role") !== "separator").map((part) => part.textContent.trim());

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
  test("show at the end of the toolbar in the plugin's order, after a separator, each kind in its own look", async () => {
    const { ran } = running();
    const storage = storagePlugin(() => [
      { kind: "status", id: "user", text: "NS", tooltip: "Signed in as Nina" },
      { kind: "button", id: "save", label: "Save", tooltip: "Save to Connect", shortcut: "Mod+S", run: ran("save") },
      { kind: "menu", id: "versions", label: "Versions", items: [] },
      { kind: "status", id: "saved", text: "Saved" },
      { kind: "button", id: "share", label: "Share", disabled: true, run: ran("share") },
      { kind: "button", id: "lock", label: "Lock", tooltip: "Locked by Ann", disabled: true, run: ran("lock") },
    ]);
    const { el } = await drawn(storage.options);

    // The toolbar's three groups, the board options, undo and redo, and the
    // storage controls, each come after a separator but the first.
    expect([...toolbar(el).children].map((part) => (part.getAttribute("role") === "separator" ? "|" : part.textContent.trim()))).toEqual([
      "Options",
      "|",
      "Undo",
      "Redo",
      "|",
      "NS",
      "Save",
      "Versions▾",
      "Saved",
      "Share",
      "Lock",
      "Locked by Ann",
    ]);
    // A button looks like the toolbar's own, names its shortcut, and shows
    // its tooltip as the keyboard's focus moves to it.
    const save = button(toolbar(el), "Save");
    expect(save.className).toBe(button(toolbar(el), "Options").className);
    expect(save.getAttribute("aria-keyshortcuts")).toBe("Control+S Meta+S");
    expect(await tipOnFocus(save)).toBe("Save to Connect");
    expect(save.getAttribute("aria-describedby")).toBe(find(document, '[role="tooltip"]').id);
    // A disabled button is greyed out, and shows its tooltip as text after
    // it, which describes it.
    expect(button(toolbar(el), "Share").disabled).toBe(true);
    const lock = button(toolbar(el), "Lock");
    expect(lock.disabled).toBe(true);
    expect(document.getElementById(lock.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Locked by Ann");
    // A menu is a button that opens it, and a status is text, which shows
    // its tooltip as the focus moves to it.
    expect(find(toolbar(el), "button[aria-haspopup]").getAttribute("aria-expanded")).toBe("false");
    const statuses = [...toolbar(el).querySelectorAll<HTMLElement>('[role="status"]')];
    expect(statuses.map((status) => [status.tagName, status.textContent])).toEqual([
      ["SPAN", "NS"],
      ["SPAN", "Saved"],
    ]);
    expect(await tipOnFocus(find(statuses[0] ?? el, '[role="button"]'))).toBe("Signed in as Nina");
    expect(statuses[1]?.querySelector('[role="button"]')).toBeNull();
  });

  test("open a menu of what it holds, a menu inside it beside its row, each item with its tooltip below it and its shortcut at its right, and close it as one of its buttons runs", async () => {
    const { runs, ran } = running();
    const storage = storagePlugin(() => [
      {
        kind: "menu",
        id: "versions",
        label: "Versions",
        items: [
          { kind: "button", id: "restore", label: "Restore", shortcut: "Mod+Shift+K", run: ran("restore") },
          { kind: "status", id: "latest", text: "Version 3", tooltip: "Saved at noon" },
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
    const opener = find(toolbar(el), "button[aria-haspopup]");
    const list = () => document.querySelector<HTMLElement>('[role="menu"][aria-label="Versions"]');

    await click(opener);
    expect(opener.getAttribute("aria-expanded")).toBe("true");
    const shown = openMenu("Versions");
    // A status is an item that the user cannot pick. An item's tooltip shows
    // as its description, and a button's shortcut at its right, as the page
    // names it off a Mac, and React Aria names both to assistive technology.
    // A menu inside it is an item, which opens it beside its row.
    expect(menuItems(shown)).toEqual(["Restore", "Version 3", "Older"]);
    expect(menuItem(shown, "Version 3").getAttribute("aria-disabled")).toBe("true");
    expect(itemDescribed(menuItem(shown, "Version 3"))).toEqual(["Saved at noon"]);
    expect(find(menuItem(shown, "Restore"), "kbd").textContent).toBe("Ctrl+Shift+K");
    expect(itemDescribed(menuItem(shown, "Restore"))).toEqual(["Ctrl+Shift+K"]);
    expect(menuItem(shown, "Older").getAttribute("aria-haspopup")).toBe("menu");
    await click(menuItem(shown, "Older"));
    const older = openMenu("Older");
    expect(menuItems(older)).toEqual(["Version 1"]);
    expect(itemDescribed(menuItem(older, "Version 1"))).toEqual(["Saved in May"]);

    await click(menuItem(older, "Version 1"));
    expect(runs).toEqual(["v1"]);
    expect(list()).toBeNull();
    expect(document.querySelector('[role="menu"]')).toBeNull();

    // It closes on Escape too, which React Aria hears where the focus is, in
    // the menu.
    await click(opener);
    expect(list()?.contains(document.activeElement)).toBe(true);
    await step(() => {
      press(focused(), "Escape");
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
    await step(() => {
      storage.set(() => [{ kind: "status", id: "saved", text: `Unsaved ${page.board().name}` }]);
    });
    await act(() => page.core.commands.run("blockr.core-change_board", { name: "Sepals" }));
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
    await act(() => core.commands.run("blockr.core-change_board", { name: "Sepals" }));

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

  test("are given the id of the storage plugin's class, under which the plugin tells the user why one of its buttons failed", async () => {
    const storage = storagePlugin(({ core, plugin }) => [
      {
        kind: "button",
        id: "save",
        label: "Save",
        run: () => {
          core.notifications.notify(plugin, { severity: "error", code: "save_failed", message: "Connect is down." });
          return Promise.resolve();
        },
      },
    ]);
    const { el } = await drawn(storage.options);

    await click(button(toolbar(el), "Save"));
    expect(toasts(el)).toEqual([["error", 'Plugin "Session" · blockr.test', "Connect is down."]]);
  });

  test("log a button whose run() rejects, run by a click or by its shortcut, and raise no notification", async () => {
    const storage = storagePlugin(() => [
      { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: () => Promise.reject(new Error("Connect is down.")) },
    ]);
    const { el, core } = await drawn(storage.options);
    const reported = logged();
    let errors: readonly LogLine[];
    try {
      await click(button(toolbar(el), "Save"));
      await step(() => {
        press(document.body, "s", { ctrlKey: true });
      });
      await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    expect(errors.map((line) => [line.package, line.error?.message])).toEqual([
      ["blockr.core", "Connect is down."],
      ["blockr.core", "Connect is down."],
    ]);
    expect(core.notifications.get()).toEqual([]);
    expect(toasts(el)).toEqual([]);
  });

  test("draw the first of the controls of one list that share an id, and a warning in their place where controls() fails, of which the page hears once", async () => {
    const { runs, ran } = running();
    const storage = storagePlugin(() => [
      { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: ran("save") },
      { kind: "button", id: "save", label: "Save again", shortcut: "Mod+E", run: ran("again") },
      { kind: "menu", id: "more", label: "More", items: [{ kind: "status", id: "a", text: "A" }, { kind: "status", id: "a", text: "B" }] },
    ]);
    const reported = logged();
    let errors: readonly LogLine[];
    try {
      const page = await drawn(storage.options);
      expect(parts(page.el)).toEqual(["Options", "Undo", "Redo", "Save", "More▾"]);
      await click(find(toolbar(page.el), "button[aria-haspopup]"));
      expect(menuItems(openMenu("More"))).toEqual(["A"]);
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
      expect(await tipOnFocus(find(warning, '[role="button"]'))).toBe("The storage controls failed: No session.");
      expect(parts(page.el)).toEqual(["Options", "Undo", "Redo", "Storage failed"]);
      // It draws again with the same failure, which the page has heard of.
      await act(() => page.core.commands.run("blockr.core-change_board", { name: "Sepals" }));
      await step(() => {
        press(document.body, "s", { ctrlKey: true });
      });
      expect(runs).toEqual([]);
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    expect(errors.map((line) => [line.package, line.message])).toEqual([
      ["blockr.core", 'Storage controls of one list share the ids "save" and "a", so only the first of each draws.'],
      ["blockr.core", "The storage controls failed: No session."],
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
    const shown = () => sections(page.el).map((drawn) => [titled(drawn).textContent, isOpen(drawn)]);

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
    expect(bodyOf(section(page.el, "To do")).contains(ui.renders[0]?.el ?? null)).toBe(true);
    expect(ui.renders[0]?.el.textContent).toBe("To do");
    expect(page.el.querySelectorAll("article")).toHaveLength(6);
    expect(buttons(toolbar(page.el)).at(-1)).toBe("Open");
    // They come before the board, which a narrow page shows below them.
    expect(page.el.querySelector("aside, article")?.tagName).toBe("ASIDE");

    // A collapsed section stays collapsed as the board changes, and its
    // plugin's UI stays mounted.
    await click(titled(section(page.el, "To do")));
    await act(() => page.core.commands.run("blockr.core-change_board", { name: "Sepals" }));
    await page.settled();
    expect(page.board().name).toBe("Sepals");
    expect(shown()).toEqual([
      ["Edit board", true],
      ["To do", false],
      ["Later", true],
    ]);
    expect(ui.renders).toHaveLength(2);
    expect(ui.cleanups()).toBe(0);

    await click(titled(section(page.el, "To do")));
    expect(shown()).toEqual([
      ["Edit board", true],
      ["To do", true],
      ["Later", true],
    ]);
    expect(ui.renders).toHaveLength(2);
  });

  test("shows the notifications about it, whoever raised them, as pills on its section's title for every severity, each of which opens their messages below the title, closed at first, whether the section is open or collapsed", async () => {
    const { el, core } = await drawn();
    const edit = section(el, "Edit board");
    let withdraw: () => void = () => undefined;
    await step(() => {
      core.notifications.notify(EDIT_BOARD, { severity: "warning", code: "registry_large", message: "The registry holds 900 classes." });
      withdraw = core.notifications.notify(EDIT_BOARD, { severity: "error", code: "table_failed", message: "The table cannot draw." });
      core.notifications.notify(FILE_STORAGE, {
        severity: "warning",
        code: "edits_unsaved",
        message: "Edits are unsaved.",
        about: { plugin: EDIT_BOARD },
      });
      core.notifications.notify(EDIT_BOARD, { severity: "message", code: "class_picked", message: "Picked a class." });
    });

    // Each pill starts closed, and no notification about the plugin shows as a
    // toast.
    expect(pills(edit)).toEqual(["error", "2 warnings", "1 message"]);
    expect(banners(edit)).toEqual([]);
    expect(toasts(el)).toEqual([]);

    // A pill opens the messages of its notifications below the title, in the
    // order they came, above the plugin's UI.
    await click(button(edit, "2 warnings"));
    expect(banners(edit)).toEqual([
      ["warning", "The registry holds 900 classes."],
      ["warning", "Edits are unsaved."],
    ]);
    const list = find(edit, 'ul[aria-label="Conditions"]');
    expect(list.previousElementSibling?.contains(titled(edit))).toBe(true);
    expect(list.parentElement?.nextElementSibling).toBe(bodyOf(edit));

    // A collapsed section keeps its pills, which still open their messages
    // and leave the section collapsed.
    await click(titled(edit));
    await click(button(edit, "error"));
    expect(isOpen(edit)).toBe(false);
    expect(pills(edit)).toEqual(["error", "2 warnings", "1 message"]);
    expect(banners(edit)).toEqual([
      ["warning", "The registry holds 900 classes."],
      ["error", "The table cannot draw."],
      ["warning", "Edits are unsaved."],
    ]);

    // A pill goes with its notifications.
    await step(() => {
      withdraw();
    });
    expect(pills(edit)).toEqual(["2 warnings", "1 message"]);
    expect(toasts(el)).toEqual([]);
  });

  test("whose ui cannot draw shows why on its section, from its pill, and in no toast, while the section leaves the element of its ui empty", async () => {
    const broken: PluginClass = {
      id: "blockr.test-broken_plugin",
      ui: {
        render() {
          throw new Error("No canvas.");
        },
      },
    };
    const reported = logged();
    let page: Drawn;
    let errors: readonly LogLine[];
    try {
      page = await drawn(withClasses([broken], plugin(broken, "Broken")));
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    expect(errors.map((line) => line.error?.message)).toEqual([
      'The ui of the plugin class "blockr.test-broken_plugin" failed in render(): No canvas.',
    ]);

    const shown = section(page.el, "Broken");
    expect(pills(shown)).toEqual(["error"]);
    expect(banners(shown)).toEqual([]);
    expect(toasts(page.el)).toEqual([]);
    await click(button(shown, "error"));
    expect(banners(shown)).toEqual([["error", 'The ui of the plugin class "blockr.test-broken_plugin" failed in render(): No canvas.']]);
    expect(bodyOf(shown).textContent).toBe("");
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

describe("what is on screen", () => {
  test("is the blocks whose cards and the plugins whose sections show in the viewport, as the page lays them out", async () => {
    // The page lays out the card of Head and the UI of the Edit board plugin
    // in view, and nothing else, which the browser has yet to report. The
    // UI's element is the one that the panel of the plugin's section holds.
    viewing();
    vi.spyOn(Element.prototype, "getBoundingClientRect").mockImplementation(function (this: Element) {
      const shown =
        this.matches('article[aria-label="Head"]') ||
        this.parentElement?.matches('section[aria-label="Edit board"] [role="group"][aria-labelledby]') === true;
      const [width, height] = shown ? [400, 100] : [0, 0];
      return { top: 0, bottom: height, left: 0, right: width, width, height, x: 0, y: 0, toJSON: () => ({}) };
    });
    const { core } = await drawn();
    expect(core.onScreen.get()).toEqual({ blocks: ["b"], plugins: ["blockr.core-edit_board_plugin"] });
  });

  test("has core ask R for a card's output once the card is in view, while the card keeps the room of its output, and keep drawing the output as the card leaves", async () => {
    const view = viewing();
    const { el, core, settled, asked } = await drawn();
    const head = card(el, "Head");
    const output = find(head, '[aria-label="Output"]');
    expect(core.scheduler.status("b")?.status).toBe("ready");
    expect(asked()).toEqual([]);
    expect(output.textContent).toBe("");
    // The card gives the output its height while the output waits, so that
    // nothing moves as the output draws.
    expect(output.className).toContain("bc:flex-1");
    const box = output.parentElement?.className;

    await step(() => {
      view.report(head, true);
    });
    await settled();
    expect(asked()).toEqual(["b"]);
    expect(output.textContent).toBe("Output of b");
    expect(output.parentElement?.className).toBe(box);

    // The card leaves the view, and Head evaluates again meanwhile.
    await step(() => {
      view.report(head, false);
    });
    await step(() => {
      core.scheduler.invalidate("b");
    });
    await settled();
    expect(core.scheduler.status("b")?.status).toBe("ready");
    expect(asked()).toEqual(["b"]);
    expect(output.textContent).toBe("Output of b");

    await step(() => {
      view.report(head, true);
    });
    await settled();
    expect(asked()).toEqual(["b", "b"]);
  });
});

describe("showing a block or a plugin", () => {
  test("opens the group or the section that the user collapsed, and scrolls the card or the section into view, leaving the keyboard's focus where it is", async () => {
    const { el, core } = await drawn();
    const scrolled: Element[] = [];
    vi.spyOn(Element.prototype, "scrollIntoView").mockImplementation(function (this: Element) {
      scrolled.push(this);
    });
    const stack = find(el, 'section[aria-label="Input"]');
    await click(titled(stack));
    const edit = section(el, "Edit board");
    await click(titled(edit));
    expect([groupOpen(stack), isOpen(edit)]).toEqual([false, false]);
    const actions = find(card(el, "Merge"), 'button[aria-label="Actions of Merge"]');
    await step(() => {
      actions.focus();
    });

    await act(() => core.show({ block: "b" }));
    expect(groupOpen(stack)).toBe(true);
    expect(scrolled).toEqual([card(el, "Head")]);
    expect(core.focus.get()).toBe("b");
    expect(document.activeElement).toBe(actions);

    await act(() => core.show({ plugin: EDIT_BOARD }));
    expect(isOpen(edit)).toBe(true);
    expect(scrolled).toEqual([card(el, "Head"), edit]);
    expect(core.focus.get()).toBe("b");
    expect(document.activeElement).toBe(actions);
  });
});

// R's reply to hello whose opening board the store opens without what the
// app cannot rebuild: the package blockr.dplyr, which R did not load, a block
// of its filter block class, the state of a plugin that the app does not run,
// and the value of the board option zoom of blockr.zoom, which R did not load
// either.
function droppingHello(): HelloReply {
  const json = HELLO.board;
  const filter = { registry_id: "blockr.dplyr-filter_block", version: 1, state: {}, name: "Filter", locked: null };
  return {
    ...HELLO,
    board: {
      ...json,
      packages: { ...json.packages, "blockr.dplyr": "0.3.0", "blockr.zoom": "0.1.0" },
      board: {
        ...json.board,
        blocks: { ...json.board.blocks, x: filter },
        plugins: { ...json.board.plugins, "blockr.gone-gone_plugin": { state: {}, version: 1 } },
        options: { ...json.board.options, zoom: { value: 2, version: 1, package: "blockr.zoom" } },
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
          'The board opened without the block "Filter", since this app lacks the package blockr.dplyr 0.3.0.',
          'The board opened without the state of the plugin "blockr.gone-gone_plugin", since this app does not run it.',
          'The board opened without the value of the board option "zoom", since this app lacks the package blockr.zoom 0.1.0.',
        ].join("\n"),
      ],
    ]);
    expect(said(toastsIn(page.el)[0] ?? page.el)).toBe("alert");

    // A board that opens later starts core and the frontend again, which shows
    // only the toast of what that board dropped.
    const read = { registry_id: "blockr.io-read_block", version: 1, state: {}, name: "Read", locked: null };
    const later = (json: JsonBoard): JsonBoard => ({
      ...json,
      packages: { ...json.packages, "blockr.io": "0.1.0" },
      board: { ...json.board, blocks: { ...json.board.blocks, r: read } },
    });
    picking(new File([savedAs(page.core, later)], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(toasts(page.el)).toEqual([["warning", "Board", 'The board opened without the block "Read", since this app lacks the package blockr.io 0.1.0.']]);

    vi.restoreAllMocks();
    picking(new File([savedAs(page.core, renamed("Clean"))], "board.json"));
    await click(button(page.el, "Open"));
    await page.settled();
    expect(page.board().name).toBe("Clean");
    expect(toasts(page.el)).toEqual([]);
  });

  test("is each notification of the board that concerns neither a block nor a plugin with a section, whoever raised it, as a toast in the colour of its severity, until it is withdrawn or the user closes it, while a block's card shows those about the block", async () => {
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
    expect(said(toast(el, "Saved to Connect."))).toBe("status");
    expect(said(toast(el, "The token expires soon."))).toBe("alert");
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

  test("leaves its toasts within reach while a dialog is open, outside the dialog, though the rest of the page is inert", async () => {
    const { el, core, settled } = await drawn();
    await step(() => {
      core.notifications.notify(FILE_STORAGE, { severity: "error", code: "share_failed", message: "Not shared." });
    });

    await click(button(el, "Options"));
    await settled();
    await step(() => {
      core.notifications.notify(FILE_STORAGE, { severity: "warning", code: "token_expiring", message: "The token expires soon." });
    });
    // The toasts stay in their region, outside the dialog, which the modal
    // leaves out of what it makes inert.
    expect(toasts(el).map(([, , message]) => message)).toEqual(["Not shared.", "The token expires soon."]);
    expect(toastsIn(el).map((t) => [t.closest('[role="dialog"]'), inert(t)])).toEqual([
      [null, false],
      [null, false],
    ]);
    expect(inert(find(el, "h1"))).toBe(true);
    // The user closes a toast, and the dialog stays open.
    await click(find(toast(el, "Not shared."), 'button[aria-label="Close"]'));
    expect(toasts(el).map(([, , message]) => message)).toEqual(["The token expires soon."]);
    expect(document.querySelector('[role="dialog"]')).not.toBeNull();

    await click(button(dialog(), "Close"));
    expect(document.querySelector('[role="dialog"]')).toBeNull();
    expect(toasts(el).map(([, , message]) => message)).toEqual(["The token expires soon."]);
  });

  test("is why the store or the command registry refused an edit, an error of the code board_edit_failed that core raises about the board, whatever block the edit names, which shows again as the same refusal comes again", async () => {
    const { el, core } = await drawn();
    // The user removes the link from Iris to Head, and the assistant then
    // feeds Head's one input from Wide sepals, in a step that is no undo step.
    // So the store refuses to undo the user's step, the history's last, which
    // would give that input a second link.
    await act(() => core.commands.run("blockr.core-remove_link", { id: "ab" }));
    act(() => {
      core.store.apply({ links: { add: { db: { from: "d", to: "b", input: "data" } } } }, "assistant", { undo: false });
    });
    await click(button(el, "Undo"));
    const refusal = core.notifications.get()[0]?.message ?? "";
    expect(refusal).toMatch(/^The store refused to undo the step/);
    expect(toasts(el)).toEqual([["error", "Board", refusal]]);

    // The user closes the toast and presses the shortcut, whose undo the
    // store refuses alike. Core raises the refusal anew, so its toast shows
    // again.
    await click(find(toast(el, refusal), 'button[aria-label="Close"]'));
    expect(toasts(el)).toEqual([]);
    await step(() => {
      press(document.body, "z", { ctrlKey: true });
    });
    expect(toasts(el)).toEqual([["error", "Board", refusal]]);

    // A command that the command registry refuses shows alike.
    await act(() => expect(core.commands.run("blockr.core-remove_block", { id: "nowhere" })).rejects.toThrow());
    expect(toasts(el)).toEqual([
      ["error", "Board", refusal],
      ["error", "Board", expect.stringMatching(/^The command "blockr\.core-remove_block" refused to run/)],
    ]);
    // The frontend raises none of its own.
    expect(core.notifications.get().map(({ source, about }) => [source, about])).toEqual([
      [{ kind: "core", code: "board_edit_failed" }, undefined],
      [{ kind: "core", code: "board_edit_failed" }, undefined],
    ]);
    expect(pills(card(el, "Head"))).toEqual([]);
  });

  test("fades from a message's toast after five seconds, but not while the pointer rests on the toasts or the keyboard's focus is in them", async () => {
    const { el, core } = await drawn();
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      await step(() => {
        core.notifications.notify(FILE_STORAGE, { severity: "message", code: "saved", message: "Saved." });
        core.notifications.notify(FILE_STORAGE, { severity: "error", code: "share_failed", message: "Not shared." });
      });
      // The pointer rests on the error's toast, which keeps the message's
      // toast too, as does the focus in the message's toast.
      const saved = toast(el, "Saved.");
      const shared = toast(el, "Not shared.");
      await step(() => {
        shared.dispatchEvent(new PointerEvent("pointerover", { bubbles: true, pointerType: "mouse" }));
        vi.advanceTimersByTime(60_000);
      });
      await step(() => {
        find(saved, 'button[aria-label="Close"]').focus();
        shared.dispatchEvent(new PointerEvent("pointerout", { bubbles: true, pointerType: "mouse" }));
        vi.advanceTimersByTime(60_000);
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Saved.", "Not shared."]);

      await step(() => {
        find(saved, 'button[aria-label="Close"]').blur();
        vi.advanceTimersByTime(4000);
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Saved.", "Not shared."]);
      await step(() => {
        vi.advanceTimersByTime(1000);
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Not shared."]);
    } finally {
      vi.useRealTimers();
    }
  });

  test("fades from a message's toast while a dialog is open", async () => {
    const { el, core, settled } = await drawn();
    await click(button(el, "Options"));
    await settled();
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      await step(() => {
        core.notifications.notify(FILE_STORAGE, { severity: "message", code: "saved", message: "Saved." });
      });
      expect(toasts(el).map(([, , message]) => message)).toEqual(["Saved."]);
      await step(() => {
        vi.advanceTimersByTime(5000);
      });
      expect(toasts(el)).toEqual([]);
      expect(document.querySelector('[role="dialog"]')).not.toBeNull();
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
      source: { kind: "module", module: { object: { plugin: FILE_STORAGE }, package: "blockr.core" }, code: "page_reload_needed" },
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
    await act(() => core.commands.run("blockr.core-change_board", { name: "Sepals" }));

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

  test("are on the toolbar, each while the history holds a step for it", async () => {
    const { el, core, board } = await drawn();
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([true, true]);

    await act(() => core.commands.run("blockr.core-change_board", { name: "Sepals" }));
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([false, true]);

    await click(button(el, "Undo"));
    expect(board().name).toBe("Iris review");
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([true, false]);
  });

  test("take back and apply again the steps of every origin, the assistant's among them", async () => {
    const { el, core, board } = await drawn();
    await act(() => core.commands.run("blockr.core-change_board", { name: "Sepals" }, "assistant"));
    expect([button(el, "Undo").disabled, button(el, "Redo").disabled]).toEqual([false, true]);

    await click(button(el, "Undo"));
    expect(board().name).toBe("Iris review");
    await step(() => {
      press(document.body, "z", { ctrlKey: true, shiftKey: true });
    });
    expect(board().name).toBe("Sepals");
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
    const page = await drawn(withClasses([notes], plugin(notes, "To do"), lockedHello({ board: true, load: true })));
    const { el } = page;

    // The board options, the history, saving and the plugins stay, since no
    // lock fixes them, and the load lock takes opening another board.
    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save"]);
    expect(sections(el).map((shown) => titled(shown).textContent)).toEqual(["Edit board", "To do"]);
    expect(el.querySelector('button[aria-label="Rename board"]')).toBeNull();
    // A card's menu offers only Copy block ID, which no lock fixes.
    expect(texts(el, "article button").every((text) => text === "⋯")).toBe(true);
    expect(labels(await menu(page, "Head"))).toEqual(["Copy block ID"]);

    expect(card(el, "Head").textContent).toContain("UI of b");
    expect(card(el, "Iris").textContent).not.toContain("UI of a");
    expect(card(el, "Iris").textContent).toContain("Output of a");
  });

  test("under the board lock alone, keeps opening another board, and sets the board options", async () => {
    const { el, board, settled } = await drawn({ hello: lockedHello({ board: true, load: false }) });
    expect(buttons(toolbar(el))).toEqual(["Options", "Undo", "Redo", "Save", "Open"]);

    await click(button(el, "Options"));
    await settled();
    await click(toggleButton(find(dialog(), 'section[aria-label="Theme options"]'), "Light/dark mode", "Dark"));
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
    await act(() => stubbed.core.commands.run("blockr.core-change_board", { name: "Sepals" }));

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
