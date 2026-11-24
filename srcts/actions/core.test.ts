// Core's board actions, on the board of the fixture hello through the CoreApi
// for tests: which targets they apply to, under the board lock too, Rename
// through its quick action, Copy block ID with the clipboard API, the
// document's copy command and neither, and those that remove a block, a link,
// a stack and what a selection holds. Those that add a block, those that
// create and edit a link or a stack, and Edit inputs have tests of their own
// (./add.test.tsx, ./links.test.tsx, ./stacks.test.tsx, ./inputs.test.tsx).

import { describe, expect, onTestFinished, test, vi } from "vitest";
import { HELLO, stubCore } from "../stubs/index.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BoardAction, Target } from "./actions.ts";

const ADD = "blockr.core-add_block_action";
const APPEND = "blockr.core-append_block_action";
const PREPEND = "blockr.core-prepend_block_action";
const INSERT = "blockr.core-insert_block_action";
const CREATE_LINK = "blockr.core-add_link_action";
const CREATE_STACK = "blockr.core-add_stack_action";
const RENAME = "blockr.core-rename_action";
const EDIT_LINK = "blockr.core-edit_link_action";
const EDIT_INPUTS = "blockr.core-edit_inputs_action";
const EDIT_STACK = "blockr.core-edit_stack_action";
const ADD_TO_STACK = "blockr.core-add_to_stack_action";
const COPY = "blockr.core-copy_block_id_action";
const REMOVE_BLOCK = "blockr.core-remove_block_action";
const REMOVE_LINK = "blockr.core-remove_link_action";
const REMOVE_STACK = "blockr.core-remove_stack_action";
const REMOVE_SELECTED = "blockr.core-remove_selected_action";

// The element that the user triggers from, which a trigger() needs.
const el = document.body;

// Each board action's id, label and category, as a menu shows them.
function rows(actions: readonly BoardAction[]): [string, string, string][] {
  return actions.map(({ id, label, category }) => [id, label, category]);
}

// The ids of the board's blocks, links and stacks.
function objects(core: CoreApi) {
  const board = core.store.board();
  return { blocks: Object.keys(board.blocks), links: Object.keys(board.links), stacks: Object.keys(board.stacks) };
}

// Gives the document the copy command that a browser has and happy-dom lacks,
// which answers `copied`, until the test ends. It records the text that each
// call copies, from the field that the copy selects.
function copyCommand(copied: boolean): string[] {
  const texts: string[] = [];
  Object.defineProperty(document, "execCommand", {
    configurable: true,
    value(command: string) {
      texts.push(`${command} ${document.querySelector("textarea")?.value ?? ""}`);
      return copied;
    },
  });
  onTestFinished(() => {
    Reflect.deleteProperty(document, "execCommand");
  });
  return texts;
}

// Has the clipboard API refuse to write, as a browser may, until the test ends.
function refusingClipboard(): void {
  const refused = vi.spyOn(navigator.clipboard, "writeText").mockRejectedValue(new DOMException("The request is not allowed."));
  onTestFinished(() => {
    refused.mockRestore();
  });
}

describe("core's board actions", () => {
  test("are Add block, Create link and Create stack on the board, Append block, Prepend block, Create link, Create stack, Rename, Edit inputs, Add to stack, Copy block ID and Remove block on a block, Insert block, Edit link and Remove link on a link, Rename, Edit stack and Remove stack on a stack, and Create stack, Add to stack and Remove on a selection, in the order of their table", async () => {
    const s = await stubCore();
    const on = (target: Target) => s.core.boardActions.on(target);

    expect(rows(on({ board: true }))).toEqual([
      [ADD, "Add block", "add"],
      [CREATE_LINK, "Create link", "add"],
      [CREATE_STACK, "Create stack", "add"],
    ]);
    // Links feed every input of the head block b, and none feeds the bind
    // block e, which is variadic. The stack s1 holds b, and no stack holds e,
    // so that Add to stack applies to e alone.
    expect(rows(on({ block: "b" }))).toEqual([
      [APPEND, "Append block", "add"],
      [CREATE_LINK, "Create link", "add"],
      [CREATE_STACK, "Create stack", "add"],
      [RENAME, "Rename", "edit"],
      [EDIT_INPUTS, "Edit inputs", "edit"],
      [COPY, "Copy block ID", "clipboard"],
      [REMOVE_BLOCK, "Remove block", "remove"],
    ]);
    expect(rows(on({ block: "e" }))).toEqual([
      [APPEND, "Append block", "add"],
      [PREPEND, "Prepend block", "add"],
      [CREATE_LINK, "Create link", "add"],
      [CREATE_STACK, "Create stack", "add"],
      [RENAME, "Rename", "edit"],
      [EDIT_INPUTS, "Edit inputs", "edit"],
      [ADD_TO_STACK, "Add to stack", "edit"],
      [COPY, "Copy block ID", "clipboard"],
      [REMOVE_BLOCK, "Remove block", "remove"],
    ]);
    // Edit inputs applies to a block whose class has inputs, a variadic one
    // among them, and to none whose class has none, as Iris's dataset block.
    const editing = (block: string) => on({ block }).some(({ id }) => id === EDIT_INPUTS);
    expect(["a", "b", "c", "e"].map(editing)).toEqual([false, true, true, true]);
    expect(rows(on({ link: "ab" }))).toEqual([
      [INSERT, "Insert block", "add"],
      [EDIT_LINK, "Edit link", "edit"],
      [REMOVE_LINK, "Remove link", "remove"],
    ]);
    expect(rows(on({ stack: "s1" }))).toEqual([
      [RENAME, "Rename", "edit"],
      [EDIT_STACK, "Edit stack", "edit"],
      [REMOVE_STACK, "Remove stack", "remove"],
    ]);
    expect(rows(on({ selection: [{ block: "b" }, { link: "ab" }] }))).toEqual([
      [CREATE_STACK, "Create stack", "add"],
      [REMOVE_SELECTED, "Remove", "remove"],
    ]);
    expect(rows(on({ selection: [{ block: "b" }, { block: "e" }] }))).toEqual([
      [CREATE_STACK, "Create stack", "add"],
      [ADD_TO_STACK, "Add to stack", "edit"],
      [REMOVE_SELECTED, "Remove", "remove"],
    ]);
    expect(on({ option: "n_rows" })).toEqual([]);
    expect(on({ plugin: "blockr.core-edit_board_plugin" })).toEqual([]);

    // Copy block ID's row shows the block's id.
    const copy = on({ block: "b" }).find(({ id }) => id === COPY);
    expect(copy?.detail?.(s.core.store.board(), { block: "b" })).toBe("b");
    // Those that add carry the plus, those that remove the bin, and the
    // others no icon.
    const icons = (target: Target) => on(target).map(({ icon }) => (icon === undefined ? "none" : icon.includes("M8 3v10M3 8h10") ? "plus" : "bin"));
    expect(icons({ board: true })).toEqual(["plus", "plus", "plus"]);
    expect(icons({ block: "e" })).toEqual(["plus", "plus", "plus", "plus", "none", "none", "none", "none", "bin"]);
    expect(icons({ link: "ab" })).toEqual(["plus", "none", "bin"]);
    expect(icons({ stack: "s1" })).toEqual(["none", "none", "bin"]);
    expect(icons({ selection: [{ block: "b" }] })).toEqual(["plus", "bin"]);
    s.stop();
  });

  test("offer nothing for a block, a link or a stack that the board does not hold, or a selection that holds none that it does", async () => {
    const s = await stubCore();
    const on = (target: Target) => s.core.boardActions.on(target);

    expect(on({ block: "nowhere" })).toEqual([]);
    expect(on({ link: "nowhere" })).toEqual([]);
    expect(on({ stack: "nowhere" })).toEqual([]);
    expect(on({ selection: [] })).toEqual([]);
    expect(on({ selection: [{ block: "nowhere" }, { option: "n_rows" }, { plugin: "blockr.core-edit_board_plugin" }] })).toEqual([]);
    s.stop();
  });

  test("but Copy block ID fail their offers under the board lock, so that trigger() runs none of them there", async () => {
    const s = await stubCore({ hello: { ...HELLO, lock: { board: true, load: false } } });
    const on = (target: Target) => s.core.boardActions.on(target);
    const board = s.core.store.board();

    expect(rows(on({ block: "b" }))).toEqual([[COPY, "Copy block ID", "clipboard"]]);
    expect(on({ link: "ab" })).toEqual([]);
    expect(on({ stack: "s1" })).toEqual([]);
    expect(on({ selection: [{ block: "b" }] })).toEqual([]);
    expect(on({ board: true })).toEqual([]);
    // Add to stack fails it as well, on a block that no stack holds.
    expect(rows(on({ block: "e" }))).toEqual([[COPY, "Copy block ID", "clipboard"]]);
    expect(on({ selection: [{ block: "e" }] })).toEqual([]);

    await s.core.boardActions.trigger(REMOVE_BLOCK, { block: "b" }, { el });
    await s.core.boardActions.trigger(REMOVE_SELECTED, { selection: [{ block: "b" }, { link: "ab" }] }, { el });
    expect(s.core.store.board()).toBe(board);
    s.stop();
  });
});

describe("Rename", () => {
  test("renames a block or a stack through its quick action, a text row that holds the name, in one step of the user", async () => {
    const s = await stubCore();
    const board = s.core.store.board();

    const menu = s.menu({ block: "b" });
    await menu.pick("Rename");
    expect(menu.level()).toEqual({
      label: "Rename",
      advanced: false,
      back: true,
      loading: false,
      rows: [{ kind: "text", label: "Name", value: "Head" }],
    });
    menu.type("  First rows  ", "Name");
    await menu.pick("Name");

    expect(menu.closed()).toBe(true);
    expect(s.core.store.board().blocks.b?.name).toBe("First rows");
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    // Through trigger(), the quick action is the menu's first level, so that
    // Escape would close the menu there.
    const renamed = s.core.boardActions.trigger(RENAME, { stack: "s1" }, { el });
    const stack = s.menu();
    expect(stack?.level()).toMatchObject({ back: false, rows: [{ kind: "text", label: "Name", value: "Input" }] });
    stack?.type("Sources", "Name");
    await stack?.pick("Name");
    await renamed;
    expect(s.core.store.board().stacks.s1?.name).toBe("Sources");
    s.stop();
  });

  test("refuses an empty name in place, and runs nothing for the name as it was", async () => {
    const s = await stubCore();
    const menu = s.menu({ stack: "s1" });
    await menu.pick("Rename");

    menu.type("   ", "Name");
    await menu.pick("Name");
    expect(menu.level()?.rows).toEqual([{ kind: "text", label: "Name", value: "   ", refusal: "A name cannot be empty." }]);
    expect(menu.closed()).toBe(false);

    menu.type("Input ", "Name");
    expect(menu.level()?.rows).toEqual([{ kind: "text", label: "Name", value: "Input " }]);
    await menu.pick("Name");
    expect(menu.closed()).toBe(true);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });
});

describe("Copy block ID", () => {
  test("puts the block's id on the clipboard", async () => {
    const s = await stubCore();

    await s.core.boardActions.trigger(COPY, { block: "b" }, { el });

    expect(await navigator.clipboard.readText()).toBe("b");
    expect(held(s.core, (n) => n.source.kind === "core")).toEqual([]);
    s.stop();
  });

  test("copies through the document's copy command where the clipboard API refuses to write", async () => {
    const s = await stubCore();
    refusingClipboard();
    const copied = copyCommand(true);

    await s.core.boardActions.trigger(COPY, { block: "c" }, { el });

    expect(copied).toEqual(["copy c"]);
    // The field that it copied from is gone again.
    expect(document.querySelector("textarea")).toBeNull();
    expect(held(s.core, (n) => n.source.kind === "core")).toEqual([]);
    s.stop();
  });

  test("copies through the document's copy command where the page has no clipboard API", async () => {
    const s = await stubCore();
    const none = vi.spyOn(navigator, "clipboard", "get").mockReturnValue(undefined as unknown as Clipboard);
    onTestFinished(() => {
      none.mockRestore();
    });
    const copied = copyCommand(true);

    await s.core.boardActions.trigger(COPY, { block: "d" }, { el });

    expect(copied).toEqual(["copy d"]);
    s.stop();
  });

  test("raises copy_failed about the block where the browser refuses both, and resolves", async () => {
    const s = await stubCore();
    refusingClipboard();
    copyCommand(false);

    await expect(s.core.boardActions.trigger(COPY, { block: "b" }, { el })).resolves.toBeUndefined();

    expect(held(s.core, (n) => notificationCode(n) === "copy_failed")).toEqual([
      {
        severity: "error",
        message: 'The browser did not let core copy the block ID "b".',
        source: { kind: "core", code: "copy_failed" },
        about: { block: "b" },
      },
    ]);
    s.stop();
  });
});

describe("the board actions that remove", () => {
  test("remove a block with its links and its place in its stack, a link and a stack, each in one step of the user", async () => {
    const s = await stubCore();
    const board = s.core.store.board();

    await s.core.boardActions.trigger(REMOVE_BLOCK, { block: "b" }, { el });
    expect(objects(s.core)).toEqual({ blocks: ["a", "c", "d", "e", "f"], links: ["ac", "ad", "de", "keen_finch"], stacks: ["s1"] });
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a"]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    await s.core.boardActions.trigger(REMOVE_LINK, { link: "ab" }, { el });
    expect(objects(s.core).links).toEqual(["ac", "bc", "ad", "be", "de", "keen_finch"]);
    s.core.store.undo();

    await s.core.boardActions.trigger(REMOVE_STACK, { stack: "s1" }, { el });
    expect(objects(s.core).stacks).toEqual([]);
    expect(objects(s.core).blocks).toEqual(Object.keys(board.blocks));
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    s.stop();
  });

  test("remove every block, link and stack of a selection in one step, a link of a removed block among them, and leave out what else it holds", async () => {
    const s = await stubCore();
    const board = s.core.store.board();
    s.core.selection.set([{ link: "ab" }, { block: "b" }, { link: "ad" }, { stack: "s1" }, { option: "n_rows" }]);

    await s.core.boardActions.trigger(REMOVE_SELECTED, { selection: s.core.selection.get() }, { el });

    expect(objects(s.core)).toEqual({ blocks: ["a", "c", "d", "e", "f"], links: ["ac", "de", "keen_finch"], stacks: [] });
    expect(Object.hasOwn(s.core.store.board().options, "n_rows")).toBe(true);
    // What left the board leaves the selection, and the board option stays.
    expect(s.core.selection.get()).toEqual([{ option: "n_rows" }]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });
});
