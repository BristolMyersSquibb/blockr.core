// Core's board actions that ask the user nothing, on the board of the fixture
// hello through the CoreApi for tests: which targets they apply to, under the
// board lock too, Copy block ID with the clipboard API, the document's copy
// command and neither, and those that remove a block, a link, a stack and what
// a selection holds.

import { describe, expect, onTestFinished, test, vi } from "vitest";
import { HELLO, stubCore } from "../stubs/index.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BoardAction, Target } from "./actions.ts";

const COPY = "blockr.core-copy_block_id_action";
const REMOVE_BLOCK = "blockr.core-remove_block_action";
const REMOVE_LINK = "blockr.core-remove_link_action";
const REMOVE_STACK = "blockr.core-remove_stack_action";
const REMOVE_SELECTED = "blockr.core-remove_selected_action";

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
  test("are Copy block ID and Remove block on a block, Remove link on a link, Remove stack on a stack and Remove on a selection, in the order of their table", async () => {
    const s = await stubCore();
    const on = (target: Target) => s.core.boardActions.on(target);

    expect(rows(on({ block: "b" }))).toEqual([
      [COPY, "Copy block ID", "clipboard"],
      [REMOVE_BLOCK, "Remove block", "remove"],
    ]);
    expect(rows(on({ link: "ab" }))).toEqual([[REMOVE_LINK, "Remove link", "remove"]]);
    expect(rows(on({ stack: "s1" }))).toEqual([[REMOVE_STACK, "Remove stack", "remove"]]);
    expect(rows(on({ selection: [{ block: "b" }, { link: "ab" }] }))).toEqual([[REMOVE_SELECTED, "Remove", "remove"]]);
    expect(on({ board: true })).toEqual([]);
    expect(on({ option: "n_rows" })).toEqual([]);
    expect(on({ plugin: "blockr.core-edit_board_plugin" })).toEqual([]);

    // Copy block ID's row shows the block's id.
    const [copy] = on({ block: "b" });
    expect(copy?.detail?.(s.core.store.board(), { block: "b" })).toBe("b");
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

    await s.core.boardActions.trigger(REMOVE_BLOCK, { block: "b" });
    await s.core.boardActions.trigger(REMOVE_SELECTED, { selection: [{ block: "b" }, { link: "ab" }] });
    expect(s.core.store.board()).toBe(board);
    s.stop();
  });
});

describe("Copy block ID", () => {
  test("puts the block's id on the clipboard", async () => {
    const s = await stubCore();

    await s.core.boardActions.trigger(COPY, { block: "b" });

    expect(await navigator.clipboard.readText()).toBe("b");
    expect(held(s.core, (n) => n.source.kind === "core")).toEqual([]);
    s.stop();
  });

  test("copies through the document's copy command where the clipboard API refuses to write", async () => {
    const s = await stubCore();
    refusingClipboard();
    const copied = copyCommand(true);

    await s.core.boardActions.trigger(COPY, { block: "c" });

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

    await s.core.boardActions.trigger(COPY, { block: "d" });

    expect(copied).toEqual(["copy d"]);
    s.stop();
  });

  test("raises copy_failed about the block where the browser refuses both, and resolves", async () => {
    const s = await stubCore();
    refusingClipboard();
    copyCommand(false);

    await expect(s.core.boardActions.trigger(COPY, { block: "b" })).resolves.toBeUndefined();

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

    await s.core.boardActions.trigger(REMOVE_BLOCK, { block: "b" });
    expect(objects(s.core)).toEqual({ blocks: ["a", "c", "d", "e", "f"], links: ["ac", "ad", "de", "keen_finch"], stacks: ["s1"] });
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a"]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    await s.core.boardActions.trigger(REMOVE_LINK, { link: "ab" });
    expect(objects(s.core).links).toEqual(["ac", "bc", "ad", "be", "de", "keen_finch"]);
    s.core.store.undo();

    await s.core.boardActions.trigger(REMOVE_STACK, { stack: "s1" });
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

    await s.core.boardActions.trigger(REMOVE_SELECTED, { selection: s.core.selection.get() });

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
