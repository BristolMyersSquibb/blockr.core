// Core's board actions that add a block, on the board of the fixture hello
// through the CoreApi for tests: Add block, Append block, Prepend block and
// Insert block, through a row of their quick actions and through their
// advanced action, the block picker, in core's dialog and in a plugin's room.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f), and its stack s1, Input, holds a and b. Links feed every input of
// b and c.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import {
  find,
  labelled,
  listed,
  optionFromLabel,
  optionLabels,
  picked,
  selectButton,
  selectShows,
  step,
  type,
} from "@blockr/ui/stubs";
import type { HelloReply } from "../protocol/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { held } from "../testing/notifications.ts";
import type { Board, Link } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { idText, interned } from "../util/index.ts";
import type { Target } from "./actions.ts";

// Core's board actions that add a block, each by its package and its id, as a
// caller names one.
const ADD = interned("blockr.core", "add_block_action");
const APPEND = interned("blockr.core", "append_block_action");
const PREPEND = interned("blockr.core", "prepend_block_action");
const INSERT = interned("blockr.core", "insert_block_action");

// Core's commands that the tests run as edits of their own.
const REMOVE_BLOCK = interned("blockr.core", "remove_block");
const ADD_LINK = interned("blockr.core", "add_link");
const REMOVE_LINK = interned("blockr.core", "remove_link");

// What trigger() takes: a board action by its package and its id, or a role by
// its id.
type Triggered = Parameters<CoreApi["boardActions"]["trigger"]>[0];

// The element that the user triggers from, which a trigger() needs.
const el = document.body;

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) stop();
});

// The CoreApi for tests on the board of the fixture hello, or with the options
// given, which stops as the test ends, closing core's dialog where it is open.
async function stubbed(options: StubCoreOptions = {}) {
  const s = await act(() => stubCore(options));
  stops.push(() => {
    act(() => {
      s.stop();
    });
  });
  return s;
}

// The blocks that the board holds beside those of the fixture's board.
function added(board: Board): [string, Board["blocks"][string]][] {
  return Object.entries(board.blocks).filter(([id]) => !Object.hasOwn(HELLO.board.board.blocks, id));
}

// The links that the board holds into or out of the block given.
function linksOf(board: Board, block: string): [string, Link][] {
  return Object.entries(board.links).filter(([, link]) => link.from === block || link.to === block);
}

// Each board action that on() gives the target, by its package and its id.
function ids(s: Awaited<ReturnType<typeof stubbed>>, target: Target): { package: string; id: string }[] {
  return s.core.boardActions.on(target).map((action) => ({ package: action.package, id: action.id }));
}

// The entry of core's block class with the id given, in the block registry of
// the fixture hello.
function coreClass(id: string): RegistryEntry {
  const entry = HELLO.block_registry.find((each) => each.package === "blockr.core" && each.id === id);
  if (entry === undefined) throw new Error(`The fixture's registry holds no ${id} of blockr.core.`);
  return entry;
}

// Opens the quick action of the board action on the target, as a button
// does, or of the one that plays the role given, and returns its menu, with
// the promise of trigger().
function triggered(s: Awaited<ReturnType<typeof stubbed>>, id: Triggered, target: Target) {
  const done = s.core.boardActions.trigger(id, target, { el });
  const menu = s.menu();
  const named = typeof id === "string" ? `the role "${id}"` : `the board action "${idText(id)}"`;
  if (menu === undefined) throw new Error(`Triggering ${named} opened no menu.`);
  return { menu, done };
}

// Opens the advanced action of the board action on the target, the picker,
// through the open icon of its quick action, after typing the filter's text
// given, and returns the element in which core's dialog shows it.
async function picker(s: Awaited<ReturnType<typeof stubbed>>, id: Triggered, target: Target, filter = ""): Promise<HTMLElement> {
  const { menu } = triggered(s, id, target);
  menu.type(filter);
  await step(() => {
    void menu.openAdvanced();
  });
  const shown = s.advanced();
  if (shown === undefined) throw new Error("Core's dialog shows no advanced action.");
  return shown.el;
}

// Picks the option of the picker's searchable dropdown of block classes,
// through its search.
async function chosen(form: ParentNode, search: string, option: string): Promise<void> {
  const field = (): HTMLInputElement => labelled(form, "Block class");
  await step(() => {
    field().click();
  });
  await step(() => {
    type(field(), search);
  });
  await step(() => {
    optionFromLabel(field(), option).click();
  });
}

// Clicks the button whose text is the one given, and lets what it set off
// run, as the commands that the picker hands done().
async function click(within: ParentNode, text: string): Promise<void> {
  const button = [...within.querySelectorAll("button")].find((b) => b.textContent.trim() === text);
  if (button === undefined) throw new Error(`Found no button ${text}.`);
  await act(async () => {
    button.click();
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
}

// What a banner says of why the store or the command registry refused the
// block: the sentence that names the edit, then each reason.
function refusals(within: ParentNode): string[][] {
  return [...within.querySelectorAll('[role="alert"]')].map((alert) => [...alert.querySelectorAll("p, li")].map((line) => line.textContent));
}

// The classes of core's registry under their categories, as the quick actions
// that add a block list them, and as the picker does.
const ALL = [
  ["input", "data upload block"],
  ["input", "dataset block"],
  ["plot", "scatter plot block"],
  ["transform", "head/tail block"],
  ["transform", "merge block"],
  ["transform", "rbind block"],
  ["transform", "subset block"],
];
const TAKING = ALL.filter(([category]) => category !== "input");

describe("the quick actions of the board actions that add a block", () => {
  test("list a row for each block class under the heading of its category, with the class's icon and its package, which the filter narrows by the label and the package, and offer the open icon", async () => {
    const s = await stubbed();
    const { menu } = triggered(s, ADD, { board: true });

    const level = menu.level();
    expect(level).toMatchObject({ label: "Add block", filter: "", advanced: true, back: false, loading: false });
    expect(level?.rows.map((row) => ("heading" in row ? [row.heading, row.label] : []))).toEqual(ALL);
    expect(level?.rows[1]).toEqual({
      kind: "item",
      label: "dataset block",
      heading: "input",
      icon: coreClass("dataset_block").icon,
      detail: "blockr.core",
    });

    menu.type("scatter");
    expect(menu.level()?.rows.map((row) => row.label)).toEqual(["scatter plot block"]);
    menu.type("core");
    expect(menu.level()?.rows).toHaveLength(ALL.length);
    menu.escape();
    expect(menu.closed()).toBe(true);
  });

  test("of Append block and Insert block list the classes that have an input, and that of Prepend block every class", async () => {
    const s = await stubbed();
    const headings = (id: Triggered, target: Target): string[][] => {
      const { menu } = triggered(s, id, target);
      const rows = menu.level()?.rows.map((row) => ("heading" in row ? [row.heading ?? "", row.label] : [])) ?? [];
      menu.escape();
      return rows;
    };

    expect(headings(APPEND, { block: "b" })).toEqual(TAKING);
    expect(headings(INSERT, { link: "ab" })).toEqual(TAKING);
    expect(headings(PREPEND, { block: "e" })).toEqual(ALL);
  });
});

describe("Add block", () => {
  test("adds a block of the class that the user picks, with an id that core mints and the class's name, in one step of the user", async () => {
    const s = await stubbed({ blockrOptions: { rand_id: "letters_8" } });
    const board = s.core.store.board();

    const { menu, done } = triggered(s, ADD, { board: true });
    await menu.pick("dataset block");
    await done;

    const [first] = added(s.core.store.board());
    expect(first?.[0]).toMatch(/^[a-z]{8}$/);
    expect(first?.[1]).toMatchObject({ registry_id: { package: "blockr.core", id: "dataset_block" }, name: "Dataset" });
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
  });
});

describe("Append block", () => {
  test("adds a block that takes the block's result through its first input, or without a name into a variadic block, each in one step of the user", async () => {
    const s = await stubbed();
    const board = s.core.store.board();

    await triggered(s, APPEND, { block: "b" }).menu.pick("merge block");
    const [merge] = added(s.core.store.board());
    expect(merge?.[1]).toMatchObject({ registry_id: { package: "blockr.core", id: "merge_block" }, name: "Merge" });
    expect(linksOf(s.core.store.board(), merge?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: "b", to: merge?.[0], input: "x" }]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    await triggered(s, APPEND, { block: "b" }).menu.pick("rbind block");
    const [bind] = added(s.core.store.board());
    expect(linksOf(s.core.store.board(), bind?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: "b", to: bind?.[0], input: "" }]);
  });

  test("runs nothing for a row that the user picks once the board no longer holds the block", async () => {
    const s = await stubbed();
    const { menu, done } = triggered(s, APPEND, { block: "f" });
    await s.core.commands.run(REMOVE_BLOCK, { id: "f" });
    const board = s.core.store.board();

    await menu.pick("merge block");
    await done;
    expect(menu.closed()).toBe(true);
    expect(s.core.store.board()).toBe(board);
  });
});

describe("Prepend block", () => {
  test("applies to a block with an input that no link feeds, and adds a block that feeds its first free input, or a variadic block's without a name", async () => {
    const s = await stubbed();
    expect(ids(s, { block: "c" })).not.toContainEqual(PREPEND);

    // With no link into y, the merge block takes one there.
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });
    expect(ids(s, { block: "c" })).toContainEqual(PREPEND);
    await triggered(s, PREPEND, { block: "c" }).menu.pick("dataset block");
    const [first] = added(s.core.store.board());
    expect(linksOf(s.core.store.board(), first?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: first?.[0], to: "c", input: "y" }]);
    expect(ids(s, { block: "c" })).not.toContainEqual(PREPEND);

    await triggered(s, PREPEND, { block: "e" }).menu.pick("data upload block");
    const upload = added(s.core.store.board()).find(([, block]) => block.registry_id === interned("blockr.core", "upload_block"));
    expect(linksOf(s.core.store.board(), upload?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: upload?.[0], to: "e", input: "" }]);
  });

  test("applies to an input that no link feeds, and adds a block that feeds that input, or the next place of a variadic block without a name, each in one step of the user", async () => {
    const s = await stubbed();
    const on = (target: Target): boolean => ids(s, target).some(({ package: pkg, id }) => pkg === PREPEND.package && id === PREPEND.id);
    expect(on({ block: "c", input: "y" })).toBe(false);

    // With no link into x or y, the merge block takes one into y, the input
    // that the target names, rather than x, its first free one.
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });
    const board = s.core.store.board();
    expect(on({ block: "c", input: "y" })).toBe(true);
    await triggered(s, PREPEND, { block: "c", input: "y" }).menu.pick("dataset block");
    const [first] = added(s.core.store.board());
    expect(linksOf(s.core.store.board(), first?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: first?.[0], to: "c", input: "y" }]);
    expect(on({ block: "c", input: "y" })).toBe(false);
    expect(on({ block: "c", input: "x" })).toBe(true);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    await triggered(s, PREPEND, { block: "e", input: "" }).menu.pick("data upload block");
    const [upload] = added(s.core.store.board());
    expect(linksOf(s.core.store.board(), upload?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: upload?.[0], to: "e", input: "" }]);

    // Merge has no next place, Bind no input but its next place, and neither
    // an input z.
    for (const target of [{ block: "c", input: "" }, { block: "e", input: "x" }, { block: "c", input: "z" }]) expect(on(target)).toBe(false);
  });

  test("on an input, has the command refuse a row that the user picks once a link has come to feed the input, rather than feed another input", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });
    const { menu, done } = triggered(s, PREPEND, { block: "c", input: "y" });
    await s.core.commands.run(ADD_LINK, { from: "b", to: "c", input: "y" });
    const board = s.core.store.board();

    await expect(menu.pick("dataset block")).rejects.toThrow("refused");
    await expect(done).rejects.toThrow("refused");
    expect(s.core.store.board()).toBe(board);
    expect(held(s.core).map(({ source, message }) => [source.kind === "core" ? source.code : undefined, message])).toEqual([
      ["board_edit_failed", expect.stringContaining('feeds the input "y" of block "c" already') as string],
    ]);
  });

  test("plays prepend_block_role on an input, so that a caller that triggers the role on an input feeds that input", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });

    const { menu, done } = triggered(s, "prepend_block_role", { block: "c", input: "y" });
    expect(menu.level()).toMatchObject({ label: "Prepend block" });
    await menu.pick("dataset block");
    await done;

    const [first] = added(s.core.store.board());
    expect(linksOf(s.core.store.board(), first?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: first?.[0], to: "c", input: "y" }]);
  });
});

describe("Insert block", () => {
  test("puts a block on the link, which then comes from the new block and keeps its id, its input and its place among the links, in one step of the user", async () => {
    const s = await stubbed();
    const board = s.core.store.board();

    await triggered(s, INSERT, { link: "ab" }).menu.pick("subset block");

    const now = s.core.store.board();
    const [subset] = added(now);
    const block = subset?.[0] ?? "";
    expect(subset?.[1]).toMatchObject({ registry_id: { package: "blockr.core", id: "subset_block" }, name: "Subset" });
    expect(Object.keys(now.links)[0]).toBe("ab");
    expect(now.links.ab).toEqual({ from: block, to: "b", input: "data" });
    expect(linksOf(now, block).map(([, link]) => link)).toEqual([
      { from: block, to: "b", input: "data" },
      { from: "a", to: block, input: "data" },
    ]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
  });
});

describe("under the board lock", () => {
  test("none of them applies, so that trigger() runs none of them", async () => {
    const s = await stubbed({ hello: { ...HELLO, lock: { board: true, load: false } } });
    expect(ids(s, { board: true })).toEqual([]);
    expect(ids(s, { block: "e" })).toEqual([{ package: "blockr.core", id: "copy_block_id_action" }]);
    expect(ids(s, { block: "e", input: "" })).toEqual([]);
    expect(ids(s, { link: "ab" })).toEqual([]);
    await s.core.boardActions.trigger(ADD, { board: true }, { el });
    expect(s.menu()).toBeUndefined();
  });
});

describe("the block picker, their advanced action,", () => {
  test("opens from the open icon in core's dialog, titled with the board action's label, with its search starting from the filter's text and the keyboard's focus in it", async () => {
    const s = await stubbed();

    const form = await picker(s, APPEND, { block: "b" }, "mer");

    expect(s.advanced()).toMatchObject({ id: APPEND, title: "Append block", target: { block: "b" } });
    const classes = labelled(form, "Block class");
    expect(classes.value).toBe("mer");
    expect(document.activeElement).toBe(classes);
    // Opening the list keeps the search, since the field shows no pick.
    await step(() => {
      classes.click();
    });
    expect(optionLabels(classes)).toEqual(["transform: merge block"]);
    // A block added after another takes its result, so the picker offers
    // the classes that have an input.
    await step(() => {
      type(classes, "");
    });
    expect(optionLabels(classes)).toEqual(TAKING.map(([category, name]) => `${category ?? ""}: ${name ?? ""}`));
  });

  test("adds a block after a block, before a block and on a link once the user submits it, with its link in one step, under the id that the user gives or one that it mints, and closes", async () => {
    const s = await stubbed();
    const board = s.core.store.board();

    // After Head, into the input that the user picks.
    let form = await picker(s, APPEND, { block: "b" });
    await chosen(form, "merge", "merge block");
    expect(added(s.core.store.board())).toEqual([]);
    await picked(selectButton(form, "Input of the new merge block"), step, "y");
    await step(() => {
      type(labelled(form, "ID"), "joined");
    });
    await click(form, "Add block");
    expect(s.core.store.board().blocks.joined).toMatchObject({ registry_id: { package: "blockr.core", id: "merge_block" }, name: "Merge" });
    expect(linksOf(s.core.store.board(), "joined").map(([, link]) => link)).toEqual([{ from: "b", to: "joined", input: "y" }]);
    expect(s.advanced()).toBeUndefined();
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);

    // After Iris, into a variadic block under the name that the user gives
    // its input.
    form = await picker(s, APPEND, { block: "a" });
    await chosen(form, "rbind", "rbind block");
    await step(() => {
      type(labelled(form, "Input name of the new rbind block"), "first");
      type(labelled(form, "ID"), "bound");
    });
    await click(form, "Add block");
    expect(linksOf(s.core.store.board(), "bound").map(([, link]) => link)).toEqual([{ from: "a", to: "bound", input: "first" }]);
    s.core.store.undo();

    // Before Bind, under the name that the user gives the input.
    form = await picker(s, PREPEND, { block: "e" });
    await chosen(form, "dataset", "dataset block");
    await step(() => {
      type(labelled(form, 'Input name of "Bind"'), "extra");
    });
    await click(form, "Add block");
    const [first] = added(s.core.store.board());
    expect(first?.[0]).toMatch(/^[a-z]+_[a-z]+$/);
    expect(linksOf(s.core.store.board(), first?.[0] ?? "").map(([, link]) => link)).toEqual([{ from: first?.[0], to: "e", input: "extra" }]);
    s.core.store.undo();

    // On the link from Iris to Head.
    form = await picker(s, INSERT, { link: "ab" });
    await chosen(form, "subset", "subset block");
    await step(() => {
      type(labelled(form, "ID"), "between");
    });
    await click(form, "Add block");
    const now = s.core.store.board();
    expect(now.links.ab).toEqual({ from: "between", to: "b", input: "data" });
    expect(linksOf(now, "between").map(([, link]) => link)).toEqual([
      { from: "between", to: "b", input: "data" },
      { from: "a", to: "between", input: "data" },
    ]);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
  });

  test("before a block, takes the link into the input that the user picks, where several of the block's inputs are free, and into the one free input without asking", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });

    let form = await picker(s, PREPEND, { block: "c" });
    expect(form.querySelectorAll('[role="combobox"]')).toHaveLength(1);
    await chosen(form, "dataset", "dataset block");
    await picked(selectButton(form, 'Input of "Merge"'), step, "y");
    await step(() => {
      type(labelled(form, "ID"), "first");
    });
    await click(form, "Add block");
    expect(linksOf(s.core.store.board(), "first").map(([, link]) => link)).toEqual([{ from: "first", to: "c", input: "y" }]);

    // A link feeds y now, so the new link goes into x, with the block class
    // as the one thing to pick.
    form = await picker(s, PREPEND, { block: "c" });
    expect(() => selectButton(form, 'Input of "Merge"')).toThrow();
    await chosen(form, "dataset", "dataset block");
    await step(() => {
      type(labelled(form, "ID"), "second");
    });
    await click(form, "Add block");
    expect(linksOf(s.core.store.board(), "second").map(([, link]) => link)).toEqual([{ from: "second", to: "c", input: "x" }]);
  });

  test("before an input, takes the link into that input, which it names in a line in place of the field of the input, and before the next place of a variadic block, asks for the name of the input and its place", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });

    let form = await picker(s, PREPEND, { block: "c", input: "y" });
    expect(s.advanced()).toMatchObject({ id: PREPEND, title: "Prepend block", target: { block: "c", input: "y" } });
    expect(form.textContent).toContain('The new block feeds the input "y" of "Merge".');
    expect(() => selectButton(form, 'Input of "Merge"')).toThrow();
    await chosen(form, "dataset", "dataset block");
    await step(() => {
      type(labelled(form, "ID"), "first");
    });
    await click(form, "Add block");
    expect(linksOf(s.core.store.board(), "first").map(([, link]) => link)).toEqual([{ from: "first", to: "c", input: "y" }]);

    // Before the next place of Bind, the picker asks for the name of the input
    // and its place among Bind's links, be and de, last until the user picks
    // another, as before Bind.
    form = await picker(s, PREPEND, { block: "e", input: "" });
    expect(form.textContent).not.toContain("The new block feeds");
    expect(await listed(selectButton(form, "Position"), step)).toEqual(["1", "2", "3"]);
    expect(selectShows(selectButton(form, "Position"))).toBe("3");
    await chosen(form, "dataset", "dataset block");
    await step(() => {
      type(labelled(form, 'Input name of "Bind"'), "extra");
      type(labelled(form, "ID"), "second");
    });
    await picked(selectButton(form, "Position"), step, "2");
    const board = s.core.store.board();
    await click(form, "Add block");
    const into = Object.entries(s.core.store.board().links).filter(([, link]) => link.to === "e");
    expect(into.map(([id, link]) => (id === "be" || id === "de" ? id : link))).toEqual(["be", { from: "second", to: "e", input: "extra" }, "de"]);
    // One undo takes back the block, its link and the link's move.
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
  });

  test("before an input, says why it adds none where another edit feeds the input meanwhile", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "ac" });
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });
    const form = await picker(s, PREPEND, { block: "c", input: "y" });
    await chosen(form, "dataset", "dataset block");

    // A link into y, while x stays free.
    await act(() => s.core.commands.run(ADD_LINK, { from: "b", to: "c", input: "y" }));

    expect(form.textContent).toContain('A link feeds the input "y" of "Merge" already.');
    const submit = find(form, 'button[type="submit"]', HTMLButtonElement);
    expect(submit.disabled).toBe(true);
    expect(added(s.core.store.board())).toEqual([]);
  });

  test("before a block, says why it adds none where another edit feeds the block's last free input meanwhile", async () => {
    const s = await stubbed();
    await s.core.commands.run(REMOVE_LINK, { id: "bc" });
    const form = await picker(s, PREPEND, { block: "c" });
    await chosen(form, "dataset", "dataset block");

    await act(() => s.core.commands.run(ADD_LINK, { from: "b", to: "c", input: "y" }));

    expect(form.textContent).toContain('Every input of "Merge" takes a link already.');
    const submit = find(form, 'button[type="submit"]', HTMLButtonElement);
    expect(submit.disabled).toBe(true);
    expect(added(s.core.store.board())).toEqual([]);
  });

  test("says below its button why the store refused the block, keeps what the user gave and stays open, and core raises no notification of it", async () => {
    // A class whose new block starts with a state that its schema refuses.
    const head = coreClass("head_block");
    const hello: HelloReply = {
      ...HELLO,
      block_registry: [
        ...HELLO.block_registry,
        { ...head, package: "blockr.test", id: "broken_block", name: "broken block", state: { ...head.state, n: "ten" } },
      ],
    };
    const s = await stubbed({ hello });
    const board = s.core.store.board();

    const form = await picker(s, ADD, { board: true });
    await chosen(form, "broken", "broken block");
    await click(form, "Add block");

    expect(refusals(form)).toEqual([["Could not add the block.", expect.stringMatching(/^The state of the new block fails its schema at \/n/) as string]]);
    expect(find(form, '[role="alert"]').previousElementSibling?.textContent).toBe("Add block");
    expect(labelled(form, "Block class").value).toBe("broken block");
    expect(s.advanced()?.closed()).toBe(false);
    expect(s.core.store.board()).toBe(board);
    expect(held(s.core)).toEqual([]);
  });

  test("in a plugin's room takes no focus as it first shows, and takes it again once the user has added a block from it", async () => {
    const s = await stubbed();
    const room = document.createElement("div");
    document.body.append(room);
    let unmount = (): void => undefined;
    await step(() => {
      unmount = s.core.boardActions.mountAdvanced(room, ADD, { board: true });
    });
    expect(document.activeElement).toBe(document.body);

    await chosen(room, "dataset", "dataset block");
    await step(() => {
      find(room, 'button[type="submit"]').focus();
    });
    await click(room, "Add block");

    expect(added(s.core.store.board())).toHaveLength(1);
    // Core rendered the picker afresh, ready for the next block.
    expect(labelled(room, "Block class").value).toBe("");
    expect(document.activeElement).toBe(labelled(room, "Block class"));
    await step(() => {
      unmount();
    });
    expect(room.childNodes).toHaveLength(0);
  });
});
