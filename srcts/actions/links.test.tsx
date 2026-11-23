// Core's board actions Create link and Edit link, on the board of the fixture
// hello through the CoreApi for tests: the targets that they apply to, their
// quick actions, which a test drives by the labels of their rows, and their
// advanced actions, the forms of a link, which the CoreApi for tests shows in
// core's dialog.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f), so that Bind alone takes a new link.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { stubCore } from "../stubs/index.ts";
import type { StubCore } from "../stubs/index.ts";
import { find, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { labelled, listed, optionFromLabel, optionLabels, picked, selectButton, selectShows } from "../testing/select.ts";
import type { BoardAction, Target } from "./actions.ts";
import type { ShownLevel } from "./quick.ts";

const CREATE_LINK = "blockr.core-add_link_action";
const EDIT_LINK = "blockr.core-edit_link_action";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) act(stop);
});

// The CoreApi for tests on the board of the fixture hello, which the test
// ends.
async function stubbed(): Promise<StubCore> {
  const s = await stubCore();
  stops.push(s.stop);
  return s;
}

function ids(actions: readonly BoardAction[]): string[] {
  return actions.map(({ id }) => id);
}

// The rows of a level by what each shows: its heading, if any, its label and
// its detail, if any.
function shown(level: Omit<ShownLevel, "id"> | undefined): string[][] {
  return (level?.rows ?? []).map((row) => [
    ...("heading" in row && row.heading !== undefined ? [row.heading] : []),
    row.label,
    ...("detail" in row && row.detail !== undefined ? [row.detail] : []),
  ]);
}

// Runs the commands given as the user's edits, each a step of its own.
async function edited(s: StubCore, ...edits: [string, Record<string, string>][]): Promise<void> {
  for (const [id, args] of edits) await act(() => s.core.commands.run(id, args));
}

// The form that core's dialog shows, by its name.
function dialogForm(s: StubCore, name: string): HTMLFormElement {
  const dialog = s.advanced();
  if (dialog === undefined) throw new Error("Core's dialog shows no advanced action.");
  return find(dialog.el, `form[aria-label="${name}"]`, HTMLFormElement);
}

// Does what the user does, and lets what it set off run.
async function acting(fn: () => void): Promise<void> {
  await step(fn);
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
}

// Picks the option of a searchable dropdown, through its search.
async function chosen(el: ParentNode, label: string, search: string, option?: string): Promise<void> {
  await step(() => {
    labelled(el, label).click();
  });
  await step(() => {
    type(labelled(el, label), search);
  });
  await acting(() => {
    optionFromLabel(labelled(el, label), option ?? search).click();
  });
}

function button(el: ParentNode, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
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

// What a form says of why the store refused an edit, banner by banner.
function refusals(el: ParentNode): string[][] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => [...alert.querySelectorAll("p, li")].map((line) => line.textContent));
}

describe("Create link", () => {
  test("applies to a block that a new link can join to another, and to the board where it holds such a block", async () => {
    const s = await stubbed();
    const on = (target: Target) => ids(s.core.boardActions.on(target));

    // Each block may feed Bind, which takes any number of links.
    for (const block of ["a", "b", "c", "d", "e", "f"]) expect(on({ block })).toContain(CREATE_LINK);
    expect(on({ board: true })).toContain(CREATE_LINK);

    // Without Bind, every input of the board takes a link already.
    await edited(s, ["blockr.core-remove_block", { id: "e" }]);
    for (const block of ["a", "b", "c", "d", "f"]) expect(on({ block })).not.toContain(CREATE_LINK);
    expect(on({ board: true })).not.toContain(CREATE_LINK);

    // With Head's input free, any block but those downstream of Head may feed
    // it.
    await edited(s, ["blockr.core-remove_link", { id: "ab" }]);
    expect(on({ block: "b" })).toContain(CREATE_LINK);
    expect(on({ block: "a" })).toContain(CREATE_LINK);
    expect(on({ board: true })).toContain(CREATE_LINK);
    // Merge is downstream of Head, so that a link from Merge into Head would
    // close a cycle, while one from Scatter would not.
    expect(on({ block: "c" })).not.toContain(CREATE_LINK);
    expect(on({ block: "f" })).toContain(CREATE_LINK);
  });

  test("on a block, lists the blocks that it can send to and those that it can take from, without closing a cycle, and adds the link picked in one step, under an ID that core mints", async () => {
    const s = await stubbed();
    await edited(s, ["blockr.core-remove_link", { id: "ab" }]);
    const links = Object.keys(s.core.store.board().links);

    const menu = s.menu({ block: "b" });
    await menu.pick("Create link");
    expect(menu.level()).toMatchObject({ label: "Create link", filter: "", advanced: true, back: true });
    // Head may feed Bind, and with its input free, take from any block that
    // is not downstream of it, which Merge and Bind are.
    expect(shown(menu.level())).toEqual([
      ["Send to", "Bind", "e"],
      ["Take from", "Iris", "a"],
      ["Take from", "Wide sepals", "d"],
      ["Take from", "Scatter", "f"],
    ]);
    menu.type("scat");
    expect(shown(menu.level())).toEqual([["Take from", "Scatter", "f"]]);
    await menu.pick();

    expect(menu.closed()).toBe(true);
    const added = Object.keys(s.core.store.board().links).filter((id) => !links.includes(id));
    expect(added.map((id) => s.core.store.board().links[id])).toEqual([{ from: "f", to: "b", input: "data" }]);
    expect(added[0]).toMatch(/^[a-z]+_[a-z]+$/);
    s.core.store.undo();
    expect(Object.keys(s.core.store.board().links)).toEqual(links);
  });

  test("asks for the input where the block that the link goes into has several free ones, and whose open icon hands the form both blocks", async () => {
    const s = await stubbed();
    await edited(s, ["blockr.core-remove_link", { id: "ac" }], ["blockr.core-remove_link", { id: "bc" }]);

    const menu = s.menu({ block: "b" });
    await menu.pick("Create link");
    expect(shown(menu.level())).toEqual([
      ["Send to", "Merge", "c"],
      ["Send to", "Bind", "e"],
    ]);
    await menu.pick("Merge");
    expect(menu.level()).toMatchObject({ title: "Into which input of Merge?", advanced: true, back: true });
    expect(shown(menu.level())).toEqual([["x"], ["y"]]);

    // The open icon opens the form on Head and Merge, with Merge's free
    // inputs.
    await step(() => {
      void menu.openAdvanced();
    });
    const form = dialogForm(s, "Add link");
    expect([labelled(form, "From").value, labelled(form, "To").value]).toEqual(["Head", "Merge"]);
    expect(await listed(selectButton(form, "Input"), step)).toEqual(["x", "y"]);
    await step(() => {
      s.advanced()?.close();
    });

    const again = s.menu({ block: "b" });
    await again.pick("Create link");
    await again.pick("Merge");
    await again.pick("y");
    expect(again.closed()).toBe(true);
    expect(Object.values(s.core.store.board().links)).toContainEqual({ from: "b", to: "c", input: "y" });

    // With one input of Merge free, picking Merge adds the link at once.
    const last = s.menu({ block: "f" });
    await last.pick("Create link");
    await last.pick("Merge");
    expect(last.closed()).toBe(true);
    expect(Object.values(s.core.store.board().links)).toContainEqual({ from: "f", to: "c", input: "x" });
  });

  test("on the board, opens its advanced action at once, the form of a link from no block, which adds the link once the user submits it and closes", async () => {
    const s = await stubbed();
    const links = Object.keys(s.core.store.board().links);

    const menu = s.menu({ board: true });
    let ended: Promise<void> | undefined;
    await step(() => {
      ended = menu.pick("Create link");
    });
    expect(menu.closed()).toBe(true);
    expect(s.advanced()).toMatchObject({ id: CREATE_LINK, title: "Create link", target: { board: true } });
    const form = dialogForm(s, "Add link");
    expect([labelled(form, "From").value, labelled(form, "To").value]).toEqual(["", ""]);

    // It runs nothing before the user submits it.
    await chosen(form, "From", "Scatter");
    await chosen(form, "To", "Bind");
    expect(Object.keys(s.core.store.board().links)).toEqual(links);
    await acting(() => {
      button(form, "Add link").click();
    });
    await ended;

    expect(s.advanced()).toBeUndefined();
    const added = Object.keys(s.core.store.board().links).filter((id) => !links.includes(id));
    expect(added.map((id) => s.core.store.board().links[id])).toEqual([{ from: "f", to: "e", input: "" }]);
  });

  test("on a block, opens the form from the block, as the link's source", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "f" });
    await menu.pick("Create link");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = dialogForm(s, "Add link");
    expect([labelled(form, "From").value, labelled(form, "To").value]).toEqual(["Scatter", ""]);
    await step(() => {
      labelled(form, "To").click();
    });
    expect(optionLabels(labelled(form, "To"))).toEqual(["Bind"]);
  });
});

describe("Edit link", () => {
  test("changes the link's source to a block that can feed its input instead, without closing a cycle", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "ac" });
    await menu.pick("Edit link");
    expect(menu.level()).toMatchObject({ label: "Edit link", advanced: true, back: true });
    expect(shown(menu.level())).toEqual([["Change source…"], ["Move to input…"]]);

    await menu.pick("Change source…");
    expect(menu.level()).toMatchObject({ title: "Feed Merge from", filter: "" });
    // Any block but Iris, the link's source, and Merge.
    expect(shown(menu.level())).toEqual([
      ["Head", "b"],
      ["Wide sepals", "d"],
      ["Bind", "e"],
      ["Scatter", "f"],
    ]);
    await menu.pick("Scatter");
    expect(s.core.store.board().links.ac).toEqual({ from: "f", to: "c", input: "x" });
    s.core.store.undo();
    expect(s.core.store.board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    expect(s.core.store.canUndo()).toBe(false);

    // A block downstream of the link's target would close a cycle.
    await edited(s, ["blockr.core-add_link", { from: "c", to: "e" }]);
    const again = s.menu({ link: "ac" });
    await again.pick("Edit link");
    await again.pick("Change source…");
    expect(shown(again.level()).map(([label]) => label)).toEqual(["Head", "Wide sepals", "Scatter"]);
  });

  test("moves the link to another input of a block that takes each by its name, swapping inputs with the link that feeds it, in one step", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "ac" });
    await menu.pick("Edit link");
    await menu.pick("Move to input…");
    expect(menu.level()).toMatchObject({ title: "Move to which input of Merge?" });
    expect(shown(menu.level())).toEqual([["y", "swaps with Head"]]);
    await menu.pick("y");
    expect([s.core.store.board().links.ac, s.core.store.board().links.bc]).toEqual([
      { from: "a", to: "c", input: "y" },
      { from: "b", to: "c", input: "x" },
    ]);
    s.core.store.undo();
    expect(s.core.store.canUndo()).toBe(false);

    // A block of one input offers nothing to move to.
    const one = s.menu({ link: "ab" });
    await one.pick("Edit link");
    expect(shown(one.level())).toEqual([["Change source…"]]);
  });

  test("renames the input of a variadic block, refusing a name that another link into the block gives", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "be" });
    await menu.pick("Edit link");
    expect(shown(menu.level())).toEqual([["Change source…"], ["Rename input…"]]);
    await menu.pick("Rename input…");
    expect(menu.level()).toMatchObject({ title: "Rename input of Bind", rows: [{ kind: "text", label: "Name", value: "" }] });
    menu.type("left", "Name");
    await menu.pick("Name");
    expect(s.core.store.board().links.be).toEqual({ from: "b", to: "e", input: "left" });

    const other = s.menu({ link: "de" });
    await other.pick("Edit link");
    await other.pick("Rename input…");
    other.type(" left ", "Name");
    await other.pick("Name");
    expect(other.level()?.rows).toEqual([
      { kind: "text", label: "Name", value: " left ", refusal: 'Another link gives the input the name "left" already.' },
    ]);
    other.type("", "Name");
    await other.pick("Name");
    // The name as it was runs nothing.
    expect(other.closed()).toBe(true);
    expect(s.core.store.board().links.de).toEqual({ from: "d", to: "e", input: "" });
  });

  test("opens the form of the link, which changes the link once the user applies it, in one step, and closes", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "ac" });
    await menu.pick("Edit link");
    let ended: Promise<void> | undefined;
    await step(() => {
      ended = menu.openAdvanced();
    });
    expect(s.advanced()).toMatchObject({ id: EDIT_LINK, title: "Edit link", target: { link: "ac" } });
    const form = (): HTMLFormElement => dialogForm(s, "Change link");
    expect([labelled(form(), "From").value, labelled(form(), "To").value]).toEqual(["Iris", "Merge"]);
    // The form offers Merge's other input too, which bc feeds, and says that
    // applying it swaps the inputs.
    expect(await listed(selectButton(form(), "Input"), step)).toEqual(["x", "y"]);

    // It runs nothing before the user applies it.
    await chosen(form(), "From", "Scatter");
    await picked(selectButton(form(), "Input"), step, "y");
    expect(note(selectButton(form(), "Input"))).toBe('The link from "Head" feeds "y". Applying gives that link "x", so the two links swap their inputs.');
    expect(s.core.store.board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
    await acting(() => {
      button(form(), "Apply").click();
    });
    await ended;

    expect(s.advanced()).toBeUndefined();
    expect([s.core.store.board().links.ac, s.core.store.board().links.bc]).toEqual([
      { from: "f", to: "c", input: "y" },
      { from: "b", to: "c", input: "x" },
    ]);
    s.core.store.undo();
    expect([s.core.store.board().links.ac, s.core.store.board().links.bc]).toEqual([
      { from: "a", to: "c", input: "x" },
      { from: "b", to: "c", input: "y" },
    ]);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("into a variadic block, names the input and moves the link to the place picked, in one step", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "de" });
    await menu.pick("Edit link");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = (): HTMLFormElement => dialogForm(s, "Change link");
    expect(() => selectButton(form(), "Input")).toThrow();
    expect(selectShows(selectButton(form(), "Position"))).toBe("2");

    await step(() => {
      type(labelled(form(), "Input name"), "right");
    });
    await picked(selectButton(form(), "Position"), step, "1");
    await acting(() => {
      button(form(), "Apply").click();
    });

    const into = (): string[] => Object.keys(s.core.store.board().links).filter((id) => s.core.store.board().links[id]?.to === "e");
    expect(into()).toEqual(["de", "be"]);
    expect(s.core.store.board().links.de).toEqual({ from: "d", to: "e", input: "right" });
    s.core.store.undo();
    expect(into()).toEqual(["be", "de"]);
    expect(s.core.store.board().links.de).toEqual({ from: "d", to: "e", input: "" });
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("says below its button why the store refused the edit, naming the link, and keeps what the user gave", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "ac" });
    await menu.pick("Edit link");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = (): HTMLFormElement => dialogForm(s, "Change link");
    await chosen(form(), "From", "Bind");

    // Another edit links Merge to Bind before the user applies the form, so
    // that the link from Bind would close a cycle.
    await act(() => s.core.commands.run("blockr.core-add_link", { id: "ce", from: "c", to: "e" }));
    await acting(() => {
      button(form(), "Apply").click();
    });

    expect(refusals(form())).toEqual([['Could not change the link from "Iris" to "Merge".', 'The link would close a cycle through the blocks "c" and "e".']]);
    expect(find(form(), '[role="alert"]').previousElementSibling?.contains(button(form(), "Apply"))).toBe(true);
    expect(s.advanced()?.closed()).toBe(false);
    expect(labelled(form(), "From").value).toBe("Bind");
    expect(s.core.store.board().links.ac).toEqual({ from: "a", to: "c", input: "x" });
  });
});
