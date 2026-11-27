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

// Core's board actions of links, each by its package and its id, as a caller
// names one.
const CREATE_LINK = { package: "blockr.core", id: "add_link_action" } as const;
const EDIT_LINK = { package: "blockr.core", id: "edit_link_action" } as const;

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

// Each board action's package and id.
function ids(actions: readonly BoardAction[]): { package: string; id: string }[] {
  return actions.map((action) => ({ package: action.package, id: action.id }));
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

// Runs core's commands given, each by its id within blockr.core, as the user's
// edits, each a step of its own.
async function edited(s: StubCore, ...edits: [string, Record<string, string>][]): Promise<void> {
  for (const [id, args] of edits) await act(() => s.core.commands.run({ package: "blockr.core", id }, args));
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

// The line above a form's fields that names what its target fixes.
function hint(form: HTMLFormElement): string | undefined {
  return form.querySelector("p")?.textContent;
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
    for (const block of ["a", "b", "c", "d", "e", "f"]) expect(on({ block })).toContainEqual(CREATE_LINK);
    expect(on({ board: true })).toContainEqual(CREATE_LINK);

    // Without Bind, every input of the board takes a link already.
    await edited(s, ["remove_block", { id: "e" }]);
    for (const block of ["a", "b", "c", "d", "f"]) expect(on({ block })).not.toContainEqual(CREATE_LINK);
    expect(on({ board: true })).not.toContainEqual(CREATE_LINK);

    // With Head's input free, any block but those downstream of Head may feed
    // it.
    await edited(s, ["remove_link", { id: "ab" }]);
    expect(on({ block: "b" })).toContainEqual(CREATE_LINK);
    expect(on({ block: "a" })).toContainEqual(CREATE_LINK);
    expect(on({ board: true })).toContainEqual(CREATE_LINK);
    // Merge is downstream of Head, so that a link from Merge into Head would
    // close a cycle, while one from Scatter would not.
    expect(on({ block: "c" })).not.toContainEqual(CREATE_LINK);
    expect(on({ block: "f" })).toContainEqual(CREATE_LINK);
  });

  test("on a block, lists the blocks that it can send to and those that it can take from, without closing a cycle, and adds the link picked in one step, under an ID that core mints", async () => {
    const s = await stubbed();
    await edited(s, ["remove_link", { id: "ab" }]);
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
    await edited(s, ["remove_link", { id: "ac" }], ["remove_link", { id: "bc" }]);

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
    // inputs. The menu was opened on Head, which the form names in place of
    // the select of the link's source.
    await step(() => {
      void menu.openAdvanced();
    });
    const form = dialogForm(s, "Add link");
    expect(hint(form)).toBe('A link from "Head".');
    expect(() => labelled(form, "From")).toThrow("Found no text field labelled From.");
    expect(labelled(form, "To").value).toBe("Merge");
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

  test("on a block, opens the form from the block, which it names in place of the select of the link's source, since the link starts or ends at the block", async () => {
    const s = await stubbed();
    const links = Object.keys(s.core.store.board().links);
    const menu = s.menu({ block: "f" });
    await menu.pick("Create link");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = dialogForm(s, "Add link");
    expect(hint(form)).toBe('A link from "Scatter".');
    expect(() => labelled(form, "From")).toThrow("Found no text field labelled From.");
    await step(() => {
      labelled(form, "To").click();
    });
    expect(optionLabels(labelled(form, "To"))).toEqual(["Bind"]);

    await chosen(form, "To", "Bind");
    await acting(() => {
      button(form, "Add link").click();
    });
    expect(s.advanced()).toBeUndefined();
    const added = Object.keys(s.core.store.board().links).filter((id) => !links.includes(id));
    expect(added.map((id) => s.core.store.board().links[id])).toEqual([{ from: "f", to: "e", input: "" }]);
  });

  test("applies to an input that no link feeds, lists the blocks that can feed it, without closing a cycle, and adds the link picked into that input in one step", async () => {
    const s = await stubbed();
    const on = (target: Target) => ids(s.core.boardActions.on(target));
    expect(on({ block: "b", input: "data" })).not.toContainEqual(CREATE_LINK);
    expect(on({ block: "c", input: "" })).not.toContainEqual(CREATE_LINK);

    await edited(s, ["remove_link", { id: "ab" }]);
    const links = Object.keys(s.core.store.board().links);
    const menu = s.menu({ block: "b", input: "data" });
    await menu.pick("Create link");
    expect(menu.level()).toMatchObject({ label: "Create link", filter: "", advanced: true, back: true });
    // Merge and Bind are downstream of Head, so that a link from either into
    // Head would close a cycle.
    expect(shown(menu.level())).toEqual([
      ["Take from", "Iris", "a"],
      ["Take from", "Wide sepals", "d"],
      ["Take from", "Scatter", "f"],
    ]);
    await menu.pick("Scatter");

    expect(menu.closed()).toBe(true);
    const added = Object.keys(s.core.store.board().links).filter((id) => !links.includes(id));
    expect(added.map((id) => s.core.store.board().links[id])).toEqual([{ from: "f", to: "b", input: "data" }]);
    s.core.store.undo();
    expect(Object.keys(s.core.store.board().links)).toEqual(links);

    // Into the next place of Bind, without a name, from any block that is
    // not downstream of Bind.
    const next = s.menu({ block: "e", input: "" });
    await next.pick("Create link");
    expect(shown(next.level()).map(([, label]) => label)).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Scatter"]);
    await next.pick("Merge");
    expect(Object.values(s.core.store.board().links)).toContainEqual({ from: "c", to: "e", input: "" });
  });

  test("on an input, opens the form into that input, which it names in place of the selects of the link's target and its input, and on the next place of a variadic block, asks for the name of the input and its place", async () => {
    const s = await stubbed();
    await edited(s, ["remove_link", { id: "ac" }], ["remove_link", { id: "bc" }]);
    const links = Object.keys(s.core.store.board().links);

    const menu = s.menu({ block: "c", input: "y" });
    await menu.pick("Create link");
    await step(() => {
      void menu.openAdvanced();
    });
    let form = dialogForm(s, "Add link");
    expect(hint(form)).toBe('A link into the input "y" of "Merge".');
    expect(() => labelled(form, "To")).toThrow("Found no text field labelled To.");
    expect(() => selectButton(form, "Input")).toThrow();
    await chosen(form, "From", "Head");
    await acting(() => {
      button(form, "Add link").click();
    });
    expect(s.advanced()).toBeUndefined();
    const added = Object.keys(s.core.store.board().links).filter((id) => !links.includes(id));
    expect(added.map((id) => s.core.store.board().links[id])).toEqual([{ from: "b", to: "c", input: "y" }]);

    // On the next place of Bind, the line names Bind alone, and the form asks
    // for the name of the input and its place among Bind's links, be and de,
    // as rbind() takes its arguments by their order and by their names.
    const room = document.createElement("div");
    document.body.append(room);
    stops.push(() => {
      room.remove();
    });
    let unmount = (): void => undefined;
    await step(() => {
      unmount = s.core.boardActions.mountAdvanced(room, "add_link_role", { block: "e", input: "" });
    });
    form = find(room, 'form[aria-label="Add link"]', HTMLFormElement);
    expect(hint(form)).toBe('A link into "Bind".');
    expect(() => selectButton(form, "Input")).toThrow();
    expect(await listed(selectButton(form, "Position"), step)).toEqual(["1", "2", "3"]);
    expect(selectShows(selectButton(form, "Position"))).toBe("3");
    await chosen(form, "From", "Scatter");
    await step(() => {
      type(labelled(form, "Input name"), "plotted");
    });
    await picked(selectButton(form, "Position"), step, "1");
    const board = s.core.store.board();
    await acting(() => {
      button(form, "Add link").click();
    });
    const into = Object.keys(s.core.store.board().links).filter((id) => s.core.store.board().links[id]?.to === "e");
    expect(into.slice(1)).toEqual(["be", "de"]);
    expect(s.core.store.board().links[into[0] ?? ""]).toEqual({ from: "f", to: "e", input: "plotted" });
    // One undo takes back the link and its move.
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    await step(() => {
      unmount();
    });
  });

  test("on a block, from the open icon of a row that takes from another block, names the block as the link's target in place of the select of that end", async () => {
    const s = await stubbed();
    await edited(s, ["remove_link", { id: "ac" }], ["remove_link", { id: "bc" }]);
    const menu = s.menu({ block: "c" });
    await menu.pick("Create link");
    await menu.pick("Head");
    expect(menu.level()).toMatchObject({ title: "Into which input of Merge?", advanced: true });
    await step(() => {
      void menu.openAdvanced();
    });

    const form = dialogForm(s, "Add link");
    expect(hint(form)).toBe('A link into "Merge".');
    expect(() => labelled(form, "To")).toThrow("Found no text field labelled To.");
    expect(labelled(form, "From").value).toBe("Head");
    expect(await listed(selectButton(form, "Input"), step)).toEqual(["x", "y"]);
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
    await edited(s, ["add_link", { from: "c", to: "e" }]);
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

  test("into a variadic block, moves the link to the last place, after the block's other links, in one step", async () => {
    const s = await stubbed();
    const menu = s.menu({ link: "be" });
    await menu.pick("Edit link");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = (): HTMLFormElement => dialogForm(s, "Change link");
    expect(selectShows(selectButton(form(), "Position"))).toBe("1");

    await picked(selectButton(form(), "Position"), step, "2");
    await acting(() => {
      button(form(), "Apply").click();
    });

    const into = (): string[] => Object.keys(s.core.store.board().links).filter((id) => s.core.store.board().links[id]?.to === "e");
    expect(into()).toEqual(["de", "be"]);
    s.core.store.undo();
    expect(into()).toEqual(["be", "de"]);
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
    await act(() => s.core.commands.run({ package: "blockr.core", id: "add_link" }, { id: "ce", from: "c", to: "e" }));
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
