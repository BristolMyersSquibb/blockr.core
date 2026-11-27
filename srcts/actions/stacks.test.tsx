// Core's board actions Create stack, Edit stack and Add to stack, on the board
// of the fixture hello through the CoreApi for tests: the targets that they
// apply to, their quick actions, which a test drives by the labels of their
// rows, and the advanced actions of the first two, the forms of a stack, which
// the CoreApi for tests shows in core's dialog.
// The fixture's board holds the blocks a, Iris; b, Head; c, Merge; d, Wide
// sepals; e, Bind; and f, Scatter, and the stack s1, Input, which holds a and
// b and has no colour.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { chipLabels, find, labelled, openDialog, picked, press, step, type } from "@blockr/ui/stubs";
import { stubCore } from "../stubs/index.ts";
import type { StubCore, StubMenu } from "../stubs/index.ts";
import type { BoardAction, Target } from "./actions.ts";
import { swatch } from "./icons.ts";
import type { ShownLevel } from "./quick.ts";

const CREATE_STACK = "blockr.core-add_stack_action";
const EDIT_STACK = "blockr.core-edit_stack_action";
const ADD_TO_STACK = "blockr.core-add_to_stack_action";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) act(stop);
});

async function stubbed(): Promise<StubCore> {
  const s = await stubCore();
  stops.push(s.stop);
  return s;
}

function ids(actions: readonly BoardAction[]): string[] {
  return actions.map(({ id }) => id);
}

// The rows of a level by what each shows: its label and its detail, if any.
function shown(level: Omit<ShownLevel, "id"> | undefined): string[][] {
  return (level?.rows ?? []).map((row) => [row.label, ...("detail" in row && row.detail !== undefined ? [row.detail] : [])]);
}

// The stacks that the board holds beside Input, by their contents.
function added(s: StubCore) {
  return Object.entries(s.core.store.board().stacks)
    .filter(([id]) => id !== "s1")
    .map(([id, stack]) => ({ id, ...stack }));
}

// The form that core's dialog shows, by its name.
function dialogForm(s: StubCore, name: string): HTMLFormElement {
  const dialog = s.advanced();
  if (dialog === undefined) throw new Error("Core's dialog shows no advanced action.");
  return find(dialog.el, `form[aria-label="${name}"]`, HTMLFormElement);
}

// Opens the form of the board action that the menu's level shows, through its
// open icon.
async function opened(menu: StubMenu): Promise<void> {
  await step(() => {
    void menu.openAdvanced();
  });
}

async function acting(fn: () => void): Promise<void> {
  await step(fn);
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
}

function button(el: ParentNode, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}


describe("Create stack", () => {
  test("applies to a block, a selection that holds a block, and the board", async () => {
    const s = await stubbed();
    const on = (target: Target) => ids(s.core.boardActions.on(target));

    expect(on({ block: "c" })).toContain(CREATE_STACK);
    expect(on({ selection: [{ link: "ab" }, { block: "c" }] })).toContain(CREATE_STACK);
    expect(on({ selection: [{ link: "ab" }, { stack: "s1" }] })).not.toContain(CREATE_STACK);
    expect(on({ board: true })).toContain(CREATE_STACK);
  });

  test("on a block, asks for the name in a text row, which holds a name that no other stack has, and adds a stack of the block in a colour that no other stack has, in one step", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "c" });
    await menu.pick("Create stack");
    // It lists no stack that the board holds, since it plays add_stack_role,
    // which adds a stack, while Add to stack puts the block into one.
    expect(menu.level()).toMatchObject({ title: "New stack of Merge", advanced: true, rows: [{ kind: "text", label: "Name", value: "Stack 1" }] });
    expect(shown(menu.level())).toEqual([["Name"]]);

    await menu.pick("Name");
    expect(menu.closed()).toBe(true);
    expect(added(s)).toEqual([{ id: expect.stringMatching(/^[a-z]+_[a-z]+$/) as string, name: "Stack 1", blocks: ["c"], color: "#66c2a5" }]);
    s.core.store.undo();
    expect(added(s)).toEqual([]);
  });

  test("takes the name that the user gives, without the spaces around it, and refuses an empty one in place", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "c" });
    await menu.pick("Create stack");
    menu.type("  ", "Name");
    await menu.pick("Name");
    expect(menu.level()?.rows).toEqual([{ kind: "text", label: "Name", value: "  ", refusal: "A name cannot be empty." }]);
    expect(added(s)).toEqual([]);

    menu.type(" Outputs ", "Name");
    await menu.pick("Name");
    expect(added(s)).toMatchObject([{ name: "Outputs", blocks: ["c"] }]);
  });

  test("on a selection, adds a stack of its blocks alone, which leave the stack that held them", async () => {
    const s = await stubbed();
    const menu = s.menu({ selection: [{ block: "a" }, { link: "ab" }, { block: "b" }] });
    await menu.pick("Create stack");
    expect(menu.level()).toMatchObject({ title: "New stack of 2 blocks" });
    await menu.pick("Name");

    expect(added(s)).toMatchObject([{ name: "Stack 1", blocks: ["a", "b"] }]);
    expect(s.core.store.board().stacks.s1?.blocks).toEqual([]);
    s.core.store.undo();
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("on the board, adds a stack that holds no block, and the next one with the next name and colour", async () => {
    const s = await stubbed();
    const menu = s.menu({ board: true });
    await menu.pick("Create stack");
    expect(menu.level()?.title).toBeUndefined();
    expect(shown(menu.level())).toEqual([["Name"]]);
    await menu.pick("Name");
    expect(added(s)).toMatchObject([{ name: "Stack 1", blocks: [], color: "#66c2a5" }]);

    const next = s.menu({ board: true });
    await next.pick("Create stack");
    expect(next.level()?.rows).toEqual([{ kind: "text", label: "Name", value: "Stack 2" }]);
    await next.pick("Name");
    expect(added(s)).toMatchObject([{ name: "Stack 1" }, { name: "Stack 2", blocks: [], color: "#fc8d62" }]);
  });

  test("opens the form of a stack with the blocks given, which it names in place of the select of blocks, since the new stack takes exactly those, and which adds the stack once the user submits it and closes", async () => {
    const s = await stubbed();
    const menu = s.menu({ selection: [{ block: "c" }, { block: "f" }] });
    await menu.pick("Create stack");
    await opened(menu);
    expect(s.advanced()).toMatchObject({ id: CREATE_STACK, title: "Create stack", target: { selection: [{ block: "c" }, { block: "f" }] } });
    const form = dialogForm(s, "Add stack");
    expect(labelled(form, "Name").placeholder).toBe("Stack 1");
    expect(form.querySelector("p")?.textContent).toBe('A stack of the blocks "Merge" and "Scatter".');
    expect(() => labelled(form, "Blocks")).toThrow("Found no text field labelled Blocks.");

    // It runs nothing before the user submits it.
    await step(() => {
      type(labelled(form, "Name"), "Outputs");
    });
    expect(added(s)).toEqual([]);
    await acting(() => {
      button(form, "Add stack").click();
    });

    expect(s.advanced()).toBeUndefined();
    expect(added(s)).toMatchObject([{ name: "Outputs", blocks: ["c", "f"], color: "#66c2a5" }]);
  });

  test("names a block that it was opened on in the same way, and opened on the board, offers the select of blocks", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "c" });
    await menu.pick("Create stack");
    await opened(menu);
    expect(dialogForm(s, "Add stack").querySelector("p")?.textContent).toBe('A stack of the block "Merge".');
    await step(() => {
      s.advanced()?.close();
    });

    const room = document.createElement("div");
    let unmount = (): void => undefined;
    await step(() => {
      unmount = s.core.boardActions.mountAdvanced(room, CREATE_STACK, { board: true });
    });
    const form = find(room, 'form[aria-label="Add stack"]', HTMLFormElement);
    expect(form.querySelector("p")).toBeNull();
    expect(chipLabels(labelled(form, "Blocks"))).toEqual([]);
    act(unmount);
  });
});

describe("Edit stack", () => {
  test("gives the stack the colour picked, the stack's own first and then those of the palette that no other stack has", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"], color: "#66c2a5" }));

    const menu = s.menu({ stack: "s1" });
    await menu.pick("Edit stack");
    expect(menu.level()).toMatchObject({ label: "Edit stack", advanced: true });
    expect(shown(menu.level())).toEqual([["Colour…"], ["Blocks…"]]);
    await menu.pick("Colour…");
    expect(menu.level()).toMatchObject({ title: "Colour of Input" });
    // Plots has the palette's first colour.
    expect(shown(menu.level())).toEqual([
      ["Current", "None"],
      ["#FC8D62"],
      ["#8DA0CB"],
      ["#E78AC3"],
      ["#A6D854"],
      ["#FFD92F"],
      ["#E5C494"],
      ["#B3B3B3"],
    ]);
    await menu.pick("#8DA0CB");
    expect(s.core.store.board().stacks.s1?.color).toBe("#8da0cb");

    // The stack's own colour changes nothing.
    const again = s.menu({ stack: "s1" });
    await again.pick("Edit stack");
    await again.pick("Colour…");
    expect(shown(again.level())[0]).toEqual(["Current", "#8DA0CB"]);
    expect(shown(again.level())).not.toContainEqual(["#8DA0CB"]);
    await again.pick("Current");
    expect(again.closed()).toBe(true);
    s.core.store.undo();
    expect(s.core.store.board().stacks.s1?.color).toBeNull();
  });

  test("gives the stack the blocks ticked, of its own and those in no stack, as the menu closes", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"] }));

    const menu = s.menu({ stack: "s1" });
    await menu.pick("Edit stack");
    await menu.pick("Blocks…");
    expect(menu.level()).toMatchObject({ title: "Blocks in Input", filter: "" });
    // Scatter sits in Plots.
    expect(menu.level()?.rows).toEqual([
      {
        kind: "multi",
        label: "Blocks",
        choices: [
          { id: "a", label: "Iris", picked: true },
          { id: "b", label: "Head", picked: true },
          { id: "c", label: "Merge", picked: false },
          { id: "d", label: "Wide sepals", picked: false },
          { id: "e", label: "Bind", picked: false },
        ],
      },
    ]);
    menu.tick("Blocks", "Head");
    menu.tick("Blocks", "Bind");
    await menu.close();

    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "e"]);
    s.core.store.undo();
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("opens the form of the stack, which renames it, colours it and gives it other blocks once the user applies it, in one step, and closes", async () => {
    const s = await stubbed();
    const menu = s.menu({ stack: "s1" });
    await menu.pick("Edit stack");
    let ended: Promise<void> | undefined;
    await step(() => {
      ended = menu.openAdvanced();
    });
    expect(s.advanced()).toMatchObject({ id: EDIT_STACK, title: "Edit stack", target: { stack: "s1" } });
    const form = (): HTMLFormElement => dialogForm(s, "Change stack");
    expect(labelled(form(), "Name").value).toBe("Input");
    expect(chipLabels(labelled(form(), "Blocks"))).toEqual(["Iris", "Head"]);

    // It runs nothing before the user applies it.
    await step(() => {
      type(labelled(form(), "Name"), "Sources");
    });
    await acting(() => {
      find(form(), 'input[type="checkbox"]', HTMLInputElement).click();
    });
    await acting(() => {
      find(form(), "fieldset button", HTMLButtonElement).click();
    });
    const swatch = [...find(openDialog(), '[role="listbox"][aria-label="Palette"]').querySelectorAll<HTMLElement>('[role="option"]')][2];
    await acting(() => {
      swatch?.click();
    });
    await step(() => {
      press(find(openDialog(), '[role="listbox"][aria-label="Palette"]'), "Escape");
    });
    await picked(labelled(form(), "Blocks"), step, "Head", "Merge");
    expect(s.core.store.board().stacks.s1).toEqual({ name: "Input", blocks: ["a", "b"], color: null });
    await acting(() => {
      button(form(), "Apply").click();
    });
    await ended;

    expect(s.advanced()).toBeUndefined();
    // The picker writes its hex in capitals.
    expect(s.core.store.board().stacks.s1).toEqual({ name: "Sources", blocks: ["a", "c"], color: "#8DA0CB" });
    s.core.store.undo();
    expect(s.core.store.board().stacks.s1).toEqual({ name: "Input", blocks: ["a", "b"], color: null });
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("keeps the stack's name where the user leaves the field empty, and runs nothing where nothing changed", async () => {
    const s = await stubbed();
    const menu = s.menu({ stack: "s1" });
    await menu.pick("Edit stack");
    await step(() => {
      void menu.openAdvanced();
    });
    const form = (): HTMLFormElement => dialogForm(s, "Change stack");
    expect(labelled(form(), "Name").placeholder).toBe("Input");
    await step(() => {
      type(labelled(form(), "Name"), "  ");
    });
    await acting(() => {
      button(form(), "Apply").click();
    });

    expect(s.advanced()).toBeUndefined();
    expect(s.core.store.board().stacks.s1?.name).toBe("Input");
    expect(s.core.store.canUndo()).toBe(false);
  });
});

describe("Add to stack", () => {
  test("applies to a block or a selection that holds a block, where the board holds a stack that lacks one of the blocks", async () => {
    const s = await stubbed();
    const on = (target: Target) => ids(s.core.boardActions.on(target));

    expect(on({ block: "c" })).toContain(ADD_TO_STACK);
    expect(on({ selection: [{ link: "ab" }, { block: "a" }, { block: "c" }] })).toContain(ADD_TO_STACK);
    // Input, the board's only stack, holds a and b already.
    expect(on({ block: "a" })).not.toContain(ADD_TO_STACK);
    expect(on({ selection: [{ block: "a" }, { block: "b" }] })).not.toContain(ADD_TO_STACK);
    expect(on({ selection: [{ link: "ab" }, { stack: "s1" }] })).not.toContain(ADD_TO_STACK);
    expect(on({ stack: "s1" })).not.toContain(ADD_TO_STACK);
    expect(on({ board: true })).not.toContain(ADD_TO_STACK);

    // A board without a stack offers it on no block.
    await act(() => s.core.commands.run("blockr.core-remove_stack", { id: "s1" }));
    expect(on({ block: "c" })).not.toContain(ADD_TO_STACK);
  });

  test("lists the board's stacks, each with its colour and how many blocks it holds, and puts the block at the end of the stack picked, in one step", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"], color: "#66c2a5" }));

    const menu = s.menu({ block: "c" });
    await menu.pick("Add to stack");
    // It has no advanced action, so that its level shows no open icon.
    expect(menu.level()).toMatchObject({ title: "Add Merge to", advanced: false });
    expect(shown(menu.level())).toEqual([
      ["Input", "2 blocks"],
      ["Plots", "1 block"],
    ]);
    expect(menu.level()?.rows.map((row) => ("icon" in row ? row.icon : undefined))).toEqual([swatch(null), swatch("#66c2a5")]);

    await menu.pick("Plots");
    expect(menu.closed()).toBe(true);
    expect(s.core.store.board().stacks.plots?.blocks).toEqual(["f", "c"]);
    s.core.store.undo();
    expect(s.core.store.board().stacks.plots?.blocks).toEqual(["f"]);
  });

  test("on a selection, leaves out a stack that holds every one of its blocks, and takes the blocks out of the stack that held them", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"], color: "#66c2a5" }));

    // Input holds a, but lacks c.
    const menu = s.menu({ selection: [{ block: "a" }, { link: "ab" }, { block: "c" }] });
    await menu.pick("Add to stack");
    expect(menu.level()).toMatchObject({ title: "Add 2 blocks to" });
    expect(shown(menu.level())).toEqual([
      ["Input", "2 blocks"],
      ["Plots", "1 block"],
    ]);
    await menu.pick("Input");
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b", "c"]);

    // Input now holds both, so that only Plots is left.
    const again = s.menu({ selection: [{ block: "a" }, { block: "c" }] });
    await again.pick("Add to stack");
    expect(shown(again.level())).toEqual([["Plots", "1 block"]]);
    await again.pick("Plots");
    expect(s.core.store.board().stacks.plots?.blocks).toEqual(["f", "a", "c"]);
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["b"]);
    s.core.store.undo();
    expect(s.core.store.board().stacks.plots?.blocks).toEqual(["f"]);
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b", "c"]);
  });

  test("plays add_to_stack_role, which trigger() takes in place of its id", async () => {
    const s = await stubbed();
    const triggered = s.core.boardActions.trigger("add_to_stack_role", { block: "c" }, { el: document.body });
    expect(s.menu()?.level()).toMatchObject({ title: "Add Merge to", advanced: false });
    await s.menu()?.pick("Input");
    await triggered;
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b", "c"]);
  });
});
