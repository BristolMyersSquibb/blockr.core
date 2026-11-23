// Core's board actions Create stack and Edit stack, on the board of the
// fixture hello through the CoreApi for tests: the targets that they apply to,
// their quick actions, which a test drives by the labels of their rows, and
// their advanced actions, the forms of a stack, which the CoreApi for tests
// shows in core's dialog.
// The fixture's board holds the blocks a, Iris; b, Head; c, Merge; d, Wide
// sepals; e, Bind; and f, Scatter, and the stack s1, Input, which holds a and
// b and has no colour.

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { stubCore } from "../stubs/index.ts";
import type { StubCore, StubMenu } from "../stubs/index.ts";
import { find, press, type } from "../testing/inputs.ts";
import { openDialog } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import { chipLabels, labelled, picked } from "../testing/select.ts";
import type { BoardAction, Target } from "./actions.ts";
import type { ShownLevel } from "./quick.ts";

const CREATE_STACK = "blockr.core-add_stack_action";
const EDIT_STACK = "blockr.core-edit_stack_action";

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

  test("on a block, lists the stacks that the block joins and New stack, which adds a stack of the block with a name and a colour that no other stack has, in one step", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "c" });
    await menu.pick("Create stack");
    expect(menu.level()).toMatchObject({ title: "Add Merge to", advanced: true });
    expect(shown(menu.level())).toEqual([["Input", "2 blocks"], ["New stack"]]);
    // Each stack shows its colour, as a square, the outline of one where it
    // has none.
    expect(menu.level()?.rows[0]).toMatchObject({ icon: expect.stringContaining('fill="none"') as string });

    await menu.pick("New stack");
    expect(added(s)).toEqual([{ id: expect.stringMatching(/^[a-z]+_[a-z]+$/) as string, name: "Stack 1", blocks: ["c"], color: "#66c2a5" }]);
    s.core.store.undo();
    expect(added(s)).toEqual([]);

    // A block that joins a stack goes last, and leaves the stack that held it.
    const join = s.menu({ block: "c" });
    await join.pick("Create stack");
    await join.pick("Input");
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b", "c"]);
  });

  test("on a selection, takes its blocks alone, and leaves out a stack that holds every one of them", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-add_stack", { id: "plots", name: "Plots", blocks: ["f"], color: "#fc8d62" }));

    const menu = s.menu({ selection: [{ block: "a" }, { link: "ab" }, { block: "b" }] });
    await menu.pick("Create stack");
    // Input holds Iris and Head already.
    expect(menu.level()).toMatchObject({ title: "Add 2 blocks to" });
    expect(shown(menu.level())).toEqual([["Plots", "1 block"], ["New stack"]]);
    expect(menu.level()?.rows[0]).toMatchObject({ icon: expect.stringContaining('fill="#fc8d62"') as string });
    await menu.pick("Plots");

    expect(s.core.store.board().stacks.plots?.blocks).toEqual(["f", "a", "b"]);
    expect(s.core.store.board().stacks.s1?.blocks).toEqual([]);
    s.core.store.undo();
    expect(s.core.store.board().stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("on the board, offers New stack alone, which adds a stack that holds no block", async () => {
    const s = await stubbed();
    const menu = s.menu({ board: true });
    await menu.pick("Create stack");
    expect(menu.level()?.title).toBeUndefined();
    expect(shown(menu.level())).toEqual([["New stack"]]);
    await menu.pick("New stack");
    expect(added(s)).toMatchObject([{ name: "Stack 1", blocks: [], color: "#66c2a5" }]);
  });

  test("opens the form of a stack with the blocks given, which adds the stack once the user submits it and closes", async () => {
    const s = await stubbed();
    const menu = s.menu({ selection: [{ block: "c" }, { block: "f" }] });
    await menu.pick("Create stack");
    await opened(menu);
    expect(s.advanced()).toMatchObject({ id: CREATE_STACK, title: "Create stack", target: { selection: [{ block: "c" }, { block: "f" }] } });
    const form = dialogForm(s, "Add stack");
    expect(labelled(form, "Name").placeholder).toBe("Stack 1");
    expect(chipLabels(labelled(form, "Blocks"))).toEqual(["Merge", "Scatter"]);

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
