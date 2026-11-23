// The advanced action of Edit inputs, one of core's board actions, on the
// board of the fixture hello through the CoreApi for tests, which shows it in
// core's dialog as a user opens it from the menu of a block, and drives it as
// the user does. It holds the user's edits until the user applies them through
// its Apply button.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f).

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCore } from "../stubs/index.ts";
import { fieldError, find, pressAndRelease, texts, type } from "../testing/inputs.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import { rows } from "../testing/rows.ts";
import { labelled, listed, picked } from "../testing/select.ts";
import type { Board, LinkId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { CommandCall } from "./actions.ts";
import { editInputs } from "./inputs.tsx";

const EDIT_INPUTS = "blockr.core-edit_inputs_action";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) act(stop);
});

// The CoreApi for tests on the board of the fixture hello, or on the hello
// given, which the test ends.
async function stubbed(hello = HELLO): Promise<StubCore> {
  const s = await act(() => stubCore({ hello }));
  stops.push(s.stop);
  return s;
}

// The advanced action of Edit inputs on the block given, which the user opens
// from the block's menu, and the element in which core's dialog shows it. It
// opens on a new CoreApi for tests, or on the one given.
// The pick resolves only once the advanced action ends, so the test does not
// wait for it.
async function opened(block: string, given?: StubCore): Promise<{ s: StubCore; core: CoreApi; el: HTMLElement; board: () => Board }> {
  const s = given ?? (await stubbed());
  const menu = s.menu({ block });
  await step(() => {
    void menu.pick("Edit inputs");
  });
  const shown = s.advanced();
  if (shown === undefined) throw new Error("Core's dialog shows no advanced action.");
  return { s, core: s.core, el: shown.el, board: () => s.core.store.board() };
}

// The advanced action of Edit inputs on the block given, rendered as a place
// renders it, with a done() that records each batch that the advanced action
// hands it and then runs the batch as one step of the user, as core's done()
// does, with the notifier given. The advanced action gets the CoreApi given,
// as where a test gives one that says the board is open.
async function rendered(s: StubCore, block: string, core: CoreApi = s.core): Promise<{ el: HTMLElement; batches: (readonly CommandCall[])[] }> {
  const el = document.createElement("div");
  document.body.append(el);
  const batches: (readonly CommandCall[])[] = [];
  let end: (() => void) | undefined;
  await act(async () => {
    const drawn = await editInputs.render({
      el,
      core,
      target: { block },
      signal: new AbortController().signal,
      done: async (calls = [], opts) => {
        batches.push(calls);
        await s.core.commands.run(calls, "user", opts);
      },
    });
    end = drawn ?? undefined;
  });
  stops.push(() => {
    act(() => {
      end?.();
    });
    el.remove();
  });
  return { el, batches };
}

// Does what the user does, and lets the commands that it runs apply.
async function acting(fn: () => void): Promise<void> {
  await step(fn);
  await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
}

// The field of the label given, as a select or a text field names it to
// assistive technology.
function field(el: ParentNode, label: string): HTMLInputElement {
  return find(el, `input[aria-label="${label}"]`, HTMLInputElement);
}

// The button whose label is the one given.
function button(el: ParentNode, label: string): HTMLButtonElement {
  return find(el, `button[aria-label="${label}"]`, HTMLButtonElement);
}

// The Apply button below the inputs.
function applyButton(el: ParentNode): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((b) => b.textContent.trim() === "Apply");
  if (found === undefined) throw new Error("Found no Apply button.");
  return found;
}

// Applies the edits, as the user does through the Apply button.
async function apply(el: ParentNode): Promise<void> {
  await acting(() => {
    applyButton(el).click();
  });
}

// Gives the input the name, as the user types it into its field.
async function named(el: ParentNode, label: string, name: string): Promise<void> {
  await step(() => {
    field(el, label).focus();
  });
  await step(() => {
    type(field(el, label), name);
  });
}

// Moves the input at the place given with the keys given, as the user drags
// it from the keyboard, from the button that drags it.
async function dragged(el: ParentNode, place: number, keys: readonly string[]): Promise<void> {
  const grip = (): HTMLButtonElement => find(rows(el)[place - 1] ?? el, 'button[slot="drag"]', HTMLButtonElement);
  await step(() => {
    grip().focus();
  });
  await step(() => {
    pressAndRelease(grip(), "Enter");
  });
  for (const key of keys) {
    await acting(() => {
      pressAndRelease(document.activeElement as HTMLElement, key);
    });
  }
}

// The labels of the buttons in `el` that remove an input or its source.
function removes(el: ParentNode): string[] {
  return [...el.querySelectorAll("button[aria-label^='Remove']")].map((b) => b.getAttribute("aria-label") ?? "");
}

// What each input of a variadic block says to assistive technology.
function inputs(el: ParentNode): (string | null)[] {
  return rows(el).map((row) => row.getAttribute("aria-label"));
}

// The links into the block, in their order on the board, each as its id, the
// block it comes from and the input it feeds.
function into(board: Board, block: string): [LinkId, string, string][] {
  return Object.entries(board.links)
    .filter(([, link]) => link.to === block)
    .map(([id, link]) => [id, link.from, link.input]);
}

// What the advanced action says of why the store or the command registry
// refused a batch, banner by banner: the sentence that names the edit, then
// each reason.
function refusals(el: ParentNode): string[][] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => [...alert.querySelectorAll("p, li")].map((line) => line.textContent));
}

describe("Edit inputs", () => {
  test("opens its advanced action at once from the row of a block's menu, since it has no quick action, in core's dialog, titled with its label, with an Apply button below the inputs", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "c" });
    await step(() => {
      void menu.pick("Edit inputs");
    });

    expect(menu.closed()).toBe(true);
    expect(s.advanced()).toMatchObject({ id: EDIT_INPUTS, title: "Edit inputs", target: { block: "c" } });
    const el = s.advanced()?.el ?? document;
    expect(texts(el, "p")[0]).toBe('The inputs of the block "Merge".');
    expect(applyButton(el).type).toBe("submit");
    expect(applyButton(el).compareDocumentPosition(labelled(el, "y")) & Node.DOCUMENT_POSITION_PRECEDING).toBeTruthy();
  });

  test("in a plugin's room, applies the edits in one step, and then shows the inputs afresh, as the board holds them", async () => {
    const s = await stubbed();
    const room = document.createElement("div");
    document.body.append(room);
    let unmount = (): void => undefined;
    await step(() => {
      unmount = s.core.boardActions.mountAdvanced(room, EDIT_INPUTS, { block: "e" });
    });
    stops.push(() => {
      act(unmount);
      room.remove();
    });

    await acting(() => {
      button(room, "Remove input 1").click();
    });
    await named(room, "Name of input 1", "low");
    await apply(room);

    expect(into(s.core.store.board(), "e")).toEqual([["de", "d", "low"]]);
    expect(inputs(room)).toEqual(["Input 1, low, from Wide sepals"]);
    s.core.store.undo();
    expect(into(s.core.store.board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
    expect(s.core.store.canUndo()).toBe(false);
  });
});

describe("on a block that is not variadic", () => {
  test("lists the inputs of the block's class in their order, each with its source and a button that removes it", async () => {
    const { el } = await opened("c");

    expect([...el.querySelectorAll("label")].map((label) => label.textContent)).toEqual(["x", "y"]);
    expect([labelled(el, "x").value, labelled(el, "y").value]).toEqual(["Iris", "Head"]);
    expect(removes(el)).toEqual(["Remove the source of x", "Remove the source of y"]);
  });

  test("offers as a source each block but the block itself and those downstream of it, from which a link would close a cycle", async () => {
    // Head feeds Merge and Bind, so neither may feed it.
    const { el } = await opened("b");

    expect(await listed(labelled(el, "data"), step)).toEqual(["Iris", "Wide sepals", "Scatter"]);
  });

  test("changes the source of an input and removes the source of another, runs nothing before the user applies the edits, and then applies them in one step that one undo takes back, as core's dialog closes", async () => {
    const { s, el, core, board } = await opened("c");
    const before = board();

    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    expect(labelled(el, "y").value).toBe("Wide sepals");
    await acting(() => {
      button(el, "Remove the source of x").click();
    });
    // The input stays, without a source or a button that removes one.
    expect(labelled(el, "x").value).toBe("");
    expect(removes(el)).toEqual(["Remove the source of y"]);
    expect(board()).toBe(before);

    await apply(el);
    expect(s.advanced()).toBeUndefined();
    expect(into(board(), "c")).toEqual([["bc", "d", "y"]]);
    await acting(() => {
      core.store.undo();
    });
    expect(into(board(), "c")).toEqual([
      ["ac", "a", "x"],
      ["bc", "b", "y"],
    ]);
    expect(core.store.canUndo()).toBe(false);
  });

  test("builds its batch from the inputs against the links of the board, in the order of edit_inputs_role, whatever order the user made the edits in", async () => {
    const s = await stubbed();
    const { el, batches } = await rendered(s, "c");

    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    await acting(() => {
      button(el, "Remove the source of x").click();
    });
    await apply(el);

    expect(batches).toEqual([
      [
        { id: "blockr.core-remove_link", args: { id: "ac" } },
        { id: "blockr.core-change_link", args: { id: "bc", from: "d" } },
      ],
    ]);
  });

  test("adds a link from the block that the user picks into an input that no link feeds, with an ID that CoreApi mints, and runs nothing for an input whose source the user picked back", async () => {
    const s = await stubbed();
    await act(() => s.core.commands.run("blockr.core-remove_link", { id: "bc" }));
    const { el, batches } = await rendered(s, "c");
    expect([labelled(el, "y").value, removes(el)]).toEqual(["", ["Remove the source of x"]]);

    await acting(() => {
      void picked(labelled(el, "x"), step, "Head");
    });
    await acting(() => {
      void picked(labelled(el, "x"), step, "Iris");
    });
    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    await apply(el);

    const links = into(s.core.store.board(), "c");
    expect(links.map(([, from, input]) => [from, input])).toEqual([
      ["a", "x"],
      ["d", "y"],
    ]);
    // The batch names the ID that the advanced action minted for the link.
    const added = links[1]?.[0];
    expect(added).toBeTypeOf("string");
    expect(batches).toEqual([[{ id: "blockr.core-add_link", args: { id: added, from: "d", to: "c", input: "y" } }]]);
  });

  test("keeps the source of an input whose field the user empties, which removes nothing", async () => {
    const { el, board } = await opened("c");
    const before = board();

    await step(() => {
      labelled(el, "x").focus();
    });
    await step(() => {
      type(labelled(el, "x"), "");
    });
    await acting(() => {
      labelled(el, "x").blur();
    });
    expect(labelled(el, "x").value).toBe("Iris");
    await apply(el);

    expect(board()).toBe(before);
  });

  test("shows an edit from elsewhere in the inputs that the user left alone, and keeps those that the user changed", async () => {
    const { el, core, board } = await opened("c");

    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    await act(() =>
      core.commands.run([
        { id: "blockr.core-change_link", args: { id: "ac", from: "f" } },
        { id: "blockr.core-change_link", args: { id: "bc", from: "a" } },
      ]),
    );
    expect([labelled(el, "x").value, labelled(el, "y").value]).toEqual(["Scatter", "Wide sepals"]);
    await apply(el);

    expect(into(board(), "c")).toEqual([
      ["ac", "f", "x"],
      ["bc", "d", "y"],
    ]);
  });

  test("passes over a source that the user picked and that has left the board since, and shows the source on the board", async () => {
    const { el, core, board } = await opened("c");

    await acting(() => {
      void picked(labelled(el, "y"), step, "Scatter");
    });
    await act(() => core.commands.run("blockr.core-remove_block", { id: "f" }));
    expect(labelled(el, "y").value).toBe("Head");
    const before = board();
    await apply(el);

    expect(board()).toBe(before);
  });
});

describe("on a variadic block", () => {
  test("lists the links into the block in their order on the board, each with a button that drags it, its source, its name and a button that removes it", async () => {
    const { el } = await opened("e");

    expect(texts(el, "p")[0]).toBe('The inputs of the block "Bind", which it takes in this order.');
    expect(inputs(el)).toEqual(["Input 1, from Head", "Input 2, from Wide sepals"]);
    expect([field(el, "Source of input 1").value, field(el, "Source of input 2").value]).toEqual(["Head", "Wide sepals"]);
    expect([field(el, "Name of input 1").value, field(el, "Name of input 2").value]).toEqual(["", ""]);
    expect(rows(el).map((row) => row.querySelectorAll('button[slot="drag"]').length)).toEqual([1, 1]);
    expect(removes(el)).toEqual(["Remove input 1", "Remove input 2"]);
  });

  test("moves an input to another place, which the user drags it to from the keyboard, and applies the order through move_link in one step that undo takes back", async () => {
    const { el, core, board } = await opened("e");
    const before = board();

    // React Aria starts at the place after the item, and moves on to the
    // place after Wide sepals.
    await dragged(el, 1, ["ArrowDown", "Enter"]);
    expect([field(el, "Source of input 1").value, field(el, "Source of input 2").value]).toEqual(["Wide sepals", "Head"]);
    expect(board()).toBe(before);

    await apply(el);
    expect(into(board(), "e")).toEqual([
      ["de", "d", ""],
      ["be", "b", ""],
    ]);
    await acting(() => {
      core.store.undo();
    });
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
    expect(core.store.canUndo()).toBe(false);
  });

  test("drags an input from its button alone, so that a press in its select or its field drags nothing until it ends", async () => {
    const { el } = await opened("e");
    const [first] = rows(el);
    if (first === undefined) throw new Error("Found no input.");
    const pointer = (target: EventTarget, type: string): void => {
      target.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, pointerType: "mouse" }));
    };
    const draggable = (): string | null => first.getAttribute("draggable");
    expect(draggable()).toBe("true");

    for (const label of ["Name of input 1", "Source of input 1"]) {
      await step(() => {
        pointer(field(el, label), "pointerdown");
      });
      expect(draggable()).toBe("false");
      await step(() => {
        pointer(document.body, "pointerup");
      });
      expect(draggable()).toBe("true");
    }

    await step(() => {
      pointer(find(first, 'button[slot="drag"]'), "pointerdown");
    });
    expect(draggable()).toBe("true");
  });

  test("names an input once the user applies the name, and takes the name away where the user empties the field", async () => {
    const { s, el, board } = await opened("e");

    await named(el, "Name of input 1", " top ");
    await acting(() => {
      field(el, "Name of input 1").blur();
    });
    expect(field(el, "Name of input 1").value).toBe(" top ");
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
    await apply(el);
    expect(into(board(), "e")).toEqual([
      ["be", "b", "top"],
      ["de", "d", ""],
    ]);

    const again = await opened("e", s);
    expect(field(again.el, "Name of input 1").value).toBe("top");
    await named(again.el, "Name of input 1", "");
    await apply(again.el);
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
  });

  test("refuses a name that another input gives already, which its field says in place once the user leaves it, and applies nothing while one does", async () => {
    const { s, el, core, board } = await opened("e");
    await act(() => core.commands.run("blockr.core-change_link", { id: "de", input: "low" }));
    const before = board();

    await named(el, "Name of input 1", "low");
    expect(fieldError(el)).toBeNull();
    await acting(() => {
      field(el, "Name of input 1").blur();
    });
    expect(fieldError(el)).toBe('Another link gives the input the name "low" already.');
    expect(field(el, "Name of input 1").getAttribute("aria-invalid")).toBe("true");

    await apply(el);
    expect(board()).toBe(before);
    expect(s.advanced()?.closed()).toBe(false);
    expect(document.activeElement).toBe(field(el, "Name of input 1"));

    // Once the other input takes another name, the name does.
    await named(el, "Name of input 2", "high");
    expect(fieldError(el)).toBeNull();
    await apply(el);
    expect(into(board(), "e")).toEqual([
      ["be", "b", "low"],
      ["de", "d", "high"],
    ]);
  });

  test("changes the source of an input and removes an input once the user applies the edits, in one step", async () => {
    const { el, core, board } = await opened("e");

    await acting(() => {
      void picked(field(el, "Source of input 2"), step, "Scatter");
    });
    await acting(() => {
      button(el, "Remove input 1").click();
    });
    expect(inputs(el)).toEqual(["Input 1, from Scatter"]);
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);

    await apply(el);
    expect(into(board(), "e")).toEqual([["de", "f", ""]]);
    await acting(() => {
      core.store.undo();
    });
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
  });

  test("adds an input from the block that the user picks, without a name, last among the block's inputs, and empties its select for the next", async () => {
    const { el, board } = await opened("e");

    // Bind feeds no block, so every other block may feed it.
    expect(await listed(labelled(el, "Add input"), step)).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Scatter"]);
    await acting(() => {
      void picked(labelled(el, "Add input"), step, "Iris");
    });
    expect(inputs(el)).toEqual(["Input 1, from Head", "Input 2, from Wide sepals", "Input 3, from Iris"]);
    expect(labelled(el, "Add input").value).toBe("");
    expect(into(board(), "e")).toHaveLength(2);

    await apply(el);
    expect(into(board(), "e").map(([, from, input]) => [from, input])).toEqual([
      ["b", ""],
      ["d", ""],
      ["a", ""],
    ]);
  });

  test("applies every kind of edit as one batch, in the order of edit_inputs_role, each command at most once for each input, which one undo takes back", async () => {
    const s = await stubbed();
    const { el, batches } = await rendered(s, "e");

    await acting(() => {
      button(el, "Remove input 1").click();
    });
    await acting(() => {
      void picked(field(el, "Source of input 1"), step, "Scatter");
    });
    await named(el, "Name of input 1", "low");
    for (const block of ["Iris", "Merge"]) {
      await acting(() => {
        void picked(labelled(el, "Add input"), step, block);
      });
    }
    await named(el, "Name of input 3", "m");
    // Iris goes first.
    await dragged(el, 2, ["ArrowUp", "Enter"]);
    expect(inputs(el)).toEqual(["Input 1, from Iris", "Input 2, low, from Scatter", "Input 3, m, from Merge"]);
    await apply(el);

    const links = into(s.core.store.board(), "e");
    expect(links.map(([, from, input]) => [from, input])).toEqual([
      ["a", ""],
      ["f", "low"],
      ["c", "m"],
    ]);
    const [iris, , merge] = links.map(([id]) => id);
    // The user changed the order, so each input goes last in turn, in the
    // order shown.
    expect(batches).toEqual([
      [
        { id: "blockr.core-remove_link", args: { id: "be" } },
        { id: "blockr.core-change_link", args: { id: "de", from: "f", input: "low" } },
        { id: "blockr.core-add_link", args: { id: iris, from: "a", to: "e" } },
        { id: "blockr.core-add_link", args: { id: merge, from: "c", to: "e", input: "m" } },
        { id: "blockr.core-move_link", args: { id: iris } },
        { id: "blockr.core-move_link", args: { id: "de" } },
        { id: "blockr.core-move_link", args: { id: merge } },
      ],
    ]);
    s.core.store.undo();
    expect(into(s.core.store.board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("through its board action, in core's dialog, adds an input and reorders the inputs in one batch, which moves the added input too and keeps edit_inputs_role", async () => {
    const s = await stubbed();
    const menu = s.menu({ block: "e" });
    await step(() => {
      void menu.pick("Edit inputs");
    });
    const el = s.advanced()?.el ?? document;
    await acting(() => {
      void picked(labelled(el, "Add input"), step, "Iris");
    });
    await named(el, "Name of input 3", "iris");
    // Head goes after Wide sepals. So the batch moves each input last in
    // turn, Iris too, whose link the batch adds itself.
    await dragged(el, 1, ["ArrowDown", "Enter"]);
    expect(inputs(el)).toEqual(["Input 1, from Wide sepals", "Input 2, from Head", "Input 3, iris, from Iris"]);
    await apply(el);

    expect(s.advanced()).toBeUndefined();
    expect(into(s.core.store.board(), "e").map(([, from, input]) => [from, input])).toEqual([
      ["d", ""],
      ["b", ""],
      ["a", "iris"],
    ]);
    expect(held(s.core, (n) => notificationCode(n) === "board_action_failed")).toEqual([]);
  });

  test("shows a link that joins the block while it is open, which applying the edits leaves in place", async () => {
    const { el, core, board } = await opened("e");

    await named(el, "Name of input 1", "top");
    await act(() => core.commands.run("blockr.core-add_link", { id: "ae", from: "a", to: "e" }));
    expect(inputs(el)).toEqual(["Input 1, top, from Head", "Input 2, from Wide sepals", "Input 3, from Iris"]);
    await apply(el);

    expect(into(board(), "e")).toEqual([
      ["be", "b", "top"],
      ["de", "d", ""],
      ["ae", "a", ""],
    ]);
  });

  test("says that no link feeds the block where none does", async () => {
    const { el, core } = await opened("e");
    await act(() =>
      core.commands.run([
        { id: "blockr.core-remove_link", args: { id: "be" } },
        { id: "blockr.core-remove_link", args: { id: "de" } },
      ]),
    );

    expect(rows(el)).toEqual([]);
    expect(find(el, '[role="grid"]').textContent).toBe("No link feeds the block yet.");
  });
});

describe("a batch that the store refuses", () => {
  test("shows why below the Apply button, under a sentence that names the edit, keeps what the user gave, and core raises no notification of it", async () => {
    const { s, el, core, board } = await opened("c");

    await acting(() => {
      void picked(labelled(el, "x"), step, "Bind");
    });
    // Another edit links Merge to Bind before the user applies the edits, so
    // that the link from Bind would close a cycle.
    await act(() => core.commands.run("blockr.core-add_link", { id: "ce", from: "c", to: "e" }));
    const before = board();
    await apply(el);

    expect(refusals(el)).toEqual([['Could not change the inputs of the block "Merge".', 'The link would close a cycle through the blocks "c" and "e".']]);
    expect(find(el, '[role="alert"]').previousElementSibling?.contains(applyButton(el))).toBe(true);
    expect(s.advanced()?.closed()).toBe(false);
    expect(board()).toBe(before);
    expect(held(core)).toEqual([]);

    // It keeps the source that the user gave, which applies once the board
    // takes it.
    await act(() => core.commands.run("blockr.core-remove_link", { id: "ce" }));
    await apply(el);
    expect(s.advanced()).toBeUndefined();
    expect(into(board(), "c")).toEqual([
      ["ac", "e", "x"],
      ["bc", "b", "y"],
    ]);
  });

  test("of a variadic block names the block, and goes as the form's own check stops the next apply", async () => {
    // The board of the fixture hello under the board lock, with a CoreApi that
    // says the board is open, since no menu offers Edit inputs there.
    const s = await stubbed({ ...HELLO, lock: { board: true, load: false } });
    const core: CoreApi = { ...s.core, lock: { ...s.core.lock, board: false } };
    const { el } = await rendered(s, "e", core);

    await acting(() => {
      button(el, "Remove input 2").click();
    });
    await apply(el);
    expect(refusals(el)).toEqual([['Could not change the inputs of the block "Bind".', 'The board lock refuses to remove link "de".']]);
    expect(inputs(el)).toEqual(["Input 1, from Head"]);

    await acting(() => {
      void picked(labelled(el, "Add input"), step, "Iris");
    });
    await named(el, "Name of input 1", "dup");
    await named(el, "Name of input 2", "dup");
    await apply(el);
    expect(refusals(el)).toEqual([]);
    expect(fieldError(el)).toBe('Another link gives the input the name "dup" already.');
    expect(held(core)).toEqual([]);
  });
});
