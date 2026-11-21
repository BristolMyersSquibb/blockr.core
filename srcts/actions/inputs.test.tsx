// The advanced action of Edit inputs, one of core's board actions, on the
// board of the fixture hello through the CoreApi for tests, which shows it in
// core's dialog as a user opens it from the menu of a block, and drives it as
// the user does.
// The fixture's board holds the blocks a, Iris, a dataset block; b, Head; c,
// Merge, with the inputs x and y; d, Wide sepals; e, Bind, a variadic block;
// and f, Scatter. Its links are ab, ac (x), bc (y), ad, be, de and keen_finch
// (a to f).

import { act } from "react";
import { afterEach, describe, expect, test } from "vitest";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCore } from "../stubs/index.ts";
import { fieldError, find, press, pressAndRelease, texts, type } from "../testing/inputs.ts";
import { held } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import { rows } from "../testing/rows.ts";
import { labelled, listed, picked } from "../testing/select.ts";
import type { Board, LinkId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { editInputs } from "./inputs.tsx";

const EDIT_INPUTS = "blockr.core-edit_inputs_action";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) stop();
  document.body.replaceChildren();
});

// The advanced action of Edit inputs on the block given, which the user opens
// from the block's menu, and the element in which core's dialog shows it.
// The pick resolves only once the advanced action ends, so the test does not
// wait for it.
async function opened(block: string): Promise<{ s: StubCore; core: CoreApi; el: HTMLElement; board: () => Board }> {
  const s = await act(() => stubCore());
  stops.push(s.stop);
  const menu = s.menu({ block });
  await step(() => {
    void menu.pick("Edit inputs");
  });
  const shown = s.advanced();
  if (shown === undefined) throw new Error("Core's dialog shows no advanced action.");
  return { s, core: s.core, el: shown.el, board: () => s.core.store.board() };
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

// The labels of the buttons in `el` that remove an input or its source.
function removes(el: ParentNode): string[] {
  return [...el.querySelectorAll("button[aria-label^='Remove']")].map((b) => b.getAttribute("aria-label") ?? "");
}

// The links into the block, in their order on the board, each as its id, the
// block it comes from and the input it feeds.
function into(board: Board, block: string): [LinkId, string, string][] {
  return Object.entries(board.links)
    .filter(([, link]) => link.to === block)
    .map(([id, link]) => [id, link.from, link.input]);
}

// What the advanced action says of why the store or the command registry
// refused an edit, banner by banner: the sentence that names the edit, then
// each reason.
function refusals(el: ParentNode): string[][] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => [...alert.querySelectorAll("p, li")].map((line) => line.textContent));
}

describe("Edit inputs", () => {
  test("opens its advanced action at once from the row of a block's menu, since it has no quick action, in core's dialog, titled with its label", async () => {
    const s = await act(() => stubCore());
    stops.push(s.stop);
    const menu = s.menu({ block: "c" });
    await step(() => {
      void menu.pick("Edit inputs");
    });

    expect(menu.closed()).toBe(true);
    expect(s.advanced()).toMatchObject({ id: EDIT_INPUTS, title: "Edit inputs", target: { block: "c" } });
    expect(texts(s.advanced()?.el ?? document, "p")[0]).toBe('The inputs of the block "Merge".');
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

  test("changes the source of an input, removes it, and adds a link from the block that the user picks into an input that no link feeds, each edit at once as one step that undo takes back", async () => {
    const { el, core, board } = await opened("c");

    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    expect(into(board(), "c")).toEqual([
      ["ac", "a", "x"],
      ["bc", "d", "y"],
    ]);
    expect(labelled(el, "y").value).toBe("Wide sepals");

    await acting(() => {
      button(el, "Remove the source of x").click();
    });
    expect(into(board(), "c")).toEqual([["bc", "d", "y"]]);
    // The input stays, without a source or a button that removes one.
    expect(labelled(el, "x").value).toBe("");
    expect(removes(el)).toEqual(["Remove the source of y"]);

    await acting(() => {
      void picked(labelled(el, "x"), step, "Scatter");
    });
    const [added] = into(board(), "c").filter(([, , input]) => input === "x");
    expect(added?.slice(1)).toEqual(["f", "x"]);
    expect(labelled(el, "x").value).toBe("Scatter");

    for (const undone of [[["bc", "d", "y"]], [["ac", "a", "x"], ["bc", "d", "y"]], [["ac", "a", "x"], ["bc", "b", "y"]]]) {
      await acting(() => {
        core.store.undo();
      });
      expect(into(board(), "c")).toEqual(undone);
    }
    expect(core.store.canUndo()).toBe(false);
    // It reads the board as it is, so it shows what undo took back.
    expect([labelled(el, "x").value, labelled(el, "y").value]).toEqual(["Iris", "Head"]);
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

    expect(board()).toBe(before);
    expect(labelled(el, "x").value).toBe("Iris");
  });
});

describe("on a variadic block", () => {
  test("lists the links into the block in their order on the board, each with a button that drags it, its source, its name and a button that removes it", async () => {
    const { el } = await opened("e");

    expect(texts(el, "p")[0]).toBe('The inputs of the block "Bind", which it takes in this order.');
    expect(rows(el).map((row) => row.getAttribute("aria-label"))).toEqual(["Input 1, from Head", "Input 2, from Wide sepals"]);
    expect([field(el, "Source of input 1").value, field(el, "Source of input 2").value]).toEqual(["Head", "Wide sepals"]);
    expect([field(el, "Name of input 1").value, field(el, "Name of input 2").value]).toEqual(["", ""]);
    expect(rows(el).map((row) => row.querySelectorAll('button[slot="drag"]').length)).toEqual([1, 1]);
    expect(removes(el)).toEqual(["Remove input 1", "Remove input 2"]);
  });

  test("moves an input to another place, which the user drags it to from the keyboard, through move_link, as one step that undo takes back", async () => {
    const { el, core, board } = await opened("e");
    const grip = (place: number): HTMLButtonElement => find(rows(el)[place - 1] ?? el, 'button[slot="drag"]', HTMLButtonElement);

    await step(() => {
      grip(1).focus();
    });
    await step(() => {
      pressAndRelease(grip(1), "Enter");
    });
    // React Aria starts at the place after the item, and moves on to the
    // place after Wide sepals.
    for (const key of ["ArrowDown", "Enter"]) {
      await acting(() => {
        pressAndRelease(document.activeElement as HTMLElement, key);
      });
    }

    expect(into(board(), "e")).toEqual([
      ["de", "d", ""],
      ["be", "b", ""],
    ]);
    expect([field(el, "Source of input 1").value, field(el, "Source of input 2").value]).toEqual(["Wide sepals", "Head"]);
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

  test("names an input as the user presses Enter or leaves its field, and takes the name away where the user empties the field", async () => {
    const { el, board } = await opened("e");

    await step(() => {
      field(el, "Name of input 1").focus();
    });
    await step(() => {
      type(field(el, "Name of input 1"), " top ");
    });
    await acting(() => {
      press(field(el, "Name of input 1"), "Enter");
    });
    expect(into(board(), "e")).toEqual([
      ["be", "b", "top"],
      ["de", "d", ""],
    ]);
    // The field keeps the focus, as the user may go on typing.
    expect(document.activeElement).toBe(field(el, "Name of input 1"));
    expect(field(el, "Name of input 1").value).toBe("top");

    await step(() => {
      type(field(el, "Name of input 1"), "");
    });
    await acting(() => {
      field(el, "Name of input 1").blur();
    });
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", ""],
    ]);
  });

  test("refuses a name that another link into the block gives already, which its field says in place", async () => {
    const { el, core, board } = await opened("e");
    await act(() => core.commands.run("blockr.core-change_link", { id: "de", input: "low" }));

    await step(() => {
      field(el, "Name of input 1").focus();
    });
    await step(() => {
      type(field(el, "Name of input 1"), "low");
    });
    await acting(() => {
      field(el, "Name of input 1").blur();
    });

    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "d", "low"],
    ]);
    expect(fieldError(el)).toBe('Another link gives the input the name "low" already.');
    expect(field(el, "Name of input 1").getAttribute("aria-invalid")).toBe("true");
  });

  test("changes the source of an input and removes an input, each at once", async () => {
    const { el, board } = await opened("e");

    await acting(() => {
      void picked(field(el, "Source of input 2"), step, "Scatter");
    });
    expect(into(board(), "e")).toEqual([
      ["be", "b", ""],
      ["de", "f", ""],
    ]);

    await acting(() => {
      button(el, "Remove input 1").click();
    });
    expect(into(board(), "e")).toEqual([["de", "f", ""]]);
    expect(rows(el).map((row) => row.getAttribute("aria-label"))).toEqual(["Input 1, from Scatter"]);
  });

  test("adds an input from the block that the user picks, without a name, last among the block's links, and empties its select for the next", async () => {
    const { el, board } = await opened("e");

    // Bind feeds no block, so every other block may feed it.
    expect(await listed(labelled(el, "Add input"), step)).toEqual(["Iris", "Head", "Merge", "Wide sepals", "Scatter"]);
    await acting(() => {
      void picked(labelled(el, "Add input"), step, "Iris");
    });

    const links = into(board(), "e");
    expect(links.map(([, from, input]) => [from, input])).toEqual([
      ["b", ""],
      ["d", ""],
      ["a", ""],
    ]);
    expect(rows(el).map((row) => row.getAttribute("aria-label"))).toEqual(["Input 1, from Head", "Input 2, from Wide sepals", "Input 3, from Iris"]);
    expect(labelled(el, "Add input").value).toBe("");
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

describe("an edit that the store refuses", () => {
  // The advanced action over a store that refuses every edit: the board of the
  // fixture hello under the board lock, with a CoreApi that says the board is
  // open, since no menu offers Edit inputs there.
  async function refusing(block: string): Promise<{ el: HTMLElement; core: CoreApi; board: () => Board }> {
    const s = await act(() => stubCore({ hello: { ...HELLO, lock: { board: true, load: false } } }));
    stops.push(s.stop);
    const core: CoreApi = { ...s.core, lock: { ...s.core.lock, board: false } };
    const el = document.createElement("div");
    document.body.append(el);
    let end: (() => void) | undefined;
    await act(async () => {
      const rendered = await editInputs.render({ el, core, target: { block }, signal: new AbortController().signal, done: () => Promise.resolve() });
      end = rendered ?? undefined;
    });
    stops.push(() => {
      act(() => {
        end?.();
      });
    });
    return { el, core, board: () => s.core.store.board() };
  }

  test("shows why below the inputs, under a sentence that names the edit, until the user's next edit, and core raises no notification of it", async () => {
    const { el, core, board } = await refusing("c");
    const before = board();

    await acting(() => {
      button(el, "Remove the source of x").click();
    });
    expect(refusals(el)).toEqual([['Could not remove the source of the input "x".', 'The board lock refuses to remove link "ac".']]);
    // It shows below the inputs.
    expect(find(el, '[role="alert"]').previousElementSibling?.contains(labelled(el, "y"))).toBe(true);

    // The next edit takes the place of the last.
    await acting(() => {
      void picked(labelled(el, "y"), step, "Wide sepals");
    });
    expect(refusals(el)).toEqual([['Could not change the source of the input "y".', 'The board lock refuses to change link "bc".']]);

    expect(board()).toBe(before);
    expect(held(core)).toEqual([]);
  });

  test("of a variadic block names the input by its place", async () => {
    const { el } = await refusing("e");

    await acting(() => {
      button(el, "Remove input 2").click();
    });
    expect(refusals(el)).toEqual([["Could not remove input 2.", 'The board lock refuses to remove link "de".']]);
  });
});
