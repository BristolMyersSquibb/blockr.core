// The rules of a quick action, on the logic that runs every menu of board
// actions and draws nothing, which core tests once here: the menu of a target
// and its rows, the levels that picks open, Escape, the ticks of a multi, a
// text row's refusal, the filter, promises that load or reject, the faults
// of a board action's functions, and the advanced actions that a menu opens,
// at once or through an open icon.

import { describe, expect, test } from "vitest";
import { logged } from "../testing/logged.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { BoardAction, CommandCall } from "./actions.ts";
import { inMenuOrder, openMenu } from "./quick.ts";
import type { MenuParts, MenuStart, QuickLevel, QuickRow } from "./quick.ts";
import { interned } from "../util/index.ts";

const BOARD = { name: "Board" } as unknown as Board;
const CORE = {} as CoreApi;

// A board action of blockr.test on blocks, labelled by its name, as core
// registers it, with its package.
function action(name: string, more: Partial<BoardAction> = {}): BoardAction {
  return { package: "blockr.test", id: `${name}_action`, label: name, on: ["block"], category: "test", ...more };
}

// A batch that renames block a.
const rename = (name: string): CommandCall[] => [{ id: interned("blockr.core", "change_block"), args: { id: "a", name } }];

// An item that picks what it is given.
function item(label: string, picked: () => ReturnType<Extract<QuickRow, { kind: "item" }>["pick"]>, more = {}): QuickRow {
  return { kind: "item", label, pick: picked, ...more };
}

// A board action whose quick action is the level given.
function asking(level: QuickLevel | (() => ReturnType<NonNullable<BoardAction["quick"]>>)): BoardAction {
  return action("asks", { quick: typeof level === "function" ? level : () => level });
}

// An advanced action that renders nothing.
const ADVANCED = { render: () => undefined };

// A menu with parts that record each batch that it runs, with the id of its
// board action, each board action that they trigger, each advanced action that
// they open, with what it was handed, each failure that they report, and each
// closing, with the promise of what closing ran. A batch of `refused` rejects,
// and so does a board action triggered whose name says that it fails. An
// advanced action ends once the test calls what `advanced` records with it.
function opened(start: MenuStart, more: Partial<MenuParts> = {}) {
  const ran: [readonly CommandCall[], string][] = [];
  const triggered: string[] = [];
  const advanced: { id: string; opened: { readonly picked?: Json; readonly filter?: string }; end: () => void }[] = [];
  const failures: [string, string][] = [];
  const closings: (Promise<void> | undefined)[] = [];
  const menu = openMenu(start, {
    board: () => BOARD,
    target: { block: "a" },
    core: () => CORE,
    applies: () => true,
    run(batch, by) {
      ran.push([batch, by.id]);
      return batch.some((call) => call.id.id === "refused") ? Promise.reject(new Error("Refused.")) : Promise.resolve();
    },
    trigger(by) {
      triggered.push(by.id);
      return by.id.includes("fails") ? Promise.reject(new Error("The clipboard is refused.")) : Promise.resolve();
    },
    advanced(by, given) {
      return new Promise<void>((resolve) => {
        advanced.push({ id: by.id, opened: given, end: resolve });
      });
    },
    failed(by, failure) {
      failures.push([by.id, failure.message]);
    },
    closed(promise) {
      closings.push(promise);
    },
    ...more,
  });
  return { menu, ran, triggered, advanced, failures, closings };
}

// The labels of the rows that the menu shows.
function labels(menu: ReturnType<typeof openMenu>): string[] {
  return menu.get().rows.map((row) => row.label);
}

// Waits for the promises that are pending to settle.
const settled = () => new Promise((resolve) => setTimeout(resolve, 0));

describe("the menu of a target", () => {
  test("lists the board actions of each category together: add, edit, clipboard, the categories of packages as they first come, and remove, each in the order of on()", () => {
    const given = [
      action("drop", { category: "remove" }),
      action("export", { category: "export" }),
      action("rename", { category: "edit" }),
      action("append", { category: "add" }),
      action("copy", { category: "clipboard" }),
      action("embed", { category: "document" }),
      action("prepend", { category: "add" }),
      action("print", { category: "export" }),
    ];
    const { menu } = opened({ actions: given });

    expect(labels(menu)).toEqual(["append", "prepend", "rename", "copy", "export", "print", "embed", "drop"]);
    expect(inMenuOrder(given).map(({ label }) => label)).toEqual(labels(menu));
    expect(menu.get()).toMatchObject({ label: "Board actions", back: false, loading: false });
    expect(menu.get().title).toBeUndefined();
  });

  test("shows each board action's label, icon, detail for the target and category, and leaves out a detail that throws, as a fault that core logs", () => {
    const got: unknown[] = [];
    const reported = logged();
    const { menu } = opened({
      actions: [
        action("copy", {
          icon: "<svg></svg>",
          category: "clipboard",
          detail(board, target) {
            got.push([board, target]);
            return "a";
          },
        }),
        action("broken", {
          detail: () => {
            throw new Error("boom");
          },
        }),
      ],
    });
    reported.restore();

    expect(menu.get().rows).toEqual([
      { kind: "item", label: "copy", icon: "<svg></svg>", detail: "a", category: "clipboard" },
      { kind: "item", label: "broken", category: "test" },
    ]);
    expect(got).toEqual([[BOARD, { block: "a" }]]);
    expect(reported.errors().map((line) => line.message)).toEqual(['The detail of the board action "broken_action of blockr.test" failed: boom']);
  });

  test("closes at once as the user picks the row of a board action with run, which its parts trigger within the pick, as trigger() does", async () => {
    const runs = action("runs", { run: () => rename("Run") });
    const { menu, triggered, closings } = opened({ actions: [action("other", { run: () => [] }), runs] });

    const picked = menu.pick(1);
    expect(triggered).toEqual([runs.id]);
    expect(menu.closed()).toBe(true);
    expect(closings).toHaveLength(1);
    expect(closings[0]).toBeInstanceOf(Promise);
    await expect(picked).resolves.toBeUndefined();
  });

  test("rejects a pick with what the board action's run failed with, once the menu has closed, since core tells the user of it as trigger() does", async () => {
    const { menu, closings } = opened({ actions: [action("fails", { run: () => rename("Never") })] });

    await expect(menu.pick(0)).rejects.toThrow("The clipboard is refused.");
    expect(menu.closed()).toBe(true);
    await expect(closings[0]).rejects.toThrow("The clipboard is refused.");
  });

  test("closes and triggers nothing where the menu of the target no longer offers the board action", async () => {
    const { menu, triggered, closings } = opened({ actions: [action("runs", { run: () => rename("Never") })] }, { applies: () => false });

    await menu.pick(0);

    expect(menu.closed()).toBe(true);
    expect(triggered).toEqual([]);
    expect(closings).toEqual([undefined]);
  });

  test("shows a board action's quick action as its next level, from which Escape goes back, and closes from its first level", () => {
    const quick: unknown[] = [];
    const asks = action("asks", {
      quick(board, target, core) {
        quick.push([board, target, core]);
        return { title: "Pick one", rows: [item("one", () => rename("One"))] };
      },
    });
    const { menu, closings } = opened({ actions: [asks] });

    void menu.pick(0);
    expect(quick).toEqual([[BOARD, { block: "a" }, CORE]]);
    expect(menu.get()).toMatchObject({ title: "Pick one", label: "Pick one", back: true, rows: [{ kind: "item", label: "one" }] });

    menu.escape();
    expect(labels(menu)).toEqual(["asks"]);
    expect(menu.get().back).toBe(false);
    menu.escape();
    expect(menu.closed()).toBe(true);
    expect(closings).toEqual([undefined]);
  });
});

describe("a quick action", () => {
  test("shows the level that a pick returns at the same spot, and runs a batch that a pick returns as the batch of its board action", async () => {
    const second: QuickLevel = { rows: [item("deeper", () => rename("Deep"))] };
    const asks = asking({ rows: [item("next", () => second), item("now", () => rename("Now"))] });

    const first = opened({ action: asks });
    expect(first.menu.get()).toMatchObject({ label: "asks", back: false });
    await first.menu.pick(0);
    expect(labels(first.menu)).toEqual(["deeper"]);
    expect(first.menu.get()).toMatchObject({ label: "asks", back: true });
    await first.menu.pick(0);
    expect(first.ran).toEqual([[rename("Deep"), asks.id]]);
    expect(first.menu.closed()).toBe(true);

    const other = opened({ action: asks });
    await other.menu.pick(1);
    expect(other.ran).toEqual([[rename("Now"), asks.id]]);
  });

  test("shows whether an item is checked, where it says so, runs its pick as for any item, and takes a level whose checked is no boolean for no level", async () => {
    const layout = asking({
      rows: [
        item("Lay out again", () => rename("Again")),
        item("Left to right", () => rename("Left to right"), { heading: "Direction", checked: true }),
        item("Top to bottom", () => rename("Top to bottom"), { heading: "Direction", checked: false }),
      ],
    });

    const first = opened({ action: layout });
    expect(first.menu.get().rows).toEqual([
      { kind: "item", label: "Lay out again" },
      { kind: "item", label: "Left to right", heading: "Direction", checked: true },
      { kind: "item", label: "Top to bottom", heading: "Direction", checked: false },
    ]);
    await first.menu.pick(1);
    expect(first.ran).toEqual([[rename("Left to right"), layout.id]]);

    const reported = logged();
    const wrong = opened({
      action: asking(() => ({ rows: [{ kind: "item", label: "Snap to grid", checked: "on", pick: () => [] }] }) as unknown as QuickLevel),
    });
    reported.restore();
    expect(wrong.menu.get().error).toBe('It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.');
    expect(reported.errors()).toHaveLength(1);
  });

  test("rejects a pick with the refusal of its batch, once the menu has closed", async () => {
    const asks = asking({ rows: [item("refused", () => [{ id: interned("blockr.test", "refused") }])] });
    const { menu, closings } = opened({ action: asks });

    await expect(menu.pick(0)).rejects.toThrow("Refused.");
    expect(menu.closed()).toBe(true);
    await expect(closings[0]).rejects.toThrow("Refused.");
  });

  test("shows that it loads while quick or pick waits, then the level or the batch, or the message that the promise rejects with, which core logs", async () => {
    let resolve: (level: QuickLevel) => void = () => undefined;
    const asks = asking(() => new Promise<QuickLevel>((done) => (resolve = done)));
    const { menu, ran } = opened({ actions: [asks] });

    const picked = menu.pick(0);
    expect(menu.get()).toMatchObject({ label: "asks", loading: true, back: true, rows: [] });
    resolve({ rows: [item("later", () => Promise.resolve(rename("Later"))), item("fails", () => Promise.reject(new Error("No columns.")))] });
    await picked;
    expect(labels(menu)).toEqual(["later", "fails"]);

    const reported = logged();
    await menu.pick(1);
    reported.restore();
    expect(menu.get()).toMatchObject({ loading: false, error: "No columns.", back: true, rows: [] });
    expect(reported.errors().map((line) => line.message)).toEqual([
      'A row of the quick action of the board action "asks_action of blockr.test" failed: No columns.',
    ]);

    menu.escape();
    await menu.pick(0);
    expect(ran).toEqual([[rename("Later"), asks.id]]);
  });

  test("drops what a promise brings once the user went back from the level that loads, or closed the menu", async () => {
    let resolve: (batch: CommandCall[]) => void = () => undefined;
    const asks = asking({ rows: [item("slow", () => new Promise<CommandCall[]>((done) => (resolve = done)))] });
    const { menu, ran } = opened({ action: asks });

    const picked = menu.pick(0);
    expect(menu.get().loading).toBe(true);
    menu.escape();
    expect(labels(menu)).toEqual(["slow"]);
    resolve(rename("Never"));
    await picked;
    await settled();
    expect(ran).toEqual([]);
    expect(labels(menu)).toEqual(["slow"]);

    const second = menu.pick(0);
    menu.escape();
    menu.escape();
    resolve(rename("Never"));
    await second;
    expect(ran).toEqual([]);
    expect(menu.closed()).toBe(true);
  });

  test("applies the ticks of a multi as the menu closes, where they differ from what it picked, with its choices in their order, and drops them on Escape", async () => {
    const applied: (readonly string[])[] = [];
    const multi: QuickRow = {
      kind: "multi",
      label: "Blocks",
      choices: [
        { id: "a", label: "Head", picked: true },
        { id: "b", label: "Filter", picked: false },
        { id: "c", label: "Plot", picked: false },
      ],
      apply(picked) {
        applied.push(picked);
        return [{ id: interned("blockr.core", "change_stack"), args: { id: "s", blocks: [...picked] } }];
      },
    };
    const asks = asking({ rows: [multi] });

    const ticking = opened({ action: asks });
    ticking.menu.tick(0, "c");
    ticking.menu.tick(0, "b");
    expect(ticking.menu.get().rows).toEqual([
      {
        kind: "multi",
        label: "Blocks",
        choices: [
          { id: "a", label: "Head", picked: true },
          { id: "b", label: "Filter", picked: true },
          { id: "c", label: "Plot", picked: true },
        ],
      },
    ]);
    ticking.menu.tick(0, "a");
    // A choice that the multi does not hold ticks nothing.
    ticking.menu.tick(0, "z");
    await ticking.menu.close();
    expect(applied).toEqual([["b", "c"]]);
    expect(ticking.ran).toEqual([[[{ id: interned("blockr.core", "change_stack"), args: { id: "s", blocks: ["b", "c"] } }], asks.id]]);

    const unchanged = opened({ action: asks });
    unchanged.menu.tick(0, "b");
    unchanged.menu.tick(0, "b");
    await unchanged.menu.close();
    expect(unchanged.ran).toEqual([]);
    expect(unchanged.closings).toEqual([undefined]);

    const dropped = opened({ action: asks });
    dropped.menu.tick(0, "b");
    dropped.menu.escape();
    expect(dropped.menu.closed()).toBe(true);
    expect(dropped.ran).toEqual([]);
    expect(applied).toHaveLength(1);
  });

  test("tells the user through its parts where a multi's apply throws or gives no batch as the menu closes, and runs what the other multis apply", async () => {
    const multi = (label: string, apply: () => readonly CommandCall[]): QuickRow => ({
      kind: "multi",
      label,
      choices: [{ id: "x", label: "X", picked: false }],
      apply,
    });
    const asks = asking({
      rows: [
        multi("throws", () => {
          throw new Error("boom");
        }),
        multi("gives", () => ({ id: "nothing" }) as unknown as CommandCall[]),
        multi("works", () => rename("Works")),
      ],
    });
    const { menu, ran, failures } = opened({ action: asks });

    for (const row of [0, 1, 2]) menu.tick(row, "x");
    await menu.close();

    expect(ran).toEqual([[rename("Works"), asks.id]]);
    expect(failures).toEqual([
      [asks.id, 'A row of the quick action of the board action "asks_action of blockr.test" failed: boom'],
      [
        asks.id,
        'A row of the quick action of the board action "asks_action of blockr.test" failed: It gave no batch of commands, as [{ id: core.id("blockr.core", "remove_block"), args: { id: "a" } }].',
      ],
    ]);
  });

  test("refuses a text row's value in place once the user tried to submit it, and from then on as the user types, and submits it on Enter", async () => {
    const submitted: string[] = [];
    const asks = asking({
      rows: [
        {
          kind: "text",
          label: "Name",
          value: "Head",
          check: (value) => (value === "" ? "A name cannot be empty." : undefined),
          submit(value) {
            submitted.push(value);
            return rename(value);
          },
        },
      ],
    });
    const { menu, ran } = opened({ action: asks });

    expect(menu.get().rows).toEqual([{ kind: "text", label: "Name", value: "Head" }]);
    menu.type("", 0);
    // No refusal before the user tried to submit.
    expect(menu.get().rows).toEqual([{ kind: "text", label: "Name", value: "" }]);
    await menu.pick(0);
    expect(menu.get().rows).toEqual([{ kind: "text", label: "Name", value: "", refusal: "A name cannot be empty." }]);
    expect(submitted).toEqual([]);
    menu.type("Iris", 0);
    expect(menu.get().rows).toEqual([{ kind: "text", label: "Name", value: "Iris" }]);
    await menu.pick(0);

    expect(submitted).toEqual(["Iris"]);
    expect(ran).toEqual([[rename("Iris"), asks.id]]);
    expect(menu.closed()).toBe(true);
  });

  test("narrows the rows by their labels and details with its filter, without regard to case or accents, narrows a multi's choices, keeps text rows, and picks the first item left on Enter", async () => {
    const asks = asking({
      filter: true,
      rows: [
        item("Café", () => rename("Café"), { detail: "drinks" }),
        item("Head", () => rename("Head"), { detail: "rows" }),
        item("Filter", () => rename("Filter"), { heading: "Transform" }),
        {
          kind: "multi",
          label: "Columns",
          choices: [
            { id: "sepal", label: "Sepal", picked: false },
            { id: "petal", label: "Petal", picked: false },
          ],
          apply: () => [],
        },
        { kind: "text", label: "Name", submit: () => [] },
      ],
    });
    const { menu, ran } = opened({ action: asks });

    expect(menu.get().filter).toBe("");
    menu.type("CAFE");
    expect(menu.get().filter).toBe("CAFE");
    expect(labels(menu)).toEqual(["Café", "Name"]);
    menu.type("rows");
    expect(labels(menu)).toEqual(["Head", "Name"]);
    menu.type("pet");
    expect(menu.get().rows).toEqual([
      { kind: "multi", label: "Columns", choices: [{ id: "petal", label: "Petal", picked: false }] },
      { kind: "text", label: "Name", value: "" },
    ]);
    // Enter where a multi comes first ticks its first choice left.
    await menu.pick();
    expect(menu.get().rows[0]).toEqual({ kind: "multi", label: "Columns", choices: [{ id: "petal", label: "Petal", picked: true }] });
    menu.type("columns");
    expect(labels(menu)).toEqual(["Columns", "Name"]);

    menu.type("er");
    expect(labels(menu)).toEqual(["Filter", "Name"]);
    await menu.pick();
    expect(ran).toEqual([[rename("Filter"), asks.id]]);
  });

  test("shows the fault of a board action's function in place of the rows, as core logs it: a quick or a pick that throws, and what is no level or no batch", async () => {
    const reported = logged();
    const throws = opened({
      action: asking(() => {
        throw new Error("boom");
      }),
    });
    const noLevel = opened({ action: asking(() => ({ rows: [{ kind: "row", label: "x" }] }) as unknown as QuickLevel) });
    const picks = opened({
      action: asking({
        rows: [
          item("throws", () => {
            throw new Error("bang");
          }),
          item("neither", () => "rename" as unknown as CommandCall[]),
        ],
      }),
    });
    await picks.menu.pick(0);
    const thrown = picks.menu.get().error;
    picks.menu.escape();
    await picks.menu.pick(1);
    reported.restore();

    expect(throws.menu.get()).toMatchObject({ error: "boom", back: false, rows: [] });
    expect(noLevel.menu.get().error).toBe('It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.');
    expect(thrown).toBe("bang");
    expect(picks.menu.get().error).toBe("It gave neither a batch of commands nor a level of rows.");
    expect(reported.errors().map((line) => line.message)).toEqual([
      'The quick action of the board action "asks_action of blockr.test" failed: boom',
      'The quick action of the board action "asks_action of blockr.test" failed: It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.',
      'A row of the quick action of the board action "asks_action of blockr.test" failed: bang',
      'A row of the quick action of the board action "asks_action of blockr.test" failed: It gave neither a batch of commands nor a level of rows.',
    ]);
  });

  test("shows a quick that gives no level for the target as a fault, which core logs, where the board action has no advanced action", async () => {
    const none = action("none", { quick: () => undefined });
    const later = action("later", { quick: () => Promise.resolve(undefined) });
    const reported = logged();
    const { menu } = opened({ actions: [none, later] });

    const shown: (string | undefined)[] = [];
    for (const row of [0, 1]) {
      await menu.pick(row);
      shown.push(menu.get().error);
      menu.escape();
    }
    reported.restore();

    const why = "It gave no level for the target, and the board action has no advanced action to open in its place.";
    expect(shown).toEqual([why, why]);
    expect(reported.errors().map((line) => line.message)).toEqual([
      `The quick action of the board action "none_action of blockr.test" failed: ${why}`,
      `The quick action of the board action "later_action of blockr.test" failed: ${why}`,
    ]);
  });
});

describe("the advanced action that a menu opens", () => {
  test("opens at once, with nothing handed, as the user picks the row of a board action with an advanced action alone, or one whose quick gives no level, and the menu closes first", async () => {
    const alone = action("alone", { advanced: ADVANCED });
    const none = action("none", { quick: () => undefined, advanced: ADVANCED });
    const later = action("later", { quick: () => Promise.resolve(undefined), advanced: ADVANCED });

    for (const each of [alone, none, later]) {
      const { menu, advanced, closings } = opened({ actions: [each] });
      const picked = menu.pick(0);
      await settled();

      expect(menu.closed()).toBe(true);
      expect(advanced.map(({ id, opened: given }) => [id, given])).toEqual([[each.id, {}]]);
      // What the menu ran as it closed, and the pick, settle once the
      // advanced action ends.
      let ended = false;
      void closings[0]?.then(() => {
        ended = true;
      });
      await settled();
      expect(ended).toBe(false);
      advanced[0]?.end();
      await picked;
      expect(ended).toBe(true);
    }
  });

  test("opens at once where the quick action of a board action that the menu opens with gives no level, and the menu closes as it opens", () => {
    const none = action("none", { quick: () => undefined, advanced: ADVANCED });
    const { menu, advanced, closings } = opened({ action: none });

    expect(menu.closed()).toBe(true);
    expect(closings).toHaveLength(1);
    expect(advanced.map(({ id }) => id)).toEqual([none.id]);
  });

  test("shows the open icon on every level of a quick action whose board action has an advanced action, and at the end of an item that gives advanced", async () => {
    const next: QuickLevel = { title: "Input", rows: [item("x", () => rename("x"))] };
    const both = action("both", {
      advanced: ADVANCED,
      quick: () => ({ filter: true, picked: { step: 1 }, rows: [item("Head", () => next, { advanced: { class: "head" } }), item("Sort", () => rename("Sort"))] }),
    });
    const quickOnly = action("quick", { quick: () => ({ rows: [item("Head", () => rename("Head"), { advanced: { class: "head" } })] }) });

    const { menu } = opened({ action: both });
    expect(menu.get()).toMatchObject({ advanced: true, rows: [{ label: "Head", advanced: true }, { label: "Sort" }] });
    expect(menu.get().rows[1]).not.toHaveProperty("advanced");
    await menu.pick(0);
    expect(menu.get()).toMatchObject({ title: "Input", advanced: true, rows: [{ label: "x" }] });

    // A board action without an advanced action shows no open icon, even on
    // an item that gives advanced, and neither does the menu of a target.
    const other = opened({ action: quickOnly });
    expect(other.menu.get()).toMatchObject({ advanced: false, rows: [{ label: "Head" }] });
    expect(other.menu.get().rows[0]).not.toHaveProperty("advanced");
    expect(opened({ actions: [both] }).menu.get().advanced).toBe(false);
  });

  test("opens from the level's open icon with the level's picked and the filter's text, and from an item's with its advanced, closing the menu first", async () => {
    const both = action("both", {
      advanced: ADVANCED,
      quick: () => ({ filter: true, picked: { step: 1 }, rows: [item("Head", () => rename("Head"), { advanced: { class: "head" } }), item("Sort", () => rename("Sort"))] }),
    });
    const plain = action("plain", { advanced: ADVANCED, quick: () => ({ rows: [item("Head", () => rename("Head"))] }) });

    const level = opened({ action: both });
    level.menu.type("Hea");
    const fromLevel = level.menu.openAdvanced();
    expect(level.menu.closed()).toBe(true);
    expect(level.advanced.map(({ id, opened: given }) => [id, given])).toEqual([[both.id, { picked: { step: 1 }, filter: "Hea" }]]);
    level.advanced[0]?.end();
    await fromLevel;

    const row = opened({ action: both });
    void row.menu.openAdvanced(0);
    expect(row.advanced.map(({ opened: given }) => given)).toEqual([{ picked: { class: "head" }, filter: "" }]);

    // A level without a filter and without picked hands neither.
    const bare = opened({ action: plain });
    void bare.menu.openAdvanced();
    expect(bare.advanced.map(({ opened: given }) => given)).toEqual([{}]);
    expect(bare.ran).toEqual([]);
  });

  test("opens nothing from an item that shows no open icon, nor from a level that loads, and closes and opens nothing where the board action no longer applies", async () => {
    const both = action("both", {
      advanced: ADVANCED,
      quick: () => ({ rows: [item("Head", () => rename("Head"), { advanced: { class: "head" } }), item("Sort", () => rename("Sort"))] }),
    });
    const slow = action("slow", { advanced: ADVANCED, quick: () => new Promise<QuickLevel>(() => undefined) });

    const plain = opened({ action: both });
    await plain.menu.openAdvanced(1);
    await plain.menu.openAdvanced(7);
    expect(plain.menu.closed()).toBe(false);
    expect(plain.advanced).toEqual([]);

    const loading = opened({ action: slow });
    await loading.menu.openAdvanced();
    expect(loading.menu.closed()).toBe(false);
    expect(loading.advanced).toEqual([]);

    const gone = opened({ action: both }, { applies: () => false });
    await gone.menu.openAdvanced();
    expect(gone.menu.closed()).toBe(true);
    expect(gone.advanced).toEqual([]);
  });
});

describe("an open menu", () => {
  test("gives the same level until it changes, tells its subscribers of each change and once more as it closes, and does nothing once closed", async () => {
    const asks = asking({ filter: true, rows: [item("one", () => rename("One"))] });
    const { menu, ran, closings } = opened({ action: asks });
    let heard = 0;
    const off = menu.subscribe(() => heard++);

    const first = menu.get();
    expect(menu.get()).toBe(first);
    menu.type("o");
    expect(menu.get()).not.toBe(first);
    expect(heard).toBe(1);
    // The same text again changes nothing.
    menu.type("o");
    expect(heard).toBe(1);

    menu.drop();
    expect(heard).toBe(2);
    expect(menu.closed()).toBe(true);
    expect(closings).toEqual([undefined]);

    await menu.pick(0);
    menu.type("x");
    menu.tick(0, "x");
    menu.escape();
    await menu.close();
    menu.drop();
    expect(ran).toEqual([]);
    expect(closings).toHaveLength(1);
    expect(heard).toBe(2);
    off();
  });

  test("drops the ticks of a multi where it is dropped, as where another board opens", () => {
    const asks = asking({ rows: [{ kind: "multi", label: "Blocks", choices: [{ id: "a", label: "Head", picked: false }], apply: () => rename("x") }] });
    const { menu, ran } = opened({ action: asks });

    menu.tick(0, "a");
    menu.drop();

    expect(ran).toEqual([]);
    expect(menu.closed()).toBe(true);
  });
});
