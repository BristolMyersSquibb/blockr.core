// The rules of a quick action, on the logic that runs every menu of board
// actions and draws nothing, which core tests once here: the menu of a target
// and its rows, the levels that picks open, Escape, the ticks of a multi, a
// text row's refusal, the filter, promises that load or reject, and the faults
// of a board action's functions.

import { describe, expect, test } from "vitest";
import { logged } from "../testing/logged.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BoardAction, CommandCall } from "./actions.ts";
import { inMenuOrder, openMenu } from "./quick.ts";
import type { MenuParts, MenuStart, QuickLevel, QuickRow } from "./quick.ts";

const BOARD = { name: "Board" } as unknown as Board;
const CORE = {} as CoreApi;

// A board action of blockr.test on blocks, labelled by its name.
function action(name: string, more: Partial<BoardAction> = {}): BoardAction {
  return { id: `blockr.test-${name}_action`, label: name, on: ["block"], category: "test", ...more };
}

// A batch that renames block a.
const rename = (name: string): CommandCall[] => [{ id: "blockr.core-rename_block", args: { id: "a", name } }];

// An item that picks what it is given.
function item(label: string, picked: () => ReturnType<Extract<QuickRow, { kind: "item" }>["pick"]>, more = {}): QuickRow {
  return { kind: "item", label, pick: picked, ...more };
}

// A board action whose quick action is the level given.
function asking(level: QuickLevel | (() => ReturnType<NonNullable<BoardAction["quick"]>>)): BoardAction {
  return action("asks", { quick: typeof level === "function" ? level : () => level });
}

// A menu with parts that record each batch that it runs, with the id of its
// board action, each failure that it reports, and each closing, with the
// promise of what closing ran. A batch of `refused` rejects.
function opened(start: MenuStart, more: Partial<MenuParts> = {}) {
  const ran: [readonly CommandCall[], string][] = [];
  const failures: [string, string][] = [];
  const closings: (Promise<void> | undefined)[] = [];
  const menu = openMenu(start, {
    board: () => BOARD,
    target: { block: "a" },
    core: () => CORE,
    applies: () => true,
    run(batch, by) {
      ran.push([batch, by.id]);
      return batch.some((call) => call.id === "refused") ? Promise.reject(new Error("Refused.")) : Promise.resolve();
    },
    failed(by, failure) {
      failures.push([by.id, failure.message]);
    },
    closed(promise) {
      closings.push(promise);
    },
    ...more,
  });
  return { menu, ran, failures, closings };
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
    expect(reported.errors().map((line) => line.message)).toEqual(['The detail of the board action "blockr.test-broken_action" failed: boom']);
  });

  test("closes and runs the batch that a board action's run returns, as the batch of that board action, calling run at once", async () => {
    const calls: unknown[] = [];
    const runs = action("runs", {
      run(board, target, core) {
        calls.push([board, target, core]);
        return rename("Run");
      },
    });
    const { menu, ran, closings } = opened({ actions: [action("other", { run: () => [] }), runs] });

    const picked = menu.pick(1);
    expect(calls).toEqual([[BOARD, { block: "a" }, CORE]]);
    expect(menu.closed()).toBe(true);
    await picked;

    expect(ran).toEqual([[rename("Run"), runs.id]]);
    expect(closings).toHaveLength(1);
    expect(closings[0]).toBeInstanceOf(Promise);
  });

  test("closes and runs nothing where run returns no batch or an empty one, or where the menu of the target no longer offers the board action", async () => {
    for (const [run, applies] of [
      [() => undefined, true],
      [() => [], true],
      [() => rename("Never"), false],
    ] as const) {
      const { menu, ran, closings } = opened({ actions: [action("runs", { run })] }, { applies: () => applies });
      await menu.pick(0);
      expect(menu.closed()).toBe(true);
      expect(ran).toEqual([]);
      expect(closings).toEqual([undefined]);
    }
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

  test("rejects a pick with the refusal of its batch, once the menu has closed", async () => {
    const asks = asking({ rows: [item("refused", () => [{ id: "refused" }])] });
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
      'A row of the quick action of the board action "blockr.test-asks_action" failed: No columns.',
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
        return [{ id: "blockr.core-change_stack", args: { id: "s", blocks: [...picked] } }];
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
    expect(ticking.ran).toEqual([[[{ id: "blockr.core-change_stack", args: { id: "s", blocks: ["b", "c"] } }], asks.id]]);

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
      [asks.id, 'A row of the quick action of the board action "blockr.test-asks_action" failed: boom'],
      [
        asks.id,
        'A row of the quick action of the board action "blockr.test-asks_action" failed: It gave no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].',
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
      'The quick action of the board action "blockr.test-asks_action" failed: boom',
      'The quick action of the board action "blockr.test-asks_action" failed: It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.',
      'A row of the quick action of the board action "blockr.test-asks_action" failed: bang',
      'A row of the quick action of the board action "blockr.test-asks_action" failed: It gave neither a batch of commands nor a level of rows.',
    ]);
  });

  test("shows that core does not open advanced actions yet, where quick gives no level for the target and where a board action has neither run nor quick", async () => {
    const none = action("none", { quick: () => undefined });
    const later = action("later", { quick: () => Promise.resolve(undefined) });
    const advanced = action("advanced");
    const { menu } = opened({ actions: [none, later, advanced] });

    const shown: (string | undefined)[] = [];
    for (const row of [0, 1, 2]) {
      await menu.pick(row);
      shown.push(menu.get().error);
      menu.escape();
    }

    expect(shown).toEqual([
      'The board action "blockr.test-none_action" gives no quick action for the target, and core does not open advanced actions yet.',
      'The board action "blockr.test-later_action" gives no quick action for the target, and core does not open advanced actions yet.',
      'The board action "blockr.test-advanced_action" asks the user through its advanced action, which core does not open yet.',
    ]);
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
