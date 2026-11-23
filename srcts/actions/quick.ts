// The quick action of a board action, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-quick-action):
// a level of rows, in which an item may open a further level at the same spot.
// The rows come in kinds that core defines: an item, which the user picks, a
// multi, whose choices the user ticks, and a text row, a field, as for a name.
//
// Here too is the logic that runs every menu of board actions by the rules of
// a quick action, and that draws nothing.
// The module that the blockr option board_action_menu names draws what it
// shows, core's own menu unless the app names another (./module.ts).
// The CoreApi for tests drives it instead, picking rows by their labels
// (../stubs/menu.ts).
// So the rules are written once, and a test of a board action needs no
// browser.
//
// A menu opens at one of two first levels:
// - The menu of a target, which menu() opens: a row for each board action that
//   on() gives the target, an item with the board action's label, icon,
//   detail and category. Picking it closes the menu and runs the board
//   action's run, as trigger() does, or opens its quick action as the menu's
//   next level.
// - The quick action of a board action, which trigger() opens.
// The rules, after the menus that blockr.dock#544 planned:
// - Picking an item whose pick returns a batch closes the menu and runs the
//   batch as one step of the user, of the board action whose row or quick
//   action the item is.
// - Picking an item whose pick returns a level shows that level in place of
//   the menu's, at the same spot. Escape goes back a level, or closes the menu
//   from its first level.
// - A multi applies its ticks as the menu closes by a click outside it or by
//   Tab, where they differ from the choices that it picked, and drops them on
//   Escape.
// - A text row refuses a value in place, with what its check returns, once
//   the user has tried to submit it, and from then on as the user types. It
//   submits on Enter.
// - The filter narrows the rows by their labels and details as the user
//   types, and a multi's choices by their labels. Enter in the filter picks
//   the first item left, or ticks the first choice left where a multi comes
//   first.
// - Where quick or pick returns a promise, the menu shows at the same spot
//   that it is loading until the promise resolves, and the error's message in
//   place of the rows where the promise rejects. Escape goes back as from any
//   level, and drops what the promise brings later.
// A function of a quick action that throws or rejects, or gives what is no
// level or no batch, is a fault of the board action's package, which core
// logs at error, and the menu shows the message in place of the rows.
// A board action's run, and a multi's apply as the menu closes, fail after the
// menu has closed. So core tells the user of their faults through a
// notification of the code board_action_failed, as trigger() does
// (MenuParts, below).
// The open icon opens the board action's advanced action (./advanced.ts):
// - Where the board action has an advanced action, every level of its quick
//   action shows the open icon at its top right, and an item that gives
//   `advanced` shows it at its end.
// - A click on the icon closes the menu and opens the advanced action with
//   the level's `picked` or the item's `advanced`, and with the text in the
//   level's filter, where the level has one. A click elsewhere on the item
//   picks it.
// - Where the board action has no quick action for the target, its row opens
//   its advanced action at once, and so does a quick that gives no level.
// A quick that gives no level where the board action has no advanced action
// is a fault of the board action's package, since nothing would open.

import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { log } from "../log/index.ts";
import { errorMessage, isObject } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";

// A level of a quick action: its rows, under the title given, with a field
// that narrows them as the user types where `filter` is true. Its `picked`
// is what was picked so far, which the level's open icon hands the advanced
// action.
export interface QuickLevel {
  readonly title?: string;
  readonly filter?: boolean;
  readonly picked?: Json;
  readonly rows: readonly QuickRow[];
}

export type QuickRow =
  // A row that the user picks, under its heading, as a block class under its
  // category, with its icon, SVG markup, and with its detail at its end. Its
  // category is that of its board action, in the menu of a target. Its pick
  // returns the batch to run, or the next level, either of which may wait.
  // Its `advanced` is what its open icon hands the advanced action.
  // An item that offers a pick or a toggle gives `checked`. It is true for the
  // current pick and for a toggle that is on, and false for another pick and
  // for a toggle that is off. A tick shows at the item's start while
  // `checked` is true. Picking the item runs its pick, as for any item, since
  // the tick only shows what holds.
  | {
      readonly kind: "item";
      readonly label: string;
      readonly heading?: string;
      readonly icon?: string;
      readonly detail?: string;
      readonly category?: BoardAction["category"];
      readonly checked?: boolean;
      pick(): readonly CommandCall[] | QuickLevel | Promise<readonly CommandCall[] | QuickLevel>;
      readonly advanced?: Json;
    }
  // Choices that the user ticks, of which `picked` says which are ticked as
  // the level opens. It applies the ticks as the menu closes, with the ids of
  // the choices ticked then, in their order, by the batch that `apply` returns.
  | {
      readonly kind: "multi";
      readonly label: string;
      readonly choices: readonly { readonly id: string; readonly label: string; readonly picked: boolean }[];
      apply(picked: readonly string[]): readonly CommandCall[];
    }
  // A field, as for a name, which holds `value` as the level opens. Its check
  // says why it refuses a value, or returns undefined for one that it takes,
  // and its submit returns the batch that the value runs.
  | {
      readonly kind: "text";
      readonly label: string;
      readonly value?: string;
      check?(value: string): string | undefined;
      submit(value: string): readonly CommandCall[];
    };

// A level as the menu shows it now, which the module that draws the menu
// draws.
export interface ShownLevel {
  // A number of its own, which another level that shows in its place does not
  // have, so that a view draws that level afresh.
  readonly id: number;
  // What names the level to assistive technology: its title, or else the label
  // of the board action whose quick action it is, or "Board actions" for the
  // menu of a target.
  readonly label: string;
  readonly title?: string;
  // The filter's text, where the level has a filter.
  readonly filter?: string;
  // Whether the level shows the open icon, as it does where the board action
  // has an advanced action.
  readonly advanced: boolean;
  // Whether Escape goes back a level, rather than closing the menu.
  readonly back: boolean;
  // Whether a promise of quick, pick or run is pending.
  readonly loading: boolean;
  // The message that such a promise rejected with, or that a fault of the
  // board action's package gave.
  readonly error?: string;
  // The rows that the filter leaves, without their functions, and a multi
  // with the choices that it leaves.
  readonly rows: readonly ShownRow[];
}

export type ShownRow =
  // With `advanced` where it shows the open icon, as it does where it gives
  // `advanced` and the board action has an advanced action.
  | (Omit<Extract<QuickRow, { kind: "item" }>, "pick" | "advanced"> & { readonly advanced?: true })
  // Its choices, each with whether it is ticked now.
  | Omit<Extract<QuickRow, { kind: "multi" }>, "apply">
  // Its value now, and why its check refuses that value, once the user has
  // tried to submit it.
  | { readonly kind: "text"; readonly label: string; readonly value: string; readonly refusal?: string };

// A menu that core runs by the rules of a quick action, and that draws
// nothing, as the module that draws the menu gets it (./module.ts).
// Each member stands for what the user does in the menu, and does nothing once
// the menu has closed.
// The rows are those of the level shown, by their index in its rows, whatever
// the order in which the module shows them.
// Core runs the batch that a pick, a text row or the ticks of a multi give, as
// one step of the user, and tells the user why the store or the command
// registry refused it.
// No member relies on `this`, and get() returns the same object until the menu
// changes, so that a module in React reads the menu with
// `useSyncExternalStore(menu.subscribe, menu.get)`.
export interface OpenMenu {
  // The level that the menu shows now, the same object until it changes.
  readonly get: () => ShownLevel;
  // Calls fn whenever that changes, and once more as the menu closes.
  readonly subscribe: (fn: () => void) => () => void;
  // A click or Enter on a row: it picks an item and submits a text row.
  // Without a row, it is Enter in the filter, which picks the first row left.
  readonly pick: (row?: number) => void;
  // A click on the open icon of an item, by the item's index, or without
  // one, on the level's, which closes the menu and opens the advanced action.
  // It does nothing where the item or the level shows no open icon.
  readonly openAdvanced: (row?: number) => void;
  // A click on a choice of a multi, by the choice's id, which ticks or unticks
  // it.
  readonly tick: (row: number, choice: string) => void;
  // The filter's text, or with a row, a text row's value, as the user types.
  readonly type: (value: string, row?: number) => void;
  // Escape.
  readonly escape: () => void;
  // A click outside the menu, or Tab, which applies the ticks of a multi.
  readonly close: () => void;
}

// The menu as core and the CoreApi for tests drive it: the members of
// OpenMenu, of which pick(), openAdvanced() and close() say when what they
// started has settled, with whether the menu has closed and drop().
export interface DrivenMenu extends Omit<OpenMenu, "pick" | "openAdvanced" | "close"> {
  // Whether the menu has closed.
  readonly closed: () => boolean;
  // It resolves once what the pick started has settled: the next level shows,
  // or the menu has closed and its batch has run.
  // It rejects with the refusal where the store or the command registry
  // refuses the batch, which a caller that waits for nothing ignores.
  readonly pick: (row?: number) => Promise<void>;
  // It resolves once the advanced action ends.
  readonly openAdvanced: (row?: number) => Promise<void>;
  // It resolves once the batch that the ticks apply has run, and rejects with
  // the refusal, as pick() does.
  readonly close: () => Promise<void>;
  // Closes the menu and drops what the user ticked, as where another board
  // opens.
  readonly drop: () => void;
}

// Where a menu opens: at the menu of a target, with the board actions that a
// menu of it offers, or at the quick action of a board action.
export type MenuStart = { readonly actions: readonly BoardAction[] } | { readonly action: BoardAction };

// What a menu calls out to.
export interface MenuParts {
  // The board as it is now, and the target and CoreApi, which a board
  // action's run, quick and detail get.
  readonly board: () => Board;
  readonly target: Target;
  readonly core: () => CoreApi;
  // Whether a menu of the target offers the board action now, which picking
  // its row checks first, as trigger() does, since the board may have changed
  // since the menu opened.
  readonly applies: (action: BoardAction) => boolean;
  // Runs a batch of the board action as one step of the user, and resolves
  // once it has run, or rejects with the refusal.
  readonly run: (batch: readonly CommandCall[], action: BoardAction) => Promise<void>;
  // Runs the board action's run as trigger() does, and resolves once its
  // batch has run, or rejects with the refusal or the failure, which core has
  // told the user of. It calls run at once, so that a copy within the user's
  // click runs within it.
  readonly trigger: (action: BoardAction) => Promise<void>;
  // Opens the board action's advanced action on the target, with what the
  // open icon hands it, if anything, and resolves once it ends.
  readonly advanced: (action: BoardAction, opened: { readonly picked?: Json; readonly filter?: string }) => Promise<void>;
  // Tells the user that a multi's apply of the board action failed as the
  // menu closed, where the menu no longer shows it, and logs it as a fault of
  // the board action's package.
  readonly failed: (action: BoardAction, failure: Error) => void;
  // Called once, as the menu closes, whatever closed it, with the promise of
  // the batch that closing it runs, if any.
  readonly closed: (ran: Promise<void> | undefined) => void;
}

// What the menu holds for one level: the level, the error in place of it, or
// that it loads.
interface Entry {
  // The number of its level (ShownLevel, above).
  readonly id: number;
  // The board action whose quick action the level is of, which none is for the
  // menu of a target.
  readonly action: BoardAction | undefined;
  // For the menu of a target, the board action of each row.
  readonly actions?: readonly BoardAction[];
  readonly state: { readonly kind: "loading" } | { readonly kind: "error"; readonly message: string } | { readonly kind: "level"; readonly level: QuickLevel };
  // What the user typed into the filter, each text row's value, by the row's
  // index in the level's rows, the text rows that the user tried to submit,
  // and the ticks of each multi.
  filter: string;
  readonly values: Map<number, string>;
  readonly tried: Set<number>;
  readonly ticks: Map<number, ReadonlySet<string>>;
}

// A row of the level shown: the level's row, by its index in the level's rows,
// and for a multi, the choices that the filter leaves.
interface Showing {
  readonly row: QuickRow;
  readonly index: number;
  readonly choices?: MultiRow["choices"];
}

type MultiRow = Extract<QuickRow, { kind: "multi" }>;
type TextRow = Extract<QuickRow, { kind: "text" }>;

// The function of a board action whose fault a menu shows: its quick, or the
// pick or the submit of a row of its quick action, whose check counts as the
// row's too.
type Part = "quick" | "pick" | "submit";

// What each part names in the line that core logs for its fault.
const SUBJECTS: Readonly<Record<Part, string>> = {
  quick: "The quick action of the board action",
  pick: "A row of the quick action of the board action",
  submit: "A row of the quick action of the board action",
};

// What core says of what a part gave, where it is none of what the part
// gives.
const GAVE: Readonly<Record<Part, string>> = {
  quick: 'It gave no level of rows, as { rows: [{ kind: "item", label: "Head", pick: () => [] }] }.',
  pick: "It gave neither a batch of commands nor a level of rows.",
  submit: 'It gave no batch of commands, as [{ id: "blockr.core-rename_block", args: { id: "a", name: "Head" } }].',
};

// What core says of what a multi's apply gave, where it is no batch.
const NO_BATCH = 'It gave no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].';

// What core says of a quick that gives no level where the board action has no
// advanced action to open in its place.
const NO_LEVEL = "It gave no level for the target, and the board action has no advanced action to open in its place.";

// What names the menu of a target to assistive technology.
const MENU = "Board actions";

// The categories that the menu of a target shows first, in their order, and
// the one that it shows last. Those of packages come between, in the order in
// which they first come.
const FIRST = ["add", "edit", "clipboard"];
const LAST = "remove";

// The board actions of the menu of a target in the order that it shows them:
// those of each category together, in the order of the categories above, and
// in the order of on() within a category.
export function inMenuOrder(actions: readonly BoardAction[]): BoardAction[] {
  const seen = [...new Set(actions.map(({ category }) => category))].filter(
    (category) => !FIRST.includes(category) && category !== LAST,
  );
  const order = [...FIRST, ...seen, LAST];
  return [...actions].sort((a, b) => order.indexOf(a.category) - order.indexOf(b.category));
}

export function openMenu(start: MenuStart, parts: MenuParts): DrivenMenu {
  const stack: Entry[] = [];
  const listeners = new Set<() => void>();
  let ended = false;
  // The levels that the menu has shown, which number them.
  let levels = 0;
  // What the menu shows now, and the rows of the level behind it, until the
  // next change.
  let view: { readonly level: ShownLevel; readonly rows: readonly Showing[] } | undefined;

  function changed(): void {
    view = undefined;
    for (const fn of [...listeners]) fn();
  }

  function now(): { readonly level: ShownLevel; readonly rows: readonly Showing[] } {
    view ??= shownNow();
    return view;
  }

  function entry(action: BoardAction | undefined, state: Entry["state"], actions?: readonly BoardAction[]): Entry {
    return {
      id: ++levels,
      action,
      ...(actions === undefined ? {} : { actions }),
      state,
      filter: "",
      values: new Map(),
      tried: new Set(),
      ticks: new Map(),
    };
  }

  // Shows the state given above the level shown, or in place of the entry
  // given.
  function place(state: Entry["state"], action: BoardAction, replacing?: Entry): Entry {
    const next = entry(action, state);
    if (replacing === undefined) stack.push(next);
    else stack.splice(stack.indexOf(replacing), 1, next);
    changed();
    return next;
  }

  // Closes the menu, once, and tells its parts with the promise of the batch
  // that closing it runs.
  function end(ran?: Promise<void>): void {
    if (ended) return;
    ended = true;
    stack.length = 0;
    changed();
    listeners.clear();
    parts.closed(ran);
  }

  // Closes the menu and runs the batch of the board action, where it holds a
  // command.
  function finish(batch: readonly CommandCall[], action: BoardAction): Promise<void> {
    if (batch.length === 0) {
      end();
      return Promise.resolve();
    }
    return closing(parts.run(batch, action));
  }

  // Closes the menu, which what it started goes on behind, and resolves or
  // rejects as that does. The caller that waits for it gets the refusal, and
  // the page hears no rejection that nobody waits for.
  function closing(ran: Promise<void>): Promise<void> {
    ran.catch(() => undefined);
    end(ran);
    return ran;
  }

  // Shows what a function of the board action gives, above the level shown,
  // or in place of the entry given: the menu closes for a batch and runs it,
  // and shows a level or why the function failed. A promise shows as loading
  // until it settles, unless the user has gone back or closed the menu by
  // then.
  function settle(given: () => unknown, action: BoardAction, what: Part, replacing?: Entry): Promise<void> {
    let value: unknown;
    try {
      value = given();
    } catch (error) {
      place(failure(action, what, error), action, replacing);
      return Promise.resolve();
    }
    if (!isThenable(value)) return outcome(value, action, what, replacing);
    const loading = place({ kind: "loading" }, action, replacing);
    const current = (): boolean => !ended && stack.at(-1) === loading;
    return Promise.resolve(value).then(
      (resolved) => (current() ? outcome(resolved, action, what, loading) : undefined),
      (error: unknown) => {
        if (current()) place(failure(action, what, error), action, loading);
      },
    );
  }

  // What a function of the board action gave, once it is no promise.
  function outcome(value: unknown, action: BoardAction, what: Part, replacing?: Entry): Promise<void> {
    if (what === "quick" && value === undefined) {
      if (action.advanced !== undefined) return advanced(action, {});
      place(failure(action, what, new TypeError(NO_LEVEL)), action, replacing);
      return Promise.resolve();
    }
    if (what !== "quick" && isBatch(value)) return finish(value, action);
    if ((what === "quick" || what === "pick") && isLevel(value)) {
      place({ kind: "level", level: value }, action, replacing);
      return Promise.resolve();
    }
    place(failure(action, what, new TypeError(GAVE[what])), action, replacing);
    return Promise.resolve();
  }

  // The error that shows in place of the rows, for what a function of the
  // board action threw or gave, which core logs as a fault of the board
  // action's package.
  function failure(action: BoardAction, what: Part, error: unknown): Entry["state"] {
    log.error(new Error(`${SUBJECTS[what]} "${action.id}" failed: ${errorMessage(error)}`, { cause: error }));
    return { kind: "error", message: errorMessage(error) };
  }

  // What the level shown shows, and the rows of the level that the filter
  // leaves behind its rows.
  function shownNow(): { readonly level: ShownLevel; readonly rows: readonly Showing[] } {
    const current = stack.at(-1);
    const back = stack.length > 1;
    if (current === undefined) return { level: { id: 0, label: MENU, advanced: false, back: false, loading: false, rows: [] }, rows: [] };
    const { id, state } = current;
    const label = current.action?.label ?? MENU;
    if (state.kind === "loading") return { level: { id, label, advanced: false, back, loading: true, rows: [] }, rows: [] };
    if (state.kind === "error") {
      return { level: { id, label, advanced: false, back, loading: false, error: state.message, rows: [] }, rows: [] };
    }
    const { title, filter } = state.level;
    const rows = filtered(state.level, filter === true ? folded(current.filter.trim()) : "");
    return {
      level: {
        id,
        label: title ?? label,
        ...(title === undefined ? {} : { title }),
        ...(filter === true ? { filter: current.filter } : {}),
        advanced: current.action?.advanced !== undefined,
        back,
        loading: false,
        rows: rows.map((showing) => shownRow(current, showing)),
      },
      rows,
    };
  }

  function shownRow(current: Entry, { row, index, choices }: Showing): ShownRow {
    if (row.kind === "item") {
      const { label, heading, icon, detail, category, checked } = row;
      return {
        kind: "item",
        label,
        ...(heading === undefined ? {} : { heading }),
        ...(icon === undefined ? {} : { icon }),
        ...(detail === undefined ? {} : { detail }),
        ...(category === undefined ? {} : { category }),
        ...(checked === undefined ? {} : { checked }),
        ...(row.advanced !== undefined && current.action?.advanced !== undefined ? { advanced: true as const } : {}),
      };
    }
    if (row.kind === "multi") {
      const ticked = ticks(current, index, row);
      return {
        kind: "multi",
        label: row.label,
        choices: (choices ?? row.choices).map(({ id, label }) => ({ id, label, picked: ticked.has(id) })),
      };
    }
    const value = current.values.get(index) ?? row.value ?? "";
    const refusal = current.tried.has(index) ? refused(current, row, value) : undefined;
    return { kind: "text", label: row.label, value, ...(refusal === undefined ? {} : { refusal }) };
  }

  // Why the text row's check refuses the value, or undefined where it takes
  // it. A check that throws refuses with what it threw, which core logs as a
  // fault of the board action's package.
  function refused(current: Entry, row: TextRow, value: string): string | undefined {
    if (row.check === undefined) return undefined;
    try {
      const why: unknown = row.check(value);
      return typeof why === "string" && why !== "" ? why : undefined;
    } catch (error) {
      if (current.action !== undefined) failure(current.action, "submit", error);
      return errorMessage(error);
    }
  }

  // The level that the menu shows, and its row at the index given, where it
  // shows a level with such a row.
  function at(index: number | undefined): { readonly current: Entry; readonly found?: Showing } | undefined {
    const current = stack.at(-1);
    if (ended || current?.state.kind !== "level") return undefined;
    const found = index === undefined ? undefined : now().rows[index];
    return found === undefined ? { current } : { current, found };
  }

  // Picks the board action of a row of the menu of a target, or opens the
  // quick action that trigger() opens: it closes the menu and runs the board
  // action's run as trigger() does, or shows its quick action as the menu's
  // next level. Where the menu of the target no longer offers the board
  // action, picking its row closes the menu and runs nothing, as trigger()
  // does.
  function enter(action: BoardAction): Promise<void> {
    if (action.run !== undefined) return closing(parts.trigger(action));
    if (action.quick !== undefined) return settle(() => action.quick?.(parts.board(), parts.target, parts.core()), action, "quick");
    return advanced(action, {});
  }

  // Closes the menu and opens the board action's advanced action, with what
  // the open icon hands it, if anything. The menu closes first, so that the
  // keyboard's focus is back at the element that the user triggered from as
  // the advanced action opens. It resolves once the advanced action ends.
  function advanced(action: BoardAction, opened: { readonly picked?: Json; readonly filter?: string }): Promise<void> {
    let resolve: () => void = () => undefined;
    let reject: (error: unknown) => void = () => undefined;
    const ran = closing(
      new Promise<void>((resolved, rejected) => {
        resolve = resolved;
        reject = rejected;
      }),
    );
    parts.advanced(action, opened).then(resolve, reject);
    return ran;
  }

  // A click on an open icon, the item's at the index given, or the level's:
  // it opens the advanced action with the item's `advanced` or the level's
  // `picked`, and with the text in the level's filter, where the level has
  // one. Where the menu of the target no longer offers the board action, it
  // closes the menu and opens nothing, as picking the board action's row
  // does.
  function openAdvanced(index?: number): Promise<void> {
    const shown = at(index);
    if (shown === undefined) return Promise.resolve();
    const { current, found } = shown;
    const { action, state } = current;
    if (action?.advanced === undefined || state.kind !== "level") return Promise.resolve();
    let picked: Json | undefined = state.level.picked;
    if (index !== undefined) {
      const row = found?.row;
      if (row?.kind !== "item" || row.advanced === undefined) return Promise.resolve();
      picked = row.advanced;
    }
    if (!parts.applies(action)) {
      end();
      return Promise.resolve();
    }
    return advanced(action, {
      ...(picked === undefined ? {} : { picked }),
      ...(state.level.filter === true ? { filter: current.filter } : {}),
    });
  }

  // Submits a text row's value where its check takes it, or shows in place
  // why it refuses the value.
  function submit(current: Entry, row: TextRow, index: number, action: BoardAction): Promise<void> {
    const value = current.values.get(index) ?? row.value ?? "";
    current.tried.add(index);
    if (refused(current, row, value) !== undefined) {
      changed();
      return Promise.resolve();
    }
    return settle(() => row.submit(value), action, "submit");
  }

  // The batch that the ticks of the level's multis apply, where they differ
  // from what each picked, in the order of the rows. A multi whose apply
  // throws or gives no batch adds nothing, and its parts tell the user why.
  function applied(current: Entry): readonly CommandCall[] {
    const { action, state } = current;
    if (action === undefined || state.kind !== "level") return [];
    return state.level.rows.flatMap((row, index): readonly CommandCall[] => {
      if (row.kind !== "multi" || !current.ticks.has(index)) return [];
      const ticked = ticks(current, index, row);
      const before = row.choices.filter((choice) => choice.picked).map(({ id }) => id);
      const picked = row.choices.filter((choice) => ticked.has(choice.id)).map(({ id }) => id);
      if (picked.length === before.length && picked.every((id, i) => id === before[i])) return [];
      try {
        const batch: unknown = row.apply(picked);
        if (!isBatch(batch)) throw new TypeError(NO_BATCH);
        return batch;
      } catch (error) {
        parts.failed(action, new Error(`${SUBJECTS.submit} "${action.id}" failed: ${errorMessage(error)}`, { cause: error }));
        return [];
      }
    });
  }

  function pick(index?: number): Promise<void> {
    const shown = at(index);
    if (shown === undefined) return Promise.resolve();
    const { current, found } = shown;
    if (index === undefined) {
      // Enter in the filter: the first item left, or the first choice of a
      // multi that comes first.
      const rows = now().rows;
      const first = rows.findIndex(({ row }) => row.kind !== "text");
      const row = rows[first];
      if (row?.row.kind === "multi") {
        const choice = (row.choices ?? row.row.choices)[0];
        if (choice !== undefined) tick(first, choice.id);
        return Promise.resolve();
      }
      return row === undefined ? Promise.resolve() : pick(first);
    }
    if (found === undefined) return Promise.resolve();
    const action = current.actions?.[found.index];
    if (action !== undefined) {
      if (parts.applies(action)) return enter(action);
      end();
      return Promise.resolve();
    }
    const { row } = found;
    if (current.action === undefined) return Promise.resolve();
    if (row.kind === "item") return settle(() => row.pick(), current.action, "pick");
    if (row.kind === "text") return submit(current, row, found.index, current.action);
    return Promise.resolve();
  }

  function tick(index: number, choice: string): void {
    const shown = at(index);
    const row = shown?.found?.row;
    if (shown?.found === undefined || row?.kind !== "multi" || !row.choices.some(({ id }) => id === choice)) return;
    const { current, found } = shown;
    const ticked = new Set(ticks(current, found.index, row));
    if (ticked.has(choice)) ticked.delete(choice);
    else ticked.add(choice);
    current.ticks.set(found.index, ticked);
    changed();
  }

  if ("actions" in start) {
    const board = parts.board();
    const actions = inMenuOrder(start.actions);
    stack.push(
      entry(undefined, { kind: "level", level: { rows: actions.map((action) => actionRow(action, board, parts.target)) } }, actions),
    );
  } else {
    void enter(start.action);
  }

  return {
    get: () => now().level,
    subscribe(fn) {
      if (ended) return () => undefined;
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    closed: () => ended,
    pick,
    openAdvanced,
    tick,
    type(value, index) {
      const shown = at(index);
      if (shown === undefined) return;
      const { current, found } = shown;
      if (index === undefined) {
        if (current.state.kind !== "level" || current.state.level.filter !== true || current.filter === value) return;
        current.filter = value;
        changed();
        return;
      }
      if (found?.row.kind !== "text") return;
      current.values.set(found.index, value);
      changed();
    },
    escape() {
      if (ended) return;
      if (stack.length > 1) {
        stack.pop();
        changed();
        return;
      }
      end();
    },
    close() {
      const current = stack.at(-1);
      if (ended || current === undefined) return Promise.resolve();
      const batch = applied(current);
      if (current.action !== undefined) return finish(batch, current.action);
      end();
      return Promise.resolve();
    },
    drop() {
      end();
    },
  };
}

// The choices of a multi that are ticked now: those that the user ticked, or
// those that it picked until the user ticks one.
function ticks(current: Entry, index: number, row: MultiRow): ReadonlySet<string> {
  return current.ticks.get(index) ?? new Set(row.choices.filter((choice) => choice.picked).map(({ id }) => id));
}

// The rows of the level that the filter's text leaves, folded: an item whose
// label or detail holds the text, a multi whose label holds it, with its
// choices, or a multi with those of its choices whose labels hold it, and
// every text row.
function filtered(level: QuickLevel, query: string): readonly Showing[] {
  return level.rows.flatMap((row, index): Showing[] => {
    if (query === "" || row.kind === "text") return [{ row, index }];
    if (row.kind === "item") return [row.label, row.detail ?? ""].some((text) => folded(text).includes(query)) ? [{ row, index }] : [];
    if (folded(row.label).includes(query)) return [{ row, index }];
    const choices = row.choices.filter((choice) => folded(choice.label).includes(query));
    return choices.length === 0 ? [] : [{ row, index, choices }];
  });
}

// The row of a board action in the menu of a target: an item with the board
// action's label, icon, detail for the target and category. Picking it is the
// menu's (enter(), above). A detail that throws is left out, as a fault of the
// board action's package, which core logs.
function actionRow(action: BoardAction, board: Board, target: Target): QuickRow {
  let detail: string | undefined;
  try {
    const given: unknown = action.detail?.(board, target);
    if (typeof given === "string" && given !== "") detail = given;
  } catch (error) {
    log.error(new Error(`The detail of the board action "${action.id}" failed: ${errorMessage(error)}`, { cause: error }));
  }
  return {
    kind: "item",
    label: action.label,
    ...(action.icon === undefined ? {} : { icon: action.icon }),
    ...(detail === undefined ? {} : { detail }),
    category: action.category,
    pick: () => [],
  };
}

// An item of a quick action that opens the level given at the same spot, as
// Edit link's Change source… opens the blocks that can feed the link.
export function levelItem(label: string, level: QuickLevel): QuickRow {
  return { kind: "item", label, pick: () => level };
}

// Whether a value is a batch of commands, each by its id.
export function isBatch(value: unknown): value is readonly CommandCall[] {
  return Array.isArray(value) && value.every((call) => isObject(call) && typeof call.id === "string");
}

// Whether a value is a level of rows of the kinds that core defines, each
// with its label and its functions, as a package in plain JS may give
// another.
function isLevel(value: unknown): value is QuickLevel {
  if (!isObject(value) || !Array.isArray(value.rows)) return false;
  if (!optional(value.title, "string") || !optional(value.filter, "boolean")) return false;
  return value.rows.every(isRow);
}

function isRow(row: unknown): boolean {
  if (!isObject(row) || typeof row.label !== "string" || row.label === "") return false;
  switch (row.kind) {
    case "item":
      return (
        typeof row.pick === "function" &&
        [row.heading, row.icon, row.detail, row.category].every((member) => optional(member, "string")) &&
        optional(row.checked, "boolean")
      );
    case "multi":
      return (
        typeof row.apply === "function" &&
        Array.isArray(row.choices) &&
        row.choices.every(
          (choice) =>
            isObject(choice) && typeof choice.id === "string" && typeof choice.label === "string" && typeof choice.picked === "boolean",
        )
      );
    case "text":
      return typeof row.submit === "function" && optional(row.value, "string") && optional(row.check, "function");
    default:
      return false;
  }
}

function optional(member: unknown, type: "string" | "boolean" | "function"): boolean {
  return member === undefined || typeof member === type;
}

function isThenable(value: unknown): value is PromiseLike<unknown> {
  return (typeof value === "object" || typeof value === "function") && value !== null && typeof (value as { then?: unknown }).then === "function";
}

// Text as the filter compares it: in lower case, without accents, so that
// "cafe" finds "Café".
function folded(text: string): string {
  return text.normalize("NFD").replace(/\p{Diacritic}/gu, "").toLocaleLowerCase();
}
