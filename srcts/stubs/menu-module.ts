// The tests that core ships for a module that draws the menus of board
// actions, which a package runs against its own module, as core runs them
// against its own menu, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#what-a-frontend-gives).
// A package runs them in its test runner, on a DOM such as happy-dom's and with
// real timers, as in
// `for (const { name, run } of menuModuleTests(await import("./menu.tsx"))) test(name, run);`.
// Each test takes four steps:
// 1. It opens a menu through the module, as core opens one on the page, beside
//    an element that the user triggered it from, which has the keyboard's
//    focus. The menu holds board actions of the test's own, which record what
//    core runs for them.
// 2. It does what a user does, on the elements that it finds by their ARIA
//    roles and their names: it clicks an element, types into a field, presses
//    Enter, Escape or Tab where the keyboard's focus is, or clicks outside the
//    menu.
// 3. It checks what core ran, which says whether the module passed on what the
//    user did, and that the module draws what the menu shows then.
// 4. As the menu closes, it checks that the module took what it drew off the
//    page, and that the keyboard's focus is back at the element that the user
//    triggered the menu from.
// Its run rejects with what the module failed to do, and with each error that
// core logged meanwhile, as where the module's cleanup throws.
//
// The tests find each part of a menu by its role and its name, as core's menu
// names them (../modules/board-action-menu):
// - An item is an element of the role menuitem, menuitemradio or option, named
//   by the item's label, or by its label followed by its detail.
// - An item that gives `checked` is an element of the role menuitemcheckbox,
//   menuitemradio or option, named as any item, which says in aria-checked or
//   aria-selected whether it is checked.
// - A choice of a multi is an element of the role menuitemcheckbox, checkbox
//   or option, named by the choice's label, which says in aria-checked or
//   aria-selected whether the choice is ticked.
// - A text row is a field, an input, a text area or an element of the role
//   textbox, searchbox or combobox, named by the row's label.
// - The filter is such a field, named Filter.
// - The open icon of a level is a button named Show all options. The open icon
//   of an item is an element inside the item whose aria-label or title starts
//   with Show all options.
// - A level that loads holds an element of the role status or progressbar, or
//   an element marked aria-busy.
// - The message of a promise that rejected, and why the check of a text row
//   refuses its value, show as text on the page.

import { act } from "react";
import { drawWith, openMenu } from "../actions/index.ts";
import type { AdvancedModule, BoardAction, CommandCall, DrivenMenu, MenuModule, MenuStart, QuickLevel, QuickRow } from "../actions/index.ts";
import { log, replaceLogger } from "../log/index.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { LogLine } from "../types/log.ts";
import { jsonEqual } from "../util/index.ts";
import { renderable } from "./mount.ts";

// One test of a module that draws the menus of board actions: what it checks,
// as a test runner names it, and what runs it, which rejects with what the
// module failed to do.
export interface MenuModuleTest {
  readonly name: string;
  readonly run: () => Promise<void>;
}

// The tests of the module, given as what it exports, as a package's test
// imports it.
// Each rejects where the module does not declare core's JS API major, or has no
// default export with a render() function, as core refuses such a module.
export function menuModuleTests(exports: unknown): readonly MenuModuleTest[] {
  return TESTS.map(({ name, check }) => ({ name, run: () => running(exports, check) }));
}

// A menu that a test opened through the module, with what core ran for it.
interface Opened {
  readonly menu: DrivenMenu;
  // Each batch that core ran, with the id of the board action that gave it.
  readonly ran: { readonly action: string; readonly batch: readonly CommandCall[] }[];
  // Each advanced action that core opened, with the id of its board action and
  // what the open icon handed it.
  readonly opened: { readonly action: string; readonly picked?: Json; readonly filter?: string }[];
}

// What a test does to a menu, as a user does, and what it finds of it.
interface Tester {
  // Opens a menu through the module, which starts at the menu of a target or
  // at a board action's quick action.
  open(start: MenuStart): Promise<Opened>;
  // The item, the item that gives `checked`, the choice of a multi, the field
  // or the button that the menu shows with the name given, once it shows.
  item(name: string): Promise<HTMLElement>;
  checkable(name: string): Promise<HTMLElement>;
  choice(name: string): Promise<HTMLElement>;
  field(name: string): Promise<HTMLElement>;
  button(name: string): Promise<HTMLElement>;
  // The open icon inside the item, or undefined where it shows none.
  icon(item: HTMLElement): HTMLElement | undefined;
  // Waits until the menu shows no item, choice or field with the name given.
  gone(name: string): Promise<void>;
  // Waits until the page shows the text given.
  shown(text: string): Promise<void>;
  // Waits until the menu shows that it loads, or until it no longer does.
  loading(shows: boolean): Promise<void>;
  // What the user does.
  click(el: HTMLElement): Promise<void>;
  clickOutside(): Promise<void>;
  type(field: HTMLElement, text: string): Promise<void>;
  // Presses the key on the element given, or where the keyboard's focus is.
  press(key: string, on?: HTMLElement): Promise<void>;
  // Runs what settles a promise that a board action's function returned.
  act(fn: () => void): Promise<void>;
  // Waits until the menu has closed, after what the user did, which `after`
  // names, and checks that the module took what it drew off the page and that
  // the keyboard's focus is back at the element that the user triggered from.
  closed(opened: Opened, after: string): Promise<void>;
  // Takes the errors that core logged so far, which the test expects, so that
  // they fail no run.
  takeErrors(): readonly LogLine[];
}

interface Test {
  readonly name: string;
  readonly check: (t: Tester) => Promise<void>;
}

// Runs one test against the module: it checks what the module exports, has the
// test open its menus beside an element of its own on the page, and then closes
// every menu, takes the element off the page, and fails where core logged an
// error that the test did not take.
async function running(exports: unknown, check: Test["check"]): Promise<void> {
  if (typeof document === "undefined") {
    throw new Error("The tests of a module that draws the menus of board actions run on a DOM, such as happy-dom's.");
  }
  const module = renderable(exports) as unknown as MenuModule;
  const lines: LogLine[] = [];
  const restore = replaceLogger((line) => {
    lines.push(line);
  });
  const trigger = document.createElement("button");
  trigger.textContent = "Open the menu";
  document.body.append(trigger);
  const menus: DrivenMenu[] = [];
  try {
    await check(tester(module, trigger, menus, lines));
  } finally {
    await acting(() => {
      for (const menu of menus) menu.drop();
    });
    restore();
    trigger.remove();
  }
  const errors = lines.filter((line) => line.level === "error");
  if (errors.length > 0) {
    throw new Error(`Core logged errors as the test ran:\n${errors.map((line) => line.message).join("\n")}`);
  }
}

function tester(module: MenuModule, trigger: HTMLElement, menus: DrivenMenu[], lines: LogLine[]): Tester {
  const draw = drawWith(module);
  return {
    async open(start) {
      const ran: Opened["ran"] = [];
      const opened: Opened["opened"] = [];
      trigger.focus();
      let hide: (() => void) | undefined;
      let menu: DrivenMenu | undefined;
      await acting(() => {
        const made = openMenu(start, {
          board: () => ({}) as Board,
          target: { block: "a" },
          core: () => ({}) as CoreApi,
          applies: () => true,
          run(batch, action) {
            ran.push({ action: action.id, batch });
            return Promise.resolve();
          },
          async trigger(action) {
            const batch = await action.run?.({} as Board, { block: "a" }, {} as CoreApi);
            if (batch !== undefined) ran.push({ action: action.id, batch });
          },
          advanced(action, how) {
            opened.push({ action: action.id, ...how });
            return Promise.resolve();
          },
          failed(_action, failure) {
            log.error(failure);
          },
          closed() {
            hide?.();
            hide = undefined;
          },
        });
        menus.push(made);
        menu = made;
        if (!made.closed()) hide = draw(made, { el: trigger, anchor: new DOMRect(16, 24, 0, 0) });
      });
      if (menu === undefined) throw new Error("No menu opened.");
      return { menu, ran, opened };
    },
    item: (name) => until(() => byRole(ITEM, name), `The menu shows no item named "${name}", an element of the role ${roles(ITEM)}.`),
    checkable: (name) =>
      until(
        () => byRole(CHECKABLE, name),
        `The menu shows no item named "${name}" that says whether it is checked, an element of the role ${roles(CHECKABLE)}.`,
      ),
    choice: (name) => until(() => byRole(CHOICE, name), `The menu shows no choice of a multi named "${name}", an element of the role ${roles(CHOICE)}.`),
    field: (name) => until(() => fieldNamed(name), `The menu shows no field named "${name}".`),
    button: (name) => until(() => byRole(["button"], name), `The menu shows no button named "${name}".`),
    icon: (item) =>
      [...item.querySelectorAll<HTMLElement>("*")].find((el) =>
        [el.getAttribute("aria-label"), el.getAttribute("title")].some((said) => said?.trim().startsWith(OPEN) === true),
      ),
    async gone(name) {
      await until(
        () => (byRole([...ITEM, ...CHOICE], name) ?? fieldNamed(name)) === undefined || undefined,
        `The menu still shows "${name}", which the level that it shows now does not hold.`,
      );
    },
    async shown(text) {
      await until(() => document.body.textContent.includes(text) || undefined, `The menu does not show "${text}".`);
    },
    async loading(shows) {
      await until(
        () => (document.querySelector(LOADS) !== null) === shows || undefined,
        shows
          ? "The menu does not show that its level loads, through an element of the role status or progressbar, or one marked aria-busy."
          : "The menu still shows that its level loads, once the level has come.",
      );
    },
    async click(el) {
      await acting(() => {
        el.click();
      });
      await pause();
    },
    async clickOutside() {
      const outside = document.createElement("div");
      document.body.append(outside);
      const init = { bubbles: true, cancelable: true, composed: true, button: 0, pointerId: 1, pointerType: "mouse", isPrimary: true };
      const Pointer = typeof PointerEvent === "undefined" ? MouseEvent : PointerEvent;
      await acting(() => {
        outside.dispatchEvent(new Pointer("pointerdown", { ...init, buttons: 1 }));
        outside.dispatchEvent(new MouseEvent("mousedown", { ...init, buttons: 1 }));
        outside.dispatchEvent(new Pointer("pointerup", init));
        outside.dispatchEvent(new MouseEvent("mouseup", init));
        outside.dispatchEvent(new MouseEvent("click", init));
      });
      outside.remove();
      await pause();
    },
    async type(field, text) {
      await acting(() => {
        field.focus();
        if (field instanceof HTMLInputElement || field instanceof HTMLTextAreaElement) {
          // Through the element's own setter, which React does not track as a
          // value that it set itself.
          Reflect.set(Object.getPrototypeOf(field) as object, "value", text, field);
        } else {
          field.textContent = text;
        }
        field.dispatchEvent(new Event("input", { bubbles: true }));
      });
      await pause();
    },
    async press(key, on) {
      if (on === undefined) await focusInMenu(trigger);
      const target = on ?? document.activeElement ?? document.body;
      await acting(() => {
        target.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true, composed: true }));
        target.dispatchEvent(new KeyboardEvent("keyup", { key, bubbles: true, cancelable: true, composed: true }));
      });
      await pause();
    },
    act: (fn) => acting(fn),
    async closed(opened, after) {
      await until(() => opened.menu.closed() || undefined, `The menu did not close ${after}, so the module did not pass that on.`);
      await until(
        () => document.querySelector(ANY_ROW) === null || undefined,
        "The module left the rows of the menu on the page as the menu closed, so the function that its render() returned does not take them off.",
      );
      if (document.activeElement !== trigger) {
        throw new Error(
          "The keyboard's focus is not back at the element that the user triggered the menu from, as the menu closed, since an element of the module's still holds it.",
        );
      }
    },
    takeErrors() {
      const errors = lines.filter((line) => line.level === "error");
      lines.splice(0, lines.length, ...lines.filter((line) => line.level !== "error"));
      return errors;
    },
  };
}

// The roles of an item, of an item that gives `checked` and of a choice of a
// multi, and what holds any row of a menu, or shows that a level loads.
const ITEM = ["menuitem", "menuitemradio", "option"];
const CHECKABLE = ["menuitemcheckbox", "menuitemradio", "option"];
const CHOICE = ["menuitemcheckbox", "checkbox", "option"];
const ANY_ROW = '[role="menu"], [role="menuitem"], [role="menuitemradio"], [role="menuitemcheckbox"], [role="listbox"], [role="option"]';
const LOADS = '[role="status"], [role="progressbar"], [aria-busy="true"]';
// The fields of a menu: a text row or the filter.
const FIELDS =
  'input:not([type]), input[type="text"], input[type="search"], textarea, [role="textbox"], [role="searchbox"], [role="combobox"]';
// The name of an open icon.
const OPEN = "Show all options";

function roles(given: readonly string[]): string {
  return given.length === 1 ? (given[0] ?? "") : `${given.slice(0, -1).join(", ")} or ${given.at(-1) ?? ""}`;
}

// The element of one of the roles given whose name is the one given, or which
// starts with it, followed by what follows a label, such as an item's detail.
function byRole(given: readonly string[], name: string): HTMLElement | undefined {
  const selector = [...given.map((role) => `[role="${role}"]`), ...(given.includes("checkbox") ? ['input[type="checkbox"]'] : []), ...(given.includes("button") ? ["button"] : [])];
  return [...document.querySelectorAll<HTMLElement>(selector.join(", "))].find((el) => {
    const said = nameOf(el);
    return said === name || said.startsWith(`${name},`) || said.startsWith(`${name} `);
  });
}

function fieldNamed(name: string): HTMLElement | undefined {
  return [...document.querySelectorAll<HTMLElement>(FIELDS)].find((el) => nameOf(el) === name);
}

// The name of an element, as assistive technology reads it: what the elements
// that label it hold, its aria-label, the labels of a field, or else its text.
function nameOf(el: Element): string {
  const doc = el.ownerDocument;
  const by = el.getAttribute("aria-labelledby")?.trim() ?? "";
  if (by !== "") return squashed(by.split(/\s+/).map((id) => doc.getElementById(id)?.textContent ?? "").join(" "));
  const label = el.getAttribute("aria-label")?.trim() ?? "";
  if (label !== "") return squashed(label);
  if (el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement) {
    const labels = [...(el.labels ?? [])].map((each) => each.textContent).join(" ");
    return squashed(labels.trim() !== "" ? labels : (el.getAttribute("title") ?? ""));
  }
  return squashed(el.textContent);
}

function squashed(text: string): string {
  return text.replace(/\s+/g, " ").trim();
}

// Whether a choice says that it is ticked.
function ticked(el: HTMLElement): boolean {
  if (el instanceof HTMLInputElement) return el.checked;
  return el.getAttribute("aria-checked") === "true" || el.getAttribute("aria-selected") === "true";
}

// What a field holds.
function valueOf(el: HTMLElement): string {
  return el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement ? el.value : el.textContent;
}

// Whether React runs what the test does through act(), as a test runner sets
// it up for React, which then warns of an update outside act().
function reactActs(): boolean {
  return (globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT === true;
}

// Does what the test does, and then what a module that draws with React
// scheduled in turn.
async function acting(fn: () => void | Promise<void>): Promise<void> {
  if (reactActs()) {
    await act(async () => {
      await fn();
    });
  } else {
    await fn();
  }
}

// Lets the module draw what it scheduled, as React Aria draws the rows of a
// menu a moment after the menu.
async function pause(ms = 10): Promise<void> {
  await acting(() => new Promise<void>((resolve) => setTimeout(resolve, ms)));
}

// The value that `found` gives once it gives one, which the test waits a few
// seconds for, or an Error that says what the module failed to do.
async function until<T>(found: () => T | undefined, failure: string): Promise<T> {
  const ends = Date.now() + 2000;
  for (;;) {
    const value = found();
    if (value !== undefined) return value;
    if (Date.now() > ends) throw new Error(failure);
    await pause();
  }
}

// Waits a moment for the keyboard's focus to move into the menu, as a module
// moves it there once it has drawn the level, so that a key that the user
// presses goes where the focus is then.
async function focusInMenu(trigger: HTMLElement): Promise<void> {
  for (let i = 0; i < 30; i++) {
    const active = document.activeElement;
    if (active !== null && active !== trigger && active !== document.body) return;
    await pause();
  }
}

function must(holds: boolean, failure: string): void {
  if (!holds) throw new Error(failure);
}

// Checks that core ran the batches given, each with its board action's id.
function ranAs(opened: Opened, expected: Opened["ran"], what: string): void {
  must(jsonEqual(opened.ran, expected), `${what}, but core ran ${JSON.stringify(opened.ran)}.`);
}

// A board action of the test's own on a block, by its label, with what the test
// gives it.
function action(label: string, more: Partial<BoardAction>): BoardAction {
  return {
    package: "blockr.test",
    id: `${label.toLowerCase().replace(/[^a-z]+/g, "_")}_action`,
    label,
    on: ["block"],
    category: "edit",
    ...more,
  };
}

// The batch of a command of the test's own, by its name, with the args given.
function call(name: string, args?: Json): CommandCall[] {
  return [{ package: "blockr.test", id: name, ...(args === undefined ? {} : { args }) }];
}

// An item that runs a batch that names its label as it is picked.
function item(label: string, more: Partial<Extract<QuickRow, { kind: "item" }>> = {}): QuickRow {
  return { kind: "item", label, pick: () => call("pick", { label }), ...more };
}

// The advanced action of a board action, which the tests never draw, since
// core's part that opens it records what it gets.
const ADVANCED: AdvancedModule = { render: () => () => undefined };

// A board action whose quick action is a multi with the choices a, b and c, of
// which a is ticked as the level opens.
const COLUMNS = action("Columns", {
  quick: () => ({
    rows: [
      {
        kind: "multi",
        label: "Columns",
        choices: [
          { id: "a", label: "a", picked: true },
          { id: "b", label: "b", picked: false },
          { id: "c", label: "c", picked: false },
        ],
        apply: (picked) => call("columns", { picked: [...picked] }),
      },
    ],
  }),
});

// A board action with an advanced action, whose quick action has a filter, has
// picked an input, and holds an item that hands the advanced action a class and
// one that hands it nothing.
const APPEND = action("Append block", {
  advanced: ADVANCED,
  quick: () => ({ filter: true, picked: { input: "data" }, rows: [item("Head", { advanced: { class: "head" } }), item("Tail")] }),
});

const TESTS: readonly Test[] = [
  {
    name: "draws the rows of the menu of a target, and runs the board action of the row that the user clicks",
    async check(t) {
      const remove = action("Remove block", { category: "remove", run: () => call("remove") });
      const opened = await t.open({
        actions: [remove, action("Add block", { category: "add", run: () => call("add") }), action("Rename", { quick: () => ({ rows: [] }) })],
      });
      await t.item("Add block");
      await t.item("Rename");
      await t.click(await t.item("Remove block"));
      await t.closed(opened, "after the user clicked the row Remove block");
      ranAs(opened, [{ action: remove.id, batch: call("remove") }], "A click on the row Remove block runs the board action Remove block");
    },
  },
  {
    name: "runs the batch of the item that the user clicks, by the item's index in the level's rows, whatever the order in which the module shows them",
    async check(t) {
      const append = action("Append block", {
        quick: () => ({
          title: "Append block",
          rows: [item("Head", { heading: "Transform" }), item("Plot", { heading: "Visualize" }), item("Filter", { heading: "Transform" })],
        }),
      });
      const opened = await t.open({ action: append });
      await t.item("Head");
      await t.item("Plot");
      await t.click(await t.item("Filter"));
      await t.closed(opened, "after the user clicked the item Filter");
      ranAs(opened, [{ action: append.id, batch: call("pick", { label: "Filter" }) }], "A click on the item Filter runs the batch of Filter");
    },
  },
  {
    name: "says whether each item that gives checked is checked, and runs the pick of such an item that the user clicks",
    async check(t) {
      const layout = action("Layout", {
        quick: () => ({
          rows: [item("Left to right", { checked: true }), item("Top to bottom", { checked: false }), item("Fit to view")],
        }),
      });
      const opened = await t.open({ action: layout });
      await t.item("Fit to view");
      must(ticked(await t.checkable("Left to right")), "The item Left to right does not say that it is checked, which its checked is.");
      must(!ticked(await t.checkable("Top to bottom")), "The item Top to bottom says that it is checked, which its checked is not.");
      // The item that is checked, so that a click on it runs its pick, rather
      // than only taking its check away.
      await t.click(await t.checkable("Left to right"));
      await t.closed(opened, "after the user clicked the item Left to right");
      ranAs(opened, [{ action: layout.id, batch: call("pick", { label: "Left to right" }) }], "A click on the item Left to right runs its pick");
    },
  },
  {
    name: "shows the next level that an item opens in place of the menu's, from which Escape goes back, and closes the menu on Escape from the first level",
    async check(t) {
      const edit = action("Edit link", {
        quick: () => ({
          rows: [{ kind: "item", label: "Change source…", pick: () => ({ title: "Source", rows: [item("Dataset")] }) }, item("Remove link")],
        }),
      });
      const opened = await t.open({ action: edit });
      await t.click(await t.item("Change source…"));
      await t.item("Dataset");
      await t.gone("Change source…");
      await t.press("Escape");
      await t.item("Change source…");
      await t.gone("Dataset");
      await t.press("Escape");
      await t.closed(opened, "on Escape from the first level");
      ranAs(opened, [], "Escape runs nothing");
    },
  },
  {
    name: "submits a text row's value on Enter, and shows in place why its check refuses a value",
    async check(t) {
      const rename = action("Rename", {
        quick: () => ({
          rows: [
            {
              kind: "text",
              label: "Name",
              value: "Head",
              check: (value) => (value.trim() === "" ? "A block needs a name." : undefined),
              submit: (value) => call("rename", { name: value }),
            },
          ],
        }),
      });
      const opened = await t.open({ action: rename });
      const field = await t.field("Name");
      must(valueOf(field) === "Head", `The field Name holds "${valueOf(field)}", where the text row's value is "Head".`);
      await t.type(field, "");
      await t.press("Enter", await t.field("Name"));
      await t.shown("A block needs a name.");
      must(!opened.menu.closed(), "The menu closed as the user submitted a value that the text row's check refuses.");
      ranAs(opened, [], "A value that the check refuses runs nothing");
      await t.type(await t.field("Name"), "Tail");
      await t.press("Enter", await t.field("Name"));
      await t.closed(opened, "after the user submitted a value on Enter");
      ranAs(opened, [{ action: rename.id, batch: call("rename", { name: "Tail" }) }], "Enter in the field Name submits its value, Tail");
    },
  },
  {
    name: "narrows the rows as the user types into the filter, and picks the first row left on Enter",
    async check(t) {
      const add = action("Add block", { quick: () => ({ filter: true, rows: [item("Head"), item("Tail"), item("Sort")] }) });
      const opened = await t.open({ action: add });
      await t.type(await t.field("Filter"), "ta");
      await t.item("Tail");
      await t.gone("Head");
      await t.gone("Sort");
      await t.press("Enter", await t.field("Filter"));
      await t.closed(opened, "after the user pressed Enter in the filter");
      ranAs(opened, [{ action: add.id, batch: call("pick", { label: "Tail" }) }], "Enter in the filter picks Tail, the first row left");
    },
  },
  {
    name: "ticks the choices of a multi that the user clicks, and applies them as the user clicks outside the menu",
    async check(t) {
      const opened = await t.open({ action: COLUMNS });
      must(ticked(await t.choice("a")), "The choice a does not show that it is ticked, as the multi picked it.");
      must(!ticked(await t.choice("b")), "The choice b shows that it is ticked, which the multi did not pick.");
      await t.click(await t.choice("b"));
      await t.act(() => undefined);
      must(ticked(await t.choice("b")), "The choice b does not show that it is ticked, once the user clicked it.");
      ranAs(opened, [], "Ticking a choice runs nothing until the menu closes");
      await t.clickOutside();
      await t.closed(opened, "after the user clicked outside the menu");
      ranAs(opened, [{ action: COLUMNS.id, batch: call("columns", { picked: ["a", "b"] }) }], "A click outside the menu applies the ticks, a and b");
    },
  },
  {
    name: "applies the ticks of a multi as Tab closes the menu",
    async check(t) {
      const opened = await t.open({ action: COLUMNS });
      await t.click(await t.choice("c"));
      await t.press("Tab");
      await t.closed(opened, "on Tab");
      ranAs(opened, [{ action: COLUMNS.id, batch: call("columns", { picked: ["a", "c"] }) }], "Tab applies the ticks, a and c");
    },
  },
  {
    name: "drops the ticks of a multi on Escape",
    async check(t) {
      const opened = await t.open({ action: COLUMNS });
      await t.click(await t.choice("b"));
      await t.press("Escape");
      await t.closed(opened, "on Escape");
      ranAs(opened, [], "Escape drops the ticks");
    },
  },
  {
    name: "shows that a level loads while its promise is pending, and then the level",
    async check(t) {
      let resolve: (level: QuickLevel) => void = () => undefined;
      const append = action("Append block", {
        quick: () =>
          new Promise<QuickLevel>((resolved) => {
            resolve = resolved;
          }),
      });
      await t.open({ action: append });
      await t.loading(true);
      await t.act(() => {
        resolve({ rows: [item("Head")] });
      });
      await t.item("Head");
      await t.loading(false);
    },
  },
  {
    name: "shows the message of a promise that rejects in place of the rows",
    async check(t) {
      const append = action("Append block", { quick: () => Promise.reject(new Error("R gave no columns.")) });
      await t.open({ action: append });
      await t.shown("R gave no columns.");
      // Core logs the rejection as a fault of the board action's package.
      t.takeErrors();
    },
  },
  {
    name: "opens the advanced action from the level's open icon, with what the level picked and the filter's text",
    async check(t) {
      const opened = await t.open({ action: APPEND });
      await t.type(await t.field("Filter"), "he");
      await t.gone("Tail");
      await t.click(await t.button(OPEN));
      await t.closed(opened, `after the user clicked the button ${OPEN}`);
      must(
        jsonEqual(opened.opened, [{ action: APPEND.id, picked: { input: "data" }, filter: "he" }]),
        `A click on the level's open icon opens the advanced action with what the level picked and the filter's text, but core opened ${JSON.stringify(opened.opened)}.`,
      );
      ranAs(opened, [], "The open icon picks no item");
    },
  },
  {
    name: "opens the advanced action from an item's open icon, with what the item hands it, and picks the item where the user clicks elsewhere on it",
    async check(t) {
      const opened = await t.open({ action: APPEND });
      const icon = t.icon(await t.item("Head"));
      if (icon === undefined) throw new Error(`The item Head shows no open icon, an element inside it whose aria-label or title starts with ${OPEN}.`);
      must(t.icon(await t.item("Tail")) === undefined, "The item Tail shows an open icon, which it does not give.");
      await t.click(icon);
      await t.closed(opened, "after the user clicked the open icon of the item Head");
      must(
        jsonEqual(opened.opened, [{ action: APPEND.id, picked: { class: "head" }, filter: "" }]),
        `A click on the open icon of the item Head opens the advanced action with what the item hands it, but core opened ${JSON.stringify(opened.opened)}.`,
      );
      ranAs(opened, [], "The open icon picks no item");

      const again = await t.open({ action: APPEND });
      await t.click(await t.item("Head"));
      await t.closed(again, "after the user clicked the item Head");
      ranAs(again, [{ action: APPEND.id, batch: call("pick", { label: "Head" }) }], "A click on the item Head, beside its open icon, picks it");
      must(again.opened.length === 0, "A click on the item Head, beside its open icon, opened the advanced action.");
    },
  },
  {
    name: "draws no open icon where the board action has no advanced action",
    async check(t) {
      const opened = await t.open({ action: action("Append block", { quick: () => ({ rows: [item("Head", { advanced: { class: "head" } })] }) }) });
      const head = await t.item("Head");
      must(t.icon(head) === undefined, "The item Head shows an open icon, where its board action has no advanced action.");
      await t.act(() => undefined);
      must(byRole(["button"], OPEN) === undefined, `The level shows the button ${OPEN}, where its board action has no advanced action.`);
      must(!opened.menu.closed(), "The menu closed by itself.");
    },
  },
];
