// The tests that core ships for a package's module that draws the menus of
// board actions, run against a module that draws without React, written to
// the contract of the module alone, so that the tests rely on neither React
// nor the markup of core's menu, and against modules that each break one part
// of the contract, so that the tests catch each break.
// Core's own menu runs them too (../modules/board-action-menu/module.test.ts).

import { describe, expect, test } from "vitest";
import type { MenuModule } from "../actions/index.ts";
import { menuModuleTests } from "./index.ts";

// What a module breaks: it passes on no click on an item, passes the index of
// an item as it shows it rather than as the level holds it, leaves what it drew
// on the page as the menu closes, passes on no Escape, no click outside the
// menu or no click on a choice, draws no open icon, draws an item that gives
// `checked` as any other item, or keeps the keyboard's focus in an element of
// its own as the menu closes.
type Break = "noPick" | "shownIndex" | "leaky" | "noEscape" | "noOutside" | "noTick" | "noOpenIcon" | "noChecked" | "keepsFocus";

// A module that draws each level in an element of its own at the end of the
// page's body, as plain elements with the roles and names of the contract,
// and that breaks what is given.
function plainMenu(broken?: Break): { api: number; default: MenuModule } {
  const render: MenuModule["render"] = ({ menu, el }) => {
    const doc = el.ownerDocument;
    const box = doc.createElement("div");
    doc.body.append(box);
    let drawn: number | undefined;
    const element = (tag: string, attrs: Readonly<Record<string, string>> = {}, text = ""): HTMLElement => {
      const made = doc.createElement(tag);
      for (const [name, value] of Object.entries(attrs)) made.setAttribute(name, value);
      made.textContent = text;
      return made;
    };
    const draw = (): void => {
      const level = menu.get();
      box.replaceChildren();
      if (level.advanced && broken !== "noOpenIcon") {
        const open = element("button", { "aria-label": "Show all options" });
        open.addEventListener("click", () => {
          menu.openAdvanced();
        });
        box.append(open);
      }
      if (level.loading) box.append(element("p", { role: "status" }, "Loading"));
      if (level.error !== undefined) box.append(element("p", {}, level.error));
      if (level.filter !== undefined) {
        const filter = element("input", { "aria-label": "Filter" }) as HTMLInputElement;
        filter.value = level.filter;
        filter.addEventListener("input", () => {
          menu.type(filter.value);
        });
        filter.addEventListener("keydown", (event) => {
          if (event.key === "Enter") menu.pick();
        });
        box.append(filter);
      }
      const list = element("div", { role: "menu" });
      // The rows in reverse, where the module passes on the index at which it
      // shows a row.
      const shown = level.rows.map((row, index) => ({ row, index }));
      if (broken === "shownIndex") shown.reverse();
      shown.forEach(({ row, index: held }, at) => {
        const index = broken === "shownIndex" ? at : held;
        if (row.kind === "item") {
          const checks = row.checked !== undefined && broken !== "noChecked";
          const item = element(
            "div",
            checks ? { role: "menuitemcheckbox", tabindex: "-1", "aria-checked": String(row.checked) } : { role: "menuitem", tabindex: "-1" },
            row.label,
          );
          if (broken !== "noPick") {
            item.addEventListener("click", () => {
              menu.pick(index);
            });
          }
          if (row.advanced === true && broken !== "noOpenIcon") {
            const icon = element("span", { title: "Show all options" });
            icon.addEventListener("click", (event) => {
              event.stopPropagation();
              menu.openAdvanced(index);
            });
            item.append(icon);
          }
          list.append(item);
        } else if (row.kind === "multi") {
          for (const choice of row.choices) {
            const ticks = element("div", { role: "menuitemcheckbox", tabindex: "-1", "aria-checked": String(choice.picked) }, choice.label);
            if (broken !== "noTick") {
              ticks.addEventListener("click", () => {
                menu.tick(index, choice.id);
              });
            }
            list.append(ticks);
          }
        } else {
          const label = element("label", {}, row.label);
          const field = doc.createElement("input");
          field.value = row.value;
          field.addEventListener("input", () => {
            menu.type(field.value, index);
          });
          field.addEventListener("keydown", (event) => {
            if (event.key === "Enter") menu.pick(index);
          });
          label.append(field);
          box.append(label);
          if (row.refusal !== undefined) box.append(element("p", {}, row.refusal));
        }
      });
      box.append(list);
      // The focus goes to the first field or row of a level as it shows.
      if (drawn !== level.id) {
        drawn = level.id;
        (box.querySelector<HTMLElement>('input, [role="menuitem"], [role="menuitemcheckbox"]') ?? box).focus();
      }
    };
    const unsubscribe = menu.subscribe(draw);
    draw();
    const keys = (event: KeyboardEvent): void => {
      if (event.key === "Escape" && broken !== "noEscape") menu.escape();
      if (event.key === "Tab") menu.close();
    };
    const outside = (event: Event): void => {
      if (broken !== "noOutside" && event.target instanceof Node && !box.contains(event.target)) menu.close();
    };
    doc.addEventListener("keydown", keys);
    doc.addEventListener("pointerdown", outside);
    return () => {
      unsubscribe();
      doc.removeEventListener("keydown", keys);
      doc.removeEventListener("pointerdown", outside);
      if (broken === "leaky") {
        box.style.display = "none";
      } else if (broken === "keepsFocus") {
        const keeper = element("button");
        box.replaceChildren(keeper);
        keeper.focus();
      } else {
        box.remove();
      }
    };
  };
  return { api: 1, default: { render } };
}

// The names of the tests that the module fails, each with the first line of
// why.
async function failures(exports: unknown): Promise<Record<string, string>> {
  const failed: Record<string, string> = {};
  for (const { name, run } of menuModuleTests(exports)) {
    try {
      await run();
    } catch (error) {
      failed[name] = (error instanceof Error ? error.message : String(error)).split("\n")[0] ?? "";
    }
  }
  return failed;
}

const ALL = menuModuleTests(plainMenu()).map(({ name }) => name);

// The test of the name that starts with the words given.
function named(start: string): string {
  const name = ALL.find((each) => each.startsWith(start));
  if (name === undefined) throw new Error(`No test starts with "${start}".`);
  return name;
}

describe("a module that draws without React, to the contract alone", () => {
  for (const { name, run } of menuModuleTests(plainMenu())) test(name, run);
});

describe("the tests of a module that draws the menus of board actions", () => {
  test("refuse what declares no major of core's JS API or has no render(), as core does", async () => {
    const [first] = menuModuleTests({ default: {} });
    await expect(first?.run()).rejects.toThrow("The module declares no major of core's JS API, where core's is 1, as `export const api = 1`.");
    const [again] = menuModuleTests({ api: 1, default: {} });
    await expect(again?.run()).rejects.toThrow("The module has no default export with a render() function.");
  });

  test.each<[Break, readonly string[], string]>([
    [
      "noPick",
      ["draws the rows", "runs the batch of the item", "says whether each item", "shows the next level", "opens the advanced action from an item's"],
      "The menu did not close after the user clicked the row Remove block, so the module did not pass that on.",
    ],
    [
      "shownIndex",
      ["draws the rows", "runs the batch of the item", "says whether each item", "shows the next level", "opens the advanced action from an item's"],
      'A click on the row Remove block runs the board action Remove block, but core ran [{"action":"add_block_action","batch":[{"package":"blockr.test","id":"add"}]}].',
    ],
    [
      "leaky",
      ALL.filter((name) => !name.startsWith("shows that a level loads") && !name.startsWith("shows the message") && !name.startsWith("draws no open icon")),
      "The module left the rows of the menu on the page as the menu closed, so the function that its render() returned does not take them off.",
    ],
    ["noEscape", ["shows the next level", "drops the ticks"], 'The menu shows no item named "Change source…", an element of the role menuitem, menuitemradio or option.'],
    ["noOutside", ["ticks the choices"], "The menu did not close after the user clicked outside the menu, so the module did not pass that on."],
    ["noTick", ["ticks the choices", "applies the ticks"], "The choice b does not show that it is ticked, once the user clicked it."],
    ["noOpenIcon", ["opens the advanced action from the level's", "opens the advanced action from an item's"], 'The menu shows no button named "Show all options".'],
    [
      "noChecked",
      ["says whether each item"],
      'The menu shows no item named "Left to right" that says whether it is checked, an element of the role menuitemcheckbox, menuitemradio or option.',
    ],
    [
      "keepsFocus",
      ALL.filter((name) => !name.startsWith("shows that a level loads") && !name.startsWith("shows the message") && !name.startsWith("draws no open icon")),
      "The keyboard's focus is not back at the element that the user triggered the menu from, as the menu closed, since an element of the module's still holds it.",
    ],
  ])("catch a module that breaks %s", async (broken, failing, first) => {
    const failed = await failures(plainMenu(broken));
    expect(Object.keys(failed).sort()).toEqual(failing.map((start) => (ALL.includes(start) ? start : named(start))).sort());
    expect(Object.values(failed)[0]).toBe(first);
  }, 60000);
});
