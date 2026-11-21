// The menus of board actions under the CoreApi for tests, which core runs by
// the rules of a quick action, as on the page, and does not draw, so that a
// test of a board action needs no browser
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#testing).
// A test opens the menu of a target, or takes the menu that trigger() or a
// frontend opened, and drives it as a user would, picking its rows by their
// labels.

import type { OpenMenu, ShowMenu, ShownLevel } from "../actions/index.ts";
import { quoted } from "../util/index.ts";

// A menu of board actions, which a test drives as a user would. Each member
// does nothing once the menu has closed.
export interface StubMenu {
  // The level that the menu shows now: its title, the filter's text where it
  // has a filter, whether Escape goes back a level, whether it loads, the
  // message in place of its rows where a promise rejected, and the rows that
  // the filter leaves, without their functions. It is undefined once the
  // menu has closed.
  level(): Omit<ShownLevel, "id"> | undefined;
  closed(): boolean;
  // Picks the first row with the label given, as a click on it does: an item
  // runs its batch or opens the next level, and a text row submits its value,
  // as Enter in it does. Without a label, it is Enter in the filter, which
  // picks the first row left. It resolves once what the pick started has
  // settled: the next level shows, or the menu has closed and its batch has
  // run. It rejects with the refusal where the store or the command registry
  // refuses the batch, and throws where the level shows no row with the label.
  pick(label?: string): Promise<void>;
  // Clicks the open icon of the item with the label given, or without a
  // label, the level's, which closes the menu and opens the board action's
  // advanced action. It resolves once the advanced action ends, and throws
  // where the item or the level shows no open icon.
  openAdvanced(label?: string): Promise<void>;
  // Ticks the choice with the label given of the multi with the label given,
  // or unticks it, as a click on it does. It throws where the level shows no
  // such choice.
  tick(row: string, choice: string): void;
  // Types the value into the text row with the label given, in place of what
  // it holds, or into the filter without a label. It throws where the level
  // shows no such text row.
  type(value: string, row?: string): void;
  // Escape, which goes back a level, or closes the menu from its first level.
  escape(): void;
  // A click outside the menu, which applies the ticks of a multi. It resolves
  // once the batch that they apply has run, and rejects with the refusal.
  close(): Promise<void>;
}

// What the CoreApi for tests hands core to show each menu, which records the
// menu that is open in place of drawing it, and that menu, as a test drives it.
export function stubMenus(): { readonly show: ShowMenu; readonly open: () => StubMenu | undefined } {
  let open: OpenMenu | undefined;
  const driven = new WeakMap<OpenMenu, StubMenu>();
  return {
    show(menu) {
      open = menu;
      return () => {
        if (open === menu) open = undefined;
      };
    },
    open() {
      if (open === undefined) return undefined;
      const existing = driven.get(open);
      if (existing !== undefined) return existing;
      const made = drive(open);
      driven.set(open, made);
      return made;
    },
  };
}

function drive(menu: OpenMenu): StubMenu {
  // The index of the row of the kind given with the label given, in the rows
  // that the level shows, or an Error that says which rows it shows.
  function find(label: string, kinds: readonly string[]): number {
    const { rows } = menu.get();
    const index = rows.findIndex((row) => row.label === label && kinds.includes(row.kind));
    if (index < 0) {
      throw new Error(`The menu shows no ${kinds.join(" or ")} row "${label}". It shows ${rows.length === 0 ? "none" : quoted(rows.map((row) => row.label))}.`);
    }
    return index;
  }

  return {
    level() {
      return menu.closed() ? undefined : withoutNumber(menu.get());
    },
    closed: () => menu.closed(),
    pick(label) {
      if (menu.closed()) return Promise.resolve();
      return label === undefined ? menu.pick() : menu.pick(find(label, ["item", "text"]));
    },
    openAdvanced(label) {
      if (menu.closed()) return Promise.resolve();
      if (label === undefined) {
        if (!menu.get().advanced) throw new Error("The level shows no open icon.");
        return menu.openAdvanced();
      }
      const index = find(label, ["item"]);
      const row = menu.get().rows[index];
      if (row?.kind !== "item" || row.advanced !== true) throw new Error(`The item "${label}" shows no open icon.`);
      return menu.openAdvanced(index);
    },
    tick(row, choice) {
      if (menu.closed()) return;
      const index = find(row, ["multi"]);
      const shown = menu.get().rows[index];
      const found = shown?.kind === "multi" ? shown.choices.find((each) => each.label === choice) : undefined;
      if (found === undefined) throw new Error(`The multi "${row}" shows no choice "${choice}".`);
      menu.tick(index, found.id);
    },
    type(value, row) {
      if (menu.closed()) return;
      menu.type(value, row === undefined ? undefined : find(row, ["text"]));
    },
    escape() {
      menu.escape();
    },
    close: () => menu.close(),
  };
}

// The level without its number, which is for a view that draws it, not for a
// test.
function withoutNumber(level: ShownLevel): Omit<ShownLevel, "id"> {
  return Object.fromEntries(Object.entries(level).filter(([key]) => key !== "id")) as Omit<ShownLevel, "id">;
}
