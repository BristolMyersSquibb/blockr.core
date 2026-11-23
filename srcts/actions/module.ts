// The module that draws every menu of board actions, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#what-a-frontend-gives).
// The blockr option board_action_menu names it by its id, as a module that a
// package offers others.
// Core's own menu is such a module, blockr.core-board_action_menu, which draws
// with the components of core's React SDK, and the blockr option names it
// unless the app names another (../modules/board-action-menu).
// So an app that draws with a UI kit other than blockr.ui's draws the menus of
// board actions with that kit too.
// Core asks R for the blockr option as soon as R has answered hello, and loads
// the module as each board opens, once per page (../core/start.ts).
// Core opens no board where the blockr option names no module by its id, or
// names one that no package offers, that fails to load, that declares another
// major of core's JS API, or whose default export has no render().
// It shows in place of the board an error that names the blockr option and the
// module, since the blockr option is the app's configuration rather than a
// board's.
//
// Core draws each menu that it opens with the module in four steps:
// 1. Core calls the module's render() with the open menu (./quick.ts), the
//    element that the user triggered from and the box at which the menu
//    opens.
// 2. The module draws the level that the menu's get() returns, draws it again
//    whenever the menu's subscribe() calls it, and calls a member of the menu
//    for each thing that the user does.
// 3. Core calls the functions of the board action's rows, and runs the batch
//    that they return, with the caller's extend, as one step of the user.
// 4. As the menu closes, whatever closed it, core calls the function that
//    render() returned, and gives the keyboard's focus back to the element
//    that the user triggered from, where the focus was in the menu or nowhere.
// So the module never sees a board action's functions, never runs a command
// and writes none of the rules of a quick action, only its look.
// Where render() throws, core closes the menu and tells the user why
// (./actions.ts).

import type { Loader } from "../mount/index.ts";
import { mountableModule } from "../mount/index.ts";
import type { Json } from "../types/json.ts";
import { log } from "../log/index.ts";
import { errorMessage, packageFromId } from "../util/index.ts";
import type { MenuPlace, ShowMenu } from "./actions.ts";
import type { DrivenMenu, OpenMenu } from "./quick.ts";

// What a module that draws the menus of board actions exports as its default,
// beside the major of core's JS API that it was built against, as
// `export const api = 1`.
// Its render() draws the open menu at the anchor, and returns what core calls
// as the menu closes.
export interface MenuModule {
  render(args: { readonly menu: OpenMenu; readonly el: HTMLElement; readonly anchor: DOMRect }): () => void;
}

// The blockr option that names the module, and the id of core's own menu,
// which it names unless the app names another.
export const MENU_OPTION = "board_action_menu";
export const CORE_MENU = "blockr.core-board_action_menu";

// Loads the module that the blockr option names, from its value as R sent it,
// and resolves with what draws each menu with that module.
// It rejects with why core opens no board, as for a module that no package
// offers.
export async function menuModule(loader: Pick<Loader, "module">, value: Json | undefined): Promise<ShowMenu> {
  if (value !== undefined && (typeof value !== "string" || value === "")) {
    throw new Error(
      `Core opens no board, since the blockr option ${MENU_OPTION} names no module by its id, as "${CORE_MENU}", but holds ${JSON.stringify(value)}.`,
    );
  }
  const id = value ?? CORE_MENU;
  let module: MenuModule;
  try {
    module = mountableModule<Parameters<MenuModule["render"]>[0]>(await loader.module(id), id, packageFromId(id)) as unknown as MenuModule;
  } catch (error) {
    throw new Error(
      `Core opens no board, since it cannot draw the menus of board actions with the module "${id}" that the blockr option ${MENU_OPTION} names. ${errorMessage(error)}`,
      { cause: error },
    );
  }
  return drawWith(module, id);
}

// What draws each menu with the module given, whose id, where given, names it
// in what core says of its faults (steps 1 and 4, above).
// It throws where the module's render() throws, which is a fault of the
// module's package.
export function drawWith(module: MenuModule, id?: string): ShowMenu {
  const named = id === undefined ? "The module" : `The module "${id}"`;
  return (menu: DrivenMenu, at: MenuPlace) => {
    const doc = at.el.ownerDocument;
    let cleanup: unknown;
    try {
      cleanup = module.render({ menu: forModule(menu), el: at.el, anchor: at.anchor });
    } catch (error) {
      throw new Error(`${named} failed to draw a menu of board actions: ${errorMessage(error)}`, { cause: error });
    }
    return () => {
      try {
        if (typeof cleanup === "function") (cleanup as () => void)();
      } catch (error) {
        log.error(new Error(`${named} failed as a menu of board actions closed: ${errorMessage(error)}`, { cause: error }));
      }
      // The cleanup took the menu's elements off the page, so a focus that was
      // in the menu is now nowhere, as after the user picked a row, pressed
      // Escape or clicked outside the menu.
      // Core then gives the focus back to the element that the user triggered
      // from, and leaves it where code of the page moved it.
      const active = doc.activeElement;
      const lost = active === null || active === doc.body || !active.isConnected;
      if (lost && at.el.isConnected) at.el.focus({ preventScroll: true });
    };
  };
}

// The open menu as the module gets it: the members of OpenMenu alone, each of
// which waits for nothing, as core tells the user why it refused a batch.
// Each checks what it is given, as a module in plain JS may give a member
// something else, which the types refuse.
function forModule(menu: DrivenMenu): OpenMenu {
  const settled = (ran: Promise<void>): void => {
    ran.catch(() => undefined);
  };
  return Object.freeze({
    get: () => menu.get(),
    subscribe: (fn: () => void) => {
      if (typeof fn !== "function") throw new TypeError("The subscribe() of an open menu takes a function, which it calls as the menu changes.");
      return menu.subscribe(fn);
    },
    pick: (row?: number) => {
      settled(menu.pick(row === undefined ? undefined : rowGiven(row, "pick()")));
    },
    openAdvanced: (row?: number) => {
      settled(menu.openAdvanced(row === undefined ? undefined : rowGiven(row, "openAdvanced()")));
    },
    tick: (row: number, choice: string) => {
      const index = rowGiven(row, "tick()");
      if (typeof choice !== "string") throw new TypeError(`The tick() of an open menu takes the id of a choice, not ${JSON.stringify(choice)}.`);
      menu.tick(index, choice);
    },
    type: (value: string, row?: number) => {
      if (typeof value !== "string") throw new TypeError(`The type() of an open menu takes the text that the user typed, not ${JSON.stringify(value)}.`);
      menu.type(value, row === undefined ? undefined : rowGiven(row, "type()"));
    },
    escape: () => {
      menu.escape();
    },
    close: () => {
      settled(menu.close());
    },
  });
}

// The index of a row that a member of the open menu is given.
function rowGiven(row: unknown, member: string): number {
  if (typeof row !== "number" || !Number.isInteger(row) || row < 0) {
    throw new TypeError(`The ${member} of an open menu takes the index of a row in the rows of the level, not ${JSON.stringify(row)}.`);
  }
  return row;
}
