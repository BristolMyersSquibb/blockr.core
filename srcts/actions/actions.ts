// Board actions, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#a-board-action):
// what a user triggers from a menu, a button or a gesture, on the board, on one
// of its board objects, on an input of a block or on a selection of board
// objects. A board action changes the board only through commands, core's and
// any package's, and runs each batch as one undo step of the user. It is no
// command: a command reads the board, returns deltas and asks nobody
// anything, while a board action is the part with a UI, which runs commands.
//
// Core registers the board actions for each board that opens: its own, in the
// order of their table (./core.ts), then those that each plugin class lists in
// `boardActions`, as it registers the class with its commands
// (../core/start.ts). It registers the roles beside them, its own and those
// that each class lists in `roles` (./roles.ts). It refuses the whole class
// where one of its board actions or its roles breaks a rule (register(),
// below), and then opens no board (../core/start.ts).
// Once every module is registered, core chooses the board action that plays
// each role: the one that the blockr option `board_actions` picks for the
// role, or else the role's default (choosePlayers(), below).
//
// CoreApi's `boardActions` gives the board actions that apply to a target,
// opens the menu of a target, and triggers a board action by its id or by the
// id of the role that it plays (boardActionsApi(), below). Core hands a board
// action only targets of the kinds that its `on` names, so that a kind added
// later reaches no board action that does not name it.
// Of the board actions that name a role, a menu offers only the one that plays
// the role, in the place of the role's default, so that the app's pick of
// another changes no menu's order. A board action that names a role that it
// does not play is in no menu, and trigger() and mountAdvanced() run nothing
// for its id, so that every caller meets one board action for the role.
// What that board action does is the role's contract, which core trusts and
// does not check (./roles.ts).
// A call of menu() opens the menu of a target, with a row for each board
// action that on() gives the target, which core runs by the rules of a quick
// action (./quick.ts).
// The module that the blockr option board_action_menu names draws the menu at
// the anchor (./module.ts).
// Where that module fails to draw it, core closes the menu, logs why, and
// tells the user through a notification of the code `module_failed`.
// Picking a row closes the menu and runs the board action's run as trigger()
// does, or opens its quick action as the menu's next level.
// A call of trigger() runs a board action that asks the user nothing in these
// steps:
// 1. Core looks the board action up by its id, or by the id of the role that
//    it plays.
// 2. Core runs nothing where on() leaves the board action out for the target,
//    as under the board lock, where every board action of core's but Copy
//    block ID fails its `offers`.
// 3. Core calls the board action's `run` with the board, the target and
//    CoreApi, and waits for the batch where `run` returns a promise of it.
//    Where `run` throws, rejects or returns no batch, core logs that as a
//    fault of the board action's package and tells the user through a
//    notification of the code `board_action_failed`, about the plugin whose
//    class lists the board action, or hands it to the caller's notifier.
//    Then trigger() rejects.
// 4. Where the caller gives `extend`, core hands it the deltas that the batch
//    will apply, as core.commands.plan() returns them, and adds the commands
//    that it returns to the end of the batch.
// 5. Core runs the batch as one step of the user, through CoreApi's command
//    registry, which tells the user why it refused the batch, or hands that to
//    the caller's notifier, with the board action.
// The promise of trigger() resolves once the batch has run, and rejects with
// the refusal.
// For a board action with a quick action, trigger() opens the quick action as
// a menu at the anchor, or at the box of the element that the user triggered
// from, and resolves once the menu has closed and the batch that it ran, if
// any, has run. A batch of a row runs as steps 4 and 5 have it.
// Core shows one menu at a time: opening one closes the menu that is open, as
// a click outside it does, and the keyboard's focus returns to the element
// that the user triggered from as a menu closes.
// For a board action with an advanced action alone, trigger() opens the
// advanced action in its place (./advanced.ts), and so does a row of the
// board action in a menu, and its quick action where it gives no level for
// the target. Then trigger() resolves once the advanced action ends, as its
// done() has run a batch or its place has closed without one.
// A call of mountAdvanced() renders a board action's advanced action in a
// plugin's own room.

import { CORE } from "../commands/index.ts";
import type { CommandCall } from "../commands/index.ts";
import { REPLACED } from "../mount/index.ts";
import type { Delta, Store } from "../store/index.ts";
import type { BlockId, Board, PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi, Notification, Notifier } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Lock } from "../protocol/index.ts";
import { log } from "../log/index.ts";
import { asError, coreNotification, errorMessage, idText, isIdRecord, isObject, sourceModule } from "../util/index.ts";
import type { Refused } from "../util/index.ts";
import { createAdvanced } from "./advanced.ts";
import type { AdvancedModule, Batching, Opening, Place, ShowDialog } from "./advanced.ts";
import { coreBoardActions } from "./core.ts";
import type { CoreParts } from "./core.ts";
import { isElement, targetGiven } from "./given.ts";
import { isBatch, openMenu } from "./quick.ts";
import type { DrivenMenu, MenuStart, QuickLevel } from "./quick.ts";
import { checkedPlayer, checkedRoles, CORE_ROLES } from "./roles.ts";
import type { Role } from "./roles.ts";
import { kindsFailure } from "./targets.ts";

// What the page shows in place of the board where the blockr option
// board_actions fails, after why.
const NO_BOARD = "Core opens no board, since the blockr option board_actions holds for every board of the app.";

// What a board action acts on: the board, as the DAG's canvas offers it, one
// board object, as a notification names one, an input of a block, as the
// DAG's handle of an input offers it, or several board objects, as the DAG's
// selection holds them.
// An input is no board object, since the store holds no inputs: it is a slot
// that the block's class declares. So its target names the block and the
// input, as { block: "b", input: "y" }.
// A variadic block's places are its links, which a target names as it names
// any link. Its next place is the input "", as { block: "b", input: "" },
// since a link with an empty input takes the next place of a variadic block.
export type Target =
  | { readonly board: true }
  | BoardObject
  | { readonly block: BlockId; readonly input: string }
  | { readonly selection: readonly BoardObject[] };

// A command of any package, by its id, with its args: one entry of the batch
// that a board action runs (../commands/commands.ts).
export type { CommandCall };

export interface BoardAction {
  // Its id within its package, unique there, and ending in `_action` by
  // convention, so that it does not read as a command's id, as
  // "remove_block_action".
  readonly id: string;
  // Its package: blockr.core for core's own, and the package of the plugin
  // class that lists it otherwise. Core sets it as it registers the board
  // action, as it sets the id of a plugin class, so that a definition leaves
  // it out, as Omit<BoardAction, "package">. A caller names the board action
  // by its id, of the two, as core.id("blockr.core", "remove_block_action").
  readonly package: string;
  // What a menu shows, as "Remove block".
  readonly label: string;
  // SVG markup, as a block class's icon in the block registry, which a button
  // that offers the board action shows.
  readonly icon?: string;
  // What its row in a menu shows beside the label for the target, as Copy
  // block ID shows the block's id.
  detail?(board: Board, target: Target): string | undefined;
  // The kinds of target that it applies to, which name the menus that offer
  // it. Core hands it an input's target only where it names "input".
  readonly on: readonly ("board" | "block" | "input" | "link" | "stack" | "option" | "plugin" | "selection")[];
  // What a menu may order and divide its rows by: core's "add", "edit",
  // "clipboard" or "remove", or one that packages name, as "export".
  readonly category: string;
  // Whether a menu offers it for the target now, which core asks only for a
  // target of a kind that `on` names. A board action without it applies to
  // every such target.
  offers?(board: Board, target: Target, lock: Lock): boolean;
  // What it does, where it asks the user nothing: the batch to run as one step
  // of the user, or a promise of it, as for reading the clipboard first. It
  // returns no batch where it does what it does without commands, as Copy
  // block ID puts the block's id on the clipboard.
  run?(board: Board, target: Target, core: CoreApi): readonly CommandCall[] | undefined | Promise<readonly CommandCall[] | undefined>;
  // Its quick action, where it asks the user: the first level of rows that
  // shows at the trigger, which may wait, as for asking R for a block's
  // columns, or none for the target, where the edit takes more picks than two,
  // as Create link's on the board (./quick.ts). It gives a level wherever the
  // board action has no advanced action, since nothing would open otherwise.
  quick?(board: Board, target: Target, core: CoreApi): QuickLevel | undefined | Promise<QuickLevel | undefined>;
  // Its advanced action, which holds every option of the board action, and
  // which core shows in a place with enough room (./advanced.ts).
  readonly advanced?: AdvancedModule;
  // The role that it plays, as "append_block_role", which ties it to the
  // commands that the role names (./roles.ts). It plays the role only where
  // the app picks it for the role, or where it is the role's default and the
  // app picks none.
  readonly role?: string;
}

// The board actions that core registers for the open board, core's own first,
// in the order of their table, then those of each plugin class, in the order
// in which core registers the classes and as each class lists them, with the
// roles beside them and the board action that plays each role.
export interface BoardActions {
  // Registers the board actions and the roles that a plugin class lists, and
  // returns what unregisters them. A class that lists none registers none. It
  // registers all of them or none: it throws where one is not a board action
  // or not a role, as a package that writes its JS without types may give, or
  // breaks a rule, naming the class. A board action breaks a rule where:
  // - It has the id of another, core's, another class's or one that the class
  //   lists before it.
  // - Its `on` is empty.
  // - It has `run` beside `quick` or `advanced`, since `run` is for a board
  //   action that asks the user nothing.
  // - It has none of the three, so that nothing would run.
  // - Its `role` is no string.
  // A role breaks the rules that checkedRoles() in ./roles.ts names, as where
  // another role has its id, which names the packages of both, or where its
  // default is no board action of the class that can play it.
  register(plugin: PluginClassId, actions: readonly Omit<BoardAction, "package">[] | undefined, roles?: readonly Role[]): () => void;
  registered(): readonly BoardAction[];
  // The plugin class that lists the board action, or undefined for one of
  // core's own.
  classOf(action: BoardAction): PluginClassId | undefined;
  // The role with the id given, or undefined where none is registered.
  role(id: string): Role | undefined;
  // Chooses the board action that plays each role, once every module is
  // registered, from the blockr option board_actions as R sent it: the board
  // action that it picks for the role, or else the role's default, which
  // register() checked. It checks each pick (checkedPlayer() in ./roles.ts).
  // Where a pick fails the check, or no package registers it, or it names a
  // role that no package registers, or the blockr option holds no picks, it
  // throws, naming the pick, the role and what failed, so that core opens no
  // board. The blockr option is the app's rather than a board's, so a wrong
  // pick would break every board that the app opens.
  choosePlayers(picks: Json | undefined): void;
  // The board action that plays the role with the id given, or undefined
  // where none does.
  player(role: string): BoardAction | undefined;
  // The board actions that menus offer, in their order: each that names no
  // role, and the one that plays each role, in the place of the role's
  // default. A board action that names a role that it does not play is in
  // none.
  offered(): readonly BoardAction[];
}

// The board actions of an open board, with core's own and core's roles, whose
// notifications, as of a copy that the browser refuses, core raises through
// the `raise` of `parts`, and which read the preflights of the command
// registry and the board's block registry through it, as Add block lists its
// classes. No board action plays a role before choosePlayers() has run.
export function createBoardActions(parts: CoreParts): BoardActions {
  let registered: readonly BoardAction[] = coreBoardActions(parts).map((action) => Object.freeze({ ...action, package: CORE }));
  // The plugin class that lists each board action that a class lists.
  const classes = new Map<BoardAction, PluginClassId>();
  // The roles, core's first, then those of each class as it lists them, each
  // with the plugin class that lists it, which core's have none of, and the
  // package of its default.
  const roles = new Map<string, { readonly role: Role; readonly plugin: PluginClassId | undefined }>(
    CORE_ROLES.map((role) => [role.id, { role, plugin: undefined }]),
  );
  // The board action that plays the role where the app picks none: the
  // board action of the role's class whose id the role names.
  const fallbackOf = (role: string): BoardAction | undefined => {
    const held = roles.get(role);
    if (held === undefined) return undefined;
    return find({ package: held.plugin?.package ?? CORE, id: held.role.default });
  };
  // The board action with the package and the id given, or undefined for none.
  const find = (id: { readonly package: string; readonly id: string }): BoardAction | undefined =>
    registered.find((each) => each.package === id.package && each.id === id.id);
  // The board action that plays each role, by the role's id.
  const players = new Map<string, BoardAction>();
  // What menus offer, until what is registered or who plays a role changes.
  let offered: readonly BoardAction[] | undefined;

  function player(role: string): BoardAction | undefined {
    const action = players.get(role);
    return action !== undefined && registered.includes(action) ? action : undefined;
  }

  // What menus offer, in their order (offered(), above). A board action that
  // plays a role takes the place of the role's default, where the board
  // actions hold it, and keeps its own place otherwise.
  function inMenus(): readonly BoardAction[] {
    const places = new Map(registered.map((action, i) => [action, i]));
    const placeOf = (action: BoardAction, i: number): number => {
      const fallback = action.role === undefined ? undefined : fallbackOf(action.role);
      return (fallback === undefined ? undefined : places.get(fallback)) ?? i;
    };
    return registered
      .map((action, i) => ({ action, at: placeOf(action, i), i }))
      .filter(({ action }) => action.role === undefined || player(action.role) === action)
      .sort((a, b) => a.at - b.at || a.i - b.i)
      .map(({ action }) => action);
  }

  return {
    register(plugin, givenActions, givenRoles) {
      if (givenActions === undefined && givenRoles === undefined) return () => undefined;
      const actions =
        givenActions === undefined
          ? []
          : checked(plugin, givenActions, new Set(registered.filter((each) => each.package === plugin.package).map(({ id }) => id)));
      // The package of each role registered already, by the role's id.
      const taken = (): Map<string, string> =>
        new Map([...roles.values()].map(({ role, plugin: by }) => [role.id, by === undefined ? CORE : by.package]));
      const listed = givenRoles === undefined ? [] : checkedRoles(plugin, givenRoles, taken(), actions);
      registered = [...registered, ...actions];
      for (const action of actions) classes.set(action, plugin);
      for (const role of listed) roles.set(role.id, { role, plugin });
      offered = undefined;
      return () => {
        registered = registered.filter((action) => !actions.includes(action));
        for (const action of actions) classes.delete(action);
        for (const role of listed) {
          if (roles.get(role.id)?.role === role) roles.delete(role.id);
          players.delete(role.id);
        }
        offered = undefined;
      };
    },
    registered: () => registered,
    classOf: (action) => classes.get(action),
    role: (id) => roles.get(id)?.role,
    choosePlayers(value) {
      const picks = picksOf(value);
      players.clear();
      offered = undefined;
      const failures: string[] = [];
      for (const [role, id] of picks) {
        if (!roles.has(role)) failures.push(`${picking(id, role)}, but no package registers that role.`);
      }
      for (const { role } of roles.values()) {
        const pick = picks.get(role.id);
        const found = checkedPlayer(role, pick === undefined ? fallbackOf(role.id) : find(pick));
        if ("player" in found) players.set(role.id, found.player);
        else if (pick !== undefined) failures.push(`${picking(pick, role.id)}, but ${found.why}.`);
      }
      if (failures.length > 0) throw new Error([...failures, NO_BOARD].join("\n"));
    },
    player,
    offered() {
      offered ??= inMenus();
      return offered;
    },
  };
}

// The sentence of a failed pick up to what failed, which names the pick and the
// role.
function picking(action: { readonly package: string; readonly id: string }, role: string): string {
  return `The blockr option board_actions picks the board action "${idText(action)}" to play the role "${role}"`;
}

// The picks that the blockr option board_actions holds, as R sent it: the
// package and the id of a board action by the id of each role whose player the
// app picks, as an R option gives them, since an environment variable holds no
// list. R sends an empty list as an empty array, and nothing where nothing sets
// the option. It throws for anything else, as for the string of an environment
// variable.
function picksOf(value: Json | undefined): ReadonlyMap<string, { readonly package: string; readonly id: string }> {
  if (value === undefined || (Array.isArray(value) && value.length === 0)) return new Map();
  if (isObject(value)) {
    const entries = Object.entries(value);
    if (entries.every((entry) => isIdRecord(entry[1]))) {
      return new Map(entries.map(([role, pick]) => [role, pick as { readonly package: string; readonly id: string }]));
    }
  }
  throw new Error(
    [
      `The blockr option board_actions holds ${JSON.stringify(value)}, where it picks a board action for a role by the role's id and the board action's package and id, as list(append_block_role = list(package = "mykit", id = "append_block_action")) does in R.`,
      NO_BOARD,
    ].join("\n"),
  );
}

// The board actions that a plugin class lists, once each is a board action that
// keeps the rules, or an Error that says why the class is refused. The ids given
// are those of the board actions of the class's package registered already.
// Each board action comes back with the package of the class, which core sets.
function checked(plugin: PluginClassId, given: unknown, taken: ReadonlySet<string>): readonly BoardAction[] {
  const of = `The plugin class "${idText(plugin)}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives board actions that are no array.`);
  }
  const ids = new Set(taken);
  return given.map((action: unknown): BoardAction => {
    if (!isObject(action)) {
      throw new Error(`${of} gives a board action that is no object.`);
    }
    const { id } = action;
    if (typeof id !== "string" || id === "") {
      throw new Error(`${of} gives a board action whose id ${JSON.stringify(id)} is no non-empty string, as "remove_block_action".`);
    }
    const named = `${of} gives the board action "${id}"`;
    if (action.package !== undefined && action.package !== plugin.package) {
      throw new Error(`${named} with the package ${JSON.stringify(action.package)}, where core sets the package of its class, ${plugin.package}.`);
    }
    if (ids.has(id)) {
      throw new Error(`${named}, which has the id of another board action of ${plugin.package}.`);
    }
    ids.add(id);
    if (typeof action.label !== "string" || action.label === "") {
      throw new Error(`${named} without a label.`);
    }
    if (action.icon !== undefined && typeof action.icon !== "string") {
      throw new Error(`${named}, whose icon is no string of SVG markup.`);
    }
    const kinds = kindsFailure(action.on, "is empty, so that no menu would offer it");
    if (kinds !== undefined) {
      throw new Error(`${named}, whose on ${kinds}.`);
    }
    if (typeof action.category !== "string" || action.category === "") {
      throw new Error(`${named} without a category.`);
    }
    for (const member of ["detail", "offers", "run", "quick"]) {
      if (action[member] !== undefined && typeof action[member] !== "function") {
        throw new Error(`${named}, whose ${member} is no function.`);
      }
    }
    const { advanced } = action;
    if (advanced !== undefined && (!isObject(advanced) || typeof advanced.render !== "function")) {
      throw new Error(`${named}, whose advanced has no render() function.`);
    }
    const asks = ["quick", "advanced"].filter((member) => action[member] !== undefined);
    if (action.run !== undefined && asks.length > 0) {
      throw new Error(`${named}, which has run beside ${asks.join(" and ")}, while run is for a board action that asks the user nothing.`);
    }
    if (action.run === undefined && asks.length === 0) {
      throw new Error(`${named}, which has none of run, quick and advanced.`);
    }
    const { role } = action;
    if (role !== undefined && (typeof role !== "string" || role === "")) {
      throw new Error(`${named}, whose role ${JSON.stringify(role)} is no role's id, as "append_block_role".`);
    }
    return Object.freeze({ ...action, package: plugin.package }) as unknown as BoardAction;
  });
}

// Where a menu opens: the element that the user triggered from, to which the
// keyboard's focus returns as the menu closes, and the box at which the menu
// opens, such as the pointer of a right click.
export interface MenuPlace {
  readonly el: HTMLElement;
  readonly anchor: DOMRect;
}

// Draws an open menu at its place, until the function that it returns runs,
// which core calls as the menu closes: the module that the blockr option
// board_action_menu names (./module.ts), or what the CoreApi for tests
// records each menu with in place of drawing it.
export type ShowMenu = (menu: DrivenMenu, at: MenuPlace) => () => void;

// What CoreApi's `boardActions` reads.
export interface BoardActionsParts {
  // The store of the open board, whose board each board action gets.
  readonly store: Pick<Store, "board" | "hasBoard" | "subscribe">;
  // The locks that the app sets, which each `offers` gets.
  readonly lock: Lock;
  // CoreApi, which each `run`, `quick` and advanced action gets, and through
  // whose command registry a batch runs, so that core tells the user why the
  // command registry refused it, or hands that to the caller's notifier.
  readonly core: () => CoreApi;
  // Tells the user why the command registry refused to plan a batch for the
  // caller's `extend`, or hands that to the caller's notifier, as CoreApi's
  // command registry does where it refuses to run one.
  readonly refused: Refused;
  // Raises one of core's notifications anew, or hands it to the caller's
  // notifier, as core raises a refusal: here, that a board action's `run`
  // failed, or that its advanced action failed in render().
  readonly raiseAgain: (notification: Omit<Notification, "id">, notifier?: Notifier) => void;
  // Draws each menu that menu() and trigger() open: the module that the
  // blockr option board_action_menu names on the page, core's menu unless the
  // app names another (./module.ts).
  readonly show: ShowMenu;
  // The place of the frontend that draws the board, where the frontend's
  // module gives `advanced`, in which core shows each advanced action that
  // the caller of menu() or trigger() shows nowhere else.
  readonly frontend?: () => Place | undefined;
  // Draws core's dialog, which shows each advanced action that neither the
  // caller nor the frontend shows (./dialog.tsx).
  readonly dialog: ShowDialog;
  // Ends what CoreApi runs as it aborts, as another board opens: the menu
  // that is open closes, dropping what the user ticked, and none opens later,
  // and so does every advanced action.
  readonly signal?: AbortSignal;
}

// The options of a call of menu() or trigger(), checked: where the menu opens,
// with the anchor that trigger() takes from the box of `el` where it is given
// none, the caller's `extend`, its notifier, which takes the board action
// whose batch was refused with the notifications, and its place for advanced
// actions.
interface Given {
  readonly call: "menu()" | "trigger()";
  readonly el: HTMLElement;
  readonly anchor: DOMRect | undefined;
  readonly extend: ((deltas: readonly Delta[]) => unknown) | undefined;
  readonly notifier: ((notifications: readonly Notification[], action: BoardAction) => void) | undefined;
  // The caller's place, called as a method of the options, and the function
  // that the caller gave, which names the place, so that a caller that gives
  // the same function again shows one advanced action there at a time.
  readonly advanced: { readonly show: Place; readonly key: unknown } | undefined;
}

// CoreApi's `boardActions`, over the board actions registered for the open
// board.
export function boardActionsApi(actions: BoardActions, parts: BoardActionsParts): CoreApi["boardActions"] {
  const { store, lock, refused, raiseAgain, signal } = parts;
  // The menu that is open, of which core shows one at a time.
  let shown: DrivenMenu | undefined;
  signal?.addEventListener(
    "abort",
    () => {
      shown?.drop();
    },
    { once: true },
  );

  // Whether a menu of the target offers the board action: its `on` names the
  // kind of the target, and its `offers` passes for the target or it has none.
  // An `offers` that throws fails, as a fault of the board action's package,
  // which core logs.
  function applies(action: BoardAction, board: Board, target: Target): boolean {
    if (!action.on.includes(kindOf(target))) return false;
    if (action.offers === undefined) return true;
    try {
      return action.offers(board, target, lock);
    } catch (error) {
      log.error(new Error(`The offers of the board action "${idText(action)}" failed: ${errorMessage(error)}`, { cause: error }));
      return false;
    }
  }

  function on(target: Target): BoardAction[] {
    if (!store.hasBoard()) return [];
    const board = store.board();
    return actions.offered().filter((action) => applies(action, board, target));
  }

  // The board action that trigger() and mountAdvanced() act on for the id
  // given: the board action with that package and id, or the one that plays
  // the role with that id, since a caller names a board action by its package
  // and its id, and a role by its id alone, a string. It is undefined where no
  // board action plays the role, and for a board action that names a role that
  // it does not play, for which they run nothing. It throws for an id that no
  // board action or role has, and for one that is neither.
  function registered(id: unknown): BoardAction | undefined {
    if (typeof id === "string") {
      if (actions.role(id) === undefined) throw new Error(`No role "${id}" is registered.`);
      return actions.player(id);
    }
    if (!isIdRecord(id)) {
      throw new TypeError(
        `A board action is named by its id, as core.id("blockr.core", "rename_action") gives it, and a role by its id, as "rename_role", not by ${JSON.stringify(id)}.`,
      );
    }
    const action = actions.registered().find((each) => each.package === id.package && each.id === id.id);
    if (action === undefined) {
      throw new Error(`No board action "${idText(id)}" is registered.`);
    }
    return action.role === undefined || actions.player(action.role) === action ? action : undefined;
  }


  // The batch that the board action's `run` returns, which it calls at once,
  // so that a copy that needs the user's click or key press, as Copy block ID's
  // does, runs within it. A `run` that throws, rejects or returns what is no
  // batch fails (failed(), below), and the call rejects with an Error that
  // names the board action.
  async function batchOf(action: BoardAction, target: Target, notifier: Notifier | undefined): Promise<readonly CommandCall[] | undefined> {
    let batch: unknown;
    try {
      batch = await action.run?.(store.board(), target, parts.core());
    } catch (error) {
      throw failed(action, new Error(`The board action "${idText(action)}" failed: ${errorMessage(error)}`, { cause: error }), notifier);
    }
    if (batch !== undefined && !isBatch(batch)) {
      throw failed(
        action,
        new TypeError(
          `The run of the board action "${idText(action)}" returned no batch of commands, as [{ id: core.id("blockr.core", "remove_block"), args: { id: "a" } }].`,
        ),
        notifier,
      );
    }
    return batch;
  }

  // The failure of a board action's `run`, or of a multi's apply as the menu
  // closes, which is a fault of the board action's package: core logs it at
  // error, and tells the user through a notification of the code
  // board_action_failed, about the plugin whose class lists the board action,
  // naming the class's module, or hands it to the notifier. Core raises it
  // anew for each failure, as a refusal, so that a frontend that has shown it
  // shows it again.
  function failed(action: BoardAction, failure: Error, notifier: Notifier | undefined): Error {
    log.error(failure);
    const plugin = actions.classOf(action);
    raiseAgain(
      coreNotification(
        "board_action_failed",
        "error",
        failure.message,
        plugin === undefined ? {} : { module: sourceModule({ plugin }, plugin.package) },
      ),
      notifier,
    );
    return failure;
  }

  // The caller's notifier, for the batch of the board action given.
  function notifierOf(given: Given, action: BoardAction): Notifier | undefined {
    const { notifier } = given;
    return notifier === undefined
      ? undefined
      : (notifications) => {
          notifier(notifications, action);
        };
  }

  // Runs a batch as one step of the user, with the commands that the caller's
  // `extend` returns for the deltas that the batch will apply at its end, and
  // tells the notifier why the store or the command registry refused it. It
  // resolves with those deltas, which core plans where `extend` is given or
  // `planned` asks for them, as for a done(). It runs nothing for an empty
  // batch.
  async function runBatch(batch: readonly CommandCall[], batching: Batching, planned = false): Promise<readonly Delta[]> {
    if (batch.length === 0) return [];
    const core = parts.core();
    const { extend, notifier } = batching;
    let deltas: readonly Delta[] = [];
    let added: readonly CommandCall[] = [];
    if (extend !== undefined || planned) {
      try {
        deltas = await core.commands.plan(batch);
      } catch (error) {
        refused(error, undefined, notifier);
        throw error;
      }
    }
    if (extend !== undefined) {
      const extra: unknown = extend(deltas);
      if (!isBatch(extra)) {
        throw new TypeError(
          `The \`extend\` of ${batching.call ?? "the call"} returned no batch of commands, as [{ id: core.id("blockr.core", "remove_block"), args: { id: "a" } }].`,
        );
      }
      added = extra;
    }
    await core.commands.run([...batch, ...added], "user", notifier === undefined ? undefined : { notifier });
    return deltas;
  }

  // How the batch of the board action runs for the caller of menu() or
  // trigger().
  function batching(given: Given, action: BoardAction): Batching {
    return { call: given.call, extend: given.extend, notifier: notifierOf(given, action) };
  }

  // Runs the board action's run on the target, and the batch that it returns
  // as one step of the user, as trigger() does for a board action that asks
  // the user nothing (steps 3 to 5, above).
  async function runOf(action: BoardAction, target: Target, given: Given): Promise<void> {
    const batch = await batchOf(action, target, notifierOf(given, action));
    if (batch !== undefined) await runBatch(batch, batching(given, action));
  }

  const advanced = createAdvanced({
    store,
    core: parts.core,
    applies: (action, target) => store.hasBoard() && applies(action, store.board(), target),
    run: (batch, how) => runBatch(batch, how, true),
    frontend: () => parts.frontend?.(),
    dialog: parts.dialog,
    raise: (notification) => {
      raiseAgain(notification);
    },
    classOf: (action) => actions.classOf(action),
    ...(signal === undefined ? {} : { signal }),
  });

  // What opens the board action's advanced action for the caller of menu()
  // or trigger(), with what the open icon hands it, if anything.
  function openingOf(given: Given, action: BoardAction, opened: { readonly picked?: Json; readonly filter?: string } = {}): Opening {
    return { ...batching(given, action), el: given.el, place: given.advanced, ...opened };
  }

  // Opens a menu at the place that the caller gives, in place of the menu
  // that is open, which closes as at a click outside it, and calls `closed`
  // with the promise of the batch that it ran, if any, as it closes. A menu
  // that closes as it opens, as a quick action that gives no level and opens
  // the advanced action at once, does not show.
  function open(start: MenuStart, target: Target, given: Given, closed?: (ran: Promise<void> | undefined) => void): void {
    void shown?.close().catch(() => undefined);
    let hide: (() => void) | undefined;
    const menu = openMenu(start, {
      board: () => store.board(),
      target,
      core: parts.core,
      applies: (action) => store.hasBoard() && applies(action, store.board(), target),
      run: async (batch, action) => {
        await runBatch(batch, batching(given, action));
      },
      trigger: (action) => runOf(action, target, given),
      advanced: (action, opened) => advanced.open(action, target, openingOf(given, action, opened)),
      failed(action, failure) {
        failed(action, failure, notifierOf(given, action));
      },
      closed(ran) {
        // The menu that closes is the one that core shows, if any, since the
        // next shows only once the one before has closed.
        if (shown?.closed() === true) shown = undefined;
        hide?.();
        hide = undefined;
        closed?.(ran);
      },
    });
    if (menu.closed()) return;
    shown = menu;
    try {
      hide = parts.show(menu, { el: given.el, anchor: given.anchor ?? given.el.getBoundingClientRect() });
    } catch (error) {
      // The module that draws the menu failed, a fault of its package. Core
      // logs why and tells the user through a notification of the code
      // module_failed, about the board. Then it closes the menu, which draws
      // nothing, so that a call of trigger() that waits for it resolves.
      log.error(asError(error));
      raiseAgain(coreNotification("module_failed", "error", errorMessage(error)));
      menu.drop();
    }
  }

  return {
    on: (target) => on(targetGiven(target)),
    menu(target, opts) {
      const checked = targetGiven(target);
      const given = optionsGiven(opts, "menu()");
      if (signal?.aborted === true) return;
      const offered = on(checked);
      if (offered.length > 0) open({ actions: offered }, checked, given);
    },
    async trigger(id, target, opts) {
      const checked = targetGiven(target);
      const given = optionsGiven(opts, "trigger()");
      const action = registered(id);
      if (action === undefined || signal?.aborted === true || !store.hasBoard() || !applies(action, store.board(), checked)) return;
      if (action.run !== undefined) {
        await runOf(action, checked, given);
        return;
      }
      if (action.quick === undefined) {
        await advanced.open(action, checked, openingOf(given, action));
        return;
      }
      await new Promise<void>((resolve, reject) => {
        open({ action }, checked, given, (ran) => {
          if (ran === undefined) resolve();
          else ran.then(resolve, reject);
        });
      });
    },
    mountAdvanced(el, id, target, picked) {
      if (signal?.aborted === true) throw new Error(REPLACED);
      const given: unknown = el;
      if (!isElement(given)) {
        throw new TypeError("The `el` of mountAdvanced() is the element in which core renders the advanced action.");
      }
      const checked = targetGiven(target);
      const action = registered(id);
      if (action === undefined) return () => undefined;
      if (action.advanced === undefined) {
        const playing = typeof id === "string" ? `, which plays the role "${id}",` : "";
        throw new Error(`The board action "${idText(action)}"${playing} has no advanced action to mount.`);
      }
      return advanced.mount(given, action, checked, picked);
    },
  };
}

// The options that a caller gives menu() or trigger(), checked, as a plugin in
// plain JS may give others: the element that the user triggered from, the
// anchor, which menu() needs and trigger() takes from the box of the element
// where it is given none, and the caller's `extend`, notifier and place for
// advanced actions. It throws a TypeError for options that are none of these.
function optionsGiven(opts: unknown, call: Given["call"]): Given {
  const given = isObject(opts) ? opts : {};
  const { el, anchor, extend, notifier, advanced } = given;
  if (!isElement(el)) {
    throw new TypeError(`The \`el\` of ${call} is the element that the user triggered from, to which the keyboard's focus returns.`);
  }
  if (anchor === undefined ? call === "menu()" : !isRect(anchor)) {
    throw new TypeError(`The \`anchor\` of ${call} is the box at which the menu opens, a DOMRect.`);
  }
  if (extend !== undefined && typeof extend !== "function") {
    throw new TypeError(`The \`extend\` of ${call} is a function.`);
  }
  if (notifier !== undefined && typeof notifier !== "function") {
    throw new TypeError(`The \`notifier\` of ${call} is a function.`);
  }
  if (advanced !== undefined && typeof advanced !== "function") {
    throw new TypeError(`The \`advanced\` of ${call} is a function, which shows an advanced action in a place of the caller's.`);
  }
  return {
    call,
    el,
    anchor: anchor as DOMRect | undefined,
    // Each is called as a method of the options, as a caller may write it.
    extend: extend === undefined ? undefined : (deltas) => (extend as Method).call(given, deltas),
    notifier:
      notifier === undefined
        ? undefined
        : (notifications, action) => {
            (notifier as Method).call(given, notifications, action);
          },
    advanced:
      advanced === undefined ? undefined : { show: (args) => (advanced as Method).call(given, args) as ReturnType<Place>, key: advanced },
  };
}

// A function in the options of a call, which the types of CoreApi type, and
// which a caller in plain JS gives as it chooses.
type Method = (this: unknown, ...args: unknown[]) => unknown;

// Whether a value is a box, as a DOMRect is, with its numbers.
function isRect(value: unknown): value is DOMRect {
  return isObject(value) && ["x", "y", "width", "height"].every((key) => typeof value[key] === "number");
}

// The kind of a target, which the `on` of a board action names. An input's
// target names a block too, so kindOf() checks for an input before a block.
function kindOf(target: Target): BoardAction["on"][number] {
  if ("board" in target) return "board";
  if ("selection" in target) return "selection";
  if ("input" in target) return "input";
  if ("block" in target) return "block";
  if ("link" in target) return "link";
  if ("stack" in target) return "stack";
  if ("option" in target) return "option";
  return "plugin";
}
