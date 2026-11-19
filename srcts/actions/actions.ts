// Board actions, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#a-board-action):
// what a user triggers from a menu, a button or a gesture, on the board, on one
// of its board objects or on a selection of them. A board action changes the
// board only through commands, core's and any package's, and runs each batch as
// one undo step of the user. It is no command: a command reads the board,
// returns deltas and asks nobody anything, while a board action is the part
// with a UI, which runs commands.
//
// Core registers the board actions for each board that opens: its own, in the
// order of their table (./core.ts), then those that each plugin class lists in
// `boardActions`, as it registers the class with its commands
// (../core/start.ts). It refuses the whole class where one of its board actions
// breaks a rule (register(), below).
//
// CoreApi's `boardActions` gives the board actions that apply to a target, and
// triggers one by its id (boardActionsApi(), below). Core hands a board action
// only targets of the kinds that its `on` names, so that a kind added later
// reaches no board action that does not name it. A call of trigger() runs a
// board action that asks the user nothing in these steps:
// 1. Core looks the board action up by its id.
// 2. Core runs nothing where on() leaves the board action out for the target,
//    as under the board lock, where every board action of core's but Copy
//    block ID fails its `offers`.
// 3. Core calls the board action's `run` with the board, the target and
//    CoreApi, and waits for the batch where `run` returns a promise of it.
// 4. Where the caller gives `extend`, core hands it the deltas that the batch
//    will apply, as core.commands.plan() returns them, and adds the commands
//    that it returns to the end of the batch.
// 5. Core runs the batch as one step of the user, through CoreApi's command
//    registry, which tells the user why it refused the batch, or hands that to
//    the caller's notifier.
// The promise of trigger() resolves once the batch has run, and rejects with
// the refusal.
// Core opens no quick action and no advanced action yet, so trigger() rejects
// for a board action without `run`.

import { isCommandId } from "../commands/index.ts";
import type { Delta, Store } from "../store/index.ts";
import type { Board, PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Lock } from "../protocol/index.ts";
import { log } from "../log/index.ts";
import { errorMessage, givenNotifier, isBoardObject, isObject, quoted } from "../util/index.ts";
import type { Raise, Refused } from "../util/index.ts";
import { coreBoardActions } from "./core.ts";

// What a board action acts on: the board, as the DAG's canvas offers it, one
// board object, as a notification names one, or several, as the DAG's
// selection holds them. A target names no input of a block.
export type Target =
  | { readonly board: true }
  | BoardObject
  | { readonly selection: readonly BoardObject[] };

// A command of any package, by its id, with its args: one entry of the batch
// that a board action runs.
export interface CommandCall {
  readonly id: string;
  readonly args?: Json;
}

export interface BoardAction {
  // Named as a command's id is, `{pkg}-{name}`, and ending in `_action` by
  // convention, so that it does not read as a command's id, as
  // "blockr.core-remove_block_action".
  readonly id: string;
  // What a menu shows, as "Remove block".
  readonly label: string;
  // SVG markup, as a block class's icon in the block registry, which a button
  // that offers the board action shows.
  readonly icon?: string;
  // What its row in a menu shows beside the label for the target, as Copy
  // block ID shows the block's id.
  detail?(board: Board, target: Target): string | undefined;
  // The kinds of target that it applies to, which name the menus that offer
  // it.
  readonly on: readonly ("board" | "block" | "link" | "stack" | "option" | "plugin" | "selection")[];
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
}

// The kinds of target, each of which names the menus that offer a board
// action.
const KINDS: Readonly<Record<BoardAction["on"][number], true>> = {
  board: true,
  block: true,
  link: true,
  stack: true,
  option: true,
  plugin: true,
  selection: true,
};

// The board actions that core registers for the open board, core's own first,
// in the order of their table, then those of each plugin class, in the order
// in which core registers the classes and as each class lists them.
export interface BoardActions {
  // Registers the board actions that a plugin class lists, and returns what
  // unregisters them. A class that lists none registers none. It registers
  // all of them or none: it throws where one is not a board action, as a
  // package that writes its JS without types may give, or breaks a rule,
  // naming the class. A board action breaks a rule where:
  // - It has the id of another, core's, another class's or one that the class
  //   lists before it.
  // - Its `on` is empty.
  // - It has `run` beside `quick` or `advanced`, since `run` is for a board
  //   action that asks the user nothing.
  // - It has none of the three, so that nothing would run.
  register(plugin: PluginClassId, actions: readonly BoardAction[] | undefined): () => void;
  registered(): readonly BoardAction[];
}

// The board actions of an open board, with core's own, whose notifications,
// as of a copy that the browser refuses, core raises through `raise`.
export function createBoardActions(raise: Raise): BoardActions {
  let registered: readonly BoardAction[] = coreBoardActions(raise);

  return {
    register(plugin, given) {
      if (given === undefined) return () => undefined;
      const actions = checked(plugin, given, new Set(registered.map(({ id }) => id)));
      registered = [...registered, ...actions];
      return () => {
        registered = registered.filter((action) => !actions.includes(action));
      };
    },
    registered: () => registered,
  };
}

// The board actions that a plugin class lists, once each is a board action that
// keeps the rules, or an Error that says why the class is refused. The ids given
// are those of the board actions registered already.
function checked(plugin: PluginClassId, given: unknown, taken: ReadonlySet<string>): readonly BoardAction[] {
  const of = `The plugin class "${plugin}"`;
  if (!Array.isArray(given)) {
    throw new Error(`${of} gives board actions that are no array.`);
  }
  const ids = new Set(taken);
  return given.map((action: unknown): BoardAction => {
    if (!isObject(action)) {
      throw new Error(`${of} gives a board action that is no object.`);
    }
    const { id } = action;
    if (typeof id !== "string" || !isCommandId(id)) {
      throw new Error(`${of} gives a board action whose id ${JSON.stringify(id)} is not named as a command's id is, "{pkg}-{name}".`);
    }
    const named = `${of} gives the board action "${id}"`;
    if (ids.has(id)) {
      throw new Error(`${named}, which has the id of another board action.`);
    }
    ids.add(id);
    if (typeof action.label !== "string" || action.label === "") {
      throw new Error(`${named} without a label.`);
    }
    if (action.icon !== undefined && typeof action.icon !== "string") {
      throw new Error(`${named}, whose icon is no string of SVG markup.`);
    }
    const on: unknown = action.on;
    if (!Array.isArray(on)) {
      throw new Error(`${named}, whose on is no array of the kinds of target that it applies to.`);
    }
    if (on.length === 0) {
      throw new Error(`${named}, whose on is empty, so that no menu would offer it.`);
    }
    const other: unknown = on.find((kind) => typeof kind !== "string" || !Object.hasOwn(KINDS, kind));
    if (other !== undefined) {
      throw new Error(`${named}, whose on names ${JSON.stringify(other)}, which is none of the kinds of target, ${quoted(Object.keys(KINDS))}.`);
    }
    if (typeof action.category !== "string" || action.category === "") {
      throw new Error(`${named} without a category.`);
    }
    for (const member of ["detail", "offers", "run"]) {
      if (action[member] !== undefined && typeof action[member] !== "function") {
        throw new Error(`${named}, whose ${member} is no function.`);
      }
    }
    const asks = ["quick", "advanced"].filter((member) => action[member] !== undefined);
    if (action.run !== undefined && asks.length > 0) {
      throw new Error(`${named}, which has run beside ${asks.join(" and ")}, while run is for a board action that asks the user nothing.`);
    }
    if (action.run === undefined && asks.length === 0) {
      throw new Error(`${named}, which has none of run, quick and advanced.`);
    }
    return action as unknown as BoardAction;
  });
}

// What CoreApi's `boardActions` reads.
export interface BoardActionsParts {
  // The store of the open board, whose board each board action gets.
  readonly store: Pick<Store, "board" | "hasBoard">;
  // The locks that the app sets, which each `offers` gets.
  readonly lock: Lock;
  // CoreApi, which each `run` gets, and through whose command registry
  // trigger() runs a batch, so that core tells the user why the command
  // registry refused it, or hands that to the caller's notifier.
  readonly core: () => CoreApi;
  // Tells the user why the command registry refused to plan a batch for the
  // caller's `extend`, or hands that to the caller's notifier, as CoreApi's
  // command registry does where it refuses to run one.
  readonly refused: Refused;
}

// CoreApi's `boardActions`, over the board actions registered for the open
// board.
export function boardActionsApi(actions: BoardActions, parts: BoardActionsParts): CoreApi["boardActions"] {
  const { store, lock, refused } = parts;

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
      log.error(new Error(`The offers of the board action "${action.id}" failed: ${errorMessage(error)}`, { cause: error }));
      return false;
    }
  }

  // The batch that the board action's `run` returns, which it calls at once,
  // so that a copy that needs the user's click or key press, as Copy block ID's
  // does, runs within it. A `run` that throws, rejects or returns what is no
  // batch is a fault of the board action's package, which core logs, and the
  // call rejects with an Error that names the board action.
  async function batchOf(action: BoardAction, target: Target): Promise<readonly CommandCall[] | undefined> {
    let batch: unknown;
    try {
      batch = await action.run?.(store.board(), target, parts.core());
    } catch (error) {
      const failure = new Error(`The board action "${action.id}" failed: ${errorMessage(error)}`, { cause: error });
      log.error(failure);
      throw failure;
    }
    if (batch !== undefined && !isBatch(batch)) {
      const failure = new TypeError(`The run of the board action "${action.id}" returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].`);
      log.error(failure);
      throw failure;
    }
    return batch;
  }

  return {
    on(given) {
      const target = targetGiven(given);
      if (!store.hasBoard()) return [];
      const board = store.board();
      return actions.registered().filter((action) => applies(action, board, target));
    },
    async trigger(id, given, opts) {
      const target = targetGiven(given);
      const notifier = givenNotifier(opts, "trigger()");
      if (opts?.extend !== undefined && typeof opts.extend !== "function") {
        throw new TypeError("The `extend` of trigger() is a function.");
      }
      const action = actions.registered().find((registered) => registered.id === id);
      if (action === undefined) {
        throw new Error(`No board action "${id}" is registered.`);
      }
      if (action.run === undefined) {
        throw new Error(`The board action "${id}" asks the user through its quick action or its advanced action, which core does not open yet.`);
      }
      if (!store.hasBoard() || !applies(action, store.board(), target)) return;
      const batch = await batchOf(action, target);
      if (batch === undefined || batch.length === 0) return;
      const core = parts.core();
      let added: readonly CommandCall[] = [];
      if (opts?.extend !== undefined) {
        let deltas: Delta[];
        try {
          deltas = await core.commands.plan(batch);
        } catch (error) {
          refused(error, undefined, notifier);
          throw error;
        }
        const extra: unknown = opts.extend(deltas);
        if (!isBatch(extra)) {
          throw new TypeError('The `extend` of trigger() returned no batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }].');
        }
        added = extra;
      }
      await core.commands.run([...batch, ...added], "user", notifier === undefined ? undefined : { notifier });
    },
  };
}

// The target that a caller gives on() or trigger(), copied, so that no board
// action changes what the next one gets. It throws for anything else, as a
// plugin in plain JS may give.
function targetGiven(given: unknown): Target {
  if (isObject(given) && Object.keys(given).length === 1) {
    if (given.board === true) return Object.freeze({ board: true });
    const { selection } = given;
    if (Array.isArray(selection) && selection.every(isBoardObject)) {
      return Object.freeze({ selection: Object.freeze(selection.map((object) => Object.freeze({ ...object }))) });
    }
    if (isBoardObject(given)) return Object.freeze({ ...given });
  }
  throw new TypeError(
    `A board action acts on the board, as { board: true }, on a board object, as { block: "a" }, or on a selection of them, as { selection: [{ block: "a" }, { link: "ab" }] }, not ${JSON.stringify(given)}.`,
  );
}

// The kind of a target, which the `on` of a board action names.
function kindOf(target: Target): BoardAction["on"][number] {
  if ("board" in target) return "board";
  if ("selection" in target) return "selection";
  if ("block" in target) return "block";
  if ("link" in target) return "link";
  if ("stack" in target) return "stack";
  if ("option" in target) return "option";
  return "plugin";
}

// Whether a value is a batch of commands, each by its id.
function isBatch(value: unknown): value is readonly CommandCall[] {
  return Array.isArray(value) && value.every((call) => isObject(call) && typeof call.id === "string");
}
