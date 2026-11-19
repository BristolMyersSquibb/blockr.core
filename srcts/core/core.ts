// CoreApi, what core gives a frontend and a plugin, as Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-plugins).
// Core makes one for each board that opens, on the store, the command registry
// and the scheduler that it creates for that board, and hands it to the load()
// of the app's storage plugin as the page starts, to the render() of the
// frontend and of every plugin's UI, and to every plugin class's start
// (./start.ts, ./plugins.ts). It ends with them, as the next board opens.
//
// Beside what it hands out, it calls the onChange() of each board option whose
// value changes, as the board opens or a delta sets it, so that a new value of
// dark_mode switches the page's theme. Where the board lacks the value of a
// board option that has an onChange(), as the board opens or once a delta
// removed it, core adds the board option to the board with its default, which
// calls onChange() in turn, so that what it does on the page follows the board
// that is open.
//
// It also holds what the frontend offers other plugins, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#what-a-frontend-offers-other-plugins):
// the notifications of the board, which the frontend draws (./notifications.ts),
// which block has focus and which blocks and plugins are on screen, both of
// which the frontend sets, and show(), through which a plugin has the frontend
// show a block or a plugin
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#showing-a-block-or-a-plugin).
// Beside them, it holds the board objects that the user selected, which
// whatever selects sets, as the DAG does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
// What they hold is the page's, outside the board, and ends with the board. Where an onChange() throws, core
// raises a notification that holds until its next call returns, and so do
// those that the call raises through its `notify`.
//
// Where the store or the command registry refuses an edit that a plugin makes
// through CoreApi, of whatever origin, core tells the user why, as an error of
// the code `board_edit_failed`, whichever plugin made the edit, unless the
// call's notifier takes it. The call still rejects or throws with the reason
// (refusing(), below). So does a host's submit(), whose notification concerns
// the host's block or board option (../mount/block.ts, ../mount/option.ts).

import { isLogged } from "../commands/index.ts";
import type { Commands } from "../commands/index.ts";
import { announceTo, createMounts } from "../mount/index.ts";
import type { Loader, Measure, OutputEvent } from "../mount/index.ts";
import type { Lock, OptionEntry, PluginEntry, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Origin, Store } from "../store/index.ts";
import type { BlockId, Board, OptionId, PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi, Notification, Notifier, OnScreen, OutputEvents, Raised } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { createLog, log } from "../log/index.ts";
import {
  asError,
  coreNotification,
  errorMessage,
  givenNotifier,
  isBoardObject,
  isPackageName,
  jsonEqual,
  own,
  sourceModule,
} from "../util/index.ts";
import type { Refused } from "../util/index.ts";
import type { BlockrOptions } from "./blockr-options.ts";
import type { Holder } from "./notifications.ts";
import { runPlugins } from "./plugins.ts";
import type { RunningPlugins } from "./plugins.ts";
import { pluginRequest } from "./requests.ts";

export interface CoreParts {
  // The store as frontends and plugins see it, whose open() starts core
  // again for the board it opens (./start.ts), and the command registry.
  // CoreApi hands out both with what tells the user why they refused an edit
  // (refusing(), below).
  readonly store: Store;
  readonly commands: Commands;
  readonly scheduler: Scheduler;
  readonly loader: Loader;
  readonly blockrOptions: BlockrOptions;
  // The locks that the app sets, from R's reply to `hello`.
  readonly lock: Lock;
  // The renderers that the app names for board options, from R's latest reply
  // to `hello`.
  readonly renderers: () => Readonly<Record<string, Renderer>>;
  // The app's plugins, which run on the board, from the reply to `hello` that
  // core starts with for it, by the ids of their classes.
  readonly plugins: Readonly<Record<PluginClassId, PluginEntry>>;
  // The board options that the loaded packages list, from the same reply, by
  // id, which name the package of each.
  readonly options: Readonly<Record<OptionId, OptionEntry>>;
  // The notifications of the board, which core made with its store before it
  // imported the modules that the reply names (./notifications.ts).
  readonly notifications: Holder;
  // Measures an output's element, through the DOM unless given.
  readonly measure?: Measure;
  // Ends what CoreApi runs as it aborts, as another board opens: every mount,
  // and what core keeps of outputs and answers, while the outputs' events
  // reach nobody more.
  readonly signal?: AbortSignal;
  // Has the frontend that draws the board show a block or a plugin, through
  // the show() of its module where it has one, as the page's frontends do
  // (./frontends.ts), and resolves once that has.
  readonly show: (object: ShownObject) => Promise<void>;
}

// What CoreApi's show() takes: a block or a plugin, by its id.
type ShownObject = Parameters<CoreApi["show"]>[0];
// The options in which an edit through CoreApi gives its notifier.
type NotifierOpts = { notifier?(notifications: readonly Notification[]): void } | undefined;
// What CoreApi's onScreen holds once the frontend has set it.
type Seen = NonNullable<ReturnType<OnScreen["get"]>>;

// What core makes for one board: CoreApi, and what draws and starts the app's
// plugins as the board opens, which core calls (./start.ts).
export interface Core {
  readonly api: CoreApi;
  readonly plugins: Pick<RunningPlugins, "frontend" | "start">;
}

export function createCore(parts: CoreParts): Core {
  const { store, commands, scheduler, loader, blockrOptions, notifications } = parts;
  const { raise } = notifications;
  // The notifications follow the scheduler ahead of any frontend, so that
  // what a frontend reads of them as the scheduler announces a status holds
  // that status's conditions.
  notifications.follow(scheduler);
  // The package that lists a board option, from R's reply.
  const optionPackage = (id: OptionId): string | undefined => own(parts.options, id)?.package;

  const listeners = new Set<(event: OutputEvent) => void>();
  parts.signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true },
  );
  const outputEvents: OutputEvents = {
    subscribe(fn) {
      const wrapped = (event: OutputEvent): void => {
        fn(event);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };

  // Tells the user why the store or the command registry refused an edit, an
  // edit through CoreApi or a host's submit(), or hands that to the call's
  // notifier, as Notifications has it
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
  // - Core raises the same refusal anew each time, so that a frontend that
  //   has shown it shows it again.
  // - Core logs the reason at debug alone, since the call hands it to its
  //   caller. Where a command's own code threw, the command registry has
  //   logged the reason at error already, with its stack, so core logs no
  //   line of its own (../commands/commands.ts).
  const refused: Refused = (error, about, notifier) => {
    const why = errorMessage(error);
    if (!isLogged(error)) log.debug(why);
    notifications.raiseAgain(coreNotification("board_edit_failed", "error", why, about === undefined ? {} : { about }), notifier);
  };

  // A host's submit() runs its command through the command registry itself,
  // not through CoreApi, and tells the user why the store refused it, about
  // the host's block or board option. The inputs of core's React SDK and its
  // controls pass a notifier, since they say below themselves why the host
  // refused what they submitted.
  const mounts = createMounts({
    store,
    commands,
    scheduler,
    loader,
    core: () => api,
    renderers: parts.renderers,
    raise,
    replied: notifications.replied,
    notify: notifications.notify,
    refused,
    optionPackage,
    ...(parts.measure === undefined ? {} : { measure: parts.measure }),
    ...(parts.signal === undefined ? {} : { signal: parts.signal }),
    emit(event) {
      for (const fn of [...listeners]) {
        try {
          fn(event);
        } catch (error) {
          log.error(asError(error));
        }
      }
    },
  });

  // It subscribes to the store before core's own subscription below, so that
  // the board options of the plugins' classes go onto the board in their
  // order as it opens, before the board options that have an onChange().
  const plugins = runPlugins({
    store,
    keepOptions: (ids, by, failed) => mounts.keepOptions(ids, by, failed),
    core: () => api,
    plugins: parts.plugins,
    raise,
    ...(parts.signal === undefined ? {} : { signal: parts.signal }),
  });

  const focus = createFocus(store, parts.signal);
  const selection = createSelection(store, plugins, parts.signal);
  const edits = refusing(store, commands, refused);

  const api: CoreApi = {
    store: edits.store,
    commands: edits.commands,
    scheduler: {
      demand: (owner, blocks, options) => scheduler.demand(owner, blocks, options),
      evaluate: (blocks, params, options) => scheduler.evaluate(blocks, params, options),
      invalidate: (id) => {
        scheduler.invalidate(id);
      },
      status: (id) => scheduler.status(id),
      subscribe: (fn) => scheduler.subscribe(fn),
    },
    blockRegistry: () => store.blockRegistry(),
    options: {
      register: (def) => store.options.register(def),
      registered: () => store.options.registered(),
      value: (id) => (store.hasBoard() ? own(store.board().options, id) : undefined),
    },
    plugins: {
      appPlugins: () => plugins.appPlugins(),
      pluginClass: (plugin) => plugins.pluginClass(plugin),
      storage: () => plugins.storage(),
    },
    lock: parts.lock,
    outputEvents,
    notifications: notifications.api,
    log(pkg) {
      // A plugin in plain JS may give something else, which the types refuse.
      const given: unknown = pkg;
      if (!isPackageName(given)) {
        throw new TypeError(`A log names the package whose lines it writes, as core.log("blockr.dock"), not ${JSON.stringify(given)}.`);
      }
      return createLog(given);
    },
    focus,
    selection,
    onScreen: createOnScreen(parts.signal),
    show(object) {
      if (parts.signal?.aborted === true) return Promise.resolve();
      const shown = shownObject(object);
      if ("block" in shown) {
        if (!store.hasBoard() || !Object.hasOwn(store.board().blocks, shown.block)) {
          throw new Error(`The board holds no block ${JSON.stringify(shown.block)} to show.`);
        }
        focus.set(shown.block);
      } else if (plugins.pluginClass(shown.plugin).ui === undefined) {
        throw new Error(`The class of the plugin "${shown.plugin}" has no ui to show.`);
      }
      return parts.show(shown);
    },
    request: <T = Json>(plugin: PluginClassId, name: string, args: Readonly<Record<string, Json>> = {}, opts = {}) =>
      pluginRequest({ plugins, scheduler, replied: notifications.replied }, plugin, name, args, opts) as Promise<T | Blob>,
    module: <T = unknown>(id: string) => loader.module(id) as Promise<T>,
    blockrOption: (name) => blockrOptions.read(name),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option),
    mountPlugin: (el, plugin) => plugins.mountPlugin(el, plugin),
  };

  // The values of the board options as each onChange() last heard them.
  let heard: Board["options"] = {};
  store.subscribe((change, board) => {
    const opened = "opened" in change;
    if (board.options !== heard || opened) {
      const before = opened ? {} : heard;
      heard = board.options;
      for (const [id, value] of Object.entries(board.options)) {
        if (!jsonEqual(own(before, id), value)) changed(id, value);
      }
    }
    if (opened || (change.delta.options?.unset?.length ?? 0) > 0) {
      const lacking = Object.values(store.options.registered())
        .filter((def) => def.onChange !== undefined && !Object.hasOwn(board.options, def.id))
        .map((def) => def.id);
      if (lacking.length > 0) void mounts.addOptions(lacking, "its onChange");
    }
  });

  // The notification of each onChange() that threw, by its board option's id,
  // which holds until its next call returns.
  const failing = new Map<OptionId, () => void>();
  // What withdraws each notification that the last call of each onChange()
  // raised, by its board option's id, which runs as its next call returns.
  const raisedBy = new Map<OptionId, (() => void)[]>();

  function changed(id: OptionId, value: Json): void {
    const def = own(store.options.registered(), id);
    if (def?.onChange === undefined) return;
    const raisedNow: (() => void)[] = [];
    // The board option's module, which defines its onChange().
    const module = sourceModule({ option: id }, optionPackage(id));
    const notify = (notification: Raised): (() => void) => {
      const withdraw = notifications.notify(module, notification);
      raisedNow.push(withdraw);
      return withdraw;
    };
    let failure: (() => void) | undefined;
    try {
      def.onChange(value, api, notify);
    } catch (error) {
      log.error(asError(error));
      failure = raise(
        coreNotification("on_change_failed", "error", `The onChange of the board option "${id}" failed: ${errorMessage(error)}`, {
          module,
        }),
      );
    }
    failing.get(id)?.();
    if (failure === undefined) failing.delete(id);
    else failing.set(id, failure);
    for (const withdraw of raisedBy.get(id) ?? []) withdraw();
    raisedBy.set(id, raisedNow);
  }

  return { api, plugins };
}

// The store and the command registry as plugins see them, through CoreApi.
// Where either refuses an edit, a command, a list of commands, a delta, an
// undo or a redo, of whatever origin, core tells the user why through a
// notification of the code `board_edit_failed`, unless the call's notifier
// takes it, and the call still rejects or throws with the reason, as
// Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
// - The notification concerns the board, since core knows the origin of an
//   edit but not the plugin that made it, so a frontend does not show it on a
//   block's card, which the dialog that made the edit may cover.
// - Core raises it where an edit enters through CoreApi, rather than in the
//   command registry and in the store, since the command registry applies its
//   deltas through the store. So core raises each refusal once.
// - A caller that shows the reason itself, or hands it on, as the assistant's
//   edit tool hands each refusal to its model, passes a notifier
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.4.2#how-failures-reach-the-model).
// - Where the store refuses the default of a board option, which core puts on
//   the board as an edit of its own, past CoreApi, core raises
//   board_option_failed instead (../mount/block.ts).
// A notifier that is no function is refused at once, with a TypeError, before
// anything runs.
function refusing(store: Store, commands: Commands, refused: Refused): { store: Store; commands: Commands } {
  // Makes an edit through the store, and tells the user why the store refused
  // it, or hands that to the notifier.
  function edit(opts: NotifierOpts, call: string, fn: () => void): void {
    const notifier = givenNotifier(opts, call);
    try {
      fn();
    } catch (error) {
      refused(error, undefined, notifier);
      throw error;
    }
  }
  // Waits for a command, or a list of commands, that the command registry
  // runs, and tells the user why the command registry or the store refused it,
  // or hands that to the notifier.
  function ran(opts: NotifierOpts, run: () => Promise<void>): Promise<void> {
    let notifier: Notifier | undefined;
    try {
      notifier = givenNotifier(opts, "run()");
    } catch (error) {
      return Promise.reject(asError(error));
    }
    return run().catch((error: unknown) => {
      refused(error, undefined, notifier);
      throw error;
    });
  }
  return {
    store: {
      ...store,
      apply(delta, origin, opts) {
        edit(opts, "apply()", () => {
          store.apply(delta, origin, opts?.undo === false ? { undo: false } : undefined);
        });
      },
      undo(origin, opts) {
        edit(opts, "undo()", () => {
          store.undo(origin);
        });
      },
      redo(opts) {
        edit(opts, "redo()", () => {
          store.redo();
        });
      },
    },
    commands: {
      ...commands,
      // A list comes with its origin second, in place of the args, and its
      // options third, as the command registry takes it.
      run(
        target: string | readonly { readonly id: string; readonly args?: Json }[],
        given?: Json,
        third?: Origin | NotifierOpts,
        fourth?: NotifierOpts,
      ) {
        if (typeof target === "string") {
          const origin = typeof third === "string" ? third : undefined;
          return ran(fourth, () => commands.run(target, given, origin));
        }
        const listed = typeof given === "string" ? given : undefined;
        return ran(typeof third === "string" ? undefined : third, () => commands.run(target, listed));
      },
    },
  };
}

// The board object that a plugin gives show(), as a block or a plugin by its
// id, which it copies. It throws for anything else, as a plugin in plain JS may
// give.
function shownObject(given: unknown): ShownObject {
  if (typeof given === "object" && given !== null && !Array.isArray(given)) {
    const entries = Object.entries(given);
    const [entry] = entries;
    if (entries.length === 1 && entry !== undefined && typeof entry[1] === "string") {
      if (entry[0] === "block") return Object.freeze({ block: entry[1] });
      if (entry[0] === "plugin") return Object.freeze({ plugin: entry[1] });
    }
  }
  throw new TypeError(`Core shows a block or a plugin, as { block: "a" } or { plugin: "blockr.md-md_plugin" }, not ${JSON.stringify(given)}.`);
}

// Which blocks and plugins are on screen, as the frontend last set them, or
// null until it first does, until the board ends.
function createOnScreen(signal: AbortSignal | undefined): OnScreen {
  let seen: Seen | null = null;
  const listeners = new Set<(shown: Seen | null) => void>();
  signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true },
  );

  return {
    get: () => seen,
    set(shown) {
      const next = seenGiven(shown);
      if (seen !== null && sameIds(seen.blocks, next.blocks) && sameIds(seen.plugins, next.plugins)) return;
      seen = next;
      announceTo(listeners, seen);
    },
    subscribe(fn) {
      const wrapped = (shown: Seen | null): void => {
        fn(shown);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };
}

// The blocks and plugins that a frontend gives onScreen.set(), copied, so that
// they change only as the frontend sets them again. It throws for anything
// else, as a frontend in plain JS may give.
function seenGiven(given: unknown): Seen {
  if (typeof given === "object" && given !== null) {
    const { blocks, plugins } = given as { blocks?: unknown; plugins?: unknown };
    if (isIds(blocks) && isIds(plugins)) {
      return Object.freeze({ blocks: Object.freeze([...blocks]), plugins: Object.freeze([...plugins]) });
    }
  }
  throw new TypeError(
    `A frontend says what is on screen as { blocks: ["a"], plugins: ["blockr.md-md_plugin"] }, not ${JSON.stringify(given)}.`,
  );
}

function isIds(given: unknown): given is string[] {
  return Array.isArray(given) && given.every((id) => typeof id === "string");
}

// Whether two lists hold the same ids, in whatever order.
function sameIds(a: readonly string[], b: readonly string[]): boolean {
  const inB = new Set(b);
  return new Set(a).size === inB.size && a.every((id) => inB.has(id));
}

// Which block has focus, which starts with none as the board opens and goes
// back to none where the block leaves the board, until the board ends.
function createFocus(store: Store, signal: AbortSignal | undefined): CoreApi["focus"] {
  let focused: BlockId | null = null;
  const listeners = new Set<(block: BlockId | null) => void>();

  const offStore = store.subscribe((_change, board) => {
    if (focused === null || Object.hasOwn(board.blocks, focused)) return;
    focused = null;
    announceTo(listeners, focused);
  });
  signal?.addEventListener(
    "abort",
    () => {
      offStore();
      listeners.clear();
    },
    { once: true },
  );

  return {
    get: () => focused,
    set(block) {
      // A plugin in plain JS may give something else, which the types refuse.
      const given: unknown = block;
      if (given !== null && (typeof given !== "string" || !store.hasBoard() || !Object.hasOwn(store.board().blocks, given))) {
        throw new Error(`The board holds no block ${JSON.stringify(given)} to give focus to.`);
      }
      if (given === focused) return;
      focused = given;
      announceTo(listeners, focused);
    },
    subscribe(fn) {
      const wrapped = (block: BlockId | null): void => {
        fn(block);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };
}

// What the selection holds where nothing is selected.
const NOTHING_SELECTED: readonly BoardObject[] = Object.freeze([]);

// The board objects that the user selected, which start with none as the
// board opens and lose each one that leaves the board, until the board ends.
function createSelection(
  store: Store,
  plugins: Pick<RunningPlugins, "appPlugins">,
  signal: AbortSignal | undefined,
): CoreApi["selection"] {
  let selected = NOTHING_SELECTED;
  const listeners = new Set<(objects: readonly BoardObject[]) => void>();

  // Whether the board holds the board object, as set() counts it: a board
  // option where the board holds its value, and a plugin where the app runs
  // it. No change removes a plugin from the open board, so a selected plugin
  // stays selected until a plugin sets the selection again.
  function holds(object: BoardObject, board: Board): boolean {
    if ("block" in object) return Object.hasOwn(board.blocks, object.block);
    if ("link" in object) return Object.hasOwn(board.links, object.link);
    if ("stack" in object) return Object.hasOwn(board.stacks, object.stack);
    if ("option" in object) return Object.hasOwn(board.options, object.option);
    return Object.hasOwn(plugins.appPlugins(), object.plugin);
  }

  const offStore = store.subscribe((_change, board) => {
    const kept = selected.filter((object) => holds(object, board));
    if (kept.length === selected.length) return;
    selected = Object.freeze(kept);
    announceTo(listeners, selected);
  });
  signal?.addEventListener(
    "abort",
    () => {
      offStore();
      listeners.clear();
    },
    { once: true },
  );

  return {
    get: () => selected,
    set(objects) {
      const given = selectionGiven(objects);
      const lacking = given.find((object) => !store.hasBoard() || !holds(object, store.board()));
      if (lacking !== undefined) throw new Error(unheld(lacking));
      if (sameIds(selected.map(objectKey), given.map(objectKey))) return;
      selected = given;
      announceTo(listeners, selected);
    },
    subscribe(fn) {
      const wrapped = (objects: readonly BoardObject[]): void => {
        fn(objects);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };
}

// The board objects that a plugin gives selection.set(), copied, in the order
// given and each once, so that they change only as a plugin sets them again.
// It throws for anything else, as a plugin in plain JS may give.
function selectionGiven(given: unknown): readonly BoardObject[] {
  if (!Array.isArray(given) || !given.every(isBoardObject)) {
    throw new TypeError(`A selection is an array of board objects, as [{ block: "a" }, { link: "ab" }], not ${JSON.stringify(given)}.`);
  }
  const copies = new Map<string, BoardObject>();
  for (const object of given) {
    const copy = Object.freeze({ ...object });
    const key = objectKey(copy);
    if (!copies.has(key)) copies.set(key, copy);
  }
  return Object.freeze([...copies.values()]);
}

// The key of a board object, by which the selection tells one board object
// from another.
function objectKey(object: BoardObject): string {
  return JSON.stringify(object);
}

// Why set() refuses a board object that the board does not hold.
function unheld(object: BoardObject): string {
  if ("block" in object) return `The board holds no block ${JSON.stringify(object.block)} to select.`;
  if ("link" in object) return `The board holds no link ${JSON.stringify(object.link)} to select.`;
  if ("stack" in object) return `The board holds no stack ${JSON.stringify(object.stack)} to select.`;
  if ("option" in object) return `The board holds no board option ${JSON.stringify(object.option)} to select.`;
  return `The app runs no plugin ${JSON.stringify(object.plugin)} to select.`;
}
