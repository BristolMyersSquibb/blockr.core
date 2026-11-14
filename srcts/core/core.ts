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
// and which block has focus, which the frontend sets. Both are the page's,
// outside the board, and end with the board. Where an onChange() throws, core
// raises a notification that holds until its next call returns, and so do
// those that the call raises through its `notify`.
//
// Where the store or the command registry refuses an edit of the origin
// "user" that a plugin makes through CoreApi, core tells the user why, as an
// error of the code `edit_refused`, whichever plugin made the edit. The call
// still rejects or throws with the reason (refusing(), below).

import type { Commands } from "../commands/index.ts";
import { announceTo, createMounts } from "../mount/index.ts";
import type { Loader, Measure, OutputEvent } from "../mount/index.ts";
import type { Lock, OptionEntry, PluginEntry, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Origin, Store } from "../store/index.ts";
import type { BlockId, Board, OptionId, PluginClassId } from "../types/board.ts";
import type { CoreApi, OutputEvents, Raised } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { createLog, log } from "../log/index.ts";
import { asError, coreNotification, errorMessage, isPackageName, jsonEqual, own, sourceModule } from "../util/index.ts";
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
}

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

  // A host's submit() runs its command through the command registry itself,
  // not through CoreApi. So where the store refuses it, submit() only
  // rejects. The inputs of core's React SDK and its controls say below
  // themselves why the host refused what they submitted, and a module that
  // draws its own inputs says why itself.
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

  const edits = refusing(store, commands, notifications.raiseAgain);
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
      register: (def) => store.plugins.register(def),
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
    focus: createFocus(store, parts.signal),
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
// Where either refuses an edit of the origin "user", a command, a delta, an
// undo or a redo, core tells the user why through a notification of the code
// `edit_refused`, and the call still rejects or throws with the reason, as
// Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
// - The notification concerns the board, since core knows the origin of an
//   edit but not the plugin that made it, so a frontend does not show it on a
//   block's card, which the dialog that made the edit may cover.
// - Core raises it where an edit enters through CoreApi, rather than in the
//   command registry and in the store, since the command registry applies its
//   deltas through the store. So core raises each refusal once.
// - Core raises the same refusal anew each time, so that a frontend that has
//   shown it shows it again.
// - Core logs the reason at debug alone, since the call hands it to its
//   caller.
// - An edit of another origin only rejects or throws. The assistant's edit
//   tool hands each refusal to its model, which edits again
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.4.2#how-failures-reach-the-model).
//   And where the store refuses the default of a board option, which core
//   puts on the board as an edit of its own, core raises board_option_failed
//   (../mount/block.ts).
function refusing(store: Store, commands: Commands, raiseAgain: Holder["raiseAgain"]): { store: Store; commands: Commands } {
  function refused(origin: Origin | undefined, error: unknown): void {
    if ((origin ?? "user") !== "user") return;
    const why = errorMessage(error);
    log.debug(why);
    raiseAgain(coreNotification("edit_refused", "error", why));
  }
  // Makes an edit through the store, and tells the user why the store refused
  // it.
  function edit(origin: Origin | undefined, fn: () => void): void {
    try {
      fn();
    } catch (error) {
      refused(origin, error);
      throw error;
    }
  }
  return {
    store: {
      ...store,
      apply(delta, origin, opts) {
        edit(origin, () => {
          store.apply(delta, origin, opts);
        });
      },
      undo(origin) {
        edit(origin, () => {
          store.undo(origin);
        });
      },
      redo(origin) {
        edit(origin, () => {
          store.redo(origin);
        });
      },
    },
    commands: {
      ...commands,
      run: (id, args, origin) =>
        commands.run(id, args, origin).catch((error: unknown) => {
          refused(origin, error);
          throw error;
        }),
    },
  };
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
