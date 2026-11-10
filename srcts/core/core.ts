// CoreApi, what core gives a frontend, a plugin and a startup module, as
// Drawing the board has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-plugins).
// Core makes one for each board that opens, on the store, the command registry
// and the scheduler that it creates for that board, and hands it to each
// startup module's initialize(), to the render() of the frontend and of every
// plugin's UI, and to every plugin class's start (./start.ts, ./plugins.ts).
// It ends with them, as the next board opens.
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
// the notes that plugins send, which the frontend draws, and which block has
// focus, which the frontend sets. Both are the page's, outside the board, and
// end with the board.

import type { Commands } from "../commands/index.ts";
import { announceTo, createMounts } from "../mount/index.ts";
import type { Loader, Measure, OutputEvent } from "../mount/index.ts";
import type { Lock, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, Board, OptionId, PluginId } from "../types/board.ts";
import type { CoreApi, Note, OutputEvents } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { isObject, jsonEqual, own, report } from "../util/index.ts";
import type { BlockrOptions } from "./blockr-options.ts";
import { runPlugins } from "./plugins.ts";
import type { RunningPlugins } from "./plugins.ts";
import { pluginRequest } from "./requests.ts";

export interface CoreParts {
  // The store as frontends and plugins see it, whose open() starts core
  // again for the board it opens (./start.ts).
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
  // Measures an output's element, through the DOM unless given.
  readonly measure?: Measure;
  // Ends what CoreApi runs as it aborts, as another board opens: every mount,
  // and what core keeps of outputs and answers, while the outputs' events
  // reach nobody more.
  readonly signal?: AbortSignal;
}

// What core makes for one board: CoreApi, and what draws and starts the
// board's plugins as the board opens, which core calls (./start.ts).
export interface Core {
  readonly api: CoreApi;
  readonly plugins: Pick<RunningPlugins, "frontend" | "start">;
}

export function createCore(parts: CoreParts): Core {
  const { store, commands, scheduler, loader, blockrOptions } = parts;

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

  const mounts = createMounts({
    store,
    commands,
    scheduler,
    loader,
    renderers: parts.renderers,
    ...(parts.measure === undefined ? {} : { measure: parts.measure }),
    ...(parts.signal === undefined ? {} : { signal: parts.signal }),
    emit(event) {
      for (const fn of [...listeners]) {
        try {
          fn(event);
        } catch (error) {
          report(error);
        }
      }
    },
  });

  // It subscribes to the store before core's own subscription below, so that
  // the board options of the plugins' classes go onto the board in their
  // order as it opens, before the board options that have an onChange().
  const plugins = runPlugins({
    store,
    loader,
    keepOptions: (ids, by, failed) => mounts.keepOptions(ids, by, failed),
    core: () => api,
    ...(parts.signal === undefined ? {} : { signal: parts.signal }),
  });

  const api: CoreApi = {
    store,
    commands,
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
      running: () => plugins.running(),
      classOf: (plugin) => plugins.classOf(plugin),
      storage: () => plugins.storage(),
    },
    lock: parts.lock,
    outputEvents,
    notifications: createNotifications(parts.signal),
    focus: createFocus(store, parts.signal),
    request: <T = Json>(plugin: PluginId, name: string, args: Readonly<Record<string, Json>> = {}, opts = {}) =>
      pluginRequest({ store, scheduler }, plugin, name, args, opts) as Promise<T | Blob>,
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

  function changed(id: OptionId, value: Json): void {
    const def = own(store.options.registered(), id);
    if (def?.onChange === undefined) return;
    try {
      def.onChange(value, api);
    } catch (error) {
      report(error);
    }
  }

  return { api, plugins };
}

const SEVERITIES: readonly unknown[] = ["message", "warning", "error"];

// The notes that plugins and core send, which reach each subscriber, as the
// frontend that draws them, until the board ends.
function createNotifications(signal: AbortSignal | undefined): CoreApi["notifications"] {
  const listeners = new Set<(note: Note) => void>();
  signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true },
  );
  return {
    notify(note) {
      // A plugin in plain JS may send something else, which the types refuse.
      const given: unknown = note;
      if (!isObject(given) || typeof given.message !== "string" || !SEVERITIES.includes(given.severity)) {
        throw new TypeError('A note holds a message, as a string, and a severity, "message", "warning" or "error".');
      }
      const sent: Note = Object.freeze({ message: given.message, severity: given.severity as Note["severity"] });
      announceTo(listeners, sent);
    },
    subscribe(fn) {
      const wrapped = (note: Note): void => {
        fn(note);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
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
