// CoreApi, what core gives a front-end, an extension and a startup module, as
// Front-ends and extension points has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-a-front-end).
// Core makes one for each board that opens, on the store, the command registry
// and the scheduler that it creates for that board, and hands it to each
// startup module's initialize() and to the front-end's render() (./start.ts).
// It ends with them, as the next board opens.
//
// Beside what it hands out, it calls the onChange() of every board option that
// has one as the board opens, with the board's value or else the board
// option's default, which it resolves then whether or not anything on the page
// requires the board option, and again as a delta sets or removes the value,
// so that what onChange() does on the page follows the board that is open, as
// the theme that dark_mode switches does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#defining-a-board-option).

import type { Commands } from "../commands/index.ts";
import { createMounts } from "../mount/index.ts";
import type { Loader, Measure, OutputEvent } from "../mount/index.ts";
import type { Lock, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Option, Store } from "../store/index.ts";
import type { Board, OptionId, PluginId } from "../types/board.ts";
import type { CoreApi, OutputEvents } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual, own, report } from "../util/index.ts";
import type { BlockrOptions } from "./blockr-options.ts";
import { pluginRequest } from "./requests.ts";

export interface CoreParts {
  // The store as front-ends and extensions see it, whose open() starts core
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

export function createCore(parts: CoreParts): CoreApi {
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
  const { defaults } = mounts;

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
      value: (id) => defaults.value(id),
      inUse: () => defaults.inUse(),
      subscribe(fn) {
        const offStore = store.subscribe((change) => {
          if ("opened" in change || change.delta.options !== undefined) fn();
        });
        const offDefaults = defaults.subscribe(fn);
        return () => {
          offStore();
          offDefaults();
        };
      },
    },
    plugins: {
      register: (def) => store.plugins.register(def),
    },
    lock: parts.lock,
    outputEvents,
    request: <T = Json>(plugin: PluginId, name: string, args: Readonly<Record<string, Json>> = {}, opts = {}) =>
      pluginRequest({ store, scheduler }, plugin, name, args, opts) as Promise<T | Blob>,
    module: <T = unknown>(id: string) => loader.module(id) as Promise<T>,
    blockrOption: (name, fallback) => blockrOptions.read(name, fallback),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option),
  };

  // The value that each onChange() last heard, by the board option's id.
  const heard = new Map<OptionId, Json>();
  store.subscribe((change, board) => {
    const ids =
      "opened" in change
        ? Object.keys(store.options.registered())
        : [...Object.keys(change.delta.options?.set ?? {}), ...(change.delta.options?.unset ?? [])];
    for (const id of ids) follow(id, board);
  });

  // Calls the board option's onChange(), if it has one, with the board's value,
  // or else with the default once core has resolved it, unless a value came
  // meanwhile, whose own call came first.
  function follow(id: OptionId, board: Board): void {
    const def = own(store.options.registered(), id);
    if (def?.onChange === undefined) return;
    const held = own(board.options, id);
    if (held !== undefined) {
      call(def, held);
      return;
    }
    void defaults.resolve([id], "its onChange").then(() => {
      if (parts.signal?.aborted === true || !store.hasBoard() || Object.hasOwn(store.board().options, id)) return;
      const value = defaults.value(id);
      if (value !== undefined) call(def, value);
    });
  }

  // Calls onChange() with a value other than the one it last heard.
  function call(def: Option, value: Json): void {
    if (heard.has(def.id) && jsonEqual(heard.get(def.id), value)) return;
    heard.set(def.id, value);
    try {
      def.onChange?.(value, api);
    } catch (error) {
      report(error);
    }
  }

  return api;
}
