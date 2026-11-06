// CoreApi, what core gives a front-end, an extension and a startup module, as
// Front-ends and extension points has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-core-gives-a-front-end).
// Core makes one for a page, on the store, the command registry and the
// scheduler that it creates there, and hands it to each startup module's
// initialize() and to the front-end's render() (./start.ts).
//
// Beside what it hands out, it calls the onChange() of each board option whose
// value changes, as the board opens or a delta sets it, so that a new value of
// dark_mode switches the page's theme.

import type { Commands } from "../commands/index.ts";
import { createMounts } from "../mount/index.ts";
import type { Loader, Measure, OutputEvent } from "../mount/index.ts";
import type { Lock, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { Board, OptionId } from "../types/board.ts";
import type { CoreApi, OutputEvents } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual, own, report } from "../util/index.ts";
import type { BlockrOptions } from "./blockr-options.ts";
import { addedRequest } from "./requests.ts";

export interface CoreParts {
  // The store as front-ends and extensions see it, whose open() switches the
  // page to the front-end of the board it opens (./start.ts).
  readonly store: Store;
  readonly commands: Commands;
  readonly scheduler: Scheduler;
  readonly loader: Loader;
  readonly blockrOptions: BlockrOptions;
  // The blocks that a locked board leaves open, from R's reply to `hello`.
  readonly lock: Lock | null;
  // The renderers that the app names for board options, from R's latest reply
  // to `hello`.
  readonly renderers: () => Readonly<Record<string, Renderer>>;
  // Measures an output's element, through the DOM unless given.
  readonly measure?: Measure;
}

export function createCore(parts: CoreParts): CoreApi {
  const { store, commands, scheduler, loader, blockrOptions } = parts;

  const listeners = new Set<(event: OutputEvent) => void>();
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
    options: store.options,
    lock: parts.lock,
    outputEvents,
    request: <T = Json>(type: string, fields: Record<string, Json> = {}, opts = {}) =>
      addedRequest({ scheduler }, type, fields, opts) as Promise<T | Blob>,
    module: <T = unknown>(id: string) => loader.module(id) as Promise<T>,
    blockrOption: (name, fallback) => blockrOptions.read(name, fallback),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option),
  };

  // The values of the board options as each onChange() last heard them.
  let heard: Board["options"] = {};
  store.subscribe((change, board) => {
    const before = "opened" in change ? {} : heard;
    if (board.options === heard && !("opened" in change)) return;
    heard = board.options;
    for (const [id, value] of Object.entries(board.options)) {
      if (!jsonEqual(own(before, id), value)) changed(id, value);
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

  return api;
}
