// Mounting a block class's UI module, with a host made for the mount, as the
// contract has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the module of the block's class, with its stylesheet, once
//    per page, and refuses one that declares another major of its JS API.
// 2. It adds the board options that the module names to the board, each with
//    its definition's default where the board lacks it.
// 3. It calls the module's render() with the element, a host of the block's
//    own and a signal, which it aborts once the mount ends, and runs the
//    cleanup that render() returns.
// Where the module fails to load, declares another major or throws in
// render(), core shows an error in the element that names the module and its
// package, while the page and the block's evaluation carry on.
//
// A host keeps nothing of its own: the state is the store's, the status the
// scheduler's and the answers core's, so one block mounts in several places
// at once, each with a host of its own. A host is bound to the board it was
// made on and to its block. Once another board opens or the block leaves the
// board, core ends the mount, and the host's submit() and evaluate() reject.
// Core makes the mounts anew for each board that opens, with a signal that
// aborts as the next board opens, which ends every mount.

import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import { unevaluated } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { Block, BlockId, OptionId } from "../types/board.ts";
import type {
  BlockContext,
  BlockHost,
  BlockModule,
  HostRequestOpts,
  State,
} from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { isObject, jsonEqual, messageOf, own, packageOf, report } from "../util/index.ts";
import { createAnswers } from "./answers.ts";
import type { Answers } from "./answers.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import { domMeasure } from "./measure.ts";
import type { Measure } from "./measure.ts";
import { mountOption } from "./option.ts";
import { createShelves, mountOutput } from "./output.ts";
import type { OutputEvent } from "./output.ts";
import { blockRequest } from "./requests.ts";
import { attempt, moduleOf, onAbort, REPLACED, showError } from "./shared.ts";

export interface Mounts {
  // Mounts the UI module of the block's class in the element until the
  // returned cleanup runs. It throws where the board holds no such block.
  mountBlock(el: HTMLElement, block: BlockId): () => void;
  // Mounts the block's output, measured, asked of R and drawn by the renderer
  // that R names in it, until the returned cleanup runs (./output.ts). It
  // throws where the board holds no such block.
  mountOutput(el: HTMLElement, block: BlockId): () => void;
  // Mounts a board option's UI, or the renderer that the app names for it,
  // until the returned cleanup runs (./option.ts). It throws where no package
  // registers such a board option.
  mountOption(el: HTMLElement, option: OptionId): () => void;
  // Adds the board options to the board, each with its definition's default
  // where the board lacks it. Core calls a default that is a function once per
  // opening of a board and keeps its result. The origin of what it adds is
  // "blockr.core", so that the user's undo leaves it.
  addOptions(ids: readonly OptionId[], by: string): Promise<void>;
}

export interface MountsOptions {
  readonly store: Store;
  readonly commands: Pick<Commands, "run">;
  readonly scheduler: Pick<Scheduler, "evaluate" | "inputs" | "request" | "status" | "subscribe">;
  readonly loader: Loader;
  // The renderers that the app names for board options, by the renderer's id,
  // as R's reply to `hello` gives them. There are none unless given.
  readonly renderers?: () => Readonly<Record<string, Renderer>>;
  // Hears each event that an output's renderer emits.
  readonly emit?: (event: OutputEvent) => void;
  // Measures an output's element, through the DOM unless given.
  readonly measure?: Measure;
  // Ends every mount as it aborts, as another board opens.
  readonly signal?: AbortSignal;
}

// The origin of what core changes on the board itself, such as the board
// options that a module requires.
const CORE = "blockr.core";

// The status of a block that the scheduler has not seen yet.
const NO_STATUS = Object.freeze(unevaluated(0, false));

export function createMounts(options: MountsOptions): Mounts {
  const { store, commands, scheduler, loader } = options;
  // Aborts as another board opens in place of this one.
  const replaced = options.signal ?? new AbortController().signal;
  const answers = createAnswers(scheduler, replaced);
  const shelves = createShelves();
  const measure = options.measure ?? domMeasure();
  const emit = options.emit ?? ((): void => undefined);
  const renderers = options.renderers ?? ((): Readonly<Record<string, Renderer>> => ({}));

  // What core keeps of the board, which goes with the mounts as another board
  // opens: the answers of requests, the defaults of board options, and of each
  // block's output its parameters and its last output.
  const defaults = new Map<OptionId, Promise<Json>>();
  store.subscribe((change) => {
    if ("opened" in change) return;
    for (const id of change.delta.blocks?.rm ?? []) shelves.drop(id);
  });

  function defaultOf(id: OptionId, def: { default: Json | (() => Promise<Json>) }): Promise<Json> {
    let value = defaults.get(id);
    if (value === undefined) {
      const given = def.default;
      value = typeof given === "function" ? Promise.resolve().then(given) : Promise.resolve(given);
      defaults.set(id, value);
    }
    return value;
  }

  async function addOptions(ids: readonly OptionId[], by: string): Promise<void> {
    await Promise.all(
      ids.map(async (id) => {
        if (Object.hasOwn(store.board().options, id)) return;
        const def = own(store.options.registered(), id);
        if (def === undefined) {
          report(new Error(`The board option "${id}", which ${by} requires, is defined by no package loaded.`));
          return;
        }
        let value: Json;
        try {
          value = await defaultOf(id, def);
        } catch (error) {
          report(new Error(`The default of the board option "${id}" failed: ${messageOf(error)}`, { cause: error }));
          return;
        }
        // Something may have set it while the default resolved, and another
        // board may have opened, which takes the board options its own mounts
        // require.
        if (replaced.aborted || Object.hasOwn(store.board().options, id)) return;
        try {
          await commands.run("blockr.core-set_option", { id, value }, CORE);
        } catch (error) {
          report(error);
        }
      }),
    );
  }

  function mountBlock(el: HTMLElement, block: BlockId): () => void {
    if (replaced.aborted) throw new Error(REPLACED);
    const found = own(store.board().blocks, block);
    if (found === undefined) {
      throw new Error(`The board holds no block "${block}" to mount.`);
    }
    const entry = own(store.blockRegistry(), found.registry_id);
    if (entry === undefined) {
      throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
    }
    const pkg = packageOf(entry.id);

    // The mount ends as its signal aborts, which may happen whenever the
    // loading below awaits.
    const controller = new AbortController();
    const { signal } = controller;
    const ended = (): boolean => signal.aborted;
    let cleanup: (() => void) | undefined;

    const end = (): void => {
      if (ended()) return;
      offEnd();
      host.drop();
      controller.abort();
      // The cleanup runs where render() has returned it, and runs at once
      // where render() resolves with it later.
      if (cleanup !== undefined) attempt(cleanup);
      el.replaceChildren();
    };

    const host = blockHost({
      store,
      commands,
      scheduler,
      loader,
      answers,
      block,
      entry,
      replaced,
      unbound: end,
    });
    const offEnd = onAbort(replaced, end);

    void (async () => {
      let exports: unknown;
      try {
        exports = await loader.load({ module: entry.module, ...(entry.css === undefined ? {} : { css: entry.css }) }, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      if (ended()) return;

      let mod: BlockModule;
      try {
        mod = moduleOf(exports, entry.module, pkg);
      } catch (error) {
        fail(error);
        return;
      }

      const named = mod.options ?? [];
      await addOptions(named, `the UI module of block class "${entry.id}"`);
      if (ended()) return;
      host.name(named);

      try {
        const rendered = await mod.render({ el, host, signal });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new ModuleError(entry.module, pkg, `failed in render(): ${messageOf(error)}`, { cause: error }));
      }
    })();

    function fail(error: unknown): void {
      if (ended()) return;
      showError(el, messageOf(error));
      report(error);
    }

    return end;
  }

  return {
    mountBlock,
    mountOutput: (el, block) =>
      mountOutput(
        { store, scheduler, loader, answers, shelves, measure, replaced, addOptions, emit },
        el,
        block,
      ),
    mountOption: (el, option) =>
      mountOption(
        { store, commands, scheduler, loader, renderers, replaced, addOptions, defaultOf },
        el,
        option,
      ),
    addOptions,
  };
}

interface HostDeps {
  readonly store: Store;
  readonly commands: Pick<Commands, "run">;
  readonly scheduler: Pick<Scheduler, "evaluate" | "inputs" | "status" | "subscribe">;
  readonly loader: Loader;
  readonly answers: Answers;
  readonly block: BlockId;
  readonly entry: RegistryEntry;
  // Aborts as another board opens in place of the host's.
  readonly replaced: AbortSignal;
  // What ends the mount once the host is no longer bound to its board and
  // its block.
  readonly unbound: () => void;
}

// A block's host, with what only its mount calls: name() takes the board
// options that the module names, and drop() stops it following the board.
interface MountedHost extends BlockHost {
  name(options: readonly OptionId[]): void;
  drop(): void;
}

function blockHost(deps: HostDeps): MountedHost {
  const { store, commands, scheduler, block, entry } = deps;

  // The board as the host last heard it from the store.
  let seen = store.board();
  let named: readonly OptionId[] = [];
  let state = blockOf(seen)?.state ?? {};
  let context = contextOf(blockOf(seen));
  // Whether the host follows the board, and whether it saw its block leave,
  // after which it changes nothing.
  let following = true;
  let gone = false;
  const listeners = { state: new Set<() => void>(), context: new Set<() => void>() };

  function blockOf(board: typeof seen): Block | undefined {
    return own(board.blocks, block);
  }

  function contextOf(now: Block | undefined): BlockContext {
    const recipe = scheduler.inputs(block)?.recipe ?? [];
    return Object.freeze({
      name: now?.name ?? "",
      status: scheduler.status(block) ?? NO_STATUS,
      inputs: Object.freeze(
        recipe.flatMap((input) =>
          input.block === undefined
            ? []
            : [Object.freeze({ input: input.input, revision: scheduler.status(input.block)?.revision ?? 0 })],
        ),
      ),
      options: Object.freeze(
        Object.fromEntries(
          named.flatMap((id) => {
            const value = own(seen.options, id);
            return value === undefined ? [] : [[id, value]];
          }),
        ),
      ),
    });
  }

  // Whether the host is still bound to the board it was made on and to its
  // block, so that an edit never lands on a board opened in the meantime. A
  // host that its mount dropped is bound while the board holds its block, so
  // that a module can submit an edit still pending as it unmounts.
  function bound(): boolean {
    return !gone && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }

  function announce(event: keyof typeof listeners): void {
    for (const fn of [...listeners[event]]) {
      try {
        fn();
      } catch (error) {
        report(error);
      }
    }
  }

  // Takes the board as it is, and announces what changed of the state and the
  // context, the state first.
  function refresh(): void {
    const now = blockOf(seen);
    if (now === undefined) {
      unbind();
      return;
    }
    const stateChanged = now.state !== state;
    if (stateChanged) state = now.state;
    const next = contextOf(now);
    const contextChanged = !sameContext(context, next);
    if (contextChanged) context = next;
    if (stateChanged) announce("state");
    if (contextChanged) announce("context");
  }

  function unbind(): void {
    if (gone) return;
    gone = true;
    stop();
    deps.unbound();
  }

  const offStore = store.subscribe((_change, board) => {
    seen = board;
    refresh();
  });
  // Within a change to the board, the scheduler hears of it before the host
  // does, and the host then waits for the store to tell it, so that it reads
  // the change as the store announces it, the state before the context.
  const offScheduler = scheduler.subscribe(() => {
    if (following && store.board() === seen) refresh();
  });

  function stop(): void {
    following = false;
    offStore();
    offScheduler();
  }

  const unboundError = (): Error =>
    new Error(
      deps.replaced.aborted
        ? `Another board opened, so the host of block "${block}" no longer changes it.`
        : `Block "${block}" left the board, so its host no longer changes it.`,
    );

  return {
    block,
    class: entry.id,
    state: () => state,
    context: () => context,
    on(event, fn) {
      // A module in plain JS may name another event, which the types refuse.
      const heard = own(listeners, event);
      if (heard === undefined) {
        throw new Error(`A block's host announces "state" and "context", not ${JSON.stringify(event)}.`);
      }
      const wrapped = (): void => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    submit(change) {
      if (!bound()) return Promise.reject(unboundError());
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const set = Object.fromEntries(Object.entries(change).filter(([, value]) => value !== undefined));
      return commands.run("blockr.core-set_block", { id: block, state: set as State });
    },
    request<T = Json>(
      name: string,
      args: Readonly<Record<string, Json | undefined>> = {},
      opts: HostRequestOpts = {},
    ): Promise<T | Blob> {
      if (!bound()) return Promise.reject(unboundError());
      return blockRequest({ ...deps, bound, unboundError }, block, entry, name, args, opts) as Promise<T | Blob>;
    },
    async evaluate(params) {
      if (!bound()) throw unboundError();
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      const statuses = await scheduler.evaluate([block], params);
      const status = own(statuses, block);
      if (status === undefined) throw unboundError();
      return status;
    },
    module<T = unknown>(id: string): Promise<T> {
      return deps.loader.module(id) as Promise<T>;
    },
    name(options) {
      named = options;
      context = contextOf(blockOf(seen));
    },
    drop: stop,
  };
}

function sameContext(a: BlockContext, b: BlockContext): boolean {
  return (
    a.name === b.name &&
    a.status === b.status &&
    a.inputs.length === b.inputs.length &&
    a.inputs.every((input, i) => input.input === b.inputs[i]?.input && input.revision === b.inputs[i].revision) &&
    jsonEqual(a.options, b.options)
  );
}
