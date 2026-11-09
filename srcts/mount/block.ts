// Mounting a block class's UI module, with a host made for the mount, as the
// contract has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the module of the block's class, with its stylesheet, once
//    per page, and refuses one that declares another major of its JS API.
// 2. It refuses a module whose `options` leaves out a board option that a
//    request of the block's class reads, as the class's registry entry names
//    them.
// 3. It adds the board options that the module names to the board, each with
//    its definition's default where the board lacks it, and adds again each
//    that a delta removes while the module is mounted, so that the board holds
//    the value of every board option that the module reads.
// 4. It calls the module's render() with the element, a host of the block's
//    own and a signal, which it aborts once the mount ends, and runs the
//    cleanup that render() returns.
// Where the module fails to load, declares another major, leaves out such a
// board option or throws in render(), core shows an error in the element that
// names the module and its package, while the page and the block's evaluation
// carry on.
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
import type { Option, Store } from "../store/index.ts";
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
import { compile, isObject, jsonEqual, messageOf, own, packageOf, quoted, report } from "../util/index.ts";
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
  // where the board lacks it, and resolves with why any did not go onto the
  // board, as core reports it, naming `by`, what requires the board option.
  // Mounts that add a board option at once share one call of a default that
  // is a function, and core keeps nothing of a default once the board holds
  // it. The origin of what it adds is "blockr.core".
  addOptions(ids: readonly OptionId[], by: string): Promise<readonly string[]>;
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
  // opens: the answers of requests, and of each block's output its parameters
  // and its last output.
  store.subscribe((change) => {
    if ("opened" in change) return;
    for (const id of change.delta.blocks?.rm ?? []) shelves.drop(id);
  });

  // The defaults that core resolves, each only until it settles, so that the
  // mounts that need one at once share one call of a default that is a
  // function.
  const resolving = new Map<OptionId, Promise<Json>>();
  // What core has reported for the board, each once.
  const reported = new Set<string>();

  // Reports why a board option did not go onto the board, once for the board.
  function failure(message: string): readonly string[] {
    if (!replaced.aborted && !reported.has(message)) {
      reported.add(message);
      report(new Error(message));
    }
    return [message];
  }

  function defaultOf(id: OptionId, def: Option): Promise<Json> {
    const pending = resolving.get(id);
    if (pending !== undefined) return pending;
    const given = def.default;
    const started = typeof given === "function" ? Promise.resolve().then(given) : Promise.resolve(given);
    resolving.set(id, started);
    const settled = (): void => {
      if (resolving.get(id) === started) resolving.delete(id);
    };
    started.then(settled, settled);
    return started;
  }

  async function addOptions(ids: readonly OptionId[], by: string): Promise<readonly string[]> {
    const failures = await Promise.all(
      [...new Set(ids)].map(async (id): Promise<readonly string[]> => {
        if (Object.hasOwn(store.board().options, id)) return [];
        const def = own(store.options.registered(), id);
        if (def === undefined) {
          return failure(`The board option "${id}", which ${by} requires, is defined by no package loaded.`);
        }
        let value: Json;
        try {
          value = await defaultOf(id, def);
        } catch (error) {
          return failure(`The default of the board option "${id}", which ${by} requires, failed: ${messageOf(error)}`);
        }
        // Something may have set it while the default resolved, and another
        // board may have opened, which takes the board options its own mounts
        // require.
        if (replaced.aborted || Object.hasOwn(store.board().options, id)) return [];
        try {
          await commands.run("blockr.core-set_option", { id, value }, CORE);
        } catch (error) {
          return failure(
            `The default of the board option "${id}", which ${by} requires, did not go onto the board: ${messageOf(error)}`,
          );
        }
        return [];
      }),
    );
    return failures.flat();
  }

  // Keeps the board options on the board while the caller needs them: it adds
  // those that the board lacks now, and adds again each that a delta removes,
  // until stop() runs.
  function keepOptions(
    ids: readonly OptionId[],
    by: string,
  ): { readonly added: Promise<readonly string[]>; readonly stop: () => void } {
    if (ids.length === 0) return { added: Promise.resolve([]), stop: () => undefined };
    const stop = store.subscribe((change, board) => {
      if ("opened" in change) return;
      const removed = (change.delta.options?.unset ?? []).filter(
        (id) => ids.includes(id) && !Object.hasOwn(board.options, id),
      );
      if (removed.length > 0) void addOptions(removed, by);
    });
    return { added: addOptions(ids, by), stop };
  }

  // The defaults of the board options given that the board lacks, by id, which
  // core sends with a request without adding them to the board. It leaves out
  // each whose default fails or does not fit its schema, which it reports,
  // naming `by`, what requires the board option.
  async function defaultsOf(ids: readonly OptionId[], by: string): Promise<Readonly<Record<OptionId, Json>>> {
    const found = await Promise.all(
      ids.map(async (id): Promise<[OptionId, Json][]> => {
        const def = own(store.options.registered(), id);
        if (def === undefined || Object.hasOwn(store.board().options, id)) return [];
        let value: Json;
        try {
          value = await defaultOf(id, def);
        } catch (error) {
          failure(`The default of the board option "${id}", which ${by} requires, failed: ${messageOf(error)}`);
          return [];
        }
        const unfit = compile(`the board option "${id}"`, def.schema)(value);
        if (unfit.length > 0) {
          failure(`The default of the board option "${id}", which ${by} requires, ${unfit.join(" ")}`);
          return [];
        }
        return [[id, value]];
      }),
    );
    return Object.fromEntries(found.flat());
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
    // Stops keeping the module's board options on the board, once it has
    // loaded.
    let stopKeeping: (() => void) | undefined;

    const end = (): void => {
      if (ended()) return;
      offEnd();
      host.drop();
      stopKeeping?.();
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
        checkOptions(mod, entry, pkg);
      } catch (error) {
        fail(error);
        return;
      }

      const named = mod.options ?? [];
      const kept = keepOptions(named, `the UI module of block class "${entry.id}"`);
      stopKeeping = kept.stop;
      await kept.added;
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
        { store, scheduler, loader, answers, shelves, measure, replaced, addOptions, keepOptions, defaultsOf, emit },
        el,
        block,
      ),
    mountOption: (el, option) =>
      mountOption({ store, commands, scheduler, loader, renderers, replaced, keepOptions }, el, option),
    addOptions,
  };
}

// Refuses a module whose `options` leaves out a board option that a request
// of the block's class reads, as the class's registry entry names them, so
// that the gap shows on the first mount rather than as a request that reaches
// R without the value
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#which-board-options-a-board-holds).
// It throws a ModuleError that names each such request and board option.
function checkOptions(mod: BlockModule, entry: RegistryEntry, pkg: string | null): void {
  const named = new Set(mod.options ?? []);
  const gaps = Object.entries(entry.requests).flatMap(([name, request]) => {
    const left = [...new Set(request.options)].filter((id) => !named.has(id));
    if (left.length === 0) return [];
    return [`- The request "${name}" reads the board option${left.length === 1 ? "" : "s"} ${quoted(left)}.`];
  });
  if (gaps.length === 0) return;
  const what = `leaves out of its \`options\` board options that requests of block class "${entry.id}" read:`;
  throw new ModuleError(entry.module, pkg, [what, ...gaps].join("\n"));
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
