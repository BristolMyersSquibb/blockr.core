// The defaults of board options that core resolves for one opening of a
// board, which it keeps beside the board rather than in it, so that a board
// holds only the values that a script, a file or a command set, and a saved
// board holds what someone chose and no default
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#which-board-options-a-board-holds):
// 1. Where something requires a board option that the board holds no value
//    for, core resolves the board option's default once per opening: the
//    value that its definition gives, or what the function that is its
//    default resolves with.
// 2. It checks the default against the board option's schema.
// 3. Every reader gets the board's value, or else that default.
// 4. A delta that removes a value gives every reader the default again. Core
//    resolves the default then, where it has not yet and something on the
//    page requires the board option.
// Where no loaded package defines the board option, or its default fails or
// does not fit its schema, core reports it, naming the board option and what
// requires it, and what requires it goes without it.
// Core makes them anew for each board that opens, so that a board opened
// later gets its defaults as a page that starts with that board would.

import type { Option, Store } from "../store/index.ts";
import type { Board, OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { compile, frozenCopy, messageOf, own, report } from "../util/index.ts";
import { announceTo, onAbort } from "./shared.ts";

export interface Defaults {
  // The value that readers get of the board option: the board's, from the
  // board options given or else from the store's board, or else the default
  // that core resolved for the opening. It is undefined where there is
  // neither.
  value(id: OptionId, held?: Board["options"]): Json | undefined;
  // The values of the board options given, by id, as value() gives them,
  // leaving out those that have none. While core resolves the default of one,
  // as after a delta removed its value, it keeps the value that `before`
  // gives, what the reader had, so that no reader goes without it meanwhile.
  values(
    ids: readonly OptionId[],
    held?: Board["options"],
    before?: Readonly<Record<OptionId, Json>>,
  ): Readonly<Record<OptionId, Json>>;
  // Whether readers have what they get of each board option given: the board
  // holds a value, core has resolved the default, or it has none to wait for,
  // since its default failed or does not fit, or no loaded package defines
  // the board option.
  settled(ids: readonly OptionId[]): boolean;
  // Resolves the default of each board option given that the board holds no
  // value for, unless core has resolved it already for the opening. It
  // settles once each has settled, with what keeps any of them from having a
  // value, as core reports it, naming `by`, what requires the board option.
  // Core reports each such failure once for each thing that requires it.
  resolve(ids: readonly OptionId[], by: string): Promise<readonly string[]>;
  // Counts the board options given as required by `by`, something on the
  // page, until the returned function runs.
  require(ids: readonly OptionId[], by: string): () => void;
  // The board options that a loaded package defines and that something on
  // the page requires or that the board holds a value for, in the order in
  // which the packages registered them. It returns the same array until that
  // changes.
  inUse(): readonly OptionId[];
  // Calls `fn` after a default settles and after what something on the page
  // requires changes, once for the changes that come together, on a later
  // microtask. The store announces each change to the board's values.
  subscribe(fn: () => void): () => void;
}

export interface DefaultsOptions {
  readonly store: Pick<Store, "board" | "hasBoard" | "options" | "subscribe">;
  // Ends them as it aborts, as another board opens, after which they announce
  // and report nothing more.
  readonly signal?: AbortSignal;
}

// The default of a board option, as core resolves it for an opening.
interface Resolution {
  done: Promise<void>;
  settled: boolean;
  // The default, once it has resolved and fits the schema.
  value: Json | undefined;
  // Why the board option has no default, once it has settled without one.
  why: string | undefined;
}

export function createDefaults(options: DefaultsOptions): Defaults {
  const { store } = options;
  const ended = (): boolean => options.signal?.aborted === true;

  const resolutions = new Map<OptionId, Resolution>();
  // What requires each board option, by what it is, with how many of it.
  const required = new Map<OptionId, Map<string, number>>();
  const listeners = new Set<() => void>();
  // What core has reported for the opening, each once.
  const reported = new Set<string>();

  // A count of the changes to what something on the page requires, so that
  // inUse() gives a new array only after one.
  let requirements = 0;
  let memo:
    | {
        readonly held: Board["options"];
        readonly registered: object;
        readonly requirements: number;
        readonly list: readonly OptionId[];
      }
    | undefined;

  function held(): Board["options"] {
    return store.hasBoard() ? store.board().options : {};
  }

  // Announces the changes since the last announcement at once, on a later
  // microtask, so that nothing hears of them while the store announces a
  // change, during which nothing may change the board.
  let queued = false;
  function announce(): void {
    if (queued || ended()) return;
    queued = true;
    void Promise.resolve().then(() => {
      queued = false;
      if (!ended()) announceTo(listeners);
    });
  }

  function value(id: OptionId, given: Board["options"] = held()): Json | undefined {
    return Object.hasOwn(given, id) ? given[id] : resolutions.get(id)?.value;
  }

  function values(
    ids: readonly OptionId[],
    given: Board["options"] = held(),
    before: Readonly<Record<OptionId, Json>> = {},
  ): Readonly<Record<OptionId, Json>> {
    return Object.fromEntries(
      ids.flatMap((id) => {
        const found = value(id, given) ?? (settledIn(id, given) ? undefined : own(before, id));
        return found === undefined ? [] : [[id, found]];
      }),
    );
  }

  function settledIn(id: OptionId, board: Board["options"]): boolean {
    return (
      Object.hasOwn(board, id) ||
      !Object.hasOwn(store.options.registered(), id) ||
      resolutions.get(id)?.settled === true
    );
  }

  function settled(ids: readonly OptionId[]): boolean {
    const board = held();
    return ids.every((id) => settledIn(id, board));
  }

  // The resolution of the board option's default for the opening, which this
  // starts where none has started, or undefined where no loaded package
  // defines the board option.
  function resolution(id: OptionId): Resolution | undefined {
    const started = resolutions.get(id);
    if (started !== undefined) return started;
    const def = own(store.options.registered(), id);
    if (def === undefined) return undefined;
    const item: Resolution = { done: Promise.resolve(), settled: false, value: undefined, why: undefined };
    resolutions.set(id, item);
    item.done = settle(item, id, def);
    return item;
  }

  async function settle(item: Resolution, id: OptionId, def: Option): Promise<void> {
    // A default that is a function runs on a later microtask, never while the
    // store announces a change.
    await Promise.resolve();
    try {
      const given = def.default;
      const found = typeof given === "function" ? await given() : given;
      const failures = compile(`the board option "${id}"`, def.schema)(found);
      if (failures.length === 0) item.value = frozenCopy(found);
      else item.why = failures.join(" ");
    } catch (error) {
      item.why = `failed: ${messageOf(error)}`;
    }
    item.settled = true;
    announce();
  }

  function once(message: string): void {
    if (ended() || reported.has(message)) return;
    reported.add(message);
    report(new Error(message));
  }

  async function resolve(ids: readonly OptionId[], by: string): Promise<readonly string[]> {
    const failures = await Promise.all(
      [...new Set(ids)].map(async (id) => {
        if (Object.hasOwn(held(), id)) return [];
        const item = resolution(id);
        if (item === undefined) {
          const message = `The board option "${id}", which ${by} requires, is defined by no package loaded.`;
          once(message);
          return [message];
        }
        await item.done;
        // Something may have set it while the default resolved.
        if (item.why === undefined || Object.hasOwn(held(), id)) return [];
        const message = `The default of the board option "${id}", which ${by} requires, ${item.why}`;
        once(message);
        return [message];
      }),
    );
    return failures.flat();
  }

  function require(ids: readonly OptionId[], by: string): () => void {
    const named = [...new Set(ids)];
    let changed = false;
    for (const id of named) {
      const of = required.get(id) ?? new Map<string, number>();
      if (of.size === 0) changed = true;
      of.set(by, (of.get(by) ?? 0) + 1);
      required.set(id, of);
    }
    if (changed) {
      requirements++;
      announce();
    }

    let released = false;
    return () => {
      if (released) return;
      released = true;
      let gone = false;
      for (const id of named) {
        const of = required.get(id);
        const count = of?.get(by);
        if (of === undefined || count === undefined) continue;
        if (count > 1) {
          of.set(by, count - 1);
          continue;
        }
        of.delete(by);
        if (of.size === 0) {
          required.delete(id);
          gone = true;
        }
      }
      if (gone) {
        requirements++;
        announce();
      }
    };
  }

  function inUse(): readonly OptionId[] {
    const board = held();
    const registered = store.options.registered();
    if (memo?.held === board && memo.registered === registered && memo.requirements === requirements) {
      return memo.list;
    }
    const list = Object.freeze(Object.keys(registered).filter((id) => Object.hasOwn(board, id) || required.has(id)));
    memo = { held: board, registered, requirements, list };
    return list;
  }

  // A delta that removes a value gives every reader the default again, which
  // core resolves then where something on the page requires the board option.
  const offStore = store.subscribe((change, board) => {
    if ("opened" in change) return;
    for (const id of change.delta.options?.unset ?? []) {
      const by = required.get(id)?.keys().next().value;
      if (by === undefined || Object.hasOwn(board.options, id)) continue;
      void resolve([id], by);
    }
  });
  if (options.signal !== undefined) {
    onAbort(options.signal, () => {
      offStore();
      listeners.clear();
    });
  }

  return {
    value,
    values,
    settled,
    resolve,
    require,
    inUse,
    subscribe(fn) {
      if (ended()) return () => undefined;
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
  };
}
