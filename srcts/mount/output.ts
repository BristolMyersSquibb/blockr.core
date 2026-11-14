// Mounting a block's output: core measures the element, asks R for the output
// and draws it with the renderer that R names in it, through an OutputHost made
// for the mount, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the renderer that the block class's entry in the block registry
//    names, and adds the board options that the renderer names to the board,
//    each with its definition's default where the board lacks it. So the board
//    holds what the output's coercion reads before core asks for an output.
//    While the renderer draws the output, core adds again each of them that a
//    delta removes. Where one does not go onto the board, as where its default
//    fails, core asks R for nothing, and raises that the renderer cannot draw
//    without it, until an output comes, as it raises the failure of a renderer
//    that cannot draw. The host's `error()` gives why meanwhile.
// 2. Once the block reads ready and the element measures more than 0 by 0, core
//    asks R for the output, through the scheduler, with the block's revision,
//    the values of the board options that the renderer names, the info it
//    measured and the block's parameters, unless it keeps an output asked with
//    the same ones. Where neither the entry nor an output names a renderer,
//    core asks with the value of every board option that a loaded package
//    defines: the board's, or else the board option's default, which it sends
//    without adding it to the board.
// 3. It mounts the renderer that the output names once it has the output, and
//    hands it each output that comes after. An output that names another
//    renderer than the one drawing, as one of a subclass may, is drawn by that
//    one, with what it names added to the board.
// 4. It asks again when the revision, one of those board options, the info or
//    the parameters change, while the block reads ready. Meanwhile it keeps
//    drawing the last output, whatever the block's status, with `loading()`
//    while it asks and `error()` where the latest ask failed.
//
// Core keeps two things of each block's output on the block's shelf: its
// parameters, which every mount of the output shares, and the last output that
// R made, with what it was asked with, which a mount draws without asking R
// where it would ask with the same. The shelves go with the mounts, which core
// makes anew for each board that opens.
// Core raises a notification for each condition of R's reply to an ask of the
// block's output, ok or error, which concerns the block, in place of those
// before. Where an ask fails without an error from R, as where it gets no
// reply, core raises one of its own with the condition that the host's
// `error()` gives. A frontend chooses which of them to show: a card shows those
// whose severity `show_conditions` names. A renderer that cannot draw, as one
// that fails to load or throws in render(), raises one that concerns the
// block, with the renderer and its package, which holds until the next board
// opens or the block leaves the board, and leaves the element empty. Core
// draws nothing in the element itself, so that the frontend alone says where
// a failure shows. What a renderer raises through its host's notify() comes
// from the block, with the renderer and its package, and holds until the
// renderer is unmounted at the latest. Each condition of R's reply to a
// request of the output, ok or error, raises a notification that concerns the
// block, which holds until R next replies to a request of the same name and
// scope.
// A mount is bound to the board it was made on and to its block. Once another
// board opens or the block leaves the board, core ends the mount.

import type { Store } from "../store/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Condition, OutputInfo, OutputReply, Reply } from "../protocol/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { OutputHost, OutputRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputRenderer } from "../types/outputs.ts";
import {
  asError,
  checkRaised,
  coreNotification,
  errorMessage,
  frozenCopy,
  isObject,
  jsonEqual,
  own,
  packageFromId,
} from "../util/index.ts";
import { createLog, log } from "../log/index.ts";
import { keepContext } from "../trace/index.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";
import type { Answers, OutputAsk } from "./answers.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import { RequestError } from "./requests.ts";
import { announceTo, attempt, mountableModule, onAbort, REPLACED, withoutOptions } from "./shared.ts";
import type { Mountable } from "./shared.ts";

// How long core waits for an element's size to settle before it asks R for an
// output of the new size, so that dragging a panel's edge asks R once.
const SETTLE_MS = 150;

// How many times a request of an output asks R before it gives up on a result
// that R keeps lacking, which only a fault in R causes.
const ATTEMPTS = 3;

// What a renderer reports that the user did, by the block whose output it
// draws.
export interface OutputEvent {
  readonly block: BlockId;
  readonly name: string;
  readonly payload: Json;
}

// What an output was asked with.
interface Asked {
  readonly revision: number;
  readonly options: Readonly<Record<OptionId, Json>>;
  readonly info: OutputInfo;
  readonly params: Readonly<Record<string, Json>>;
}

// An output that R made, with what it was asked with.
interface Made {
  readonly asked: Asked;
  readonly output: OutputReply["output"];
  readonly body: Blob | null;
}

// An ask on its way to R, with how many mounts wait on it.
interface Flight {
  readonly done: Promise<{ reply: Reply; body: Blob | null }>;
  readonly controller: AbortController;
  callers: number;
}

// What a getter returns where it holds nothing, the same object every time, as
// React's useSyncExternalStore needs.
const EMPTY: Readonly<Record<string, Json>> = Object.freeze({});

// What core keeps of a block's output.
interface Shelf {
  params: Readonly<Record<string, Json>>;
  last: Made | undefined;
  // What withdraws each notification of the latest ask of the output: one for
  // each condition of R's reply, and one of core's where the ask failed
  // without an error from R.
  notified: readonly (() => void)[];
  // The mounts of the block's output, which ask again as its parameters change.
  readonly mounts: Set<() => void>;
  readonly flights: Map<string, Flight>;
}

export interface Shelves {
  of(block: BlockId): Shelf;
  // Drops the shelf of a block that left the board.
  drop(block: BlockId): void;
}

export function createShelves(): Shelves {
  const shelves = new Map<BlockId, Shelf>();
  const giveUp = (shelf: Shelf): void => {
    for (const flight of shelf.flights.values()) flight.controller.abort();
    shelf.flights.clear();
  };
  return {
    of(block) {
      let shelf = shelves.get(block);
      if (shelf === undefined) {
        shelf = { params: {}, last: undefined, notified: [], mounts: new Set(), flights: new Map() };
        shelves.set(block, shelf);
      }
      return shelf;
    },
    drop(block) {
      const shelf = shelves.get(block);
      if (shelf === undefined) return;
      giveUp(shelf);
      shelves.delete(block);
    },
  };
}

export interface OutputDeps {
  readonly store: Pick<Store, "board" | "blockRegistry" | "options" | "subscribe">;
  readonly scheduler: Pick<Scheduler, "request" | "status" | "subscribe">;
  readonly loader: Loader;
  readonly answers: Answers;
  readonly shelves: Shelves;
  readonly measure: Measure;
  // Aborts as another board opens, which ends the mount.
  readonly replaced: AbortSignal;
  readonly addOptions: (ids: readonly OptionId[], by: string) => Promise<readonly string[]>;
  // Keeps the board options on the board until stop() runs, and calls `failed`
  // where one that a delta removed does not go onto the board again
  // (./block.ts).
  readonly keepOptions: (
    ids: readonly OptionId[],
    by: string,
    failed: (failures: readonly string[]) => void,
  ) => { readonly stop: () => void };
  // The defaults of the board options that the board lacks, which core sends
  // without adding them to the board (./block.ts).
  readonly resolveDefaults: (ids: readonly OptionId[], by: string) => Promise<Readonly<Record<OptionId, Json>>>;
  readonly emit: (event: OutputEvent) => void;
  // Raises a notification of the board (../core/notifications.ts).
  readonly raise: Raise;
  // Holds a notification for each condition of R's latest reply to each of
  // the output's requests.
  readonly replied: Replied;
  // Raises the notification that a renderer gives through its host.
  readonly notify: Notify;
}

interface OutputArgs {
  el: HTMLElement;
  host: OutputHost;
  signal: AbortSignal;
}

// A renderer loaded and checked, with the board options it names, which are
// on the board by then.
interface Prepared {
  readonly renderer: OutputRenderer;
  readonly mod: Mountable<OutputArgs>;
  readonly named: readonly OptionId[];
}

// A renderer that core mounted, with the host it was mounted with.
interface Drawing {
  readonly prepared: Prepared;
  readonly host: Announcing;
  readonly controller: AbortController;
  cleanup: (() => void) | undefined;
}

// A host, with what only its mount calls: announce() tells its renderer of a
// change, and drop() stops it.
interface Announcing extends OutputHost {
  announce(): void;
  drop(): void;
}

// The condition of an output that R could not make, which core writes itself
// where R gives none.
function noOutput(message: string): Condition {
  return { severity: "error", class: ["blockr_error", "error", "condition"], phase: "output", message };
}

// Why R could not make the output of the block, from R's error reply: the
// reply's condition of the severity error. Where the coercion failed, that is
// the error that stopped it, and the reply holds it after the messages and
// warnings that the coercion raised before. Where the reply holds no error,
// core writes its own condition.
function whyFailed(reply: Reply, block: BlockId): { readonly condition: Condition; readonly by: "r" | "core" } {
  const conditions = "conditions" in reply ? reply.conditions : [];
  const error = conditions.find((condition) => condition.severity === "error");
  if (error !== undefined) return { condition: error, by: "r" };
  return { condition: noOutput(`R failed to make the output of block "${block}".`), by: "core" };
}

// Mounts the output of the block in the element until the returned cleanup
// runs. It throws where the board holds no such block.
export function mountOutput(deps: OutputDeps, el: HTMLElement, block: BlockId): () => void {
  const { store, scheduler, loader, measure } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const found = own(store.board().blocks, block);
  if (found === undefined) {
    throw new Error(`The board holds no block "${block}" whose output to mount.`);
  }
  const entry = own(store.blockRegistry(), found.registry_id);
  if (entry === undefined) {
    throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
  }

  const shelf = deps.shelves.of(block);
  const controller = new AbortController();
  const ended = (): boolean => controller.signal.aborted;

  // The renderer that draws the block's output: the one that the block
  // class's entry names until an output names one, and the one that the last
  // output named after. The mount asks with the board options it names once it
  // is loaded, and keeps them on the board meanwhile. Where neither names one,
  // the mount asks with every board option that a loaded package defines, and
  // learns the renderer from the first output.
  let renderer: OutputRenderer | undefined = entry.renderer;
  let ready: Prepared | undefined;
  let keeping: { readonly stop: () => void } | undefined;
  const preparing = new Map<string, Promise<Prepared>>();
  // The defaults that the mount sends where it knows no renderer, of the board
  // options that the board lacks, once core has them, which the board does not
  // take.
  let unwritten: Readonly<Record<OptionId, Json>> | undefined;
  let fetching = false;

  let info = measure.info(el);
  let held: Made | undefined;
  let asking: { readonly key: string; leave(): void } | undefined;
  let error: Condition | null = null;
  // What withdraws the notification that the renderer cannot draw without the
  // board options that it names, while it holds.
  let unfit: (() => void) | undefined;
  let drawing: Drawing | undefined;
  let options = store.board().options;

  // Requests of the output that wait for the output of the current revision.
  const waiting = new Set<() => void>();
  // The renderers that failed to load, each reported once, however many
  // outputs name it.
  const broken = new WeakSet();

  function bound(): boolean {
    return !ended() && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }

  const unboundError = (): Error =>
    new Error(
      deps.replaced.aborted
        ? `Another board opened, so the host of the output of block "${block}" no longer asks R.`
        : `Block "${block}" left the board, so the host of its output no longer asks R.`,
    );

  // Loads a renderer and adds the board options that it names to the board.
  function prepare(given: OutputRenderer): Promise<Prepared> {
    let loading = preparing.get(given.id);
    if (loading === undefined) {
      loading = (async () => {
        const pkg = packageFromId(given.id);
        const source = given.css === undefined ? { module: given.module } : { module: given.module, css: given.css };
        const mod = mountableModule<OutputArgs>(await loader.load(source, pkg), given.module, pkg);
        const named = mod.options ?? [];
        // Core has reported why one did not go onto the board.
        const failures = await deps.addOptions(named, `the renderer "${given.id}"`);
        if (failures.length > 0) throw missingOptions(given, named);
        return { renderer: given, mod, named };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }

  // Loads the renderer that the mount asks for, then asks.
  function expect(given: OutputRenderer): void {
    renderer = given;
    unwritten = undefined;
    if (ready?.renderer.id === given.id) return;
    prepare(given).then(
      (prepared) => {
        if (ended() || renderer?.id !== prepared.renderer.id) return;
        ready = prepared;
        keeping?.stop();
        keeping = deps.keepOptions(prepared.named, `the renderer "${prepared.renderer.id}"`, () => {
          fail(missingOptions(prepared.renderer, prepared.named), prepared.renderer);
        });
        sync();
      },
      (failure: unknown) => {
        fail(failure, given);
      },
    );
  }

  // Why the renderer cannot draw without the board options that it names and
  // that the board lacks.
  function missingOptions(given: OutputRenderer, ids: readonly OptionId[]): Error {
    const missing = ids.filter((id) => !Object.hasOwn(store.board().options, id));
    return new Error(withoutOptions(`the renderer "${given.id}"`, missing));
  }

  // The board options that the mount asks with: those that the renderer
  // names, or every one where it knows no renderer, or undefined while the
  // renderer loads.
  function named(): readonly OptionId[] | null | undefined {
    if (renderer === undefined) return null;
    return ready?.renderer.id === renderer.id ? ready.named : undefined;
  }

  // The values that the board holds now of the board options named, or of
  // every one, with the defaults of those that it lacks. Within a change to
  // the board, the scheduler may announce a new status before the store has
  // told the mount of the change, so the mount reads the board from the store
  // rather than from what it last heard.
  function values(ids: readonly OptionId[] | null): Readonly<Record<OptionId, Json>> {
    const options = store.board().options;
    if (ids === null) return { ...unwritten, ...options };
    return Object.fromEntries(
      ids.flatMap((id) => {
        const value = own(options, id);
        return value === undefined ? [] : [[id, value]];
      }),
    );
  }

  // What the mount would ask R for now, or undefined where it asks for
  // nothing: while the element measures nothing, the renderer loads, the
  // board lacks a board option that the renderer names, as after a delta
  // removed it, the block reads anything but ready, or, where the mount knows
  // no renderer, core resolves the defaults that it sends.
  function wanted(): { asked: Asked; ids: readonly OptionId[] | null } | undefined {
    if (info === null) return undefined;
    const ids = named();
    if (ids === undefined) return undefined;
    if (ids?.some((id) => !Object.hasOwn(store.board().options, id)) === true) return undefined;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return undefined;
    if (ids === null && unwritten === undefined) {
      fetchDefaults();
      return undefined;
    }
    return { asked: { revision: status.revision, options: values(ids), info, params: shelf.params }, ids };
  }

  // Resolves the defaults of the board options that the board lacks, which
  // the mount sends where it knows no renderer, then asks.
  function fetchDefaults(): void {
    if (fetching) return;
    fetching = true;
    const lacking = Object.keys(store.options.registered()).filter((id) => !Object.hasOwn(store.board().options, id));
    void deps.resolveDefaults(lacking, `the first output of block "${block}"`).then((found) => {
      fetching = false;
      if (ended() || renderer !== undefined) return;
      unwritten = found;
      sync();
    });
  }

  // Whether an output asked with `asked` is the one that `want` asks for.
  function fits(asked: Asked, want: Asked, ids: readonly OptionId[] | null): boolean {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
    if (ids === null) return jsonEqual(asked.options, want.options);
    return ids.every((id) => jsonEqual(own(asked.options, id), own(want.options, id)));
  }

  function sync(): void {
    if (ended()) return;
    const want = wanted();
    if (want !== undefined) {
      if (held !== undefined && fits(held.asked, want.asked, want.ids)) {
        stopAsking();
      } else if (shelf.last !== undefined && shelf.last !== held && fits(shelf.last.asked, want.asked, want.ids)) {
        stopAsking();
        take(shelf.last);
      } else {
        const key = outputRequestKey(want.asked);
        if (asking?.key !== key) {
          stopAsking();
          asking = join(key, want.asked);
          announce();
        }
      }
    }
    wake();
  }

  function stopAsking(): void {
    if (asking === undefined) return;
    asking.leave();
    asking = undefined;
    announce();
  }

  // Joins the ask for the key, or starts it.
  function join(key: string, asked: Asked): { readonly key: string; leave(): void } {
    let flight = shelf.flights.get(key);
    if (flight === undefined) {
      const aborted = new AbortController();
      const done = scheduler
        .request(
          { kind: "output", block, revision: asked.revision, options: asked.options, info: asked.info, params: asked.params },
          { signal: aborted.signal },
        )
        .then((answer) => {
          // The last output that R made is the block's, whichever mount asked.
          if (answer.reply.status === "ok" && "output" in answer.reply && !deps.replaced.aborted) {
            shelf.last = { asked, output: answer.reply.output, body: answer.body };
          }
          heard(answer.reply);
          return answer;
        });
      const started: Flight = { done, controller: aborted, callers: 0 };
      void done
        .catch((failure: unknown) => {
          // An ask that nothing waits on any more ends without a reply.
          if (!aborted.signal.aborted) unanswered(errorMessage(failure));
        })
        .finally(() => {
          if (shelf.flights.get(key) === started) shelf.flights.delete(key);
        });
      shelf.flights.set(key, started);
      flight = started;
    }

    const mine = flight;
    mine.callers += 1;
    let left = false;
    const ask = {
      key,
      leave() {
        if (left) return;
        left = true;
        mine.callers -= 1;
        if (mine.callers === 0) {
          if (shelf.flights.get(key) === mine) shelf.flights.delete(key);
          mine.controller.abort();
        }
      },
    };
    mine.done.then(
      (answer) => {
        if (left) return;
        left = true;
        mine.callers -= 1;
        if (asking === ask) asking = undefined;
        received(asked, answer);
      },
      (failure: unknown) => {
        if (left) return;
        left = true;
        mine.callers -= 1;
        if (asking === ask) asking = undefined;
        if (!ended()) failed(noOutput(errorMessage(failure)));
      },
    );
    return ask;
  }

  function received(asked: Asked, { reply, body }: { reply: Reply; body: Blob | null }): void {
    if (ended()) return;
    switch (reply.status) {
      case "ok":
        take({ asked, output: (reply as OutputReply).output, body });
        return;
      case "error":
        failed(whyFailed(reply, block).condition);
        return;
      default:
        // A reply of `missing` comes where the block's revision moved on, or
        // where R lost its result, which the scheduler then has evaluated
        // again. A reply of `cancelled` comes to an ask that nothing waits on.
        // Either way, the mount asks again once the block reads ready.
        announce();
    }
  }

  // Raises the notifications of R's reply to an ask of the block's output, in
  // place of those before, for the block, whichever mount asked: one for each
  // condition of the reply, ok or error. Where R could not make the output and
  // gave no error, core raises one of its own with the condition that the
  // host's `error()` gives.
  function heard(reply: Reply): void {
    if (deps.replaced.aborted) return;
    if (reply.status === "ok" && "output" in reply) {
      hold(reply.conditions);
    } else if (reply.status === "error") {
      const { condition, by } = whyFailed(reply, block);
      hold("conditions" in reply ? reply.conditions : [], by === "core" ? [condition] : []);
    }
  }

  // An ask of the block's output that got no reply.
  function unanswered(message: string): void {
    if (!deps.replaced.aborted) hold([], [noOutput(message)]);
  }

  // Raises a notification of each condition, those that R wrote and then those
  // that core did, in place of those before.
  function hold(byR: readonly Condition[], byCore: readonly Condition[] = []): void {
    const before = shelf.notified;
    const raise = (condition: Condition, by: "r" | "core") =>
      deps.raise({
        severity: condition.severity,
        message: condition.message,
        source:
          by === "r"
            ? { kind: "r", class: condition.class, request: { type: "output" } }
            : { kind: "core", code: "output_unanswered" },
        about: { block },
      });
    shelf.notified = [...byR.map((condition) => raise(condition, "r")), ...byCore.map((condition) => raise(condition, "core"))];
    for (const withdraw of before) withdraw();
  }

  // Keeps why the latest ask failed, which the host's error() gives, while the
  // renderer keeps what it holds.
  function failed(condition: Condition): void {
    error = condition;
    announce();
  }

  // A renderer that cannot draw, which nothing else can: one that fails to
  // load, which holds until the next board opens or the block leaves the
  // board, or one without the board options that it names, which holds until
  // an output comes and the host's error() gives meanwhile.
  function fail(failure: unknown, by: OutputRenderer): void {
    if (ended()) return;
    const notification = coreNotification("module_failed", "error", errorMessage(failure), {
      about: { block },
      package: packageFromId(by.id),
      renderer: by.id,
    });
    if (failure instanceof ModuleError) {
      if (!broken.has(failure)) {
        log.error(failure);
        deps.raise(notification);
      }
      broken.add(failure);
      return;
    }
    const before = unfit;
    unfit = deps.raise(notification);
    before?.();
    failed(noOutput(errorMessage(failure)));
  }

  // Withdraws that the renderer cannot draw without the board options that it
  // names, as an output comes or the mount ends.
  function fit(): void {
    const before = unfit;
    unfit = undefined;
    before?.();
  }

  // Holds an output, and draws it with the renderer that it names.
  function take(next: Made): void {
    held = next;
    error = null;
    fit();
    const named = next.output.renderer;
    if (renderer?.id !== named.id) expect(named);
    if (drawing !== undefined && drawing.prepared.renderer.id !== named.id) unmountRenderer();
    if (drawing === undefined) {
      prepare(named).then(
        (prepared) => {
          if (ended() || drawing !== undefined || held?.output.renderer.id !== prepared.renderer.id) return;
          mountRenderer(prepared);
        },
        (failure: unknown) => {
          fail(failure, named);
        },
      );
    }
    announce();
    wake();
  }

  function mountRenderer(prepared: Prepared): void {
    const signal = new AbortController();
    const current: Drawing = { prepared, host: outputHost(prepared), controller: signal, cleanup: undefined };
    drawing = current;
    el.replaceChildren();
    void (async () => {
      try {
        const rendered = await prepared.mod.render({ el, host: current.host, signal: signal.signal });
        if (typeof rendered !== "function") return;
        if (signal.signal.aborted) attempt(rendered);
        else current.cleanup = rendered;
      } catch (failure) {
        if (signal.signal.aborted) return;
        const pkg = packageFromId(prepared.renderer.id);
        const why = new ModuleError(prepared.renderer.module, pkg, `failed in render(): ${errorMessage(failure)}`, {
          cause: failure,
        });
        el.replaceChildren();
        fail(why, prepared.renderer);
      }
    })();
  }

  function unmountRenderer(): void {
    const current = drawing;
    if (current === undefined) return;
    drawing = undefined;
    current.host.drop();
    current.controller.abort();
    if (current.cleanup !== undefined) attempt(current.cleanup);
    el.replaceChildren();
  }

  function announce(): void {
    drawing?.host.announce();
  }

  function wake(): void {
    announceTo(waiting);
  }

  // The values of the board options that a renderer names, which an output
  // was asked with, and for those that it was not asked with, the board's.
  function optionsFor(prepared: Prepared, asked: Asked | undefined): Readonly<Record<OptionId, Json>> {
    const now = store.board().options;
    return Object.fromEntries(
      prepared.named.flatMap((id) => {
        const value = asked !== undefined && Object.hasOwn(asked.options, id) ? asked.options[id] : own(now, id);
        return value === undefined ? [] : [[id, value]];
      }),
    );
  }

  // Replaces the block's parameters, so that every mount of the block asks
  // again.
  function replaceParams(value: Readonly<Record<string, Json>>): void {
    if (!bound()) return;
    // A copy that nothing changes, since every mount of the block shares it.
    const next = frozenCopy(value);
    if (jsonEqual(next, shelf.params)) return;
    shelf.params = next;
    announceTo(shelf.mounts);
  }

  // The output of the block's current revision, once core holds it, or a
  // rejection where no result is in sight, as the block reads failed,
  // invalid or waiting.
  function current(name: string, signal: AbortSignal | undefined): Promise<Made> {
    return new Promise<Made>((resolve, reject) => {
      const settle = (): boolean => {
        if (!bound()) {
          reject(unboundError());
          return true;
        }
        if (signal?.aborted === true) {
          reject(asError(signal.reason));
          return true;
        }
        const status: BlockStatus | undefined = scheduler.status(block);
        if (status === undefined) {
          reject(unboundError());
          return true;
        }
        if (status.status === "failed" || status.status === "invalid" || status.status === "waiting") {
          reject(
            new RequestError(
              `The request "${name}" of the output of block "${block}" did not go to R, since the block reads ${status.status}.`,
              { blocks: [{ block, status }] },
            ),
          );
          return true;
        }
        if (status.status === "ready" && held?.asked.revision === status.revision) {
          resolve(held);
          return true;
        }
        return false;
      };
      if (settle()) return;
      const check = (): void => {
        if (settle()) stop();
      };
      const stop = (): void => {
        waiting.delete(check);
        signal?.removeEventListener("abort", check);
      };
      waiting.add(check);
      signal?.addEventListener("abort", check, { once: true });
    });
  }

  async function request(
    prepared: Prepared,
    name: string,
    args: Readonly<Record<string, Json>>,
    opts: OutputRequestOpts,
  ): Promise<Json | Blob> {
    const what = `The request "${name}" of the output of block "${block}"`;
    // The request's span goes under the span that was active as the module
    // called the host's request(), though the host sends it only once the
    // block's output is there (../trace/index.ts).
    const inCaller = keepContext();
    for (let attempt = 1; ; attempt++) {
      const target = await current(name, opts.signal);
      const output = { block, revision: target.asked.revision };
      const fields = { output, name, args: { ...args }, options: target.asked.options, params: target.asked.params };
      const asked: OutputAsk =
        opts.scope === "renderer"
          ? { kind: "request", type: "renderer", renderer: prepared.renderer.id, ...fields }
          : { kind: "request", type: "output_class", ...fields };
      const { reply, body } = await inCaller(() =>
        deps.answers.ask(
          { request: asked, share: opts.file === undefined ? "keep" : "once", ...(opts.file === undefined ? {} : { body: opts.file }) },
          {
            ...(opts.signal === undefined ? {} : { signal: opts.signal }),
            ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
          },
        ),
      );
      if (!bound()) throw unboundError();
      const requested =
        opts.scope === "renderer"
          ? { type: "renderer" as const, renderer: prepared.renderer.id, name }
          : { type: "output_class" as const, name };
      const conditions = "conditions" in reply ? reply.conditions : [];
      switch (reply.status) {
        case "ok":
          deps.replied({ block }, requested, conditions);
          if (body !== null) return body;
          return "value" in reply && reply.value !== undefined ? reply.value : null;
        case "missing":
          if (attempt < ATTEMPTS) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          deps.replied({ block }, requested, conditions);
          throw new RequestError(`R replied ${reply.status} to ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, {
            status: reply.status,
            conditions,
          });
      }
    }
  }

  function checkRequest(name: unknown, args: unknown, opts: unknown): Error | undefined {
    if (typeof name !== "string" || name === "") return new TypeError("A request of an output is named by a non-empty string.");
    if (!isObject(args)) return new TypeError(`The args of the request "${name}" are an object.`);
    if (!isObject(opts)) return new TypeError(`The options of the request "${name}" are an object.`);
    const scope = opts.scope;
    if (scope !== undefined && scope !== "class" && scope !== "renderer") {
      return new TypeError(`A request of an output has the scope "class" or "renderer", not ${JSON.stringify(scope)}.`);
    }
    return undefined;
  }

  function outputHost(prepared: Prepared): Announcing {
    const listeners = new Set<() => void>();
    let named: { for: Made | undefined; values: Readonly<Record<OptionId, Json>> } | undefined;
    let following = true;
    // What withdraws each notification that the renderer raised and that
    // holds.
    const notified = new Set<() => void>();
    const pkg = packageFromId(prepared.renderer.id);
    return {
      block,
      output: () => held?.output ?? null,
      body: () => held?.body ?? null,
      params: () => held?.asked.params ?? EMPTY,
      options() {
        let memo = named;
        if (memo === undefined || memo.for !== held) {
          memo = { for: held, values: optionsFor(prepared, held?.asked) };
          named = memo;
        }
        return memo.values;
      },
      loading: () => asking !== undefined,
      error: () => error,
      on(event, fn) {
        // A module in plain JS may name another event, which the types refuse.
        const heard: string = event;
        if (heard !== "output") {
          throw new Error(`The host of an output announces "output", not ${JSON.stringify(event)}.`);
        }
        const wrapped = (): void => {
          fn();
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
      setParams(params) {
        if (!isObject(params)) throw new TypeError("An output's parameters are an object.");
        if (following) replaceParams(params);
      },
      request<T = Json>(
        name: string,
        args: Readonly<Record<string, Json>> = {},
        opts: OutputRequestOpts = {},
      ): Promise<T | Blob> {
        const invalid = checkRequest(name, args, opts);
        if (invalid !== undefined) return Promise.reject(invalid);
        if (!following || !bound()) return Promise.reject(unboundError());
        return request(prepared, name, args, opts) as Promise<T | Blob>;
      },
      emit(name, payload) {
        if (following && bound()) deps.emit({ block, name, payload });
      },
      notify(notification) {
        const given = checkRaised(notification);
        if (!following || !bound()) return () => undefined;
        const withdraw = deps.notify({ object: { block }, package: pkg, renderer: prepared.renderer.id }, given);
        const mine = (): void => {
          notified.delete(mine);
          withdraw();
        };
        notified.add(mine);
        return mine;
      },
      log: createLog(pkg),
      announce() {
        if (following) announceTo(listeners);
      },
      drop() {
        following = false;
        listeners.clear();
        for (const withdraw of [...notified]) withdraw();
      },
    };
  }

  // Follows the board, the block's status and the element's size, until the
  // block leaves the board or another board opens.
  const offStore = store.subscribe((_change, next) => {
    if (!Object.hasOwn(next.blocks, block)) {
      end();
      return;
    }
    if (next.options === options) return;
    options = next.options;
    sync();
  });
  const offScheduler = scheduler.subscribe((changed) => {
    if (changed.includes(block)) sync();
  });
  shelf.mounts.add(sync);

  let settling: ReturnType<typeof setTimeout> | undefined;
  const stopWatching = measure.watch(el, () => {
    if (settling !== undefined) clearTimeout(settling);
    settling = setTimeout(
      () => {
        settling = undefined;
        const next = measure.info(el);
        if (jsonEqual(next, info)) return;
        info = next;
        sync();
      },
      info === null ? 0 : SETTLE_MS,
    );
  });

  const offEnd = onAbort(deps.replaced, end);

  function end(): void {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    offScheduler();
    keeping?.stop();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== undefined) clearTimeout(settling);
    asking?.leave();
    asking = undefined;
    fit();
    unmountRenderer();
    el.replaceChildren();
    wake();
  }

  if (renderer !== undefined) expect(renderer);
  if (shelf.last !== undefined) take(shelf.last);
  sync();

  return end;
}

// What makes two asks of one block's output identical: the revision, the
// board option values, the info and the parameters.
function outputRequestKey(asked: Asked): string {
  return JSON.stringify([asked.revision, sorted(asked.options), sorted(asked.info as unknown as Json), sorted(asked.params)]);
}

function sorted(x: Json): Json {
  if (Array.isArray(x)) return (x as readonly Json[]).map(sorted);
  if (isObject(x)) {
    const object = x as Readonly<Record<string, Json>>;
    return Object.fromEntries(
      Object.keys(object)
        .sort()
        .map((name) => [name, sorted(object[name] ?? null)]),
    );
  }
  return x;
}
