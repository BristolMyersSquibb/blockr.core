// Mounting a block's output: core measures the element, asks R for the output
// and draws it with the renderer that R names in it, through an OutputHost made
// for the mount, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core adds the board options that the block class's entry in the block
//    registry names in `output_options` to the board, those that the output
//    family of the class names for its outputs, each with its definition's
//    default where the board lacks it, and loads the renderers that the entry
//    names. So the board holds every board option that the output's family
//    and its renderer read before core asks for an output, since no renderer
//    names any of its own. While the mount lasts, core adds again each of them
//    that a delta removes. Where one does not go onto the board, as where its
//    default fails, core reports it and asks R for nothing, and the host's
//    `error()` gives why meanwhile.
// 2. Once the block reads ready and the output is on screen, core asks R for
//    the output, through the scheduler, with the block's revision, the values
//    of those board options, the info it measured and the block's parameters,
//    unless it keeps an output asked with the same ones. Where neither the
//    entry nor an output names a renderer, core asks with the board options
//    of the class's outputs alone.
//    The output is on screen while the element measures more than 0 by 0,
//    while CoreApi's onScreen holds what the element sits in, and while the
//    caller's onScreen says so, where the caller gives one (MountOutputOpts
//    in ../types/core-api.ts).
//    Core also asks while a request of the output waits for the output of
//    the block's revision, as step 5 has it.
// 3. It mounts the renderer that the output names once it has the output and
//    the output is on screen, and hands it each output that comes after. An
//    output that names another renderer than the one drawing, as one of a
//    subclass may, is drawn by that one. The renderer reads the values of
//    those board options through its host's option(), as the output was
//    asked with them.
// 4. It asks again when the revision, one of those board options, the info or
//    the parameters change, while the block reads ready and the output is on
//    screen. Meanwhile it keeps drawing the last output, whatever the block's
//    status and whether the output is on screen, with `loading()` while it
//    asks and `error()` where the latest ask failed. An ask that is on its
//    way as the output leaves the screen still lands, and the renderer draws
//    what it brings.
// 5. A request of the output goes to R with the output of the block's
//    revision. Where core holds none, the request waits for it, and core asks
//    for it wherever the element measures more than 0 by 0, whatever the two
//    onScreen say, as a request that reads blocks has them evaluated even off
//    screen.
//
// Core keeps two things of each block's output on the block's shelf: its
// parameters, which every mount of the output shares, and the last output that
// R made, with what it was asked with, which a mount draws without asking R
// where it would ask with the same. The shelves go with the mounts, which core
// makes anew for each board that opens.
// Core raises a notification for each condition of R's reply to an ask of the
// block's output, ok or error, which concerns the block, in place of those
// before. Where an ask fails without an error from R, as where it gets no
// reply, core raises one of its own with the failure that the host's
// `error()` gives. A frontend shows every one of them, as a card does in its
// pills, whatever their severity. A renderer that cannot draw, as one
// that fails to load or throws in render(), raises one that concerns the
// block, with the renderer and its package, which holds until the next board
// opens or the block leaves the board, and leaves the element empty. Core
// draws nothing in the element itself, so that the frontend alone says where
// a failure shows. What a renderer raises through its host's notify() comes
// from the block, with the renderer and its package, and holds until the
// renderer is unmounted at the latest. Each condition of R's reply to a
// request of the output, ok or error, raises a notification that concerns the
// block, which holds until R next replies to a request of the same name and
// scope. Its source names the renderer, which made the request, whichever
// scope the request has.
// A mount is bound to the board it was made on and to its block. Once another
// board opens or the block leaves the board, core ends the mount.

import type { Store } from "../store/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Condition, OutputInfo, OutputReply, Reply } from "../protocol/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { CoreFailure, MountOutputOpts, SourceModule } from "../types/core-api.ts";
import type { OutputHost, OutputRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputRenderer } from "../types/outputs.ts";
import {
  asError,
  checkRaised,
  coreNotification,
  errorMessage,
  frozenCopy,
  givenNotifier,
  isObject,
  jsonEqual,
  own,
  packageFromId,
  sourceModule,
} from "../util/index.ts";
import { createLog, log } from "../log/index.ts";
import { keepContext } from "../trace/index.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";
import type { Answers, OutputAsk } from "./answers.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import { RequestError } from "./requests.ts";
import { announceTo, attempt, mountableModule, onAbort, optionReader, REPLACED, withoutOptions } from "./shared.ts";
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
  ) => { readonly added: Promise<readonly string[]>; readonly stop: () => void };
  readonly emit: (event: OutputEvent) => void;
  // Raises a notification of the board (../core/notifications.ts).
  readonly raise: Raise;
  // Holds a notification for each condition of R's latest reply to each of
  // the output's requests.
  readonly replied: Replied;
  // Raises the notification that a renderer gives through its host.
  readonly notify: Notify;
  // What CoreApi's onScreen says of the output (../core/core.ts): whether it
  // holds what the output's element sits in, and what calls `fn` as it
  // changes.
  readonly onScreen: OnScreenOf;
}

// What CoreApi's onScreen says of the output of a block, by the output's
// element.
export interface OnScreenOf {
  holds(el: HTMLElement, block: BlockId): boolean;
  subscribe(fn: () => void): () => void;
}

// What the caller of mountOutput() says of the output, through the options.
type CallerOnScreen = NonNullable<MountOutputOpts["onScreen"]>;

interface OutputArgs {
  el: HTMLElement;
  host: OutputHost;
  signal: AbortSignal;
}

// A renderer loaded and checked.
interface Prepared {
  readonly renderer: OutputRenderer;
  readonly mod: Mountable<OutputArgs>;
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

// Core's failure at the output of a block, with the code of the notification
// that core raises for it.
function coreFailure(code: CoreFailure["code"], message: string): CoreFailure {
  return { kind: "core", code, message };
}

// Why R could not make the output of the block, from R's error reply: the
// reply's condition of the severity error. Where the coercion failed, that is
// the error that stopped it, and the reply holds it after the messages and
// warnings that the coercion raised before. Where the reply holds no error,
// it is core's failure.
function whyFailed(reply: Reply, block: BlockId): Condition | CoreFailure {
  const conditions = "conditions" in reply ? reply.conditions : [];
  const error = conditions.find((condition) => condition.severity === "error");
  return error ?? coreFailure("output_unanswered", `R failed to make the output of block "${block}".`);
}

// The onScreen of the options of mountOutput(), if any, as a frontend or a
// plugin in plain JS may give anything. It throws for options that are no
// object, and for an onScreen without get() and subscribe().
function callerOnScreen(opts: unknown): CallerOnScreen | undefined {
  if (!isObject(opts)) throw new TypeError(`The options of mountOutput() are an object, not ${JSON.stringify(opts)}.`);
  const given = opts.onScreen;
  if (given === undefined) return undefined;
  if (!isObject(given) || typeof given.get !== "function" || typeof given.subscribe !== "function") {
    throw new TypeError("The onScreen of mountOutput() has get() and subscribe(), as CoreApi's focus has.");
  }
  return given as unknown as CallerOnScreen;
}

// Mounts the output of the block in the element until the returned cleanup
// runs. It throws where the board holds no such block, and for options that
// are not MountOutputOpts.
export function mountOutput(deps: OutputDeps, el: HTMLElement, block: BlockId, opts: MountOutputOpts = {}): () => void {
  const { store, scheduler, loader, measure } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const caller = callerOnScreen(opts);
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

  // The board options of the class's outputs, which the output family of the
  // class names for its outputs and every renderer of them reads, which the
  // mount keeps on the board while it lasts and asks with, the first time
  // included.
  const outputOptions = entry.output_options ?? [];
  // The registry id of the block's class, which names it in what the mount
  // says.
  const registryId = entry.id;
  // The renderers that may draw the block's output, by id: those that the
  // block class's entry names, one for each family whose outputs the class's
  // family makes, and any other that an output names. The mount loads each,
  // and asks once all are loaded, so that it asks for no output that no
  // renderer can draw. Where none is named, the mount learns the renderer from
  // the first output.
  const expected = new Map<string, OutputRenderer>();
  const ready = new Map<string, Prepared>();
  const preparing = new Map<string, Promise<Prepared>>();

  let info = measure.info(el);
  let held: Made | undefined;
  let asking: { readonly key: string; leave(): void } | undefined;
  let error: Condition | CoreFailure | null = null;
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

  // The renderer as the module of the block's output, with its package.
  const rendererModule = (renderer: OutputRenderer): SourceModule =>
    sourceModule({ block }, packageFromId(renderer.id), renderer.id);

  // Loads a renderer.
  function prepare(given: OutputRenderer): Promise<Prepared> {
    let loading = preparing.get(given.id);
    if (loading === undefined) {
      loading = (async () => {
        const pkg = packageFromId(given.id);
        const source = given.css === undefined ? { module: given.module } : { module: given.module, css: given.css };
        const mod = mountableModule<OutputArgs>(await loader.load(source, pkg), given.module, pkg);
        return { renderer: given, mod };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }

  // Loads a renderer that may draw the block's output, then asks.
  function expect(given: OutputRenderer): void {
    if (expected.has(given.id)) return;
    expected.set(given.id, given);
    prepare(given).then(
      (prepared) => {
        if (ended()) return;
        ready.set(given.id, prepared);
        sync();
      },
      (failure: unknown) => {
        fail(failure, given);
      },
    );
  }

  // The board options that the mount asks with, those of the class's outputs,
  // or undefined while a renderer that may draw the output loads.
  function named(): readonly OptionId[] | undefined {
    if ([...expected.keys()].some((id) => !ready.has(id))) return undefined;
    return outputOptions;
  }

  // The values that the board holds now of the board options named. Within a
  // change to the board, the scheduler may announce a new status before the
  // store has told the mount of the change, so the mount reads the board from
  // the store rather than from what it last heard.
  function values(ids: readonly OptionId[]): Readonly<Record<OptionId, Json>> {
    const options = store.board().options;
    return Object.fromEntries(
      ids.flatMap((id) => {
        const value = own(options, id);
        return value === undefined ? [] : [[id, value]];
      }),
    );
  }

  // Whether the output is on screen, as far as core can tell: whether its
  // element measures more than 0 by 0, CoreApi's onScreen holds what the
  // element sits in, and the caller's onScreen returns true, where the caller
  // gives one.
  function onScreen(): boolean {
    if (info === null || !deps.onScreen.holds(el, block)) return false;
    // A caller in plain JS may return something else, which the types refuse.
    const shows: unknown = caller === undefined ? true : caller.get();
    return shows === true;
  }

  // What the mount would ask R for now, or undefined where it asks for
  // nothing: while the element measures nothing, the output is off screen
  // and no request of it waits, the renderer loads, the board lacks a board
  // option that the mount asks with, as before core has put it there or
  // after a delta removed it, or the block reads anything but ready.
  function wanted(): { asked: Asked; ids: readonly OptionId[] } | undefined {
    if (info === null) return undefined;
    if (waiting.size === 0 && !onScreen()) return undefined;
    const ids = named();
    if (ids === undefined) return undefined;
    if (ids.some((id) => !Object.hasOwn(store.board().options, id))) return undefined;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return undefined;
    return { asked: { revision: status.revision, options: values(ids), info, params: shelf.params }, ids };
  }

  // Whether an output asked with `asked` is the one that `want` asks for.
  function fits(asked: Asked, want: Asked, ids: readonly OptionId[]): boolean {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
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
    draw();
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
        if (!ended()) failed(coreFailure("output_unanswered", errorMessage(failure)));
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
        failed(whyFailed(reply, block));
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
  // gave no error, core raises one of its own with the failure that the
  // host's `error()` gives.
  function heard(reply: Reply): void {
    if (deps.replaced.aborted) return;
    if (reply.status === "ok" && "output" in reply) {
      hold(reply.conditions);
    } else if (reply.status === "error") {
      const why = whyFailed(reply, block);
      hold("conditions" in reply ? reply.conditions : [], "kind" in why ? [why] : []);
    }
  }

  // An ask of the block's output that got no reply.
  function unanswered(message: string): void {
    if (!deps.replaced.aborted) hold([], [coreFailure("output_unanswered", message)]);
  }

  // Raises a notification of each condition that R wrote and then of each of
  // core's failures, in place of those before.
  function hold(byR: readonly Condition[], byCore: readonly CoreFailure[] = []): void {
    const before = shelf.notified;
    const fromR = (condition: Condition) =>
      deps.raise({
        severity: condition.severity,
        message: condition.message,
        source: { kind: "r", class: condition.class, request: { type: "output" } },
        about: { block },
      });
    const fromCore = (failure: CoreFailure) =>
      deps.raise({ severity: "error", message: failure.message, source: { kind: "core", code: failure.code }, about: { block } });
    shelf.notified = [...byR.map(fromR), ...byCore.map(fromCore)];
    for (const withdraw of before) withdraw();
  }

  // Keeps why the latest ask failed, which the host's error() gives, while the
  // renderer keeps what it holds.
  function failed(why: Condition | CoreFailure): void {
    error = why;
    announce();
  }

  // A renderer that cannot draw, which nothing else can, as one that fails to
  // load or throws in render(), which holds until the next board opens or the
  // block leaves the board. Core reports each failure once, however many
  // outputs name the renderer.
  function fail(failure: unknown, by: OutputRenderer): void {
    if (ended() || broken.has(failure as object)) return;
    broken.add(failure as object);
    log.error(asError(failure));
    deps.raise(coreNotification("module_failed", "error", errorMessage(failure), { module: rendererModule(by) }));
  }

  // Holds an output, and draws it with the renderer that it names.
  function take(next: Made): void {
    held = next;
    error = null;
    const named = next.output.renderer;
    expect(named);
    if (drawing !== undefined && drawing.prepared.renderer.id !== named.id) unmountRenderer();
    draw();
    announce();
    wake();
  }

  // Mounts the renderer that the output held names, once the renderer has
  // loaded, where none draws and the output is on screen.
  function draw(): void {
    if (held === undefined || drawing !== undefined || !onScreen()) return;
    const named = held.output.renderer;
    prepare(named).then(
      (prepared) => {
        if (ended() || drawing !== undefined || held?.output.renderer.id !== prepared.renderer.id || !onScreen()) return;
        mountRenderer(prepared);
      },
      (failure: unknown) => {
        fail(failure, named);
      },
    );
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

  // The values of the board options of the class's outputs, which an output
  // was asked with, and for those that it was not asked with, the board's.
  function optionsFor(asked: Asked | undefined): Readonly<Record<OptionId, Json>> {
    const now = store.board().options;
    return Object.fromEntries(
      outputOptions.flatMap((id) => {
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
  // invalid or waiting. While it waits, the mount asks for that output
  // whatever the two onScreen say.
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
      sync();
    });
  }

  async function request(
    prepared: Prepared,
    name: string,
    args: Readonly<Record<string, Json>>,
    opts: OutputRequestOpts,
  ): Promise<Json | Blob> {
    const what = `The request "${name}" of the output of block "${block}"`;
    const notifier = givenNotifier(opts, `the request "${name}" of the output of block "${block}"`);
    // The renderer, which made the request, whichever scope the request has.
    const module = rendererModule(prepared.renderer);
    const requested = { type: opts.scope === "renderer" ? ("renderer" as const) : ("output_class" as const), name };
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
      const conditions = "conditions" in reply ? reply.conditions : [];
      switch (reply.status) {
        case "ok":
          deps.replied(module, requested, conditions, notifier);
          if (body !== null) return body;
          return "value" in reply && reply.value !== undefined ? reply.value : null;
        case "missing":
          if (attempt < ATTEMPTS) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          deps.replied(module, requested, conditions, notifier);
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
    // What gives the values of the board options of the class's outputs, for
    // the output that the host held as it made it.
    let named: { for: Made | undefined; read: (id: OptionId) => Json } | undefined;
    let following = true;
    // What withdraws each notification that the renderer raised and that
    // holds.
    const notified = new Set<() => void>();
    const module = rendererModule(prepared.renderer);
    return {
      block,
      output: () => held?.output ?? null,
      body: () => held?.body ?? null,
      params: () => held?.asked.params ?? EMPTY,
      option(id) {
        let memo = named;
        if (memo === undefined || memo.for !== held) {
          const read = optionReader(
            optionsFor(held?.asked),
            outputOptions,
            (unnamed) =>
              `The output family of block class "${registryId}" names no board option "${unnamed}" in R, so the renderer "${prepared.renderer.id}" cannot read it.`,
          );
          memo = { for: held, read };
          named = memo;
        }
        return memo.read(id);
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
        const withdraw = deps.notify(module, given);
        const mine = (): void => {
          notified.delete(mine);
          withdraw();
        };
        notified.add(mine);
        return mine;
      },
      log: createLog(module.package ?? null),
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

  // Follows the board, the block's status, the element's size and whether the
  // output is on screen, until the block leaves the board or another board
  // opens. The mount follows the caller's onScreen once it has set up the
  // rest, below.
  let offCaller: (() => void) | undefined;
  const offOnScreen = deps.onScreen.subscribe(sync);
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

  // Keeps the board options of the class's outputs on the board while the
  // mount lasts. Where one does not go onto the board, which core has
  // reported, the mount asks R for nothing, and the host's error() gives why.
  const by = `the outputs of block class "${entry.id}"`;
  const keepingOutputOptions = deps.keepOptions(outputOptions, by, lackOutputOptions);
  void keepingOutputOptions.added.then((failures) => {
    if (failures.length > 0) lackOutputOptions();
  });

  function lackOutputOptions(): void {
    if (ended()) return;
    const missing = outputOptions.filter((id) => !Object.hasOwn(store.board().options, id));
    failed(coreFailure("board_option_failed", withoutOptions(by, missing)));
  }

  function end(): void {
    if (ended()) return;
    controller.abort();
    offEnd();
    if (offCaller !== undefined) attempt(offCaller);
    offOnScreen();
    offStore();
    offScheduler();
    keepingOutputOptions.stop();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== undefined) clearTimeout(settling);
    asking?.leave();
    asking = undefined;
    unmountRenderer();
    el.replaceChildren();
    wake();
  }

  for (const given of entry.renderers ?? []) expect(given);
  if (shelf.last !== undefined) take(shelf.last);
  // A caller's subscribe() that throws ends the mount, and the call throws
  // with it.
  try {
    // A caller in plain JS may return something else, which the types refuse.
    const off: unknown = caller?.subscribe(sync);
    if (typeof off === "function") offCaller = off as () => void;
  } catch (failure) {
    end();
    throw failure;
  }
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
