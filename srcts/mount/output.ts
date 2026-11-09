// Mounting a block's output: core measures the element, asks R for the output
// and draws it with the renderer that R names in it, through an OutputHost made
// for the mount, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the renderer that the block class's entry in the block registry
//    names, and resolves the default of each board option that the renderer
//    names and the board holds no value for, which it keeps beside the board
//    for the opening (./defaults.ts). So core has what the output's coercion
//    reads before it asks for an output.
// 2. Once the block reads ready and the element measures more than 0 by 0, core
//    asks R for the output, through the scheduler, with the block's revision,
//    the values of the board options that the renderer names, the board's or
//    else their defaults, the info it measured and the block's parameters,
//    unless it keeps an output asked with the same ones. Where it knows no
//    renderer, it asks with the value of every board option that a loaded
//    package defines.
// 3. It mounts the renderer that the output names once it has the output, and
//    hands it each output that comes after. An output that names another
//    renderer than the one drawing, as one of a subclass may, is drawn by that
//    one, once core has resolved what it names.
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
// A mount is bound to the board it was made on and to its block. Once another
// board opens or the block leaves the board, core ends the mount.

import type { Store } from "../store/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Condition, OutputInfo, OutputReply, Reply } from "../protocol/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { OutputHost, OutputRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputRenderer } from "../types/outputs.ts";
import { asError, frozenCopy, isObject, jsonEqual, messageOf, own, packageOf, report } from "../util/index.ts";
import type { Answers, OutputAsk } from "./answers.ts";
import type { Defaults } from "./defaults.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import { RequestError } from "./requests.ts";
import { announceTo, attempt, moduleOf, onAbort, REPLACED, showError } from "./shared.ts";
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
        shelf = { params: {}, last: undefined, mounts: new Set(), flights: new Map() };
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
  readonly defaults: Defaults;
  readonly emit: (event: OutputEvent) => void;
}

interface OutputArgs {
  el: HTMLElement;
  host: OutputHost;
  signal: AbortSignal;
}

// A renderer loaded and checked, with the board options it names, whose
// defaults core has resolved by then.
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

// Mounts the output of the block in the element until the returned cleanup
// runs. It throws where the board holds no such block.
export function mountOutput(deps: OutputDeps, el: HTMLElement, block: BlockId): () => void {
  const { store, scheduler, loader, measure, defaults } = deps;
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
  // is loaded, and requires them while it asks with them. Where neither names
  // one, the mount asks with every board option that a loaded package
  // defines, and learns the renderer from the first output.
  let renderer: OutputRenderer | undefined = entry.renderer;
  let ready: Prepared | undefined;
  let release: (() => void) | undefined;
  const preparing = new Map<string, Promise<Prepared>>();

  let info = measure.info(el);
  let held: Made | undefined;
  let asking: { readonly key: string; leave(): void } | undefined;
  let error: Condition | null = null;
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

  // What requires the board options that the mount asks with.
  function requiredBy(): string {
    return renderer === undefined ? `the first output of block "${block}"` : `the renderer "${renderer.id}"`;
  }

  // Loads a renderer and resolves the defaults of the board options that it
  // names.
  function prepare(given: OutputRenderer): Promise<Prepared> {
    let loading = preparing.get(given.id);
    if (loading === undefined) {
      loading = (async () => {
        const pkg = packageOf(given.id);
        const source = given.css === undefined ? { module: given.module } : { module: given.module, css: given.css };
        const mod = moduleOf<OutputArgs>(await loader.load(source, pkg), given.module, pkg);
        const named = mod.options ?? [];
        await defaults.resolve(named, `the renderer "${given.id}"`);
        return { renderer: given, mod, named };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }

  // Loads the renderer that the mount asks for, then asks.
  function expect(given: OutputRenderer): void {
    renderer = given;
    if (ready?.renderer.id === given.id) return;
    prepare(given).then(
      (prepared) => {
        if (ended() || renderer?.id !== prepared.renderer.id) return;
        ready = prepared;
        release?.();
        release = defaults.require(prepared.named, `the renderer "${prepared.renderer.id}"`);
        sync();
      },
      (failure: unknown) => {
        fail(failure);
      },
    );
  }

  // The board options that the mount asks with: those that the renderer
  // names, or every one where it knows no renderer, or undefined while the
  // renderer loads.
  function named(): readonly OptionId[] | null | undefined {
    if (renderer === undefined) return null;
    return ready?.renderer.id === renderer.id ? ready.named : undefined;
  }

  // What the mount would ask R for now, or undefined where it asks for
  // nothing: while the element measures nothing, the renderer loads, the
  // block reads anything but ready, or a default that it asks with resolves.
  // It asks with the values of the board options named, or of every one that
  // a loaded package defines, the board's or else their defaults. Within a
  // change to the board, the scheduler may announce a new status before the
  // store has told the mount of the change, so the mount reads the board from
  // the store rather than from what it last heard.
  function wanted(): { asked: Asked; ids: readonly OptionId[] | null } | undefined {
    if (info === null) return undefined;
    const ids = named();
    if (ids === undefined) return undefined;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return undefined;
    const sent = ids ?? Object.keys(store.options.registered());
    // As after a delta removed a value whose default core had not resolved,
    // the mount asks once the default has settled, which the defaults
    // announce.
    if (!defaults.settled(sent)) {
      void defaults.resolve(sent, requiredBy());
      return undefined;
    }
    return { asked: { revision: status.revision, options: defaults.values(sent), info, params: shelf.params }, ids };
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
        const key = keyOf(want.asked);
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
          return answer;
        });
      const started: Flight = { done, controller: aborted, callers: 0 };
      void done.catch(() => undefined).finally(() => {
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
        fail(failure);
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
      case "error": {
        const conditions = "conditions" in reply ? reply.conditions : [];
        failed(
          conditions[0] ?? {
            severity: "error",
            class: ["blockr_error", "error", "condition"],
            phase: "output",
            message: `R failed to make the output of block "${block}".`,
          },
          conditions,
        );
        return;
      }
      default:
        // A reply of `missing` comes where the block's revision moved on, or
        // where R lost its result, which the scheduler then has evaluated
        // again. A reply of `cancelled` comes to an ask that nothing waits on.
        // Either way, the mount asks again once the block reads ready.
        announce();
    }
  }

  // Keeps why the latest ask failed, while the renderer keeps what it holds,
  // or shows it in the element where no output came before.
  function failed(condition: Condition, conditions: readonly Condition[]): void {
    error = condition;
    if (held === undefined) {
      showError(el, conditions.length > 0 ? conditions.map((c) => c.message).join("\n") : condition.message);
    }
    announce();
  }

  // A renderer that cannot draw, which nothing else can, or an ask that got no
  // reply.
  function fail(failure: unknown): void {
    if (ended()) return;
    if (failure instanceof ModuleError) {
      showError(el, failure.message);
      if (!broken.has(failure)) report(failure);
      broken.add(failure);
      return;
    }
    failed(
      { severity: "error", class: ["blockr_error", "error", "condition"], phase: "output", message: messageOf(failure) },
      [],
    );
  }

  // Holds an output, and draws it with the renderer that it names.
  function take(next: Made): void {
    held = next;
    error = null;
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
          fail(failure);
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
        const pkg = packageOf(prepared.renderer.id);
        const why = new ModuleError(prepared.renderer.module, pkg, `failed in render(): ${messageOf(failure)}`, {
          cause: failure,
        });
        showError(el, why.message);
        report(why);
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
  // was asked with, and for those that it was not asked with, the board's or
  // else their defaults.
  function optionsFor(prepared: Prepared, asked: Asked | undefined): Readonly<Record<OptionId, Json>> {
    return Object.fromEntries(
      prepared.named.flatMap((id) => {
        const value = asked !== undefined && Object.hasOwn(asked.options, id) ? asked.options[id] : defaults.value(id);
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
    for (let attempt = 1; ; attempt++) {
      const target = await current(name, opts.signal);
      const output = { block, revision: target.asked.revision };
      const fields = { output, name, args: { ...args }, options: target.asked.options, params: target.asked.params };
      const asked: OutputAsk =
        opts.scope === "renderer"
          ? { kind: "request", type: "renderer", renderer: prepared.renderer.id, ...fields }
          : { kind: "request", type: "output_class", ...fields };
      const { reply, body } = await deps.answers.ask(
        { request: asked, share: opts.file === undefined ? "keep" : "once", ...(opts.file === undefined ? {} : { body: opts.file }) },
        {
          ...(opts.signal === undefined ? {} : { signal: opts.signal }),
          ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
        },
      );
      if (!bound()) throw unboundError();
      switch (reply.status) {
        case "ok":
          if (body !== null) return body;
          return "value" in reply && reply.value !== undefined ? reply.value : null;
        case "missing":
          if (attempt < ATTEMPTS) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          throw new RequestError(`R replied ${reply.status} to ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, {
            status: reply.status,
            conditions: "conditions" in reply ? reply.conditions : [],
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
      announce() {
        if (following) announceTo(listeners);
      },
      drop() {
        following = false;
        listeners.clear();
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
  // A default that settles may be what the mount waits for to ask.
  const offDefaults = defaults.subscribe(sync);
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
    offDefaults();
    release?.();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== undefined) clearTimeout(settling);
    asking?.leave();
    asking = undefined;
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
function keyOf(asked: Asked): string {
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
