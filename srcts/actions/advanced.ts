// The advanced action of a board action, as Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action):
// a module that holds every option of its board action, as the block picker
// holds a new block's class, id, name and input, and that core shows in a
// place with enough room. It meets the contract of a UI module, with CoreApi
// in place of a host
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// Core renders it with the element, CoreApi, the target, a signal, which
// aborts as core unmounts it, and done(). Opened from the open icon of a
// quick action, it gets `picked` and `filter` too (./quick.ts). Its render()
// may be async and may return its cleanup.
//
// Core opens a board action's advanced action from the board action's row in a
// menu, where the board action has no quick action or its quick gives no level
// for the target, from trigger() on such a board action, and from the open
// icon of a level of its quick action or of an item (./actions.ts).
// It shows the advanced action in a place, the first of these that is given:
// 1. The `advanced` that the caller of menu() or trigger() gives.
// 2. The `advanced` member of the module of the frontend that draws the
//    board.
// 3. Core's dialog (./dialog.tsx).
// Core calls the place with the board action's id, its label as the title, the
// target, a mount() and a close(). The place renders the advanced action
// through mount(), and calls close() as the user closes the place. A place
// shows one advanced action at a time: where core opens another in the same
// place, the one before ends, as at a close().
//
// The call done() ends the advanced action in these steps:
// 1. Where the advanced action hands it no batch, or an empty one, core runs
//    nothing.
// 2. Otherwise core plans the batch once, as core.commands.plan() does, and
//    hands its deltas to the caller's `extend`. Then it runs the batch as one
//    step of the user, with the commands that `extend` returns at its end.
// 3. Where the store or the command registry refuses the batch, done()
//    rejects with the refusal, and the advanced action stays as it is, so that
//    it shows why close to its submit. The notifier of done() takes the
//    batch's notifications, or else the caller's notifier, or else core raises
//    them.
// 4. Otherwise core unmounts the advanced action, and calls the function that
//    the place returned, with the deltas of the batch, or with none where it
//    ran none. The place then closes, or mounts the advanced action again, as
//    a place that the user pinned does.
// Where a board object that the target names leaves the board, core ends the
// advanced action as a done() without a batch does, so that no form edits
// what the board no longer holds. An input's target names its block.
// What the user triggered ends with the first end: the promise of the opening
// resolves then, which is what trigger() waits for. The `picked` and `filter`
// of the open icon, and the caller's `extend` and notifier, go to the
// advanced action on the target that it opened on until then. A pinned place
// that mounts it again, or mounts it on another target, gets it without them,
// so that no block lands where the user right-clicked for another.
//
// A place may pass mount() another target, on which core renders the board
// action where a menu of the target would offer it, as trigger() checks, and
// otherwise renders nothing. Each mount renders the module afresh, so that a
// module never sees its target change. Core renders nothing either where the
// board does not hold a board object that the target names.
//
// A plugin may render an advanced action in room of its own through
// mountAdvanced(). There done() runs the batch, without `extend`, and core
// renders the advanced action again, so that the plugin's picker is ready for
// the next block. Where the target leaves the board, core unmounts it and
// renders it no more.
//
// Every mount ends as the next board opens, and so does core's dialog.
// Where an advanced action throws in render(), core logs that as a fault of
// the board action's package and raises a notification of the code
// `module_failed`, about the plugin whose class lists the board action, and
// leaves the element empty.

import { attempt, onAbort } from "../mount/index.ts";
import type { Delta, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi, FrontendModule, Notification, Notifier } from "../types/core-api.ts";
import type { Rendered } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { log } from "../log/index.ts";
import { asError, coreNotification, errorMessage, givenNotifier, holdsObject, packageFromId, sourceModule } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import { isElement, targetGiven } from "./given.ts";
import { isBatch } from "./quick.ts";

export interface AdvancedModule {
  render(args: {
    el: HTMLElement;
    core: CoreApi;
    target: Target;
    // What the open icon handed it: the level's `picked` or the item's
    // `advanced`, and the text in the level's filter.
    picked?: Json;
    filter?: string;
    signal: AbortSignal;
    // Runs the batch as one step of the user and ends the advanced action, or
    // with no batch only ends it, and rejects where the batch is refused. It
    // needs no `this`, so that a module may take it out of its args.
    done: (calls?: readonly CommandCall[], opts?: { notifier?(notifications: readonly Notification[]): void }) => Promise<void>;
  }): Rendered;
}

// A place with enough room for an advanced action, as the `advanced` member
// of a frontend's module gives one, and as the caller of menu() or trigger()
// may give one in its place.
export type Place = NonNullable<FrontendModule["advanced"]>;
// What core hands a place.
export type PlaceArgs = Parameters<Place>[0];

// Shows an advanced action in core's dialog, as a place does, until the
// function that it returns runs, or until the signal aborts. The keyboard's
// focus returns to `el` as the dialog closes.
export type ShowDialog = (
  args: PlaceArgs,
  at: { readonly el: HTMLElement; readonly signal?: AbortSignal },
) => (deltas?: readonly Delta[]) => void;

// How a batch of a done() runs: with the caller's `extend`, named by the call
// that gave it, and with the notifier that takes the batch's notifications.
export interface Batching {
  readonly call?: string;
  readonly extend?: ((deltas: readonly Delta[]) => unknown) | undefined;
  readonly notifier?: Notifier | undefined;
}

export interface AdvancedParts {
  // The store of the open board, whose changes end an advanced action whose
  // target leaves the board.
  readonly store: Pick<Store, "board" | "hasBoard" | "subscribe">;
  // CoreApi, which each advanced action gets.
  readonly core: () => CoreApi;
  // Whether a menu of the target offers the board action now.
  readonly applies: (action: BoardAction, target: Target) => boolean;
  // Runs a batch of the board action as one step of the user, as menu() runs
  // the batch of a row, and resolves with the deltas that it planned for the
  // batch, as core.commands.plan() returns them.
  readonly run: (batch: readonly CommandCall[], batching: Batching) => Promise<readonly Delta[]>;
  // The place of the frontend that draws the board, where its module gives
  // `advanced`.
  readonly frontend: () => Place | undefined;
  readonly dialog: ShowDialog;
  // Raises one of core's notifications: here, that an advanced action failed
  // in render().
  readonly raise: (notification: Omit<Notification, "id">) => void;
  // The plugin class that lists the board action with the id given, or
  // undefined for one of core's own.
  readonly classOf: (id: string) => PluginClassId | undefined;
  // Aborts as the next board opens, which ends every advanced action.
  readonly signal?: AbortSignal;
}

// What opens an advanced action from a menu or a trigger: the element that the
// user triggered from, the caller's place, `extend` and notifier, and what the
// open icon hands the advanced action. The caller's place is the function that
// shows the advanced action, and what names the place, so that the place shows
// one advanced action at a time.
export interface Opening {
  readonly el: HTMLElement;
  readonly place?: { readonly show: Place; readonly key: unknown } | undefined;
  readonly extend?: ((deltas: readonly Delta[]) => unknown) | undefined;
  readonly call?: string;
  readonly notifier?: Notifier | undefined;
  readonly picked?: Json;
  readonly filter?: string;
}

export interface Advanced {
  // Shows the board action's advanced action on the target in its place, and
  // resolves once it first ends: once done() ends it, its place closes, or
  // another board opens.
  open(action: BoardAction, target: Target, opening: Opening): Promise<void>;
  // Renders the board action's advanced action in a plugin's room, as
  // CoreApi's mountAdvanced() does, until the function that it returns runs.
  mount(el: HTMLElement, action: BoardAction, target: Target, picked: Json | undefined): () => void;
}

// What names the place of core's dialog, which shows what neither a caller
// nor the frontend shows, and the frontend's place.
const DIALOG = Symbol("core's dialog");
const FRONTEND = Symbol("the frontend's place");

// An advanced action that a place shows, from the call that opened it until
// the place closes.
interface Shown {
  readonly action: BoardAction;
  readonly target: Target;
  readonly opening: Opening;
  // What shows it, core's dialog or a place, and what names its place.
  place: Place | typeof DIALOG;
  key: unknown;
  // What unmounts each module that the place mounted.
  readonly mounts: Set<() => void>;
  // Whether it has not ended yet, so that what the user triggered goes to it.
  first: boolean;
  closed: boolean;
  // What the place returned, which core calls once done() ends the advanced
  // action, and what it is to hear where the advanced action ended before
  // the place returned it.
  ended: ((deltas?: readonly Delta[]) => void) | undefined;
  early: { readonly deltas: readonly Delta[] | undefined } | undefined;
  // Resolves the promise of open().
  readonly settle: () => void;
}

// What runs a batch of a mounted advanced action's done(), and what follows.
type Finish = (calls: readonly CommandCall[] | undefined, notifier: Notifier | undefined) => Promise<void>;

export function createAdvanced(parts: AdvancedParts): Advanced {
  const { store } = parts;
  // The advanced action that each place shows, by what names the place.
  const places = new Map<unknown, Shown>();

  if (parts.signal !== undefined) {
    onAbort(parts.signal, () => {
      for (const shown of [...places.values()]) close(shown);
    });
  }

  // Renders the advanced action's module in the element, with the target and
  // what else it gets, until the function that it returns runs. The store's
  // changes call `left` where a board object that the target names leaves the
  // board while no done() runs, and so does a done() that the store refused
  // once the board no longer holds it.
  function render(
    el: HTMLElement,
    action: BoardAction,
    target: Target,
    given: { readonly picked?: Json; readonly filter?: string },
    finish: Finish,
    left: () => void,
  ): () => void {
    const module = action.advanced;
    const controller = new AbortController();
    const { signal } = controller;
    let cleanup: (() => void) | undefined;
    let pending = false;

    const gone = (): boolean => !signal.aborted && !pending && !held(target);
    const offStore = store.subscribe(() => {
      if (gone()) left();
    });
    const end = (): void => {
      if (signal.aborted) return;
      controller.abort();
      offStore();
      offBoard();
      if (cleanup !== undefined) attempt(cleanup);
      el.replaceChildren();
    };
    const offBoard = parts.signal === undefined ? () => undefined : onAbort(parts.signal, end);

    const done = (calls?: readonly CommandCall[], opts?: { notifier?(notifications: readonly Notification[]): void }): Promise<void> => {
      if (signal.aborted) {
        return Promise.reject(new Error(`The advanced action of the board action "${action.id}" has ended, so its done() runs nothing.`));
      }
      if (pending) {
        return Promise.reject(new Error(`The advanced action of the board action "${action.id}" runs a done() already.`));
      }
      let notifier: Notifier | undefined;
      try {
        notifier = givenNotifier(opts, "done()");
      } catch (error) {
        return Promise.reject(asError(error));
      }
      // A module in plain JS may give what is no batch, which the types refuse.
      const batch: unknown = calls;
      if (batch !== undefined && !isBatch(batch)) {
        return Promise.reject(
          new TypeError(
            `The done() of the advanced action of the board action "${action.id}" takes a batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }], or none.`,
          ),
        );
      }
      pending = true;
      return finish(batch, notifier).finally(() => {
        pending = false;
        if (gone()) left();
      });
    };

    // The cleanup that render() returns, at once or once it resolves, which
    // runs at once where core has unmounted the module by then.
    const took = (rendered: unknown): void => {
      if (typeof rendered !== "function") return;
      if (signal.aborted) attempt(rendered as () => void);
      else cleanup = rendered as () => void;
    };
    if (module !== undefined) {
      let rendered: Rendered;
      try {
        rendered = module.render({ el, core: parts.core(), target, ...given, signal, done });
      } catch (error) {
        failed(action, error);
        return end;
      }
      if (isThenable(rendered)) {
        Promise.resolve(rendered).then(took, (error: unknown) => {
          if (!signal.aborted) failed(action, error);
        });
      } else {
        took(rendered);
      }
    }
    return end;
  }

  // An advanced action that throws in render(), which is a fault of the
  // board action's package.
  function failed(action: BoardAction, error: unknown): void {
    const why = new Error(`The advanced action of the board action "${action.id}" failed in render(): ${errorMessage(error)}`, { cause: error });
    log.error(why);
    const plugin = parts.classOf(action.id);
    parts.raise(
      coreNotification("module_failed", "error", why.message, plugin === undefined ? {} : { module: sourceModule({ plugin }, packageFromId(plugin)) }),
    );
  }

  // Whether the board action renders on the target now: the board holds every
  // board object that the target names, and a menu of the target offers the
  // board action.
  function renders(action: BoardAction, target: Target): boolean {
    return parts.signal?.aborted !== true && held(target) && parts.applies(action, target);
  }

  // Whether the board holds every board object that the target names, the
  // board as a whole too, and the block of an input.
  function held(target: Target): boolean {
    if (!store.hasBoard()) return false;
    if ("board" in target) return true;
    const board = store.board();
    const plugins = parts.core().plugins.appPlugins();
    const objects: readonly BoardObject[] =
      "selection" in target ? target.selection : ["input" in target ? { block: target.block } : target];
    return objects.every((object) => holdsObject(board, object, plugins));
  }

  // The mount() that core hands a place: it renders the advanced action in
  // the element, on the target that it opened on, or on the one given.
  function mountIn(shown: Shown, el: unknown, other: unknown): () => void {
    if (!isElement(el)) {
      throw new TypeError("The `el` of mount() is the element in which core renders the advanced action.");
    }
    const target = other === undefined ? shown.target : targetGiven(other);
    if (shown.closed || !renders(shown.action, target)) return () => undefined;
    // What the user triggered goes to the target that it opened on alone.
    const own = sameTarget(target, shown.target);
    const { picked, filter } = shown.opening;
    const given = shown.first && own ? { ...(picked === undefined ? {} : { picked }), ...(filter === undefined ? {} : { filter }) } : {};
    const unmount = render(
      el,
      shown.action,
      target,
      given,
      (calls, notifier) => finishShown(shown, own, calls, notifier),
      () => {
        end(shown);
      },
    );
    shown.mounts.add(unmount);
    return () => {
      shown.mounts.delete(unmount);
      unmount();
    };
  }

  // A done() of an advanced action that a place shows: it runs the batch, if
  // any, and then ends the advanced action. The caller's `extend` and
  // notifier take part until the advanced action first ends, in a mount on
  // the target that it opened on, which `own` tells.
  async function finishShown(shown: Shown, own: boolean, calls: readonly CommandCall[] | undefined, notifier: Notifier | undefined): Promise<void> {
    if (calls === undefined || calls.length === 0) {
      end(shown);
      return;
    }
    const { opening } = shown;
    const triggered = shown.first && own;
    const deltas = await parts.run(calls, {
      ...(opening.call === undefined ? {} : { call: opening.call }),
      extend: triggered ? opening.extend : undefined,
      notifier: notifier ?? (triggered ? opening.notifier : undefined),
    });
    end(shown, deltas);
  }

  // Ends the advanced action that the place shows, as done() does: core
  // unmounts it, and tells the place, with the deltas of the batch that ran,
  // if any.
  function end(shown: Shown, deltas?: readonly Delta[]): void {
    if (shown.closed) return;
    unmountAll(shown);
    if (shown.first) {
      shown.first = false;
      shown.settle();
    }
    const { ended } = shown;
    if (ended === undefined) {
      shown.early = { deltas };
      return;
    }
    attempt(() => {
      if (deltas === undefined) ended();
      else ended(deltas);
    });
  }

  // Ends the advanced action that the place shows, without telling the place,
  // which closed, shows another or ended with the board.
  function close(shown: Shown): void {
    if (shown.closed) return;
    shown.closed = true;
    unmountAll(shown);
    if (shown.first) {
      shown.first = false;
      shown.settle();
    }
    if (places.get(shown.key) === shown) places.delete(shown.key);
  }

  function unmountAll(shown: Shown): void {
    const mounts = [...shown.mounts];
    shown.mounts.clear();
    for (const unmount of mounts) unmount();
  }

  // Hands the place what it shows the advanced action with, and returns what
  // the place returned. A place that throws is a fault of its package, which
  // core logs, and core's dialog shows the advanced action in its place.
  function showIn(shown: Shown, args: PlaceArgs): (deltas?: readonly Delta[]) => void {
    const { place } = shown;
    const at = { el: shown.opening.el, ...(parts.signal === undefined ? {} : { signal: parts.signal }) };
    if (place === DIALOG) return parts.dialog(args, at);
    let returned: unknown;
    try {
      returned = place(args);
    } catch (error) {
      log.error(
        new Error(`The place that was to show the advanced action of the board action "${args.id}" failed, so core's dialog shows it: ${errorMessage(error)}`, {
          cause: error,
        }),
      );
      if (places.get(shown.key) === shown) places.delete(shown.key);
      shown.place = DIALOG;
      shown.key = DIALOG;
      replace(shown);
      return parts.dialog(args, at);
    }
    if (typeof returned === "function") return returned as (deltas?: readonly Delta[]) => void;
    log.error(`The place that shows the advanced action of the board action "${args.id}" returned no function, which core calls once done() ends it.`);
    return () => undefined;
  }

  // Makes the advanced action the one that its place shows, ending the one
  // that the place showed before. Core's dialog closes for the one before,
  // since core shows each in a dialog of its own.
  function replace(shown: Shown): void {
    const before = places.get(shown.key);
    if (before !== undefined) {
      const { ended } = before;
      if (before.place === DIALOG && ended !== undefined) {
        attempt(() => {
          ended();
        });
      }
      close(before);
    }
    places.set(shown.key, shown);
  }

  return {
    open(action, target, opening) {
      if (parts.signal?.aborted === true || action.advanced === undefined) return Promise.resolve();
      return new Promise<void>((resolve) => {
        const caller = opening.place;
        const frontend = caller === undefined ? parts.frontend() : undefined;
        const place: Place | typeof DIALOG = caller?.show ?? frontend ?? DIALOG;
        const key: unknown = caller !== undefined ? caller.key : frontend !== undefined ? FRONTEND : DIALOG;
        const shown: Shown = {
          action,
          target,
          opening,
          place,
          key,
          mounts: new Set(),
          first: true,
          closed: false,
          ended: undefined,
          early: undefined,
          settle: resolve,
        };
        replace(shown);
        const args: PlaceArgs = Object.freeze({
          id: action.id,
          title: action.label,
          target,
          mount: (el: HTMLElement, other?: Target) => mountIn(shown, el, other),
          close: () => {
            close(shown);
          },
        });
        shown.ended = showIn(shown, args);
        const { early } = shown;
        if (early !== undefined && !shown.closed) {
          shown.early = undefined;
          const { ended } = shown;
          attempt(() => {
            if (early.deltas === undefined) ended();
            else ended(early.deltas);
          });
        }
      });
    },
    mount(el, action, target, picked) {
      if (!renders(action, target)) return () => undefined;
      const given = picked === undefined ? {} : { picked };
      let stopped = false;
      let current: (() => void) | undefined;
      const stop = (): void => {
        stopped = true;
        current?.();
        current = undefined;
      };
      const again = (): void => {
        current?.();
        current = undefined;
        if (stopped) return;
        if (!held(target)) {
          stop();
          return;
        }
        current = render(el, action, target, given, finish, stop);
      };
      const finish: Finish = async (calls, notifier) => {
        if (calls !== undefined && calls.length > 0) {
          await parts.run(calls, notifier === undefined ? {} : { notifier });
        }
        again();
      };
      again();
      return stop;
    },
  };
}

function sameTarget(a: Target, b: Target): boolean {
  return JSON.stringify(a) === JSON.stringify(b);
}

function isThenable(value: unknown): value is PromiseLike<unknown> {
  return (typeof value === "object" || typeof value === "function") && value !== null && typeof (value as { then?: unknown }).then === "function";
}
