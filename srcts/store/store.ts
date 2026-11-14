// The store: the TypeScript object in core's JS API that holds a board and
// through which every change to it passes, as a delta, whoever makes it. There
// is one per board, which core creates and hands to plugins, a frontend among
// them, and modules through `CoreApi` and their hosts, rather than a global, so
// that tests, and a page that shows two boards one day, can create more than
// one.
//
// It derives what it holds with signals inside, while its getters return the
// same object until the change that replaces it is announced, as React's
// `useSyncExternalStore` needs.

import { signal } from "@preact/signals-core";
import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, PluginClassId, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { log } from "../log/index.ts";
import { asError, own, packageFromId } from "../util/index.ts";
import { check } from "./check.ts";
import { commit, target } from "./commit.ts";
import type { Changed, Inverse, Target } from "./commit.ts";
import { isDeltas } from "./delta.ts";
import type { Delta, Origin } from "./delta.ts";
import { StoreError } from "./error.ts";
import { expand } from "./expand.ts";
import { fixes, lockFailures } from "./lock.ts";
import { pluginClassFromPath } from "./patch.ts";
import { fit, isEmpty } from "./undo.ts";
import { checkJsonBoard, FORMAT, openJsonBoard, parse } from "./open.ts";
import type { Dropped } from "./open.ts";
import { createRegistrations } from "./registrations.ts";
import type { Migration, Options, Plugins } from "./registrations.ts";

export type Change =
  | { readonly delta: Delta; readonly origin: Origin }
  | { readonly opened: Board; readonly dropped: readonly Dropped[] };

export interface Store {
  // The current board, whose changes subscribe() announces. The store holds
  // none until it opens one, and throws until then.
  board(): Board;
  // Whether the store holds a board. It holds one once it has opened one.
  hasBoard(): boolean;
  // Whether a lock fixes the block's state: the board lock is on and the
  // block's `locked` is not false, so that a block whose `locked` is null
  // follows the board lock. A frontend reads it, rather than the block's
  // `locked`, to know which blocks a reader may still change.
  locked(block: BlockId): boolean;
  // Calls `fn` after every commit, once for each delta as expanded, and after
  // every opening of a board, with the board as it then is.
  subscribe(fn: (change: Change, board: Board) => void): () => void;

  // Expands, checks, commits and announces the deltas as one step of the
  // origin, "user" unless given, or throws a StoreError naming what rejected
  // them, having changed nothing. What changes nothing makes no step: a delta
  // that changes nothing is no delta, and a step keeps of the others only what
  // they change.
  // The step goes onto the end of the undo list with its origin, and empties
  // the redo list, whoever took the steps there.
  // A step that sets or removes only board option values is no undo step:
  // it leaves the undo and redo lists as they are.
  // Nor is a step applied with `{ undo: false }`, as the command registry
  // applies the deltas of a command with `undo: false`. The store applies and
  // announces such a step as any other, but keeps no inverse of it.
  // Under the board lock, it refuses every delta but one that changes the
  // state of a block that the board lock leaves open, a board option's value
  // or what sits inside a plugin's state, which the plugin locks itself
  // (./lock.ts).
  apply(delta: Delta | readonly Delta[], origin?: Origin, opts?: { readonly undo?: false }): void;
  // The board that apply() would leave, given the same deltas: it expands and
  // checks them as apply() does, or throws the StoreError that apply() would
  // throw, but commits nothing, announces nothing and keeps no undo step.
  // So it may run while the store applies or announces a change.
  // The command registry plans each command of a list on the board that a
  // dry run of the deltas of the commands before it gives (../commands).
  dryRun(delta: Delta | readonly Delta[]): Board;
  // Opens the opening board that R sent with its reply to `hello`, or a JSON
  // board given as its text, with the block registry of the last such reply,
  // and resolves with what it dropped.
  // Under the load lock, it opens no JSON board's text, so that the app shows
  // its opening board alone.
  // A store opens one board, and refuses another once it holds one. Through
  // `CoreApi`, open() starts core again in place with the board given, in a
  // new store, once the store on the page has checked the board
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board).
  open(given: string | HelloReply): Promise<Dropped[]>;
  // Opens a JSON board given as its text as open() would, with what is
  // registered and the last reply to `hello`, without holding it: it resolves
  // with what opening the board would drop, or rejects with the StoreError that
  // would refuse it, having changed nothing. It runs nothing that has to run
  // before an opening, so that core checks a board this way before it stops the
  // core on the page for it.
  check(text: string): Promise<Dropped[]>;
  // The board as a JSON board, as a file holds it.
  // It records the version of each package that the board's content comes
  // from, as the reply to `hello` gives it: the package of the class of each
  // plugin whose state it holds, which the class's id names, and the package
  // of each block's class, which its registry id names.
  // It records the versions of the shapes that the board holds as well: of
  // each plugin class whose plugin's state it holds, as the app's plugin of
  // the class carries it in the reply to `hello`, 1 where it gives none, and of
  // each block's state, as the class's entry in the block registry gives it.
  jsonBoard(): JsonBoard;
  // The text of the board's file: its JSON board, written with
  // JSON.stringify(), indented by two spaces and ending in a newline, as R
  // writes the fixtures of boards.
  // The store opens the text to the same board.
  // It saves nothing itself: core's basic frontend downloads the text, and
  // session management saves it where it decides.
  // It asks R for nothing, so a board saves while R is busy or gone.
  save(): string;

  // The store keeps one undo list and one redo list for the steps of every
  // origin, in the order the board took them, so that the user's Undo takes
  // back the assistant's edits as it takes back the user's own
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#undo).
  // With no origin, undo() takes back the undo list's last step,
  // whoever took it.
  // Given an origin, it takes back that origin's last step instead, past the
  // steps of others after it, as the assistant's tool `undo_edit` does.
  // Either way, the step goes onto the end of the redo list.
  // The method redo() applies again the redo list's last step, the one that
  // the last undo took back, and puts it back onto the end of the undo list.
  // A step keeps its origin through both, so that once redo() has applied
  // the assistant's step again, undo("assistant") still takes it back.
  // An undo or a redo is a change of the origin that asks for it, "user"
  // where none is given, so that the assistant hears the user's undo of its
  // edit as the user's.
  // Each is a new edit on the board as it is: it applies over what another
  // origin has changed since, the last writer winning, and does nothing where
  // what the step changed is gone.
  // Each throws a StoreError, having changed nothing, where the board it would
  // make fails a check, such as a fixed input that takes two links.
  // Neither changes a board option's value, since a board option never changes
  // what the board computes.
  // Whether there is a step to take back or to apply again: canUndo() says
  // whether the undo list holds a step, of the origin given if any, and
  // canRedo() whether the redo list holds one.
  undo(origin?: Origin): void;
  redo(): void;
  canUndo(origin?: Origin): boolean;
  canRedo(): boolean;

  // The plugin classes packages define, by id.
  readonly plugins: Plugins;
  // The board options packages define, by id.
  readonly options: Options;
  // The block registry's entries that the store opened the board with, and
  // the versions of the packages R loaded, from the reply to `hello`.
  blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
  packages(): Readonly<Record<string, string>>;

  // The edit id of a block or of a plugin, by the id of its class, which only
  // their own edits change and which a delta names to be refused where another
  // edit came first.
  editId(of: { readonly block: BlockId } | { readonly plugin: PluginClassId }): number | undefined;
}

// The store sends R nothing: it opens its board with what the reply to `hello`
// holds, and core has R drop the results of a board that another replaces with
// the `hello` with which it starts again.
export interface StoreOptions {
  // The reply to `hello` that the store opens a JSON board's text with, where
  // it opens no reply's board, as core opens the board given as it starts
  // again.
  readonly hello?: HelloReply;
  // Ends the store as it aborts, as another board opens in a new store: the
  // store drops its subscribers and refuses every change from then on, so that
  // nothing changes or follows the board before.
  readonly signal?: AbortSignal;
  // A block class's one migration of its state, by its registry id, which the
  // store reads as it opens or checks a board, and which core takes from the
  // class's UI module before then (../core/migrations.ts). A class without one
  // keeps the state that a board holds.
  readonly migration?: (registryId: RegistryId) => Migration | undefined;
}

// Why an ended store refuses a change.
const ENDED = "Another board opened in a new store, so this store changes its board no more.";

// What takes one step back, on the undo list, or applies it again, on the redo
// list: the origin that took the step, and the deltas to apply, each with
// where what it adds back goes, taken from the board the step replaced, so
// that applying them sets back what the step changed.
// The deltas hold no board option values (./commit.ts).
interface Step {
  readonly origin: Origin;
  readonly inverses: readonly Inverse[];
}

// The steps of every origin, in the order the board took them.
interface History {
  readonly undo: readonly Step[];
  readonly redo: readonly Step[];
}

interface Applied {
  readonly delta: Delta;
  readonly board: Board;
  readonly changed: Changed;
}

export function createStore(options: StoreOptions = {}): Store {
  const registrations = createRegistrations(options.migration);

  const board = signal<Board | undefined>(undefined);
  // The last reply to `hello`, whose block registry, versions and opening board
  // the store opens its board with, and whose locks say what the store
  // refuses: its load lock every JSON board's text, and its board lock every
  // delta but those it leaves open.
  let hello: HelloReply | undefined = options.hello;
  const registry = signal<Readonly<Record<RegistryId, RegistryEntry>>>(options.hello?.block_registry ?? {});
  const packages = signal<Readonly<Record<string, string>>>(options.hello?.packages ?? {});
  const history = signal<History>({ undo: [], redo: [] });
  const subscribers = new Set<(change: Change, board: Board) => void>();
  // Whether the store opens a board, or has opened one, which it does once.
  let opening = false;

  const ended = (): boolean => options.signal?.aborted === true;
  options.signal?.addEventListener(
    "abort",
    () => {
      subscribers.clear();
    },
    { once: true },
  );

  // The edit ids, kept beside the board rather than in it.
  // One count numbers the steps, the opening of a board,
  // an undo and a redo included, and the edit id of a block or of a plugin is
  // the number of the step that last changed it.
  // No number is given out twice, so an id names one version of what it
  // belongs to.
  const edits = new Map<string, number>();
  let lastStep = 0;

  // Whether the store is applying or announcing a change,
  // during which a subscriber, a plugin class's update or a migration may not
  // change the board.
  let busy = false;

  function guard(): void {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a plugin class's update or a migration may not change, since what follows from a delta is a plugin class's update.",
      );
    }
  }

  // The board the store holds, or a StoreError where it holds none yet, so
  // that nothing builds on a board that no opening made, or where the store
  // has ended.
  function current(refused: string): Board {
    if (ended()) throw new StoreError(refused, [ENDED]);
    if (board.value !== undefined) return board.value;
    throw new StoreError(refused, ["The store holds no board until it opens one."]);
  }

  function announce(change: Change, next: Board): void {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        log.error(asError(error));
      }
    }
  }

  // Applies deltas in order as one step, expanding and checking each on the
  // board the one before it left, or throws what rejected them. A delta that
  // changes nothing is no delta, which is neither applied nor announced. Undo
  // and redo apply only what still fits the board, and a delta of which
  // nothing does applies as nothing.
  // A delta that sets or removes only board option values is applied and
  // announced, but leaves no inverse, since an inverse holds no board option
  // values.
  function run(
    start: Board,
    deltas: readonly Delta[],
    how: { fit?: boolean; places?: readonly Inverse["places"][] },
    refused: string,
  ): { applied: Applied[]; inverses: Inverse[] } {
    const applied: Applied[] = [];
    const inverses: Inverse[] = [];
    let current = start;

    for (const [i, given] of deltas.entries()) {
      const prefix = deltas.length > 1 ? `Delta ${String(i + 1)}: ` : "";
      const fail = (failures: readonly string[]) =>
        new StoreError(refused, failures.map((failure) => prefix + failure));

      const delta = how.fit === true ? fit(given, current) : given;
      if (how.fit === true && isEmpty(delta)) continue;

      const expanded = expand(delta, current, registrations);
      if ("failures" in expanded) throw fail(expanded.failures);

      const committed = commit(current, expanded, how.places?.[i]);
      if ("failures" in committed) throw fail(committed.failures);
      if (hello?.lock.board === true) {
        const locked = lockFailures(current, committed.inverse.delta);
        if (locked.length > 0) throw fail(locked);
      }
      const undoable = !isEmpty(committed.inverse.delta);
      if (!undoable && committed.changed.options.size === 0) continue;

      const failures = check(
        committed.board,
        registry.value,
        registrations,
        committed.changed,
      );
      if (failures.length > 0) throw fail(failures);

      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      if (undoable) inverses.unshift(committed.inverse);
      current = committed.board;
    }

    return { applied, inverses };
  }

  // Whether the board holds what an edit id belongs to, a block or a plugin.
  function exists(next: Board, key: string): boolean {
    const [kind, id] = JSON.parse(key) as [string, string];
    return Object.hasOwn(kind === "block" ? next.blocks : next.plugins, id);
  }

  function describe(key: string): string {
    const [kind, id] = JSON.parse(key) as [string, string];
    return kind === "block" ? `block "${id}"` : `the plugin "${id}"`;
  }

  // The edit ids that the deltas name and that differ from those the board had
  // as the step started, since every delta of a step is built on that board.
  function named(deltas: readonly Delta[]): string[] {
    const failures: string[] = [];
    const compare = (key: string, edit: number | undefined) => {
      if (edit === undefined) return;
      const id = edits.get(key);
      if (id !== edit) {
        failures.push(
          id === undefined
            ? `The delta was built on edit ${String(edit)} of ${describe(key)}, which the board lacks.`
            : `The delta was built on edit ${String(edit)} of ${describe(key)}, which another edit has replaced with edit ${String(id)}.`,
        );
      }
    };
    for (const delta of deltas) {
      for (const [id, mod] of Object.entries(delta.blocks?.mod ?? {})) {
        compare(target({ block: id }), mod.edit);
      }
      for (const patch of delta.patches ?? []) {
        const plugin = pluginClassFromPath(patch.path);
        if (plugin !== undefined) compare(target({ plugin }), patch.edit);
      }
    }
    return [...new Set(failures)];
  }

  // Gives what the step changed the number of a new step, or drops the edit id
  // of what the step removed.
  function number(applied: readonly Applied[], last: Board): void {
    const step = ++lastStep;
    for (const key of new Set(applied.flatMap(({ changed }) => [...changed.targets]))) {
      if (exists(last, key)) edits.set(key, step);
      else edits.delete(key);
    }
  }

  // Runs the deltas as one step on the board the store holds, once the edit
  // ids that they name are those of that board, or throws what rejected them.
  // It commits nothing, so that apply() and dryRun() both build on it.
  function runOnBoard(deltas: readonly Delta[]): { start: Board; applied: Applied[]; inverses: Inverse[] } {
    const refused = "The store rejected the delta";
    const start = current(refused);
    const failures = named(deltas);
    if (failures.length > 0) throw new StoreError(refused, failures);
    return { start, ...run(start, deltas, {}, refused) };
  }

  function apply(
    delta: Delta | readonly Delta[],
    origin: Origin = "user",
    opts: { readonly undo?: false } = {},
  ): void {
    guard();
    const deltas: readonly Delta[] = isDeltas(delta) ? delta : [delta];
    if (deltas.length === 0) return;

    busy = true;
    try {
      const { start, applied, inverses } = runOnBoard(deltas);
      if (applied.length === 0) return;
      number(applied, applied.at(-1)?.board ?? start);
      if (inverses.length > 0 && opts.undo !== false) {
        history.value = { undo: [...history.value.undo, { origin, inverses }], redo: [] };
      }

      for (const { delta, board: next } of applied) {
        board.value = next;
        announce({ delta, origin }, next);
      }
    } finally {
      busy = false;
    }
  }

  function dryRun(delta: Delta | readonly Delta[]): Board {
    const deltas: readonly Delta[] = isDeltas(delta) ? delta : [delta];
    // The store is busy while the dry run calls the update of each plugin's
    // class, so that no update applies a delta. Afterwards the store is as
    // busy as before, since a subscriber may run a dry run while the store
    // announces a change.
    const was = busy;
    busy = true;
    try {
      const { start, applied } = runOnBoard(deltas);
      return applied.at(-1)?.board ?? start;
    } finally {
      busy = was;
    }
  }

  // Takes back the undo list's last step, or the origin's last step where an
  // undo names one, or applies again the redo list's last step, as a new edit
  // on the board as it is.
  // It keeps what would take that edit back on the other list, with the
  // step's origin, so that undoing and redoing leaves the board as it was,
  // whatever another origin changed in between.
  // It announces the edit as a change of the origin that asks for it, "user"
  // where none is given.
  function revert(back: boolean, origin?: Origin): void {
    guard();
    const { undo, redo } = history.value;
    const at = back ? lastOf(undo, origin) : redo.length - 1;
    const step = (back ? undo : redo)[at];
    if (step === undefined) return;

    busy = true;
    try {
      const refused = back ? "The store refused to undo the step" : "The store refused to redo the step";
      const start = current(refused);
      const { applied, inverses } = run(
        start,
        step.inverses.map(({ delta }) => delta),
        { fit: true, places: step.inverses.map(({ places }) => places) },
        refused,
      );
      number(applied, applied.at(-1)?.board ?? start);
      const kept: Step = { origin: step.origin, inverses };
      history.value = back
        ? { undo: [...undo.slice(0, at), ...undo.slice(at + 1)], redo: [...redo, kept] }
        : { undo: [...undo, kept], redo: redo.slice(0, -1) };

      const asking = origin ?? "user";
      for (const { delta, board: next } of applied) {
        board.value = next;
        announce({ delta, origin: asking }, next);
      }
    } finally {
      busy = false;
    }
  }

  const refusedOpen = "The store refused to open the board";

  // The JSON board given, with the reply to `hello` that the store opens it
  // with: a reply's opening board with that reply, which the store keeps as
  // its last, or a JSON board's text with the last reply, which the store
  // opens none of under the load lock.
  function prepareOpen(given: string | HelloReply): { json: JsonBoard; reply: HelloReply } {
    if (typeof given !== "string") {
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      return { json: checkJsonBoard(given.board), reply: given };
    }
    if (hello === undefined) {
      throw new StoreError(refusedOpen, ["The store opens a JSON board only once R has answered `hello`."]);
    }
    if (hello.lock.load) {
      throw new StoreError(refusedOpen, [
        "The load lock fixes the app to its opening board, so the store opens no other board.",
      ]);
    }
    return { json: checkJsonBoard(parse(given)), reply: hello };
  }

  // The board that a JSON board opens to, as openJsonBoard() migrates it and
  // gives it the state of the app's plugins, which the reply names.
  function build(json: JsonBoard, reply: HelloReply): { board: Board; dropped: Dropped[] } {
    guard();
    busy = true;
    try {
      return openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }
  }

  function stillOpen(): void {
    if (ended()) throw new StoreError(refusedOpen, [ENDED]);
  }

  function open(given: string | HelloReply): Promise<Dropped[]> {
    return settled(() => {
      stillOpen();
      if (opening) {
        throw new StoreError(refusedOpen, [
          "The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again.",
        ]);
      }
      opening = true;
      try {
        const { json, reply } = prepareOpen(given);
        const opened = build(json, reply);

        guard();
        busy = true;
        try {
          board.value = opened.board;
          const step = ++lastStep;
          for (const key of everyTarget(opened.board)) edits.set(key, step);
          announce({ opened: opened.board, dropped: opened.dropped }, opened.board);
          return opened.dropped;
        } finally {
          busy = false;
        }
      } catch (error) {
        // A board that the store refused leaves it free to open another.
        opening = board.value !== undefined;
        throw error;
      }
    });
  }

  function checkText(text: string): Promise<Dropped[]> {
    return settled(() => {
      stillOpen();
      const { json, reply } = prepareOpen(text);
      return build(json, reply).dropped;
    });
  }

  function jsonBoard(): JsonBoard {
    const next = board.value;
    if (next === undefined) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = Object.keys(next.plugins);
    const owned = new Set([
      ...classes.flatMap((cls) => packageFromId(cls) ?? []),
      ...Object.values(next.blocks).flatMap(({ registry_id }) => packageFromId(registry_id) ?? []),
    ]);
    const recorded = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === undefined ? [] : [[pkg, version]];
      }),
    );
    const versions = Object.fromEntries(
      classes.map((cls) => [cls, own(hello?.plugins ?? {}, cls)?.version ?? 1]),
    );
    const blocks = Object.fromEntries(
      Object.entries(next.blocks).map(([id, block]) => [
        id,
        {
          registry_id: block.registry_id,
          version: own(registry.value, block.registry_id)?.version ?? 1,
          state: block.state,
          name: block.name,
          locked: block.locked,
        },
      ]),
    );
    const options = Object.fromEntries(
      Object.entries(next.options).map(([id, value]) => [
        id,
        { value, version: own(hello?.options ?? {}, id)?.version ?? 1 },
      ]),
    );
    const { id, name, links, stacks, plugins } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id, versions, name, blocks, links, stacks, options, plugins },
    };
  }

  function save(): string {
    return `${JSON.stringify(jsonBoard(), null, 2)}\n`;
  }

  function everyTarget(next: Board): string[] {
    const targets: Target[] = [
      ...Object.keys(next.blocks).map((id) => ({ block: id })),
      ...Object.keys(next.plugins).map((id) => ({ plugin: id })),
    ];
    return targets.map(target);
  }

  return {
    board() {
      if (board.value === undefined) {
        throw new Error("The store holds no board until it opens one.");
      }
      return board.value;
    },
    hasBoard: () => board.value !== undefined,
    locked: (block) => fixes(board.value, block, hello?.lock.board === true),
    subscribe(fn) {
      // An ended store announces nothing more.
      if (ended()) return () => undefined;
      const wrapped = (change: Change, next: Board) => {
        fn(change, next);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    apply,
    dryRun,
    open,
    check: checkText,
    undo: (origin) => {
      revert(true, origin);
    },
    redo: () => {
      revert(false);
    },
    canUndo: (origin) => lastOf(history.value.undo, origin) >= 0,
    canRedo: () => history.value.redo.length > 0,
    plugins: registrations.plugins,
    options: registrations.options,
    jsonBoard,
    save,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of)),
  };
}

// The index of the list's last step, or of the origin's last step where one is
// given, or -1 where the list holds no such step.
function lastOf(steps: readonly Step[], origin: Origin | undefined): number {
  return origin === undefined ? steps.length - 1 : steps.map((step) => step.origin).lastIndexOf(origin);
}

// What `fn` returns, as a promise that rejects with what it throws, for a
// method whose interface gives a promise, as open() does, which starts core
// again through CoreApi.
function settled<T>(fn: () => T): Promise<T> {
  try {
    return Promise.resolve(fn());
  } catch (error) {
    return Promise.reject(asError(error));
  }
}
