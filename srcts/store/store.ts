// The store: the TypeScript object in core's JS API that holds a board and
// through which every change to it passes, as a delta, whoever makes it. There
// is one per board, which core creates and hands to front-ends, extensions and
// modules through `CoreApi` and their hosts, rather than a global, so that
// tests, and a page that shows two boards one day, can create more than one.
//
// It derives what it holds with signals inside, while its getters return the
// same object until the change that replaces it is announced, as React's
// `useSyncExternalStore` needs.

import { signal } from "@preact/signals-core";
import type {
  HelloReply,
  OpenReply,
} from "../protocol/messages.ts";
import type { Transport } from "../protocol/transport.ts";
import type {
  BlockId,
  Board,
  RegistryId,
  SavedBoard,
} from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { check } from "./check.ts";
import { commit, target } from "./commit.ts";
import type { Changed, Inverse } from "./commit.ts";
import { isValueDelta } from "./delta.ts";
import type { Delta, Origin } from "./delta.ts";
import { expand } from "./expand.ts";
import { fit, isEmpty } from "./undo.ts";
import { checkFile, FORMAT, frontEndOf, keyed, openFile, parse } from "./open.ts";
import type { Dropped } from "./open.ts";
import { createRegistrations } from "./registrations.ts";
import type {
  Attribute,
  Migration,
  Options,
  StateKey,
} from "./registrations.ts";
import { getAt, isObject, pathText, StoreError } from "./util.ts";

export type Change =
  | { readonly delta: Delta; readonly origin: Origin }
  | { readonly opened: Board; readonly dropped: readonly Dropped[] };

export interface Store {
  // The current board, whose changes subscribe() announces. The store holds
  // none until it opens one, and throws until then.
  board(): Board;
  // Calls `fn` after every commit, once for each delta as expanded, and after
  // every opening of a board, with the board as it then is.
  subscribe(fn: (change: Change, board: Board) => void): () => void;

  // Expands, checks, commits and announces the deltas as one step of the
  // origin, "user" unless given, or throws a StoreError naming what rejected
  // them, having changed nothing.
  apply(delta: Delta | readonly Delta[], origin?: Origin): void;
  // Opens a saved file, given as its text, having R load the packages it
  // names, or the opening board that R sent with its reply to `hello`, and
  // replaces the board, resolving with what it dropped.
  open(saved: string | HelloReply): Promise<Dropped[]>;

  // Reverts or applies again that origin's last step, or throws a StoreError
  // where another origin has since changed what the step changed.
  undo(origin?: Origin): void;
  redo(origin?: Origin): void;
  canUndo(origin?: Origin): boolean;
  canRedo(origin?: Origin): boolean;

  // A key of the front-end's state or an attribute, and what is registered.
  register(def: StateKey | Attribute): () => void;
  registered(): readonly (StateKey | Attribute)[];
  // A package's one migration of its saved state.
  registerMigration(pkg: string, fn: Migration): () => void;
  // The board options packages define, by id.
  readonly options: Options;

  // The block registry's entries that the store opened the board with, and
  // the versions of the packages R loaded, which a saved file records.
  blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
  packages(): Readonly<Record<string, string>>;

  // The edit id of a block, or of an entry or a value of the front-end's
  // state, which only their own edits change and which a delta names to be
  // refused where another edit came first.
  editId(
    of:
      | { readonly block: BlockId }
      | { readonly path: readonly string[]; readonly entry?: string },
  ): number | undefined;
}

export interface StoreOptions {
  // What the store sends R the `open` of a saved file through.
  readonly transport: Transport;
  // What has to run once R has loaded the packages that a board names and
  // before the store opens it, given R's reply and the front-end the board
  // opens with: initializing the startup modules of the packages R loaded,
  // which register their keys, attributes, board options and migrations, and
  // switching the page to that front-end.
  readonly loaded?: (reply: OpenReply, frontEnd: string) => void | Promise<void>;
}

// What takes one step back, on an origin's undo list, or applies it again, on
// its redo list: the deltas to apply, each with where what it adds back goes,
// taken from the board the step replaced, so that applying them sets back what
// the step changed.
type Step = readonly Inverse[];

interface History {
  readonly undo: readonly Step[];
  readonly redo: readonly Step[];
}

interface Applied {
  readonly delta: Delta;
  readonly board: Board;
  readonly changed: Changed;
}

export function createStore(options: StoreOptions): Store {
  const registrations = createRegistrations();
  registrations.register(visible);

  const board = signal<Board | undefined>(undefined);
  const registry = signal<Readonly<Record<RegistryId, RegistryEntry>>>({});
  const packages = signal<Readonly<Record<string, string>>>({});
  const history = signal<ReadonlyMap<Origin, History>>(new Map());
  const subscribers = new Set<(change: Change, board: Board) => void>();

  // The edit ids, kept beside the board rather than in it. One count numbers
  // the steps, the opening of a board, an undo and a redo included, and the
  // edit id of a block, or of an entry or a value of the front-end state, is
  // the number of the step that last changed it. No number is given out twice,
  // so an id names one version of what it belongs to.
  const edits = new Map<string, number>();
  let lastStep = 0;

  // Whether the store is applying or announcing a change, during which a
  // subscriber, an `expand`, a `check` or a migration may not change the board.
  let busy = false;

  function guard(): void {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a key's expand or check, or a migration may not change, since what follows from a delta is a key's expand.",
      );
    }
  }

  // The board the store holds, or a StoreError where it holds none yet, so
  // that nothing builds on a board that no opening made.
  function current(refused: string): Board {
    if (board.value !== undefined) return board.value;
    throw new StoreError(refused, ["The store holds no board until it opens one."]);
  }

  function announce(change: Change, next: Board): void {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        queueMicrotask(() => {
          throw error;
        });
      }
    }
  }

  function steps(origin: Origin): History {
    return history.value.get(origin) ?? { undo: [], redo: [] };
  }

  function setSteps(origin: Origin, steps: History): void {
    history.value = new Map([...history.value, [origin, steps]]);
  }

  // Applies deltas in order as one step, expanding and checking each on the
  // board the one before it left, or throws what rejected them. Undo and redo
  // apply only what still fits the board, and a delta of which nothing does
  // applies as nothing.
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

      const delta = how.fit === true ? fit(given, current, registrations) : given;
      if (how.fit === true && isEmpty(delta)) continue;

      const expanded = expand(delta, current, registrations);
      if ("failures" in expanded) throw fail(expanded.failures);

      const committed = commit(current, expanded, registrations, how.places?.[i]);
      if ("failures" in committed) throw fail(committed.failures);

      const failures = check(
        committed.board,
        registry.value,
        registrations,
        committed.changed,
      );
      if (failures.length > 0) throw fail(failures);

      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      inverses.unshift(committed.inverse);
      current = committed.board;
    }

    return { applied, inverses };
  }

  function exists(next: Board, key: string): boolean {
    const [kind, ...rest] = JSON.parse(key) as [string, ...unknown[]];
    if (kind === "block") return Object.hasOwn(next.blocks, rest[0] as string);
    const at = getAt(next.front_end.state, rest[0] as string[]);
    if (kind === "value") return at !== undefined;
    return isObject(at) && Object.hasOwn(at, rest[1] as string);
  }

  function describe(key: string): string {
    const [kind, ...rest] = JSON.parse(key) as [string, ...unknown[]];
    if (kind === "block") return `block "${String(rest[0])}"`;
    const path = pathText(rest[0] as string[]);
    if (kind === "value") return `the value of ${path}`;
    return `entry "${String(rest[1])}" of ${path}`;
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
      for (const { path, change } of delta.front_end?.state ?? []) {
        if (isValueDelta(change)) {
          compare(target({ path }), change.edit);
        } else {
          for (const [id, mod] of Object.entries(change.mod ?? {})) {
            compare(target({ path, entry: id }), mod.edit);
          }
        }
      }
    }
    return failures;
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

  function apply(delta: Delta | readonly Delta[], origin: Origin = "user"): void {
    guard();
    const deltas: readonly Delta[] = isDeltas(delta) ? delta : [delta];
    if (deltas.length === 0) return;

    busy = true;
    try {
      const refused = "The store rejected the delta";
      const start = current(refused);
      const failures = named(deltas);
      if (failures.length > 0) throw new StoreError(refused, failures);

      const { applied, inverses } = run(start, deltas, {}, refused);
      number(applied, applied.at(-1)?.board ?? start);
      setSteps(origin, { undo: [...steps(origin).undo, inverses], redo: [] });

      for (const { delta, board: next } of applied) {
        board.value = next;
        announce({ delta, origin }, next);
      }
    } finally {
      busy = false;
    }
  }

  // Takes the origin's last step back, or applies the last it took back again,
  // as a new edit on the board as it is, and keeps what would take that edit
  // back on the other list, so that undoing and redoing leaves the board as it
  // was, whatever another origin changed in between.
  function revert(origin: Origin, back: boolean): void {
    guard();
    const { undo, redo } = steps(origin);
    const step = back ? undo.at(-1) : redo.at(-1);
    if (step === undefined) return;

    busy = true;
    try {
      const refused = back ? "The store refused to undo the step" : "The store refused to redo the step";
      const start = current(refused);
      const { applied, inverses } = run(
        start,
        step.map(({ delta }) => delta),
        { fit: true, places: step.map(({ places }) => places) },
        refused,
      );
      number(applied, applied.at(-1)?.board ?? start);
      setSteps(
        origin,
        back
          ? { undo: undo.slice(0, -1), redo: [...redo, inverses] }
          : { undo: [...undo, inverses], redo: redo.slice(0, -1) },
      );

      for (const { delta, board: next } of applied) {
        board.value = next;
        announce({ delta, origin }, next);
      }
    } finally {
      busy = false;
    }
  }

  async function open(saved: string | HelloReply): Promise<Dropped[]> {
    let file: SavedBoard;
    let reply: OpenReply;

    if (typeof saved === "string") {
      file = checkFile(parse(saved));
      const answer = await options.transport.send({
        kind: "open",
        packages: Object.keys(file.packages),
      });
      if (answer.status !== "ok" || !("block_registry" in answer)) {
        throw new StoreError(
          "R refused to load the packages of the board",
          "conditions" in answer
            ? answer.conditions.map((condition) => condition.message)
            : [`R replied ${answer.status}.`],
        );
      }
      reply = answer;
    } else {
      reply = saved;
      // The board an app script built opens as the saved file it would be,
      // which R wrote with the versions of the packages it loaded, so that it
      // ends as the same board opened from a file ends.
      file =
        saved.file === undefined
          ? { format: FORMAT, packages: saved.packages, board: keyed(saved.board) }
          : checkFile(saved.file);
    }

    registry.value = reply.block_registry;
    packages.value = reply.packages;
    await options.loaded?.(reply, frontEndOf(file.board, reply.front_ends));

    guard();
    busy = true;
    try {
      const { board: next, dropped } = openFile(file, reply, registrations);

      board.value = next;
      edits.clear();
      const step = ++lastStep;
      for (const key of everyTarget(next)) edits.set(key, step);
      history.value = new Map();

      announce({ opened: next, dropped }, next);
      return dropped;
    } finally {
      busy = false;
    }
  }

  function everyTarget(next: Board): string[] {
    const keys = Object.keys(next.blocks).map((id) => target({ block: id }));
    for (const { def } of registrations.keys()) {
      const value = getAt(next.front_end.state, def.path);
      if (value === undefined) continue;
      if (def.kind === "value") {
        keys.push(target({ path: def.path }));
      } else if (isObject(value)) {
        for (const id of Object.keys(value)) keys.push(target({ path: def.path, entry: id }));
      }
    }
    return keys;
  }

  return {
    board() {
      if (board.value === undefined) {
        throw new Error("The store holds no board until it opens one.");
      }
      return board.value;
    },
    subscribe(fn) {
      const wrapped = (change: Change, next: Board) => {
        fn(change, next);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    apply,
    open,
    undo: (origin = "user") => {
      revert(origin, true);
    },
    redo: (origin = "user") => {
      revert(origin, false);
    },
    canUndo: (origin = "user") => steps(origin).undo.length > 0,
    canRedo: (origin = "user") => steps(origin).redo.length > 0,
    register: (def) => registrations.register(def),
    registered: () => registrations.registered(),
    registerMigration: (pkg, fn) => registrations.registerMigration(pkg, fn),
    options: registrations.options,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of)),
  };
}

// Core's attribute, which says which of a block's sections show, its inputs
// and its outputs, and which a locked board reads.
const visible: Attribute = {
  name: "visible",
  on: "block",
  schema: {
    type: "array",
    items: { enum: ["inputs", "outputs"] },
    uniqueItems: true,
    description: "Which of the block's sections show, its inputs and its outputs.",
  },
  initial: ["inputs", "outputs"],
};

function isDeltas(delta: Delta | readonly Delta[]): delta is readonly Delta[] {
  return Array.isArray(delta);
}
