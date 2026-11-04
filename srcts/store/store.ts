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
import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own, packageOf, report } from "../util/index.ts";
import { check } from "./check.ts";
import { commit, target } from "./commit.ts";
import type { Changed, Inverse, Target } from "./commit.ts";
import { isDeltas } from "./delta.ts";
import type { Delta, Origin } from "./delta.ts";
import { StoreError } from "./error.ts";
import { expand } from "./expand.ts";
import { pointer } from "./patch.ts";
import { fit, isEmpty } from "./undo.ts";
import { checkJsonBoard, FORMAT, openJsonBoard, parse, required } from "./open.ts";
import type { Dropped } from "./open.ts";
import { createRegistrations } from "./registrations.ts";
import type {
  Attribute,
  BoardClass,
  Built,
  Migration,
  Options,
} from "./registrations.ts";

export type Change =
  | { readonly delta: Delta; readonly origin: Origin }
  | { readonly opened: Board; readonly dropped: readonly Dropped[] };

export interface Store {
  // The current board, whose changes subscribe() announces. The store holds
  // none until it opens one, and throws until then.
  board(): Board;
  // Whether the store holds a board. It holds one once it has opened one.
  hasBoard(): boolean;
  // Calls `fn` after every commit, once for each delta as expanded, and after
  // every opening of a board, with the board as it then is.
  subscribe(fn: (change: Change, board: Board) => void): () => void;

  // Expands, checks, commits and announces the deltas as one step of the
  // origin, "user" unless given, or throws a StoreError naming what rejected
  // them, having changed nothing. What changes nothing makes no step: a delta
  // that changes nothing is no delta, and a step keeps of the others only what
  // they change.
  apply(delta: Delta | readonly Delta[], origin?: Origin): void;
  // Opens the opening board that R sent with its reply to `hello`, or a JSON
  // board given as its text, with the block registry of the last such reply,
  // and replaces the board, resolving with what it dropped.
  // A locked board opens no JSON board.
  open(given: string | HelloReply): Promise<Dropped[]>;
  // The board as a JSON board, as a file holds it, which records the version
  // of each package that the board's content comes from: the package of each
  // class that the board is built as, core's among them, as the reply to
  // `hello` gives it, and the package of each block's class, which its
  // registry id names.
  jsonBoard(): JsonBoard;

  // Reverts or applies again that origin's last step, or throws a StoreError
  // where another origin has since changed what the step changed.
  undo(origin?: Origin): void;
  redo(origin?: Origin): void;
  canUndo(origin?: Origin): boolean;
  canRedo(origin?: Origin): boolean;

  // Registers a board class, with the properties it adds,
  // or an attribute of blocks.
  // What is registered holds core's board class "board" and its attribute
  // "visible" too.
  register(def: BoardClass | Attribute): () => void;
  registered(): readonly (BoardClass | Attribute)[];
  // The board class that a board of these classes is built as:
  // the first class in the vector that is registered, with its parents,
  // or core's "board" where none is.
  boardClass(classes: readonly string[]): BoardClass;
  // The id of the module that draws a board of these classes: the front-end
  // that R names for the class it is built as, as the reply to `hello` gives
  // it, or for that class's nearest parent that R names one for.
  // Core switches the page to it.
  frontEnd(classes: readonly string[]): string;
  // A package's one migration of its saved state.
  registerMigration(pkg: string, fn: Migration): () => void;
  // The board options packages define, by id.
  readonly options: Options;

  // The block registry's entries that the store opened the board with, and
  // the versions of the packages R loaded, from the reply to `hello`.
  blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
  packages(): Readonly<Record<string, string>>;

  // The edit id of a block, or of a property that the board's class adds,
  // which only their own edits change and which a delta names to be refused
  // where another edit came first.
  editId(of: { readonly block: BlockId } | { readonly property: string }): number | undefined;
}

// The store sends R nothing: it opens every board with what the reply to
// `hello` holds, and the scheduler has R forget the blocks of a board that
// another replaces.
export interface StoreOptions {
  // What has to run before the store opens a board, given the reply to `hello`
  // and the board's class vector:
  // initializing the startup modules of the packages R loaded that it has not
  // initialized yet,
  // which define their board classes and register their attributes,
  // board options and migrations, and switching the page to the front-end
  // that frontEnd() gives for the vector.
  // Which class the store builds the board as, and so the board's front-end,
  // is known only once the startup modules have run.
  readonly loaded?: (reply: HelloReply, classes: readonly string[]) => void | Promise<void>;
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

export function createStore(options: StoreOptions = {}): Store {
  const registrations = createRegistrations();
  registrations.register(visible);

  const board = signal<Board | undefined>(undefined);
  // The class that the store built the board as, with its parents,
  // as it was registered when the board opened.
  let built: Built | undefined;
  // The last reply to `hello`, whose block registry and package versions the
  // store opens every board with, and whose lock keeps it from opening one.
  let hello: HelloReply | undefined;
  const registry = signal<Readonly<Record<RegistryId, RegistryEntry>>>({});
  const packages = signal<Readonly<Record<string, string>>>({});
  const history = signal<ReadonlyMap<Origin, History>>(new Map());
  const subscribers = new Set<(change: Change, board: Board) => void>();

  // The edit ids, kept beside the board rather than in it.
  // One count numbers the steps, the opening of a board,
  // an undo and a redo included, and the edit id of a block,
  // or of a property of the board's class, is the number of the step that last
  // changed it.
  // No number is given out twice, so an id names one version of what it
  // belongs to.
  const edits = new Map<string, number>();
  let lastStep = 0;

  // Whether the store is applying or announcing a change,
  // during which a subscriber, a board class's update or a migration may not
  // change the board.
  let busy = false;

  function guard(): void {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a board class's update or a migration may not change, since what follows from a delta is a board class's update.",
      );
    }
  }

  // The board the store holds, with the class it built the board as,
  // or a StoreError where it holds none yet, so that nothing builds on a board
  // that no opening made.
  function current(refused: string): { board: Board; built: Built } {
    if (board.value !== undefined && built !== undefined) return { board: board.value, built };
    throw new StoreError(refused, ["The store holds no board until it opens one."]);
  }

  function announce(change: Change, next: Board): void {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        report(error);
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
  // board the one before it left, or throws what rejected them. A delta that
  // changes nothing is no delta, which is neither applied nor announced. Undo
  // and redo apply only what still fits the board, and a delta of which
  // nothing does applies as nothing.
  function run(
    start: Board,
    as: Built,
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

      const delta = how.fit === true ? fit(given, current, as) : given;
      if (how.fit === true && isEmpty(delta)) continue;

      const expanded = expand(delta, current, as);
      if ("failures" in expanded) throw fail(expanded.failures);

      const committed = commit(current, expanded, as, how.places?.[i]);
      if ("failures" in committed) throw fail(committed.failures);
      if (isEmpty(committed.inverse.delta)) continue;

      const failures = check(
        committed.board,
        registry.value,
        registrations,
        as,
        committed.changed,
      );
      if (failures.length > 0) throw fail(failures);

      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      inverses.unshift(committed.inverse);
      current = committed.board;
    }

    return { applied, inverses };
  }

  // Whether the board holds what an edit id belongs to: a block,
  // since a board holds every property of its class.
  function exists(next: Board, key: string): boolean {
    const [kind, id] = JSON.parse(key) as [string, string];
    return kind !== "block" || Object.hasOwn(next.blocks, id);
  }

  function describe(key: string): string {
    const [kind, id] = JSON.parse(key) as [string, string];
    return kind === "block" ? `block "${id}"` : `the property "${id}"`;
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
        const property = pointer(patch.path)?.[0];
        if (property !== undefined) compare(target({ property }), patch.edit);
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

  function apply(delta: Delta | readonly Delta[], origin: Origin = "user"): void {
    guard();
    const deltas: readonly Delta[] = isDeltas(delta) ? delta : [delta];
    if (deltas.length === 0) return;

    busy = true;
    try {
      const refused = "The store rejected the delta";
      const { board: start, built: as } = current(refused);
      const failures = named(deltas);
      if (failures.length > 0) throw new StoreError(refused, failures);

      const { applied, inverses } = run(start, as, deltas, {}, refused);
      if (applied.length === 0) return;
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
      const { board: start, built: as } = current(refused);
      const { applied, inverses } = run(
        start,
        as,
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

  async function open(given: string | HelloReply): Promise<Dropped[]> {
    let json: JsonBoard;
    let reply: HelloReply;

    if (typeof given === "string") {
      const refused = "The store refused to open the board";
      if (hello === undefined) {
        throw new StoreError(refused, ["The store opens a JSON board only once R has answered `hello`."]);
      }
      if (hello.lock !== null) {
        throw new StoreError(refused, ["The board is locked, so it opens no other board."]);
      }
      reply = hello;
      json = checkJsonBoard(parse(given));
    } else {
      reply = given;
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      json = checkJsonBoard(given.board);
    }

    await options.loaded?.(reply, json.board.class);

    guard();
    let opened: ReturnType<typeof openJsonBoard>;
    busy = true;
    try {
      opened = openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }

    // The board options that its class requires, whose defaults may have to
    // wait, as one read from a blockr option asks R for it.
    const values = await required(opened.board, opened.built, registrations);

    guard();
    busy = true;
    try {
      const { dropped } = opened;
      const next: Board =
        Object.keys(values).length === 0
          ? opened.board
          : { ...opened.board, options: { ...opened.board.options, ...values } };

      board.value = next;
      built = opened.built;
      edits.clear();
      const step = ++lastStep;
      for (const key of everyTarget(next, opened.built)) edits.set(key, step);
      history.value = new Map();

      announce({ opened: next, dropped }, next);
      return dropped;
    } finally {
      busy = false;
    }
  }

  function jsonBoard(): JsonBoard {
    if (board.value === undefined || built === undefined) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = hello?.board_classes ?? {};
    const owned = new Set([
      ...built.chain.flatMap(({ def }) => own(classes, def.class)?.package ?? []),
      ...Object.values(board.value.blocks).flatMap(({ registry_id }) => packageOf(registry_id) ?? []),
    ]);
    const versions = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === undefined ? [] : [[pkg, version]];
      }),
    );
    return { format: FORMAT, packages: versions, board: board.value };
  }

  function frontEnd(classes: readonly string[]): string {
    if (hello === undefined) {
      throw new Error("The store knows the front-ends of board classes only once R has answered `hello`.");
    }
    const { chain } = registrations.builtAs(classes);
    for (const { def } of [...chain].reverse()) {
      const named = own(hello.board_classes, def.class)?.front_end;
      if (named !== undefined) return named;
    }
    throw new Error(
      `R names no front-end for the board class "${chain.at(-1)?.def.class ?? ""}" or for its parents.`,
    );
  }

  function everyTarget(next: Board, as: Built): string[] {
    const targets: Target[] = [
      ...Object.keys(next.blocks).map((id) => ({ block: id })),
      ...[...as.properties.keys()].map((property) => ({ property })),
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
    boardClass: (classes) => registrations.builtAs(classes).def,
    frontEnd,
    registerMigration: (pkg, fn) => registrations.registerMigration(pkg, fn),
    options: registrations.options,
    jsonBoard,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of)),
  };
}

// Core's attribute, which says which of a block's sections show, its inputs
// and its outputs, and which a locked board reads.
const visible: Attribute = {
  name: "visible",
  schema: {
    type: "array",
    items: { enum: ["inputs", "outputs"] },
    uniqueItems: true,
    description: "Which of the block's sections show, its inputs and its outputs.",
  },
  initial: ["inputs", "outputs"],
};
