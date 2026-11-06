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
import { createRegistrations, propertiesOf } from "./registrations.ts";
import type {
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
  // What the store dropped as it opened its board, as it told its followers
  // then, so that a front-end that draws the board once it is open tells the
  // user what to install. It is empty until the store opens its board.
  dropped(): readonly Dropped[];
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
  // and resolves with what it dropped.
  // A locked board opens no JSON board.
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
  // from: the package of each class that the board is built as, core's among
  // them, as the reply to `hello` gives it, and the package of each block's
  // class, which its registry id names.
  // It records the versions of the shapes that the board holds as well: of
  // each class that the board is built as that R names one for, and of each
  // block's state, as the class's entry in the block registry gives it.
  jsonBoard(): JsonBoard;
  // The text of the board's file: its JSON board, written with
  // JSON.stringify(), indented by two spaces and ending in a newline, as R
  // writes the fixtures of boards.
  // The store opens the text to the same board.
  // It saves nothing itself: core's basic front-end downloads the text, and
  // session management saves it where it decides.
  // It asks R for nothing, so a board saves while R is busy or gone.
  save(): string;

  // Reverts or applies again that origin's last step, or throws a StoreError
  // where another origin has since changed what the step changed.
  undo(origin?: Origin): void;
  redo(origin?: Origin): void;
  canUndo(origin?: Origin): boolean;
  canRedo(origin?: Origin): boolean;

  // Registers a board class, with the properties it adds.
  // What is registered holds core's board class "board" too.
  register(def: BoardClass): () => void;
  registered(): readonly BoardClass[];
  // The board class that a board of these classes is built as:
  // the first class in the vector that is registered, with its parents,
  // or core's "board" where none is.
  boardClass(classes: readonly string[]): BoardClass;
  // The id of the module that draws a board of these classes: the front-end
  // that R names for the class it is built as, as the reply to `hello` gives
  // it, or for that class's nearest parent that R names one for.
  // Core switches the page to it.
  frontEnd(classes: readonly string[]): string;
  // A block class's one migration of its state, by its registry id.
  registerMigration(registryId: RegistryId, fn: Migration): () => void;
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

// The store sends R nothing: it opens its board with what the reply to `hello`
// holds, and core has R drop the results of a board that another replaces with
// the `hello` with which it starts again.
export interface StoreOptions {
  // The reply to `hello` that the store opens a JSON board's text with, where
  // it opens no reply's board, as core opens the board given as it starts
  // again.
  readonly hello?: HelloReply;
  // What has to run before the store opens a board, given the reply to `hello`
  // and the board's class vector: importing the front-end that frontEnd()
  // gives for the vector. Which class the store builds the board as, and so
  // the board's front-end, is known only once the startup modules have run,
  // which define their board classes.
  readonly loaded?: (reply: HelloReply, classes: readonly string[]) => void | Promise<void>;
  // Ends the store as it aborts, as another board opens in a new store: the
  // store drops its subscribers and refuses every change from then on, so that
  // nothing changes or follows the board before.
  readonly signal?: AbortSignal;
}

// Why an ended store refuses a change.
const ENDED = "Another board opened in a new store, so this store changes its board no more.";

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

  const board = signal<Board | undefined>(undefined);
  // The class that the store built the board as, with its parents,
  // as it was registered when the board opened.
  let built: Built | undefined;
  // The last reply to `hello`, whose block registry and package versions the
  // store opens its board with, and whose lock keeps it from opening a JSON
  // board.
  let hello: HelloReply | undefined = options.hello;
  const registry = signal<Readonly<Record<RegistryId, RegistryEntry>>>(options.hello?.block_registry ?? {});
  const packages = signal<Readonly<Record<string, string>>>(options.hello?.packages ?? {});
  const history = signal<ReadonlyMap<Origin, History>>(new Map());
  let dropped: readonly Dropped[] = [];
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
  // that no opening made, or where the store has ended.
  function current(refused: string): { board: Board; built: Built } {
    if (ended()) throw new StoreError(refused, [ENDED]);
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

  const refusedOpen = "The store refused to open the board";

  // The JSON board given, with the reply to `hello` that the store opens it
  // with: a reply's opening board with that reply, which the store keeps as
  // its last, or a JSON board's text with the last reply, which a locked board
  // opens none of.
  function jsonOf(given: string | HelloReply): { json: JsonBoard; reply: HelloReply } {
    if (typeof given !== "string") {
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      return { json: checkJsonBoard(given.board), reply: given };
    }
    if (hello === undefined) {
      throw new StoreError(refusedOpen, ["The store opens a JSON board only once R has answered `hello`."]);
    }
    if (hello.lock !== null) {
      throw new StoreError(refusedOpen, ["The board is locked, so it opens no other board."]);
    }
    return { json: checkJsonBoard(parse(given)), reply: hello };
  }

  // The board that a JSON board opens to, as openJsonBoard() builds and
  // migrates it, with the values of the board options that its class
  // requires, whose defaults may have to wait, as one read from a blockr
  // option asks R for it.
  async function build(json: JsonBoard, reply: HelloReply): Promise<{ board: Board; built: Built; dropped: Dropped[] }> {
    guard();
    let opened: ReturnType<typeof openJsonBoard>;
    busy = true;
    try {
      opened = openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }
    const values = await required(opened.board, opened.built, registrations);
    const next: Board =
      Object.keys(values).length === 0
        ? opened.board
        : { ...opened.board, options: { ...opened.board.options, ...values } };
    return { board: next, built: opened.built, dropped: opened.dropped };
  }

  function stillOpen(): void {
    if (ended()) throw new StoreError(refusedOpen, [ENDED]);
  }

  async function open(given: string | HelloReply): Promise<Dropped[]> {
    stillOpen();
    if (opening) {
      throw new StoreError(refusedOpen, [
        "The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again.",
      ]);
    }
    opening = true;
    try {
      const { json, reply } = jsonOf(given);
      await options.loaded?.(reply, json.board.class);
      stillOpen();
      const opened = await build(json, reply);
      stillOpen();

      guard();
      busy = true;
      try {
        board.value = opened.board;
        built = opened.built;
        const step = ++lastStep;
        for (const key of everyTarget(opened.board, opened.built)) edits.set(key, step);
        dropped = opened.dropped;
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
  }

  async function checkText(text: string): Promise<Dropped[]> {
    stillOpen();
    const { json, reply } = jsonOf(text);
    const opened = await build(json, reply);
    stillOpen();
    return opened.dropped;
  }

  function jsonBoard(): JsonBoard {
    const next = board.value;
    if (next === undefined || built === undefined) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = hello?.board_classes ?? {};
    const chain = new Set(built.chain.map(({ def }) => def.class));
    const owned = new Set([
      ...built.chain.flatMap(({ def }) => own(classes, def.class)?.package ?? []),
      ...Object.values(next.blocks).flatMap(({ registry_id }) => packageOf(registry_id) ?? []),
    ]);
    const recorded = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === undefined ? [] : [[pkg, version]];
      }),
    );
    const versions = Object.fromEntries(
      next.class.flatMap((cls) => {
        const version = chain.has(cls) ? own(classes, cls)?.version : undefined;
        return version === undefined ? [] : [[cls, version]];
      }),
    );
    const blocks = Object.fromEntries(
      Object.entries(next.blocks).map(([id, block]) => [
        id,
        {
          registry_id: block.registry_id,
          version: own(registry.value, block.registry_id)?.version ?? 1,
          state: block.state,
          name: block.name,
        },
      ]),
    );
    const options = Object.fromEntries(
      Object.entries(next.options).map(([id, value]) => [
        id,
        { value, version: own(hello?.option_versions ?? {}, id) ?? 1 },
      ]),
    );
    const { id, class: classVector, name, links, stacks } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id, class: classVector, versions, name, blocks, links, stacks, options, ...propertiesOf(next) },
    };
  }

  function save(): string {
    return `${JSON.stringify(jsonBoard(), null, 2)}\n`;
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
    dropped: () => dropped,
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
    open,
    check: checkText,
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
    registerMigration: (registryId, fn) => registrations.registerMigration(registryId, fn),
    options: registrations.options,
    jsonBoard,
    save,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of)),
  };
}
