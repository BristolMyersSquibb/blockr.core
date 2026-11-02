// Opening a board, which replaces the board the store holds: a saved file, or
// the opening board that R sends with its reply to `hello`, which the store
// opens as the saved file it would be. Every board goes the same way from
// there, so that it ends as the same board whichever way it came in: the store
// runs the migration of every package whose saved version differs from the
// one R loaded, drops and names what the app cannot rebuild, and gives the
// board what packages register for a board that lacks it.

import type { OpenReply } from "../protocol/messages.ts";
import type { Board, OpeningBoard, SavedBoard } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { check } from "./check.ts";
import { commit } from "./commit.ts";
import { expand } from "./expand.ts";
import { mintId, sentenceCase } from "./ids.ts";
import type { Checked, Registrations, Saved } from "./registrations.ts";
import {
  frozenCopy,
  getAt,
  isObject,
  messageOf,
  own,
  packageOf,
  setAt,
  StoreError,
  without,
} from "./util.ts";

// The highest format of a saved file that this client reads and writes.
export const FORMAT = 1;

// Core's basic front-end, which draws a board made for a front-end that R
// serves no module for.
export const BASIC_FRONT_END = "blockr.core-basic";

// What the store dropped as it opened a board, since the app cannot rebuild
// it, each with the package to install where its id or path gives one.
export type Dropped =
  | {
      readonly block: string;
      readonly registry_id: string;
      readonly package: string | null;
    }
  | { readonly front_end: string; readonly package: string | null }
  | { readonly key: readonly string[]; readonly package: string | null }
  | {
      readonly attribute: string;
      readonly on: "block" | "front_end";
      readonly package: null;
    }
  | { readonly option: string; readonly package: null };

const refused = "The store refused to open the board";

// A saved file's text, as JSON.parse() reads it.
export function parse(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The file holds no JSON: ${messageOf(error)}`]);
  }
}

// A saved file, refusing a format above any this client knows, and a file of
// today's stack, which has none.
export function checkFile(file: unknown): SavedBoard {
  if (!isObject(file)) throw new StoreError(refused, ["The file holds no saved board."]);
  if (!Object.hasOwn(file, "format")) {
    throw new StoreError(refused, [
      "The file has no format, as a file of today's stack has none, which a conversion outside core turns into a saved file first.",
    ]);
  }
  if (typeof file.format !== "number" || !Number.isInteger(file.format)) {
    throw new StoreError(refused, [`The file's format ${JSON.stringify(file.format)} is no whole number.`]);
  }
  if (file.format > FORMAT) {
    throw new StoreError(refused, [
      `The file's format is ${String(file.format)}, while this client reads format ${String(FORMAT)} at most.`,
    ]);
  }
  if (!isObject(file.packages) || !Object.values(file.packages).every((v) => typeof v === "string")) {
    throw new StoreError(refused, ["The file names no packages with their versions."]);
  }
  if (!isObject(file.board)) throw new StoreError(refused, ["The file holds no board."]);
  return file as unknown as SavedBoard;
}

// The opening board that an app script built, with its collections keyed by
// id and each id it lacks minted, and named after its id where it has no name.
export function keyed(opening: OpeningBoard): Board {
  const failures: string[] = [];

  function collection<T extends { readonly id?: string }>(
    what: string,
    entries: readonly T[],
  ): Readonly<Record<string, Omit<T, "id">>> {
    const given = new Set<string>();
    for (const entry of entries) {
      if (entry.id === undefined) continue;
      if (given.has(entry.id)) {
        failures.push(`The board holds two ${what}s with the id "${entry.id}".`);
      }
      given.add(entry.id);
    }
    const taken = (id: string) => given.has(id);
    return Object.fromEntries(
      entries.map(({ id, ...entry }) => {
        const key = id ?? mintId(taken);
        given.add(key);
        return [key, entry];
      }),
    );
  }

  const blocks = collection("block", opening.blocks);
  const links = collection("link", opening.links);
  const stacks = collection("stack", opening.stacks);

  if (failures.length > 0) throw new StoreError(refused, failures);

  const id = opening.id ?? mintId(() => false);
  return {
    id,
    front_end: opening.front_end,
    name: opening.name ?? sentenceCase(id),
    blocks,
    links,
    stacks,
    options: opening.options,
  };
}

// What the file holds that migrations convert, as each package's migration
// reads it.
function saved(board: Board): Saved {
  return frozenCopy({
    front_end: { state: board.front_end.state as Json },
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        { registry_id: block.registry_id, state: block.state },
      ]),
    ),
  });
}

// The package whose migration converts what a key of the front-end state
// holds: the one whose extension the key's path names by its id, or else the
// package of the front-end the board is made for.
function migratedBy(path: readonly string[], board: Board): string | null {
  return path.map(packageOf).find((pkg) => pkg !== null) ?? packageOf(board.front_end.id);
}

// The board after the migration of every package whose version the file
// records differs from the one R loaded, a package the board holds nothing of
// included, each on the same saved state, of which the store takes the states
// of the blocks of the package's block classes and the front-end state that
// the package migrates. What a migration leaves out of its return stays as
// the file holds it.
function migrate(
  board: Board,
  versions: Readonly<Record<string, string>>,
  loaded: Readonly<Record<string, string>>,
  registrations: Registrations,
): Board {
  const before = saved(board);
  let next = board;

  for (const [pkg, migration] of registrations.migrations()) {
    const version = own(versions, pkg) ?? null;
    if (version === own(loaded, pkg)) continue;

    let out: Partial<Saved>;
    try {
      out = migration(before, version);
    } catch (error) {
      throw new StoreError(refused, [`The migration of ${pkg} failed: ${messageOf(error)}`]);
    }

    let state = next.front_end.state;
    if (out.front_end !== undefined) {
      for (const { def } of registrations.keys()) {
        if (migratedBy(def.path, board) !== pkg) continue;
        const value = getAt(out.front_end.state, def.path);
        if (value !== undefined) state = setAt(state, def.path, value);
      }
    }

    const blocks = { ...next.blocks };
    for (const [id, block] of Object.entries(next.blocks)) {
      const state = own(out.blocks ?? {}, id)?.state;
      if (state !== undefined && packageOf(block.registry_id) === pkg) {
        blocks[id] = { ...block, state };
      }
    }

    next = { ...next, blocks, front_end: { ...next.front_end, state } };
  }

  return next;
}

// The board that a saved file opens to, with what opening it dropped, or a
// StoreError naming what refused it.
export function openFile(
  file: SavedBoard,
  reply: OpenReply,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const migrated = migrate(file.board, file.packages, reply.packages, registrations);

  const gone = Object.entries(migrated.blocks)
    .filter(([, block]) => own(reply.block_registry, block.registry_id) === undefined)
    .map(([id]) => id);
  const blocks: Dropped[] = gone.map((id) => {
    const registryId = migrated.blocks[id]?.registry_id ?? "";
    return { block: id, registry_id: registryId, package: packageOf(registryId) };
  });

  const { board: pruned, dropped } = prune(migrated, gone, reply.front_ends, registrations);
  const board = remove(complete(pruned, registrations), gone, registrations);

  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);

  const [frontEnd, ...rest] = dropped;
  return {
    board,
    dropped:
      frontEnd !== undefined && "front_end" in frontEnd
        ? [frontEnd, ...blocks, ...rest]
        : [...blocks, ...dropped],
  };
}

// The front-end a board opens with: the one it names, or core's basic one
// where R serves no module for that.
export function frontEndOf(
  board: Board,
  frontEnds: Readonly<Record<string, unknown>>,
): string {
  return own(frontEnds, board.front_end.id) === undefined
    ? BASIC_FRONT_END
    : board.front_end.id;
}

// The board without every key of the front-end state, attribute and board
// option value that no package registered, and every attribute and board
// option value that its schema refuses, as one an older version of its
// package saved, which no migration converts, so that it reads as its initial
// value or default. It is made for the front-end it opens with, and
// what it dropped is named. The blocks that go keep what they hold, which goes
// with them.
function prune(
  board: Board,
  gone: readonly string[],
  frontEnds: Readonly<Record<string, unknown>>,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const dropped: Dropped[] = [];

  let frontEnd = board.front_end;
  const id = frontEndOf(board, frontEnds);
  if (id !== frontEnd.id) {
    dropped.push({ front_end: frontEnd.id, package: packageOf(frontEnd.id) });
    frontEnd = { ...frontEnd, id };
  }
  const owner = packageOf(board.front_end.id);

  // An attribute or a board option value that no package registered, or that
  // its schema refuses.
  const unfit = (
    values: Readonly<Record<string, Json>>,
    registered: (name: string) => Checked<unknown> | undefined,
  ): string[] =>
    Object.entries(values)
      .filter(([name, value]) => {
        const def = registered(name);
        return def === undefined || def.validate(value).length > 0;
      })
      .map(([name]) => name);

  const blockAttrs = new Set<string>();
  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id, block]) => {
      if (gone.includes(id)) return [id, block];
      const unknown = unfit(block.attrs, (name) => registrations.attribute("block", name));
      for (const name of unknown) blockAttrs.add(name);
      return [id, unknown.length === 0 ? block : { ...block, attrs: without(block.attrs, unknown) }];
    }),
  );
  for (const name of blockAttrs) dropped.push({ attribute: name, on: "block", package: null });

  // A dropped key is named with the package its path gives, and where none
  // does, with that of each entry it holds by an id that gives one, as the
  // dock keeps its extensions' state by their ids, or else with the package
  // of the front-end the board was made for.
  const name = (path: readonly string[], value: unknown): void => {
    const pkg = path.map(packageOf).find((pkg) => pkg !== null);
    const ids = isObject(value) ? Object.keys(value) : [];
    if (pkg !== undefined || !ids.some((id) => packageOf(id) !== null)) {
      dropped.push({ key: path, package: pkg ?? owner });
      return;
    }
    for (const id of ids) {
      dropped.push({ key: [...path, id], package: packageOf(id) ?? owner });
    }
  };
  const paths = registrations.keys().map(({ def }) => def.path);
  const state = pruneState(frontEnd.state, [], paths, name);

  const unknownAttrs = unfit(frontEnd.attrs, (name) => registrations.attribute("front_end", name));
  for (const name of unknownAttrs) dropped.push({ attribute: name, on: "front_end", package: null });

  const unknownOptions = unfit(board.options, (id) => registrations.option(id));
  for (const id of unknownOptions) dropped.push({ option: id, package: null });

  return {
    board: {
      ...board,
      front_end: { ...frontEnd, state, attrs: without(frontEnd.attrs, unknownAttrs) },
      blocks,
      options: without(board.options, unknownOptions),
    },
    dropped,
  };
}

// The board without the blocks, removed as a delta removes them, so that what
// follows from removing a block goes with it: its links and its places in
// stacks, and what each key of the front-end's state keeps about it.
function remove(
  board: Board,
  gone: readonly string[],
  registrations: Registrations,
): Board {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, registrations);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta, registrations);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}

// The state without what lies at no registered key's path, keeping the objects
// on the way to one.
function pruneState(
  state: Readonly<Record<string, unknown>>,
  at: readonly string[],
  paths: readonly (readonly string[])[],
  dropped: (path: readonly string[], value: unknown) => void,
): Readonly<Record<string, unknown>> {
  let next = state;
  for (const [key, value] of Object.entries(state)) {
    const path = [...at, key];
    const below = paths.filter((other) => path.every((part, i) => other[i] === part));
    if (below.some((other) => other.length === path.length)) continue;
    if (below.length > 0 && isObject(value)) {
      const pruned = pruneState(value, path, below, dropped);
      if (pruned !== value) next = { ...next, [key]: pruned };
      continue;
    }
    dropped(path, value);
    next = without(next, [key]);
  }
  return next;
}

// The board with each registered key that it lacks, a collection empty and a
// value as its initial. A board option it holds no value of needs nothing
// here: whatever needs one takes the `default` of its definition.
function complete(board: Board, registrations: Registrations): Board {
  let state = board.front_end.state;
  for (const { def } of registrations.keys()) {
    if (getAt(state, def.path) !== undefined) continue;
    const value = def.kind === "collection" ? {} : def.initial;
    if (value !== undefined) state = setAt(state, def.path, value);
  }

  return state === board.front_end.state
    ? board
    : { ...board, front_end: { ...board.front_end, state } };
}
