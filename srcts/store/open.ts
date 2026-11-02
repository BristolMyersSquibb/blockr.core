// Opening a board, which replaces the board the store holds: a saved file, or
// the opening board that R sends with its reply to `hello`. The store runs the
// migration of every package whose saved version differs from the one R
// loaded, drops and names what the app cannot rebuild, and gives each board
// option that the board lacks the default of its schema.

import type {
  Board,
  OpeningBoard,
  RegistryId,
  SavedBoard,
} from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { mintId, sentenceCase } from "./ids.ts";
import type { Registrations, Saved } from "./registrations.ts";
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
      readonly registry_id: RegistryId;
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

// What the file holds for every package, as each package's migration reads it.
function saved(board: Board): Saved {
  return frozenCopy({
    front_end: {
      state: board.front_end.state as Json,
      attrs: board.front_end.attrs,
    },
    attrs: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [id, block.attrs]),
    ),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        { registry_id: block.registry_id, state: block.state },
      ]),
    ),
    options: board.options,
  });
}

// The board after the migration of every package whose version the file
// records differs from the one R loaded, a package the board holds nothing of
// included, each on the same saved state, writing only the keys, attributes
// and board options its package registers and the states of its own block
// classes' blocks. What a migration leaves out of its return stays as the file
// holds it.
export function migrate(
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

    const keys = registrations.keys().filter(({ def }) => def.package === pkg);
    const attrs = registrations
      .attributes()
      .map(({ def }) => def)
      .filter((def) => def.package === pkg);
    const options = Object.values(registrations.options.registered()).filter(
      (option) => option.package === pkg,
    );

    let state = next.front_end.state;
    let frontEndAttrs = next.front_end.attrs;
    if (out.front_end !== undefined) {
      for (const { def } of keys) {
        const value = getAt(out.front_end.state, def.path);
        if (value !== undefined) state = setAt(state, def.path, value);
      }
      for (const attr of attrs.filter((attr) => attr.on === "front_end")) {
        const value = own(out.front_end.attrs, attr.name);
        if (value !== undefined) frontEndAttrs = { ...frontEndAttrs, [attr.name]: value };
      }
    }

    const blocks = { ...next.blocks };
    for (const [id, block] of Object.entries(next.blocks)) {
      const state = own(out.blocks ?? {}, id)?.state;
      const blockAttrs = own(out.attrs ?? {}, id);
      let migrated = block;
      if (state !== undefined && packageOf(block.registry_id) === pkg) {
        migrated = { ...migrated, state };
      }
      for (const attr of attrs.filter((attr) => attr.on === "block")) {
        const value = blockAttrs === undefined ? undefined : own(blockAttrs, attr.name);
        if (value !== undefined) migrated = { ...migrated, attrs: { ...migrated.attrs, [attr.name]: value } };
      }
      blocks[id] = migrated;
    }

    let values = next.options;
    for (const option of options) {
      const value = own(out.options ?? {}, option.id);
      if (value !== undefined) values = { ...values, [option.id]: value };
    }

    next = {
      ...next,
      blocks,
      options: values,
      front_end: { ...next.front_end, state, attrs: frontEndAttrs },
    };
  }

  return next;
}

// The board without what the app cannot rebuild, and what that was: a block
// whose class the block registry lacks, with its links and its places in
// stacks, and every key of the front-end's state, attribute and board option
// value that no package registered. A board made for a front-end that R serves
// no module for opens as one made for core's basic front-end.
export function drop(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  frontEnds: Readonly<Record<string, unknown>>,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const dropped: Dropped[] = [];

  let frontEnd = board.front_end;
  if (own(frontEnds, frontEnd.id) === undefined && frontEnd.id !== BASIC_FRONT_END) {
    dropped.push({ front_end: frontEnd.id, package: packageOf(frontEnd.id) });
    frontEnd = { ...frontEnd, id: BASIC_FRONT_END };
  }
  const owner = packageOf(board.front_end.id);

  const gone = Object.entries(board.blocks)
    .filter(([, block]) => own(registry, block.registry_id) === undefined)
    .map(([id]) => id);
  for (const id of gone) {
    const registryId = board.blocks[id]?.registry_id ?? "";
    dropped.push({ block: id, registry_id: registryId, package: packageOf(registryId) });
  }

  const blockAttrs = new Set<string>();
  const blocks = Object.fromEntries(
    Object.entries(without(board.blocks, gone)).map(([id, block]) => {
      const unknown = Object.keys(block.attrs).filter(
        (name) => registrations.attribute("block", name) === undefined,
      );
      for (const name of unknown) blockAttrs.add(name);
      return [id, unknown.length === 0 ? block : { ...block, attrs: without(block.attrs, unknown) }];
    }),
  );
  for (const name of blockAttrs) dropped.push({ attribute: name, on: "block", package: null });

  const links = Object.fromEntries(
    Object.entries(board.links).filter(([, link]) => !gone.includes(link.from) && !gone.includes(link.to)),
  );
  const stacks = Object.fromEntries(
    Object.entries(board.stacks).map(([id, stack]) => [
      id,
      stack.blocks.some((block) => gone.includes(block))
        ? { ...stack, blocks: stack.blocks.filter((block) => !gone.includes(block)) }
        : stack,
    ]),
  );

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
  const state = prune(frontEnd.state, [], paths, name);

  const unknownAttrs = Object.keys(frontEnd.attrs).filter(
    (name) => registrations.attribute("front_end", name) === undefined,
  );
  for (const name of unknownAttrs) dropped.push({ attribute: name, on: "front_end", package: null });

  const unknownOptions = Object.keys(board.options).filter(
    (id) => registrations.option(id) === undefined,
  );
  for (const id of unknownOptions) dropped.push({ option: id, package: null });

  return {
    board: {
      ...board,
      front_end: { ...frontEnd, state, attrs: without(frontEnd.attrs, unknownAttrs) },
      blocks,
      links,
      stacks,
      options: without(board.options, unknownOptions),
    },
    dropped,
  };
}

// The state without what lies at no registered key's path, keeping the objects
// on the way to one.
function prune(
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
      const pruned = prune(value, path, below, dropped);
      if (pruned !== value) next = { ...next, [key]: pruned };
      continue;
    }
    dropped(path, value);
    next = without(next, [key]);
  }
  return next;
}

// The board with the default of each board option that it lacks, and each
// registered key that it lacks, a collection empty and a value as its initial.
export function complete(board: Board, registrations: Registrations): Board {
  let options = board.options;
  for (const [id, option] of Object.entries(registrations.options.registered())) {
    const value = option.schema.default;
    if (value !== undefined && own(options, id) === undefined) {
      options = { ...options, [id]: value };
    }
  }

  let state = board.front_end.state;
  for (const { def } of registrations.keys()) {
    if (getAt(state, def.path) !== undefined) continue;
    const value = def.kind === "collection" ? {} : def.initial;
    if (value !== undefined) state = setAt(state, def.path, value);
  }

  return {
    ...board,
    options,
    front_end: state === board.front_end.state ? board.front_end : { ...board.front_end, state },
  };
}
