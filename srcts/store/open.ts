// Opening a board, which replaces the board the store holds: a saved file,
// or the opening board that R sends with its reply to `hello`,
// which the store opens as the saved file it would be.
// Every board goes the same way from there, so that it ends as the same board
// whichever way it came in:
// 1. The store runs the migration of every package whose saved version differs
//    from the one R loaded.
// 2. It builds the board as the first class in the board's class vector that a
//    loaded package defines, or as core's `board` where none is.
// 3. It drops and names what the app cannot rebuild,
//    and drops the properties that the class does not add.
// 4. It gives the board what packages define for a board that lacks it:
//    the defaults of the properties of its class, and of the board options that
//    the class and its parents require.

import type { OpenReply } from "../protocol/index.ts";
import type { Board, OpeningBoard, OptionId, SavedBoard } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import {
  frozenCopy,
  isObject,
  messageOf,
  own,
  packageOf,
  without,
} from "../util/index.ts";
import { check, isClass } from "./check.ts";
import { commit } from "./commit.ts";
import { StoreError } from "./error.ts";
import { expand } from "./expand.ts";
import { mintId, sentenceCase } from "./ids.ts";
import { CORE_PROPERTIES, defaultOf, propertiesOf, recordOf } from "./registrations.ts";
import type { Built, Checked, Registrations, Saved } from "./registrations.ts";

// The highest format of a saved file that this client reads and writes.
export const FORMAT = 1;

// What the store names of what it dropped as it opened a board, since the app
// cannot rebuild it.
// It names each package that the file lists and R could not load, with the
// version the file records, so that the user learns what to install.
// Where such a package defines the board's class, the store builds the board
// as the next class in its class vector that a loaded package defines.
// A class's name gives no package, but the file's packages do.
// It names each block whose class the block registry lacks, with the package
// to install where its registry id gives one.
// It does not name what it drops of the properties, attributes and board
// option values.
export type Dropped =
  | { readonly package: string; readonly version: string }
  | {
      readonly block: string;
      readonly registry_id: string;
      readonly package: string | null;
    };

const refused = "The store refused to open the board";

// A saved file's text, as JSON.parse() reads it.
export function parse(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The file holds no JSON: ${messageOf(error)}`]);
  }
}

// A saved file, refusing a format above any this client knows,
// a file of today's stack, which has none, and a board that names no class.
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
  classOf(file.board as unknown as Board);
  return file as unknown as SavedBoard;
}

// The board's class vector, which the store builds the board by,
// or a StoreError where the board names none.
export function classOf(board: Board): readonly string[] {
  if (!isClass(board.class)) {
    throw new StoreError(refused, [
      "The board's class is no array of class names, as R's class vector of a board is.",
    ]);
  }
  return board.class;
}

// The opening board that an app script built,
// with its collections keyed by id and each id it lacks minted,
// and named after its id where it has no name.
// The properties of its class that the script gives come after core's.
export function keyed(opening: OpeningBoard): Board {
  classOf(opening as unknown as Board);
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
  const board: Board = {
    id,
    class: opening.class,
    name: opening.name ?? sentenceCase(id),
    blocks,
    links,
    stacks,
    options: opening.options,
  };
  return { ...board, ...propertiesOf(opening) };
}

// What the file holds that migrations convert: the properties of the board's
// class and every block's state, with its class.
function saved(board: Board): Saved {
  return frozenCopy({
    properties: propertiesOf(board) as Readonly<Record<string, Json>>,
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        { registry_id: block.registry_id, state: block.state },
      ]),
    ),
  });
}

// The board without the properties beside core's.
function coreOf(board: Board): Board {
  return Object.fromEntries(
    Object.entries(recordOf(board)).filter(([name]) => CORE_PROPERTIES.includes(name)),
  ) as unknown as Board;
}

// The board after the migration of every package whose version the file records
// differs from the one R loaded, a package the board holds nothing of included,
// one after another in the order they were registered in,
// which is the order in which packages declare their startup modules,
// since those register them and core initializes them in that order,
// so that a package's comes after those of the packages it imports.
// Each rewrites the file as the one before it left it,
// and the board takes the properties and the blocks' states the
// last one returns, while what a migration leaves out is gone,
// so that the board opens in today's shape.
function migrate(
  board: Board,
  versions: Readonly<Record<string, string>>,
  loaded: Readonly<Record<string, string>>,
  registrations: Registrations,
): Board {
  let file = saved(board);
  let migrated = false;

  for (const [pkg, migration] of registrations.migrations()) {
    const version = own(versions, pkg) ?? null;
    if (version === own(loaded, pkg)) continue;

    let out: unknown;
    try {
      out = migration(file, version);
    } catch (error) {
      throw new StoreError(refused, [`The migration of ${pkg} failed: ${messageOf(error)}`]);
    }
    if (!isObject(out) || !isObject(out.properties) || !isObject(out.blocks)) {
      throw new StoreError(refused, [
        `The migration of ${pkg} returned no properties and blocks' states.`,
      ]);
    }
    file = frozenCopy(out as Json) as unknown as Saved;
    migrated = true;
  }

  if (!migrated) return board;

  return {
    ...coreOf(board),
    ...file.properties,
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => {
        const state = own(file.blocks, id)?.state;
        return [id, state === undefined ? block : { ...block, state }];
      }),
    ),
  };
}

// The board that a saved file opens to, with what opening it dropped and the
// class it is built as, or a StoreError naming what refused it.
// The board options that its class requires come after,
// since a default may have to wait (see required()).
export function openFile(
  file: SavedBoard,
  reply: OpenReply,
  registrations: Registrations,
): { board: Board; dropped: Dropped[]; built: Built } {
  const migrated = migrate(file.board, file.packages, reply.packages, registrations);
  const built = registrations.builtAs(classOf(migrated));

  const gone = Object.entries(migrated.blocks)
    .filter(([, block]) => own(reply.block_registry, block.registry_id) === undefined)
    .map(([id]) => id);
  const blocks: Dropped[] = gone.map((id) => {
    const registryId = migrated.blocks[id]?.registry_id ?? "";
    return { block: id, registry_id: registryId, package: packageOf(registryId) };
  });

  const pruned = prune(migrated, gone, built, registrations);
  const board = remove(complete(pruned, built), gone, built);

  const failures = check(board, reply.block_registry, registrations, built);
  if (failures.length > 0) throw new StoreError(refused, failures);

  const packages: Dropped[] = Object.entries(file.packages)
    .filter(([pkg]) => own(reply.packages, pkg) === undefined)
    .map(([pkg, version]) => ({ package: pkg, version }));

  return { board, dropped: [...packages, ...blocks], built };
}

// The board without the properties that the class it is built as adds neither
// itself nor through its parents, such as the dock's views on a board built as
// core's.
// It lacks as well every attribute and board option value that no package
// registered or that its schema refuses, as one that an older version of its
// package saved, which no migration converts, so that it reads as its initial
// value or default.
// The blocks that go keep what they hold, which goes with them.
function prune(
  board: Board,
  gone: readonly string[],
  built: Built,
  registrations: Registrations,
): Board {
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

  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id, block]) => {
      if (gone.includes(id)) return [id, block];
      const unknown = unfit(block.attrs, (name) => registrations.attribute(name));
      return [id, unknown.length === 0 ? block : { ...block, attrs: without(block.attrs, unknown) }];
    }),
  );

  const options = unfit(board.options, (id) => registrations.option(id));
  const kept = Object.fromEntries(
    Object.entries(recordOf(board)).filter(
      ([name]) => CORE_PROPERTIES.includes(name) || built.properties.has(name),
    ),
  ) as unknown as Board;

  return { ...kept, blocks, options: without(board.options, options) };
}

// The board with each property of its class that it lacks,
// holding the property's default.
function complete(board: Board, built: Built): Board {
  const record = recordOf(board);
  const lacking = [...built.properties.keys()].filter((name) => !Object.hasOwn(record, name));
  if (lacking.length === 0) return board;
  return {
    ...board,
    ...Object.fromEntries(lacking.map((name) => [name, defaultOf(built, name)])),
  };
}

// The board without the blocks, removed as a delta removes them, so that what
// follows from removing a block goes with it: its links, its places in stacks
// and what the board's class keeps about it, through its `update`.
function remove(board: Board, gone: readonly string[], built: Built): Board {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, built);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta, built);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}

// The values of the board options that the board's class and its parents
// require and the board lacks: each its definition's default, or what the
// function that is its default resolves with.
// A board option that no loaded package defines refuses the board, before the
// store calls any default, and so does a default that fails or does not fit
// its schema.
// What the class requires the board option for would fail later otherwise,
// where its cause is harder to see.
export async function required(
  board: Board,
  built: Built,
  registrations: Registrations,
): Promise<Readonly<Record<OptionId, Json>>> {
  const by = new Map<OptionId, string>();
  for (const { def } of built.chain) {
    for (const id of def.options ?? []) if (!by.has(id)) by.set(id, def.class);
  }
  const lacking = [...by]
    .filter(([id]) => !Object.hasOwn(board.options, id))
    .map(([id, cls]) => ({ id, cls, option: registrations.option(id) }));

  const missing = lacking.filter(({ option }) => option === undefined);
  if (missing.length > 0) {
    throw new StoreError(
      refused,
      missing.map(
        ({ id, cls }) =>
          `The board class "${cls}" requires the board option "${id}", which no loaded package defines.`,
      ),
    );
  }

  const defined = lacking.flatMap(({ id, cls, option }) =>
    option === undefined ? [] : [{ id, cls, option }],
  );
  const values = await Promise.all(
    defined.map(async ({ id, cls, option }): Promise<[OptionId, Json] | string> => {
      const what = `The default of the board option "${id}", which the board class "${cls}" requires,`;
      let value: Json;
      try {
        const given = option.def.default;
        value = typeof given === "function" ? await given() : given;
      } catch (error) {
        return `${what} failed: ${messageOf(error)}`;
      }
      const failures = option.validate(value);
      return failures.length > 0 ? `${what} ${failures.join(" ")}` : [id, value];
    }),
  );

  const failures = values.filter((entry) => typeof entry === "string");
  if (failures.length > 0) throw new StoreError(refused, failures);
  return Object.fromEntries(values.filter((entry) => typeof entry !== "string"));
}
