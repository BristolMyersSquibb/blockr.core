// Opening a board, which replaces the board the store holds: a JSON board,
// as the text of a file or as the opening board that R sends with its reply to
// `hello`.
// Every board goes the same way, so that it ends as the same board whichever
// way it came in:
// 1. The store builds the board as the first class in the board's class vector
//    that a loaded package defines, or as core's `board` where none is.
// 2. It migrates what the JSON board holds in an older shape, by the versions
//    of the shapes that it records: the properties of the classes that it
//    builds the board as, and the states of the blocks.
// 3. It drops and names what the app cannot rebuild,
//    and drops the properties that the class does not add.
// 4. It gives the board what packages define for a board that lacks it:
//    the defaults of the properties of its class, and of the board options that
//    the class and its parents require.

import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId } from "../types/board.ts";
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
import { CORE_PROPERTIES, defaultOf, propertiesOf, recordOf } from "./registrations.ts";
import type { Built, Checked, Registrations } from "./registrations.ts";

// The highest format of a JSON board that this client reads and writes.
export const FORMAT = 1;

// What the store names of what it dropped as it opened a board, since the app
// cannot rebuild it.
// It names each package that the JSON board lists and R could not load, with
// the version the JSON board records, so that the user learns what to install.
// Where such a package defines the board's class, the store builds the board
// as the next class in its class vector that a loaded package defines.
// A class's name gives no package, but the JSON board's packages do.
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

// A JSON board's text, as JSON.parse() reads it.
export function parse(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The text holds no JSON: ${messageOf(error)}`]);
  }
}

// A JSON board, refusing a format above any this client knows,
// a file of today's stack, which has none, and a board that names no class.
export function checkJsonBoard(json: unknown): JsonBoard {
  if (!isObject(json)) throw new StoreError(refused, ["The JSON is no JSON board."]);
  if (!Object.hasOwn(json, "format")) {
    throw new StoreError(refused, [
      "The JSON board has no format, as a file of today's stack has none, which a conversion outside core turns into a JSON board first.",
    ]);
  }
  if (typeof json.format !== "number" || !Number.isInteger(json.format)) {
    throw new StoreError(refused, [`The JSON board's format ${JSON.stringify(json.format)} is no whole number.`]);
  }
  if (json.format > FORMAT) {
    throw new StoreError(refused, [
      `The JSON board's format is ${String(json.format)}, while this client reads format ${String(FORMAT)} at most.`,
    ]);
  }
  if (!isObject(json.packages) || !Object.values(json.packages).every((v) => typeof v === "string")) {
    throw new StoreError(refused, ["The JSON board names no packages with their versions."]);
  }
  if (!isObject(json.board)) throw new StoreError(refused, ["The JSON board holds no board."]);
  classOf(json.board as unknown as Board);
  checkVersions(json.board);
  return json as unknown as JsonBoard;
}

// Whether a value is a version of a shape, a whole number from 1.
function isVersion(value: unknown): value is number {
  return typeof value === "number" && Number.isInteger(value) && value >= 1;
}

// The versions that a JSON board records, which its migrations key on:
// one for each class that R names one for, and one for each block's state.
function checkVersions(board: Readonly<Record<string, unknown>>): void {
  const failures: string[] = [];
  if (!isObject(board.versions) || !Object.values(board.versions).every(isVersion)) {
    failures.push("The board records its classes' versions as no whole numbers from 1 by class.");
  }
  if (isObject(board.blocks)) {
    for (const [id, block] of Object.entries(board.blocks)) {
      if (isObject(block) && !isVersion(block.version)) {
        failures.push(`The block "${id}" records no version of its class's state.`);
      }
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
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

// The board that a JSON board holds, without the versions that it records,
// which only its migrations read.
function unversioned(board: JsonBoard["board"]): Board {
  return {
    ...without(recordOf(board as unknown as Board), ["versions"]),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        without(block as unknown as Readonly<Record<string, unknown>>, ["version"]),
      ]),
    ),
  } as unknown as Board;
}

// The board without the properties beside core's.
function coreOf(board: Board): Board {
  return Object.fromEntries(
    Object.entries(recordOf(board)).filter(([name]) => CORE_PROPERTIES.includes(name)),
  ) as unknown as Board;
}

// The board with what the JSON board holds in an older shape converted to
// today's: the properties beside core's, by the `migrate` of each class that
// the board is built as whose version the JSON board records below the one R
// gives, root first, and the state of each block whose version is below its
// class's, by the class's migration.
// A class without a migration keeps what the board holds,
// and so does a class whose version the JSON board records none of,
// since its writer knew no version of it.
// A version above the one R gives refuses the board, since no migration
// converts a board back.
function migrate(
  board: Board,
  json: JsonBoard["board"],
  built: Built,
  reply: Pick<HelloReply, "block_registry" | "board_classes">,
  registrations: Registrations,
): Board {
  const failures: string[] = [];
  let changed = false;

  let properties = frozenCopy(propertiesOf(board) as Readonly<Record<string, Json>>);
  for (const { def } of built.chain) {
    const from = own(json.versions, def.class);
    const to = own(reply.board_classes, def.class)?.version;
    if (from === undefined || to === undefined || from === to) continue;
    if (from > to) {
      failures.push(
        `The board records version ${String(from)} of the board class "${def.class}", while R gives version ${String(to)}.`,
      );
      continue;
    }
    if (def.migrate === undefined) continue;
    const given = properties;
    properties = migrated(`The migration of the board class "${def.class}"`, () =>
      def.migrate?.(given, from),
    );
    changed = true;
  }

  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id, block]) => {
      const from = own(json.blocks, id)?.version;
      const to = own(reply.block_registry, block.registry_id)?.version;
      if (from === undefined || to === undefined || from === to) return [id, block];
      if (from > to) {
        failures.push(
          `The block "${id}" records version ${String(from)} of the state of "${block.registry_id}", while its class is at version ${String(to)}.`,
        );
        return [id, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === undefined) return [id, block];
      changed = true;
      const state = migrated(`The migration of the block class "${block.registry_id}"`, () =>
        fn(frozenCopy(block.state), from),
      );
      return [id, { ...block, state }];
    }),
  );

  if (failures.length > 0) throw new StoreError(refused, failures);
  if (!changed) return board;
  return { ...coreOf(board), ...properties, blocks };
}

// What a migration returns, frozen, or a StoreError naming the migration where
// it fails or returns no object.
function migrated(what: string, fn: () => unknown): Readonly<Record<string, Json>> {
  let out: unknown;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [`${what} failed: ${messageOf(error)}`]);
  }
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return frozenCopy(out as Json) as Readonly<Record<string, Json>>;
}

// The board that a JSON board opens to, with what the reply to `hello` holds,
// the block registry, the board classes and the versions of the packages that
// R loaded, with what opening it dropped and the class it is built as, or a
// StoreError naming what refused it.
// The board options that its class requires come after,
// since a default may have to wait (see required()).
export function openJsonBoard(
  json: JsonBoard,
  reply: Pick<HelloReply, "packages" | "block_registry" | "board_classes">,
  registrations: Registrations,
): { board: Board; dropped: Dropped[]; built: Built } {
  const plain = unversioned(json.board);
  const built = registrations.builtAs(classOf(plain));
  const migrated = migrate(plain, json.board, built, reply, registrations);

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

  const packages: Dropped[] = Object.entries(json.packages)
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
