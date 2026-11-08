// Opening a board, which replaces the board the store holds: a JSON board,
// as the text of a file or as the opening board that R sends with its reply to
// `hello`.
// Every board goes the same way, so that it ends as the same board whichever
// way it came in:
// 1. The store migrates what the JSON board holds in an older shape, by the
//    versions of the shapes that it records: the states of the plugins whose
//    classes packages register, the states of the blocks and the values of the
//    board options.
// 2. It drops and names what the app cannot rebuild: each plugin whose class
//    no package registers and each block whose class the block registry
//    lacks, and it drops the board option values that no package registers.
// 3. It takes over what the board then lacks of the plugins of the app's
//    opening board, the board of the reply to `hello`.
// 4. It gives the board the defaults of the board options that core's basic
//    frontend requires, where the board lacks them.

import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId, Plugin, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import {
  frozenCopy,
  isObject,
  listing,
  messageOf,
  own,
  packageOf,
  without,
} from "../util/index.ts";
import { check } from "./check.ts";
import { commit } from "./commit.ts";
import { StoreError } from "./error.ts";
import { expand } from "./expand.ts";
import { takeOver } from "./plugins.ts";
import { BASIC_OPTIONS } from "./registrations.ts";
import type { Registrations } from "./registrations.ts";

// The highest format of a JSON board that this client reads and writes.
export const FORMAT = 1;

// The collections of a board, which a JSON board holds as objects by id.
const COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"] as const;

// The properties of a board, which core holds. A board that holds any other,
// such as one that a later version of core adds, opens without it.
const PROPERTIES: readonly string[] = ["id", "name", ...COLLECTIONS];

// What the store names of what it dropped as it opened a board, since the app
// cannot rebuild it.
// It names each package that the JSON board lists and R could not load, with
// the version the JSON board records, so that the user learns what to install.
// It names each block whose class the block registry lacks, and each plugin
// whose class no package registers, with the package to install where the
// registry id of the block's class or the id of the plugin's class gives one.
// It does not name what it drops of the board option values.
export type Dropped =
  | { readonly package: string; readonly version: string }
  | {
      readonly block: string;
      readonly registry_id: string;
      readonly package: string | null;
    }
  | {
      readonly plugin: PluginId;
      readonly class: string;
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
// a file of today's stack, which has none, and a board that holds a collection
// in no object by id.
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
  const board = json.board;
  const lacking = COLLECTIONS.filter((name) => !isObject(board[name]));
  if (lacking.length > 0) {
    throw new StoreError(refused, [`The board holds its ${listing(lacking)} in no object by id.`]);
  }
  checkVersions(board);
  return json as unknown as JsonBoard;
}

// Whether a value is a version of a shape, a whole number from 1.
function isVersion(value: unknown): value is number {
  return typeof value === "number" && Number.isInteger(value) && value >= 1;
}

// The versions that a JSON board records, which its migrations key on:
// one for each plugin class whose plugins it holds, one for each block's state
// and one for each board option's value, which it holds beside the value.
function checkVersions(board: Readonly<Record<string, unknown>>): void {
  const failures: string[] = [];
  if (!isObject(board.versions) || !Object.values(board.versions).every(isVersion)) {
    failures.push("The board records the versions of its plugin classes as no whole numbers from 1 by class.");
  }
  for (const [id, block] of Object.entries(board.blocks as Readonly<Record<string, unknown>>)) {
    if (isObject(block) && !isVersion(block.version)) {
      failures.push(`The block "${id}" records no version of its class's state.`);
    }
  }
  for (const [id, option] of Object.entries(board.options as Readonly<Record<string, unknown>>)) {
    if (!isObject(option) || !Object.hasOwn(option, "value") || !isVersion(option.version)) {
      failures.push(`The board option "${id}" is recorded as no value with the version of its shape.`);
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
}

// The board that a JSON board holds, without the versions that it records,
// which only its migrations read, so that each board option holds its value,
// and without what core's board does not hold.
function unversioned(board: JsonBoard["board"]): Board {
  const record = board as unknown as Readonly<Record<string, unknown>>;
  return {
    ...Object.fromEntries(Object.entries(record).filter(([name]) => PROPERTIES.includes(name))),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        without(block as unknown as Readonly<Record<string, unknown>>, ["version"]),
      ]),
    ),
    options: Object.fromEntries(Object.entries(board.options).map(([id, option]) => [id, option.value])),
  } as unknown as Board;
}

// The board with what the JSON board holds in an older shape converted to
// today's: the state of each plugin whose class a package registers and whose
// version the JSON board records below the one R gives, by the class's
// `migrate`, the state of each block whose version is below its class's, by
// the class's migration, and the value of each board option whose version is
// below the one R gives, by the board option's `migrate`.
// R gives version 1 for a plugin class or a board option that it names no
// version of.
// A class or a board option without a migration keeps what the board holds,
// and so does a plugin class whose version the JSON board records none of,
// since its writer knew no version of it.
// A version above the one R gives refuses the board, since no migration
// converts a board back.
// The plugins of a class that no package registers keep what they hold, since
// the store drops them next.
function migrate(
  board: Board,
  json: JsonBoard["board"],
  reply: Pick<HelloReply, "block_registry" | "plugin_versions" | "option_versions">,
  registrations: Registrations,
): Board {
  const failures = new Set<string>();
  // What a migration changed, which the callbacks below record, since a flag
  // that they set reads as never set where it is read.
  const changed = new Set<string>();

  const plugins = Object.fromEntries(
    Object.entries(board.plugins).map(([id, plugin]) => {
      const cls = classOf(plugin);
      const def = cls === undefined ? undefined : registrations.plugin(cls)?.def;
      if (cls === undefined || def === undefined) return [id, plugin];
      const from = own(json.versions, cls);
      const to = own(reply.plugin_versions, cls) ?? 1;
      if (from === undefined || from === to) return [id, plugin];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the plugin class "${cls}", while R gives version ${String(to)}.`,
        );
        return [id, plugin];
      }
      if (def.migrate === undefined) return [id, plugin];
      changed.add("plugins");
      const state = migratedValue(`The migration of the plugin class "${cls}"`, () =>
        def.migrate?.(frozenCopy(plugin.state), from),
      );
      return [id, { ...plugin, state }];
    }),
  );

  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id, block]) => {
      const from = own(json.blocks, id)?.version;
      const to = own(reply.block_registry, block.registry_id)?.version;
      if (from === undefined || to === undefined || from === to) return [id, block];
      if (from > to) {
        failures.add(
          `The block "${id}" records version ${String(from)} of the state of "${block.registry_id}", while its class is at version ${String(to)}.`,
        );
        return [id, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === undefined) return [id, block];
      changed.add("blocks");
      const state = migrated(`The migration of the block class "${block.registry_id}"`, () =>
        fn(frozenCopy(block.state), from),
      );
      return [id, { ...block, state }];
    }),
  );

  const options = Object.fromEntries(
    Object.entries(board.options).map(([id, value]) => {
      const from = own(json.options, id)?.version;
      const to = own(reply.option_versions, id) ?? 1;
      if (from === undefined || from === to) return [id, value];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the value of the board option "${id}", while R gives version ${String(to)}.`,
        );
        return [id, value];
      }
      const def = registrations.option(id)?.def;
      if (def?.migrate === undefined) return [id, value];
      changed.add("options");
      return [
        id,
        migratedValue(`The migration of the board option "${id}"`, () => def.migrate?.(frozenCopy(value), from)),
      ];
    }),
  );

  if (failures.size > 0) throw new StoreError(refused, [...failures]);
  if (changed.size === 0) return board;
  return { ...board, plugins, blocks, options };
}

// What a migration returns, frozen, or a StoreError naming the migration where
// it fails or returns no object.
function migrated(what: string, fn: () => unknown): Readonly<Record<string, Json>> {
  const out = migratedValue(what, fn);
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return out;
}

// What a migration of a value returns, frozen, or a StoreError naming the
// migration where it fails or returns nothing.
function migratedValue(what: string, fn: () => unknown): Json {
  let out: unknown;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [`${what} failed: ${messageOf(error)}`]);
  }
  if (out === undefined) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out as Json);
}

// The board that a JSON board opens to, with what the reply to `hello` holds,
// the block registry, the versions of the plugin classes and of the board
// options' values, the opening board, whose plugins the board takes over where
// it lacks them, and the versions of the packages that R loaded, with what
// opening it dropped, or a StoreError naming what refused it.
// The board options that core's basic frontend requires come after,
// since a default may have to wait (see required()).
export function openJsonBoard(
  json: JsonBoard,
  reply: Pick<HelloReply, "packages" | "block_registry" | "plugin_versions" | "option_versions" | "board">,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const migrated = migrate(unversioned(json.board), json.board, reply, registrations);

  const unknown = Object.entries(migrated.plugins).flatMap(([id, plugin]) => {
    const cls = classOf(plugin);
    return cls === undefined || registrations.plugin(cls) !== undefined ? [] : [{ id, cls }];
  });
  const plugins: Dropped[] = unknown.map(({ id, cls }) => ({ plugin: id, class: cls, package: packageOf(cls) }));
  const known = { ...migrated, plugins: without(migrated.plugins, unknown.map(({ id }) => id)) };

  const gone = Object.entries(known.blocks)
    .filter(([, block]) => own(reply.block_registry, block.registry_id) === undefined)
    .map(([id]) => id);
  const blocks: Dropped[] = gone.map((id) => {
    const registryId = known.blocks[id]?.registry_id ?? "";
    return { block: id, registry_id: registryId, package: packageOf(registryId) };
  });

  const kept = remove(prune(known, registrations), gone, registrations);
  const board: Board = {
    ...kept,
    plugins: { ...kept.plugins, ...takeOver(kept, openingPlugins(reply.board), (id) => registrations.plugin(id)?.def) },
  };

  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);

  const packages: Dropped[] = Object.entries(json.packages)
    .filter(([pkg]) => own(reply.packages, pkg) === undefined)
    .map(([pkg, version]) => ({ package: pkg, version }));

  return { board, dropped: [...packages, ...blocks, ...plugins] };
}

// The id of a plugin's class, or undefined where what the board holds is no
// plugin that names its class, which the board's checks refuse.
function classOf(plugin: unknown): string | undefined {
  return isObject(plugin) && typeof plugin.class === "string" ? plugin.class : undefined;
}

// The plugins of the app's opening board, whose classes a board takes over
// where it holds no plugin of them, or none where R sent no board that holds
// its plugins in an object by id.
export function openingPlugins(opening: JsonBoard | undefined): Readonly<Record<PluginId, Plugin>> {
  const plugins: unknown = isObject(opening) && isObject(opening.board) ? opening.board.plugins : undefined;
  if (!isObject(plugins)) return {};
  return Object.fromEntries(
    Object.entries(plugins).filter(
      (entry): entry is [string, Plugin] =>
        isObject(entry[1]) && typeof entry[1].class === "string" && typeof entry[1].name === "string",
    ),
  );
}

// The board without every board option value that no package registered or
// that its schema refuses, so that it reads as its default.
function prune(board: Board, registrations: Registrations): Board {
  const unfit = Object.entries(board.options)
    .filter(([id, value]) => {
      const def = registrations.option(id);
      return def === undefined || def.validate(value).length > 0;
    })
    .map(([id]) => id);

  return { ...board, options: without(board.options, unfit) };
}

// The board without the blocks, removed as a delta removes them, so that what
// follows from removing a block goes with it: its links, its places in stacks
// and what the board's plugins keep about it, through the `update` of their
// classes.
function remove(board: Board, gone: readonly string[], registrations: Registrations): Board {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, registrations);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}

// The values of the board options that core's basic frontend requires and the
// board lacks: each its definition's default, or what the function that is its
// default resolves with.
// A board option that no loaded package defines refuses the board, before the
// store calls any default, and so does a default that fails or does not fit
// its schema.
// What core's basic frontend requires the board option for would fail later
// otherwise, where its cause is harder to see.
export async function required(
  board: Board,
  registrations: Registrations,
): Promise<Readonly<Record<OptionId, Json>>> {
  const lacking = BASIC_OPTIONS.filter((id) => !Object.hasOwn(board.options, id)).map((id) => ({
    id,
    option: registrations.option(id),
  }));

  const missing = lacking.filter(({ option }) => option === undefined);
  if (missing.length > 0) {
    throw new StoreError(
      refused,
      missing.map(
        ({ id }) => `Core's basic frontend requires the board option "${id}", which no loaded package defines.`,
      ),
    );
  }

  const defined = lacking.flatMap(({ id, option }) => (option === undefined ? [] : [{ id, option }]));
  const values = await Promise.all(
    defined.map(async ({ id, option }): Promise<[OptionId, Json] | string> => {
      const what = `The default of the board option "${id}", which core's basic frontend requires,`;
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
