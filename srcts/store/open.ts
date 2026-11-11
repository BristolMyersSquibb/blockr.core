// Opening a board, which replaces the board the store holds: a JSON board,
// as the text of a file or as the opening board that R sends with its reply to
// `hello`.
// Every board goes the same way, so that it ends as the same board whichever
// way it came in:
// 1. The store keeps the board's plugins that the app's opening board, the
//    board of the reply to `hello`, holds under the same id and of the same
//    class, and drops and names the others, since the app's opening board
//    sets which plugins run.
// 2. It migrates what the JSON board holds in an older shape, by the versions
//    of the shapes that it records: the states of the plugins it keeps, the
//    states of the blocks and the values of the board options.
// 3. It drops and names each block whose class the block registry lacks, and
//    each board option value that no package registers.
// 4. It gives the board each other plugin of the app's opening board, with
//    its class's default state.
// 5. It refuses the board where it fails a check, naming every failure, as
//    where a block's state, a plugin's state or a board option's value fails
//    its schema.
// It gives no board option its default: core adds the board options that
// something on the page requires as the board opens, the classes of its
// plugins among them (../core/plugins.ts).

import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId, Plugin, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import {
  errorMessage,
  frozenCopy,
  isObject,
  listing,
  own,
  packageFromId,
  without,
} from "../util/index.ts";
import { check } from "./check.ts";
import { commit } from "./commit.ts";
import { StoreError } from "./error.ts";
import { expand } from "./expand.ts";
import { appPlugins, keptPlugins } from "./plugins.ts";
import type { Registrations } from "./registrations.ts";

// The highest format of a JSON board that this client reads and writes.
export const FORMAT = 1;

// The collections of a board, which a JSON board holds as objects by id.
const COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"] as const;

// The properties of a board, which core holds. A board that holds any other,
// such as one that a later version of core adds, opens without it.
const PROPERTIES: readonly string[] = ["id", "name", ...COLLECTIONS];

// What the store names of what it dropped as it opened a board.
// It names each package that the JSON board lists and R could not load, with
// the version the JSON board records, so that the user learns what to install.
// It names each block whose class the block registry lacks, with the package
// to install where the registry id of the block's class gives one.
// It names each plugin that the app does not run, since the app's opening
// board holds no plugin under its id and of its class, or since no package
// registers its class, with the package that the id of its class gives, if
// any.
// It names each board option that no package registers, by the board
// option's id, and drops its value.
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
    }
  | { readonly option: OptionId };

const refused = "The store refused to open the board";

// A JSON board's text, as JSON.parse() reads it.
export function parse(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The text holds no JSON: ${errorMessage(error)}`]);
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
// The board holds only the plugins that the store keeps, so that the version
// of a plugin class whose plugins it drops refuses nothing.
// Neither does the version of a block's class that the block registry lacks,
// nor that of a board option that no package registers, since the store drops
// the block and the value.
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
      const cls = pluginClassId(plugin);
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
      const def = registrations.option(id)?.def;
      if (def === undefined) return [id, value];
      const from = own(json.options, id)?.version;
      const to = own(reply.option_versions, id) ?? 1;
      if (from === undefined || from === to) return [id, value];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the value of the board option "${id}", while R gives version ${String(to)}.`,
        );
        return [id, value];
      }
      if (def.migrate === undefined) return [id, value];
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
    throw new StoreError(refused, [`${what} failed: ${errorMessage(error)}`]);
  }
  if (out === undefined) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out as Json);
}

// The board that a JSON board opens to, with what the reply to `hello` holds,
// the block registry, the versions of the plugin classes and of the board
// options' values, the opening board, whose plugins are the plugins of every
// board that opens in the app, and the versions of the packages that R
// loaded, with what opening it dropped, or a StoreError naming what refused
// it.
// The board options that core's basic frontend requires come after,
// since a default may have to wait (see required()).
export function openJsonBoard(
  json: JsonBoard,
  reply: Pick<HelloReply, "packages" | "block_registry" | "plugin_versions" | "option_versions" | "board">,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const opening = openingPlugins(reply.board);
  const registeredClass = (id: string) => registrations.plugin(id)?.def;
  const read = unversioned(json.board);

  const held = keptPlugins(read.plugins, opening, registeredClass);
  const plugins: Dropped[] = Object.entries(read.plugins).flatMap(([id, plugin]) => {
    const cls = pluginClassId(plugin);
    return cls === undefined || Object.hasOwn(held, id) ? [] : [{ plugin: id, class: cls, package: packageFromId(cls) }];
  });

  const migrated = migrate({ ...read, plugins: held }, json.board, reply, registrations);

  const gone = Object.entries(migrated.blocks)
    .filter(([, block]) => own(reply.block_registry, block.registry_id) === undefined)
    .map(([id]) => id);
  const blocks: Dropped[] = gone.map((id) => {
    const registryId = migrated.blocks[id]?.registry_id ?? "";
    return { block: id, registry_id: registryId, package: packageFromId(registryId) };
  });

  const unclaimed = Object.keys(migrated.options).filter((id) => registrations.option(id) === undefined);
  const options: Dropped[] = unclaimed.map((id) => ({ option: id }));

  const kept = remove({ ...migrated, options: without(migrated.options, unclaimed) }, gone, registrations);
  const board: Board = { ...kept, plugins: appPlugins(kept.plugins, opening, registeredClass) };

  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);

  const packages: Dropped[] = Object.entries(json.packages)
    .filter(([pkg]) => own(reply.packages, pkg) === undefined)
    .map(([pkg, version]) => ({ package: pkg, version }));

  return { board, dropped: [...packages, ...blocks, ...plugins, ...options] };
}

// The id of a plugin's class, or undefined where what the board holds is no
// plugin that names its class, which the board's checks refuse.
function pluginClassId(plugin: unknown): string | undefined {
  return isObject(plugin) && typeof plugin.class === "string" ? plugin.class : undefined;
}

// The plugins of the app's opening board, which every board that opens in the
// app holds, or none where R sent no board that holds its plugins in an
// object by id.
// It keeps each that names its class, whatever else it holds, so that the
// store refuses one that is no plugin as it opens the opening board itself.
function openingPlugins(opening: JsonBoard | undefined): Readonly<Record<PluginId, Plugin>> {
  const plugins: unknown = isObject(opening) && isObject(opening.board) ? opening.board.plugins : undefined;
  if (!isObject(plugins)) return {};
  return Object.fromEntries(
    Object.entries(plugins).filter(
      (entry): entry is [string, Plugin] => isObject(entry[1]) && typeof entry[1].class === "string",
    ),
  );
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
