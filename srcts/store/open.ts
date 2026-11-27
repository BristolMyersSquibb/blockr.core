// Opening a board, which replaces the board the store holds: a JSON board,
// as the text of a file or as the opening board that R sends with its reply to
// `hello`.
// Every board goes the same way, so that it ends as the same board whichever
// way it came in, with the app's plugins, which the reply to `hello` names and
// which run on every board that the app opens, the opening board among them:
// 1. The store keeps the state that the board holds for each of the app's
//    plugins whose class a package registers and has `state`, under the
//    package and the id of the plugin's class. It drops every other state, and names each plugin
//    whose state it drops, since the app runs no plugin of its class or no
//    package registers the class, but for the state that the board holds for
//    a plugin of the app whose class keeps none, which goes unnamed.
// 2. It migrates what the JSON board holds in an older shape, by the versions
//    of the shapes that it records: the states of the plugins it keeps, the
//    states of the blocks and the values of the board options.
//    What a migration throws refuses the board, and the store logs it at
//    error (migratedValue(), below).
// 3. It drops and names each block whose class the block registry lacks, and
//    each board option value that no package registers.
// 4. It gives each other plugin of the app whose class has `state` its class's
//    default state.
// 5. It refuses the board where it fails a check, naming every failure, as
//    where a block's state, a plugin's state or a board option's value fails
//    its schema.
// It gives no board option its default: core adds the board options that
// something on the page requires as the board opens, the classes of its
// plugins among them (../core/plugins.ts).

import { appPlugins, blockRegistry } from "../protocol/index.ts";
import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, OptionId, PluginClassId, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import {
  errorMessage,
  frozenCopy,
  idAttributes,
  idText,
  internedRecord,
  isIdRecord,
  isObject,
  isPackageName,
  listing,
  nestedEntries,
  nestedRecord,
  nestedValue,
  own,
  without,
} from "../util/index.ts";
import { check } from "./check.ts";
import { commit } from "./commit.ts";
import { StoreError, thrown } from "./error.ts";
import { expand } from "./expand.ts";
import { droppedPlugins, keptPlugins, pluginsWithDefaults } from "./plugins.ts";
import type { Registrations } from "./registrations.ts";

// The highest format of a JSON board that this client reads and writes.
export const FORMAT = 1;

// The collections of a board, which a JSON board holds as objects by id.
const COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"] as const;

// The properties of a board, which core holds. A board that holds any other,
// such as one that a later version of core adds, opens without it.
const PROPERTIES: readonly string[] = ["id", "name", ...COLLECTIONS];

// What the store names of what it dropped as it opened a board, so that the
// user learns what to install.
// The member `about` names what the store dropped, as a notification's `about`
// names a board object, such as `{ block: "a" }`. It also holds what only its
// kind of record has:
// - A package that R could not load and that something on the board comes
//   from, as `{ package, version }`, with the version that the JSON board
//   records. A package that the JSON board lists for nothing on the board goes
//   unnamed.
// - A block whose class the block registry lacks, as
//   `{ block, name, registry_id }`, with the block's name, by which the user
//   knows it, and the registry id of its class.
// - The state of a plugin, as `{ plugin }`, by the id of its class, its package
//   and its id within the package, which the store drops since the app runs no
//   plugin of the class or no package registers the class.
// - The value of a board option that no package registers, as `{ option }`,
//   by the board option's id.
// The member `package` names the package that what the store dropped comes
// from: the package itself, the package of a block's class or of a plugin's
// class, which their ids hold, or the package that the JSON board records
// beside a board option's value.
// It is null where nothing names one, as for a block class that an app script
// defines, whose id holds no package, or for a board option that no package
// listed where the board was written.
export interface Dropped {
  readonly about:
    | { readonly package: string; readonly version: string }
    | { readonly block: BlockId; readonly name: string; readonly registry_id: RegistryId }
    | { readonly plugin: PluginClassId }
    | { readonly option: OptionId };
  readonly package: string | null;
}

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
  checkRecorded(board);
  return json as unknown as JsonBoard;
}

// Whether a value is a version of a shape, a whole number from 1.
function isVersion(value: unknown): value is number {
  return typeof value === "number" && Number.isInteger(value) && value >= 1;
}

// What a JSON board records beside what the board holds: the versions that its
// migrations key on, one for each plugin's state, one for each block's state and
// one for each board option's value, the package that lists each board option,
// or null for none, the package and the id of each block's class, and the
// plugins by the packages and the ids of their classes.
function checkRecorded(board: Readonly<Record<string, unknown>>): void {
  const failures: string[] = [];
  for (const [pkg, byId] of Object.entries(board.plugins as Readonly<Record<string, unknown>>)) {
    if (!isPackageName(pkg) || !isObject(byId)) {
      failures.push(`The board holds plugins under "${pkg}", where it holds the plugins of a package by the ids of their classes.`);
      continue;
    }
    for (const [cls, plugin] of Object.entries(byId)) {
      if (cls === "" || !isObject(plugin) || !Object.hasOwn(plugin, "state") || !isVersion(plugin.version)) {
        failures.push(`The plugin "${cls}" of ${pkg} is recorded as no state with the version of its class's state.`);
      }
    }
  }
  for (const [id, block] of Object.entries(board.blocks as Readonly<Record<string, unknown>>)) {
    if (isObject(block) && !isVersion(block.version)) {
      failures.push(`The block "${id}" records no version of its class's state.`);
    }
    if (isObject(block) && !isIdRecord(block.registry_id, true)) {
      failures.push(
        `The block "${id}" names its class by ${JSON.stringify(block.registry_id)}, where it names a package, or null, and an id, as { "package": "blockr.core", "id": "head_block" }.`,
      );
    }
  }
  for (const [id, option] of Object.entries(board.options as Readonly<Record<string, unknown>>)) {
    if (
      !isObject(option) ||
      !Object.hasOwn(option, "value") ||
      !isVersion(option.version) ||
      !(option.package === null || typeof option.package === "string")
    ) {
      failures.push(
        `The board option "${id}" is recorded as no value with the version of its shape and the package that lists it.`,
      );
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
}

// The board that a JSON board holds, without the versions that it records,
// which only its migrations read, and without the package of each board
// option, which only names what the store drops, so that each board option
// holds its value and each plugin its state, and without what core's board
// does not hold.
// Each block names its class by the registry id as core's table holds it, so
// that === compares it with the ids of the block registry.
function unversioned(board: JsonBoard["board"]): Board {
  const record = board as unknown as Readonly<Record<string, unknown>>;
  return {
    ...Object.fromEntries(Object.entries(record).filter(([name]) => PROPERTIES.includes(name))),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id, block]) => [
        id,
        {
          ...without(block as unknown as Readonly<Record<string, unknown>>, ["version"]),
          registry_id: internedRecord(block.registry_id),
        },
      ]),
    ),
    options: Object.fromEntries(Object.entries(board.options).map(([id, option]) => [id, option.value])),
    plugins: nestedRecord(nestedEntries(board.plugins).map(([cls, plugin]) => [cls, plugin.state])),
  } as unknown as Board;
}

// The board with what the JSON board holds in an older shape converted to
// today's: the state of each plugin whose class a package registers and whose
// version the JSON board records below the one that the app's plugin of the
// class carries in the reply to `hello`, by the class's `migrate`, the state of
// each block whose version is below its class's, by the class's migration, and
// the value of each board option whose version is below the one R gives, by
// the board option's `migrate`.
// R gives version 1 for a board option that it names no version of.
// A class or a board option without a migration keeps what the board holds.
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
  reply: Pick<HelloReply, "block_registry" | "plugins" | "options">,
  registrations: Registrations,
): Board {
  const failures = new Set<string>();
  // What a migration changed, which the callbacks below record, since a flag
  // that they set reads as never set where it is read.
  const changed = new Set<string>();

  const app = appPlugins(reply);
  const plugins = nestedRecord(
    nestedEntries(board.plugins).map(([cls, state]): [PluginClassId, Json] => {
      const def = registrations.plugin(cls)?.def;
      if (def === undefined) return [cls, state];
      const from = nestedValue(json.plugins, cls)?.version;
      const to = app.get(cls)?.version ?? 1;
      if (from === undefined || from === to) return [cls, state];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the plugin class "${idText(cls)}", while R gives version ${String(to)}.`,
        );
        return [cls, state];
      }
      if (def.state?.migrate === undefined) return [cls, state];
      changed.add("plugins");
      return [
        cls,
        migratedValue(`The migration of the plugin class "${idText(cls)}"`, idAttributes("blockr.plugin.class_id", cls), () =>
          def.state?.migrate?.(frozenCopy(state), from),
        ),
      ];
    }),
  );

  const registry = blockRegistry(reply);
  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id, block]) => {
      const from = own(json.blocks, id)?.version;
      const to = registry.get(block.registry_id)?.version;
      if (from === undefined || to === undefined || from === to) return [id, block];
      if (from > to) {
        failures.add(
          `The block "${id}" records version ${String(from)} of the state of "${idText(block.registry_id)}", while its class is at version ${String(to)}.`,
        );
        return [id, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === undefined) return [id, block];
      changed.add("blocks");
      const state = migrated(
        `The migration of the block class "${idText(block.registry_id)}"`,
        idAttributes("blockr.block.registry_id", block.registry_id),
        () => fn(frozenCopy(block.state), from),
      );
      return [id, { ...block, state }];
    }),
  );

  const options = Object.fromEntries(
    Object.entries(board.options).map(([id, value]) => {
      const def = registrations.option(id)?.def;
      if (def === undefined) return [id, value];
      const from = own(json.options, id)?.version;
      const to = own(reply.options, id)?.version ?? 1;
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
        migratedValue(`The migration of the board option "${id}"`, { "blockr.option.id": id }, () =>
          def.migrate?.(frozenCopy(value), from),
        ),
      ];
    }),
  );

  if (failures.size > 0) throw new StoreError(refused, [...failures]);
  if (changed.size === 0) return board;
  return { ...board, plugins, blocks, options };
}

// What a migration returns, frozen, or a StoreError naming the migration where
// it fails or returns no object.
function migrated(
  what: string,
  attributes: Readonly<Record<string, Json>>,
  fn: () => unknown,
): Readonly<Record<string, Json>> {
  const out = migratedValue(what, attributes, fn);
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return out;
}

// What a migration of a value returns, frozen, or a StoreError naming the
// migration where it fails or returns nothing.
// The store logs what the migration throws at error, with its stack, and with
// the `attributes` that name the class or the board option of the migration
// (thrown(), ./error.ts).
function migratedValue(what: string, attributes: Readonly<Record<string, Json>>, fn: () => unknown): Json {
  let out: unknown;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [thrown(`${what} failed`, error, attributes)]);
  }
  if (out === undefined) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out as Json);
}

// The board that a JSON board opens to, with what the reply to `hello` holds,
// the block registry, the app's plugins, which run on every board that opens in
// the app, each with the version of its class, the versions of the board
// options' values, and the versions of the packages that R loaded, with what
// opening it dropped, or a StoreError naming what refused it.
// The board options that the classes of the app's plugins require come after,
// since a default may have to wait (../core/plugins.ts).
export function openJsonBoard(
  json: JsonBoard,
  reply: Pick<HelloReply, "packages" | "block_registry" | "plugins" | "options">,
  registrations: Registrations,
): { board: Board; dropped: Dropped[] } {
  const registeredClass = (id: PluginClassId) => registrations.plugin(id)?.def;
  const read = unversioned(json.board);
  const app = appPlugins(reply);
  const registry = blockRegistry(reply);

  const held = keptPlugins(read.plugins, app, registeredClass);
  const plugins: Dropped[] = droppedPlugins(read.plugins, app, registeredClass).map((id) => ({
    about: { plugin: id },
    package: id.package,
  }));

  const migrated = migrate({ ...read, plugins: held }, json.board, reply, registrations);

  const lacking = Object.entries(migrated.blocks).filter(([, block]) => !registry.has(block.registry_id));
  const blocks: Dropped[] = lacking.map(([id, block]) => ({
    about: { block: id, name: block.name, registry_id: block.registry_id },
    package: block.registry_id.package,
  }));
  const gone = lacking.map(([id]) => id);

  const unclaimed = Object.keys(migrated.options).filter((id) => registrations.option(id) === undefined);
  const options: Dropped[] = unclaimed.map((id) => ({
    about: { option: id },
    package: own(json.board.options, id)?.package ?? null,
  }));

  const kept = remove({ ...migrated, options: without(migrated.options, unclaimed) }, gone, registrations);
  const board: Board = { ...kept, plugins: pluginsWithDefaults(kept.plugins, app, registeredClass) };

  const failures = check(board, registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);

  // The packages that something on the board comes from: a block's class, a
  // plugin's class, or a board option, which the JSON board records beside the
  // board option's value.
  const used = new Set([
    ...Object.values(read.blocks).map((block) => block.registry_id.package),
    ...nestedEntries(read.plugins).map(([cls]) => cls.package),
    ...Object.values(json.board.options).map((option) => option.package),
  ]);
  const packages: Dropped[] = Object.entries(json.packages)
    .filter(([pkg]) => used.has(pkg) && own(reply.packages, pkg) === undefined)
    .map(([pkg, version]) => ({ about: { package: pkg, version }, package: pkg }));

  return { board, dropped: [...packages, ...blocks, ...plugins, ...options] };
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
