// What packages register with the store, from their startup modules, before it
// opens a board: the keys of the front-end state, each at its path there,
// attributes, board options and each package's one migration of its saved
// state. A key, an attribute or a board option id registered twice fails.

import { computed, signal } from "@preact/signals-core";
import { Validator } from "@cfworker/json-schema";
import type { OutputUnit, SchemaDraft } from "@cfworker/json-schema";
import type {
  Block,
  BlockId,
  Board,
  OptionId,
  RegistryId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { Delta, KeyDelta } from "./delta.ts";
import { jsonEqual, messageOf, pathText } from "./util.ts";

// A key of the front-end's state, registered at its path there by the module
// that owns it, as the dock registers ["views"] and the DAG its positions under
// ["extensions", "blockr.dag-dag", "positions"].
export interface StateKey {
  readonly path: readonly string[];
  // A collection holds entries keyed by id in insertion order, as the board's
  // blocks are, and a value is set as a whole, as the board's name is.
  readonly kind: "collection" | "value";
  // An entry's schema or the value's, with descriptions and examples.
  readonly schema: JsonSchema;
  // What a value holds on a board that has none; a collection starts empty.
  readonly initial?: Json;
  // What follows in this key from a delta, such as dropping a removed block's
  // position, from the delta and the board before it, as a delta of this key
  // alone.
  expand?(delta: Delta, board: Board): KeyDelta | undefined;
  // What the schema cannot say about the key's value on the board after a
  // delta, each failure in a sentence.
  check?(value: Json, board: Board): string[];
}

// A value kept on every block, or on the front-end beside its state, that is
// not state, such as core's `visible`.
export interface Attribute {
  readonly name: string;
  readonly on: "block" | "front_end";
  readonly schema: JsonSchema;
  // What it holds where nothing set it.
  readonly initial?: Json;
}

// A board option, whose value the board holds under its id once something sets
// one, while the package that ships it defines the rest. Until then, whatever
// needs the board option takes its `default`, a value or a function that core
// calls the first time it is needed, so a board option that a block class
// registered while the app runs brings needs nothing more.
export interface Option {
  readonly id: OptionId;
  readonly label: string;
  readonly category: string;
  readonly schema: JsonSchema;
  readonly default: Json | (() => Promise<Json>);
}

// The board options packages define, by id.
export interface Options {
  register(option: Option): () => void;
  registered(): Readonly<Record<OptionId, Option>>;
}

// What a saved board holds that migrations convert: every block's state, with
// its class, and the front-end state.
export interface Saved {
  readonly front_end: { readonly state: Json };
  readonly blocks: Readonly<
    Record<BlockId, { readonly registry_id: RegistryId; readonly state: Block["state"] }>
  >;
}

// A package's migration, which rewrites what a file holds, as the migrations
// before it left it, with its package's part in today's shape, given the
// version of the package that saved the file, or null where the file records
// none. Migrations run one after another, in the order in which packages
// declare their startup modules, which puts each package's after those of the
// packages it imports, and the board opens with the front-end state and the
// blocks' states that the last one returns, so that what a migration leaves out
// is gone. Each changes only its own package's part, which nothing but the
// package enforces: the states of its block classes' blocks and the front-end
// state its front-end or its extensions keep.
//
// A migration converts what older versions of its package saved into today's
// shape, and changes nothing on a board in today's shape, so that a board
// saved and opened again comes back as it was, though a file that records no
// version of a package runs its migration. What a board reads where nothing
// set a value comes from what packages register instead, a key's `initial`,
// an attribute's `initial` and a board option's `default`. Attributes and
// board option values are not migrated: one that no longer fits its schema is
// dropped as the board opens, so that it reads as its initial value or
// default.
export type Migration = (saved: Saved, version: string | null) => Saved;

// A validator of a schema, which gives each way a value fails it in a clause
// that follows what the value is, such as `fails its schema at /x: Instance
// type "string" is invalid. Expected "number".`
export type Validate = (value: unknown) => readonly string[];

// The validator of a schema, or an error where the schema cannot be read. Its
// clauses start with `fails`, or with another verb for a value named in the
// plural, as "fail their schema" follows a command's args.
export function compile(
  what: string,
  schema: JsonSchema,
  fails = "fails its schema",
): Validate {
  let validator: Validator;
  try {
    validator = new Validator(schema, draftOf(schema), false);
  } catch (error) {
    throw new Error(`The schema of ${what} cannot be read: ${messageOf(error)}`, {
      cause: error,
    });
  }
  return (value) => {
    const result = validator.validate(value);
    return result.valid ? [] : describe(result.errors, fails);
  };
}

// A registration with the validator of its schema.
export interface Checked<T> {
  readonly def: T;
  readonly validate: Validate;
}

export interface Registrations {
  register(def: StateKey | Attribute): () => void;
  registered(): readonly (StateKey | Attribute)[];
  readonly options: Options;
  registerMigration(pkg: string, fn: Migration): () => void;
  keys(): readonly Checked<StateKey>[];
  key(path: readonly string[]): Checked<StateKey> | undefined;
  attributes(): readonly Checked<Attribute>[];
  attribute(on: Attribute["on"], name: string): Checked<Attribute> | undefined;
  option(id: OptionId): Checked<Option> | undefined;
  migrations(): ReadonlyMap<string, Migration>;
}

export function createRegistrations(): Registrations {
  const keys = signal<readonly Checked<StateKey>[]>([]);
  const attributes = signal<readonly Checked<Attribute>[]>([]);
  const options = signal<readonly Checked<Option>[]>([]);
  const migrations = signal<ReadonlyMap<string, Migration>>(new Map());

  const keyIndex = computed(
    () => new Map(keys.value.map((key) => [JSON.stringify(key.def.path), key])),
  );
  const attributeIndex = computed(
    () =>
      new Map(
        attributes.value.map((attr) => [
          JSON.stringify([attr.def.on, attr.def.name]),
          attr,
        ]),
      ),
  );
  const optionIndex = computed(
    () => new Map(options.value.map((option) => [option.def.id, option])),
  );
  const registered = computed(() => [
    ...keys.value.map((key) => key.def),
    ...attributes.value.map((attr) => attr.def),
  ]);
  const optionDefs = computed(() =>
    Object.fromEntries(options.value.map((option) => [option.def.id, option.def])),
  );

  function checkInitial(
    what: string,
    validate: Validate,
    initial: Json | undefined,
    field: string,
  ): void {
    if (initial === undefined) return;
    const failures = validate(initial);
    if (failures.length > 0) {
      throw new Error(`The ${field} of ${what} ${failures.join(" ")}`);
    }
  }

  function registerKey(def: StateKey): () => void {
    const what = `the key of the front-end state at ${pathText(def.path)}`;
    if (
      def.path.length === 0 ||
      !def.path.every((part) => typeof part === "string" && part !== "")
    ) {
      throw new Error("A key of the front-end state has a path that is empty or holds an empty part.");
    }
    for (const other of keys.value) {
      if (jsonEqual(def.path, other.def.path)) {
        throw new Error(`The key of the front-end state at ${pathText(def.path)} is registered already.`);
      }
      if (overlap(def.path, other.def.path)) {
        throw new Error(
          `The key of the front-end state at ${pathText(def.path)} overlaps the one at ${pathText(other.def.path)}, which is registered already.`,
        );
      }
    }
    const validate = compile(what, def.schema);
    if (def.kind === "value") {
      checkInitial(what, validate, def.initial, "initial value");
    }
    const item = { def, validate };
    keys.value = [...keys.value, item];
    return () => {
      keys.value = keys.value.filter((x) => x !== item);
    };
  }

  function registerAttribute(def: Attribute): () => void {
    const what = `the attribute "${def.name}" of ${def.on === "block" ? "blocks" : "the front-end"}`;
    if (attributeIndex.value.has(JSON.stringify([def.on, def.name]))) {
      throw new Error(`The ${what.slice("the ".length)} is registered already.`);
    }
    const validate = compile(what, def.schema);
    checkInitial(what, validate, def.initial, "initial value");
    const item = { def, validate };
    attributes.value = [...attributes.value, item];
    return () => {
      attributes.value = attributes.value.filter((x) => x !== item);
    };
  }

  return {
    register(def) {
      return "path" in def ? registerKey(def) : registerAttribute(def);
    },
    registered: () => registered.value,
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        if (optionIndex.value.has(def.id)) {
          throw new Error(`The board option "${def.id}" is registered already.`);
        }
        const validate = compile(what, def.schema);
        if (typeof def.default !== "function") {
          checkInitial(what, validate, def.default, "default");
        }
        const item = { def, validate };
        options.value = [...options.value, item];
        return () => {
          options.value = options.value.filter((x) => x !== item);
            };
      },
      registered: () => optionDefs.value,
    },
    registerMigration(pkg, fn) {
      if (migrations.value.has(pkg)) {
        throw new Error(`${pkg} registers a migration, which it registered already.`);
      }
      migrations.value = new Map([...migrations.value, [pkg, fn]]);
      return () => {
        if (migrations.value.get(pkg) !== fn) return;
        const rest = new Map(migrations.value);
        rest.delete(pkg);
        migrations.value = rest;
      };
    },
    keys: () => keys.value,
    key: (path) => keyIndex.value.get(JSON.stringify(path)),
    attributes: () => attributes.value,
    attribute: (on, name) => attributeIndex.value.get(JSON.stringify([on, name])),
    option: (id) => optionIndex.value.get(id),
    migrations: () => migrations.value,
  };
}

// Whether two keys would hold the same place, one at or under the other.
function overlap(a: readonly string[], b: readonly string[]): boolean {
  const n = Math.min(a.length, b.length);
  return a.slice(0, n).every((part, i) => part === b[i]);
}

// The draft a schema declares in its `$schema`, 2020-12 where it declares
// none, as Zod's z.toJSONSchema() writes it.
function draftOf(schema: JsonSchema): SchemaDraft {
  const declared = typeof schema.$schema === "string" ? schema.$schema : "";
  if (declared.includes("draft-04")) return "4";
  if (declared.includes("draft-07")) return "7";
  if (declared.includes("2019-09")) return "2019-09";
  return "2020-12";
}

// The keywords whose failures only say that a part of the value failed, beside
// the failure of that part, which says how.
const wrappers = new Set([
  "properties",
  "items",
  "prefixItems",
  "additionalItems",
  "$ref",
  "$dynamicRef",
  "$recursiveRef",
  "allOf",
  "false",
]);

function describe(errors: readonly OutputUnit[], fails: string): string[] {
  return errors
    .filter((error) => !wrappers.has(error.keyword))
    .map((error) => {
      const at = error.instanceLocation.replace(/^#/, "");
      return `${fails}${at === "" ? "" : ` at ${at}`}: ${error.error}`;
    });
}
