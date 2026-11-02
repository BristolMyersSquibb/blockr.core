// What packages register with the store, from their startup modules, before it
// opens a board: the keys of the front-end's state, attributes, board options
// and each package's one migration of its saved state. Each names its package,
// so that a second registration of a key, an attribute or a board option id
// fails naming both packages, and so that a migration writes only what its own
// package registers.

import { computed, signal } from "@preact/signals-core";
import Ajv2020 from "ajv/dist/2020.js";
import type { ErrorObject } from "ajv/dist/2020.js";
import type {
  Block,
  BlockId,
  Board,
  OptionId,
  RegistryId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { Delta, KeyDelta } from "./delta.ts";
import { messageOf, pathText } from "./util.ts";

// A key of the front-end's state, registered at its path there by the module
// that owns it, as the dock registers ["views"] and the DAG its positions under
// ["extensions", "blockr.dag-dag", "positions"].
export interface StateKey {
  readonly package: string;
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
  readonly package: string;
  readonly name: string;
  readonly on: "block" | "front_end";
  readonly schema: JsonSchema;
  // What it holds where nothing set it.
  readonly initial?: Json;
}

// A board option, whose value the board holds under its id, while the package
// that ships it defines the rest. The schema's `default` is what a board that
// lacks a value takes as it opens.
export interface Option {
  readonly package: string;
  readonly id: OptionId;
  readonly label: string;
  readonly category: string;
  readonly schema: JsonSchema;
}

// The board options packages define, by id.
export interface Options {
  register(option: Option): () => void;
  registered(): Readonly<Record<OptionId, Option>>;
}

// What a saved board holds for every package, which each package's migration
// reads, and of which it returns its own part in today's shape.
export interface Saved {
  readonly front_end: {
    readonly state: Json;
    readonly attrs: Readonly<Record<string, Json>>;
  };
  readonly attrs: Readonly<Record<BlockId, Block["attrs"]>>;
  readonly blocks: Readonly<
    Record<BlockId, { readonly registry_id: RegistryId; readonly state: Block["state"] }>
  >;
  readonly options: Readonly<Record<OptionId, Json>>;
}

// A package's migration, given everything a file holds and the version of the
// package that saved it, or null where the file records none. It converts what
// older versions of its package saved into today's shape, and changes nothing
// on a board in today's shape, so that a board saved and opened again comes
// back as it was, though a file that records no version of a package runs its
// migration. What a board takes where it lacks a value comes from what
// packages register instead, a key's `initial` and a board option's
// `default`, which the store gives every board it opens.
export type Migration = (saved: Saved, version: string | null) => Partial<Saved>;

// A validator of a schema, which gives each failure in a sentence.
export type Validate = (value: unknown) => readonly string[];

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
  const ajv = new Ajv2020({
    allErrors: true,
    strict: false,
    validateFormats: false,
  });

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

  function compile(what: string, pkg: string, schema: JsonSchema): Validate {
    let validate;
    try {
      validate = ajv.compile(schema);
    } catch (error) {
      throw new Error(
        `${pkg} registers ${what} with a schema that Ajv cannot compile: ${messageOf(error)}`,
        { cause: error },
      );
    }
    return (value) => (validate(value) ? [] : describe(validate.errors ?? []));
  }

  function checkInitial(
    what: string,
    pkg: string,
    validate: Validate,
    initial: Json | undefined,
    field: string,
  ): void {
    if (initial === undefined) return;
    const failures = validate(initial);
    if (failures.length > 0) {
      throw new Error(
        `${pkg} registers ${what} with ${field} that its schema refuses: ${failures.join("; ")}.`,
      );
    }
  }

  function registerKey(def: StateKey): () => void {
    const what = `the key ${pathText(def.path)}`;
    if (
      def.path.length === 0 ||
      !def.path.every((part) => typeof part === "string" && part !== "")
    ) {
      throw new Error(
        `${def.package} registers a key at a path that is empty or holds an empty part.`,
      );
    }
    for (const other of keys.value) {
      if (overlap(def.path, other.def.path)) {
        throw new Error(
          `${def.package} registers ${what}, which ${other.def.package} registered as ${pathText(other.def.path)} already.`,
        );
      }
    }
    const validate = compile(what, def.package, def.schema);
    if (def.kind === "value") {
      checkInitial(what, def.package, validate, def.initial, "an initial value");
    }
    const item = { def, validate };
    keys.value = [...keys.value, item];
    return () => {
      keys.value = keys.value.filter((x) => x !== item);
      ajv.removeSchema(def.schema);
    };
  }

  function registerAttribute(def: Attribute): () => void {
    const what = `the attribute "${def.name}" of ${def.on === "block" ? "blocks" : "the front-end"}`;
    const other = attributeIndex.value.get(JSON.stringify([def.on, def.name]));
    if (other !== undefined) {
      throw new Error(
        `${def.package} registers ${what}, which ${other.def.package} registered already.`,
      );
    }
    const validate = compile(what, def.package, def.schema);
    checkInitial(what, def.package, validate, def.initial, "an initial value");
    const item = { def, validate };
    attributes.value = [...attributes.value, item];
    return () => {
      attributes.value = attributes.value.filter((x) => x !== item);
      ajv.removeSchema(def.schema);
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
        const other = optionIndex.value.get(def.id);
        if (other !== undefined) {
          throw new Error(
            `${def.package} registers ${what}, which ${other.def.package} registered already.`,
          );
        }
        const validate = compile(what, def.package, def.schema);
        checkInitial(what, def.package, validate, def.schema.default, "a default");
        const item = { def, validate };
        options.value = [...options.value, item];
        return () => {
          options.value = options.value.filter((x) => x !== item);
          ajv.removeSchema(def.schema);
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

function describe(errors: readonly ErrorObject[]): string[] {
  return errors.map((error) => {
    const where = error.instancePath === "" ? "" : `${error.instancePath} `;
    const extra =
      "additionalProperty" in error.params
        ? ` "${String(error.params.additionalProperty)}"`
        : "allowedValues" in error.params
          ? ` ${JSON.stringify(error.params.allowedValues)}`
          : "";
    return `${where}${error.message ?? "is invalid"}${extra}`;
  });
}
