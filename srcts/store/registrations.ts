// What packages register with the store, from their startup modules, before
// it opens a board: board classes, attributes, board options and each
// package's one migration of its saved state.
// A board class is registered under its R class name, with the properties it
// adds.
// A board class, an attribute or a board option id registered twice fails.

import { computed, signal } from "@preact/signals-core";
import type {
  Block,
  BlockId,
  Board,
  OptionId,
  RegistryId,
} from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, own } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";

// A board class, which the package that ships it defines in JS under its R
// class name, so that the definition and R's class meet at the name.
// It names its parent and the board options it requires, and gives the
// properties it adds beside core's, each with a schema and a default.
// Its package and its front-end come from R, where the package names the
// class's front-end through its method of `board_front_end()`, from its
// namespace, and R sends both with its reply to `hello`.
// A class that R names no front-end for comes from no package, and its
// parent's front-end draws it.
// What sits inside a property is the class's own design.
// The store applies patches to a property without knowing that design.
export interface BoardClass {
  // The R class it defines, as "dock_board".
  readonly class: string;
  // The class it extends, as "board", which a package registers before it.
  // Only core's `board` has none.
  readonly parent?: string;
  // The board options it requires, which the board gets with their defaults as
  // it opens, where it lacks them.
  readonly options?: readonly OptionId[];
  readonly properties: Readonly<Record<string, Property>>;
  // The patches to its own properties that follow from a change, such as
  // removing a removed block's panels from the dock's views.
  // The store applies them in the same step.
  // It gets the delta, as expanded so far, and the board before the delta,
  // so that it sees what the delta removes.
  // It reads nothing else: it calls no R and reads nothing on screen, so that
  // every peer that applies a change computes the same patches.
  // It mints an id only into the step it is part of.
  update?(delta: Delta, board: Board): readonly Patch[];
}

// A property that a board class adds.
export interface Property {
  // What values it takes, with descriptions and examples for a model.
  // The store checks the whole property against it after every change.
  readonly schema: JsonSchema;
  // What a board that lacks it holds as it opens.
  readonly default: Json;
}

// A value kept on every block that is not its state, such as core's `visible`.
export interface Attribute {
  readonly name: string;
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

// What a JSON board holds that migrations convert:
// the properties that board classes add, as the file holds them,
// and every block's state, with its class.
export interface Saved {
  readonly properties: Readonly<Record<string, Json>>;
  readonly blocks: Readonly<
    Record<BlockId, { readonly registry_id: RegistryId; readonly state: Block["state"] }>
  >;
}

// A package's migration, which rewrites what a JSON board holds,
// as the migrations before it left it, with its package's part in
// today's shape, given the version of the package that saved it,
// or null where the JSON board records none.
// Migrations run one after another,
// in the order in which packages declare their startup modules,
// which puts each package's after those of the packages it imports,
// and the board opens with the properties and the blocks' states that the last
// one returns, so that what a migration leaves out is gone.
// Each changes only its own package's part, which nothing but the package
// enforces: the properties its board classes add and the states of its block
// classes' blocks.
//
// A migration converts what older versions of its package saved into
// today's shape, and changes nothing on a board in today's shape,
// so that a board saved and opened again comes back as it was,
// though a JSON board that records no version of a package runs its migration,
// unless every package that it records is at the version R loaded.
// What a board reads where nothing set a value comes from
// what packages register instead, a property's `default`,
// an attribute's `initial` and a board option's `default`.
// Attributes and board option values are not migrated:
// one that no longer fits its schema is dropped as the board opens,
// so that it reads as its initial value or default.
export type Migration = (saved: Saved, version: string | null) => Saved;

// A registration with the validator of its schema.
export interface Checked<T> {
  readonly def: T;
  readonly validate: Validate;
}

// A board class with the validators of its properties' schemas, by name.
export interface CheckedClass {
  readonly def: BoardClass;
  readonly validate: Readonly<Record<string, Validate>>;
}

// The class that a board is built as: the first in the board's class vector
// that is registered, or core's `board` where none is.
export interface Built {
  readonly def: BoardClass;
  // The class and its parents, core's `board` first,
  // whose properties a board built as the class holds.
  readonly chain: readonly CheckedClass[];
  // Of each property that the chain adds,
  // the class that adds it and the validator of its schema,
  // by the property's name.
  readonly properties: ReadonlyMap<string, { readonly owner: CheckedClass; readonly validate: Validate }>;
}

export interface Registrations {
  register(def: BoardClass | Attribute): () => void;
  registered(): readonly (BoardClass | Attribute)[];
  readonly options: Options;
  registerMigration(pkg: string, fn: Migration): () => void;
  builtAs(classes: readonly string[]): Built;
  attributes(): readonly Checked<Attribute>[];
  attribute(name: string): Checked<Attribute> | undefined;
  option(id: OptionId): Checked<Option> | undefined;
  migrations(): ReadonlyMap<string, Migration>;
}

// Core's board class.
// The store builds a board as it where no class in the board's class vector
// is registered, so core's basic front-end draws every board at least.
export const CORE_BOARD: BoardClass = {
  class: "board",
  properties: {},
};

// The properties that every board holds, which no board class may add again.
export const CORE_PROPERTIES: readonly string[] = [
  "id",
  "class",
  "name",
  "blocks",
  "links",
  "stacks",
  "options",
];

export function createRegistrations(): Registrations {
  const classes = signal<ReadonlyMap<string, CheckedClass>>(new Map());
  const attributes = signal<readonly Checked<Attribute>[]>([]);
  const options = signal<readonly Checked<Option>[]>([]);
  const migrations = signal<ReadonlyMap<string, Migration>>(new Map());

  const attributeIndex = computed(
    () => new Map(attributes.value.map((attr) => [attr.def.name, attr])),
  );
  const optionIndex = computed(
    () => new Map(options.value.map((option) => [option.def.id, option])),
  );
  const registered = computed(() => [
    ...[...classes.value.values()].map((cls) => cls.def),
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

  // The class and its parents, core's `board` first,
  // or undefined where a parent is no longer registered.
  function chainOf(cls: CheckedClass): CheckedClass[] | undefined {
    const chain = [cls];
    let parent = cls.def.parent;
    while (parent !== undefined) {
      const next = classes.value.get(parent);
      if (next === undefined || chain.includes(next)) return undefined;
      chain.unshift(next);
      parent = next.def.parent;
    }
    return chain;
  }

  function registerClass(def: BoardClass, root = false): () => void {
    const name = def.class;
    if (typeof name !== "string" || name === "") {
      throw new Error(`The board class ${JSON.stringify(name)} has no name.`);
    }
    const what = `the board class "${name}"`;
    if (classes.value.has(name)) {
      throw new Error(`The board class "${name}" is registered already.`);
    }
    const parent = def.parent === undefined ? undefined : classes.value.get(def.parent);
    if (!root && parent === undefined) {
      throw new Error(
        def.parent === undefined
          ? `The board class "${name}" names no parent, which every board class but core's "board" does.`
          : `The board class "${name}" extends "${def.parent}", which no package registered.`,
      );
    }
    if (def.update !== undefined && typeof def.update !== "function") {
      throw new Error(`The board class "${name}" has an update that is no function.`);
    }

    const inherited = new Map<string, string>();
    for (const cls of (parent === undefined ? undefined : chainOf(parent)) ?? []) {
      for (const property of Object.keys(cls.def.properties)) inherited.set(property, cls.def.class);
    }

    const validate: Record<string, Validate> = {};
    for (const [property, given] of Object.entries(def.properties)) {
      if (CORE_PROPERTIES.includes(property)) {
        throw new Error(`The board class "${name}" adds the property "${property}", which every board holds as core's.`);
      }
      const owner = inherited.get(property);
      if (owner !== undefined) {
        throw new Error(
          `The board class "${name}" adds the property "${property}", which its parent "${owner}" adds already.`,
        );
      }
      const of = `the property "${property}" of ${what}`;
      if (!Object.hasOwn(given, "default")) {
        throw new Error(`The property "${property}" of ${what} has no default.`);
      }
      validate[property] = compile(of, given.schema);
      checkInitial(of, validate[property], given.default, "default");
    }

    const item: CheckedClass = { def, validate };
    classes.value = new Map([...classes.value, [name, item]]);
    return () => {
      if (classes.value.get(name) !== item) return;
      const rest = new Map(classes.value);
      rest.delete(name);
      classes.value = rest;
    };
  }

  function registerAttribute(def: Attribute): () => void {
    const what = `the attribute "${def.name}" of blocks`;
    if (attributeIndex.value.has(def.name)) {
      throw new Error(`The attribute "${def.name}" of blocks is registered already.`);
    }
    const validate = compile(what, def.schema);
    checkInitial(what, validate, def.initial, "initial value");
    const item = { def, validate };
    attributes.value = [...attributes.value, item];
    return () => {
      attributes.value = attributes.value.filter((x) => x !== item);
    };
  }

  function builtAs(vector: readonly string[]): Built {
    for (const name of vector) {
      const cls = classes.value.get(name);
      const chain = cls === undefined ? undefined : chainOf(cls);
      if (chain !== undefined) return built(chain);
    }
    const core = classes.value.get(CORE_BOARD.class);
    if (core === undefined) throw new Error('Core\'s board class "board" is not registered.');
    return built([core]);
  }

  registerClass(CORE_BOARD, true);

  return {
    register(def) {
      return "class" in def ? registerClass(def) : registerAttribute(def);
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
    builtAs,
    attributes: () => attributes.value,
    attribute: (name) => attributeIndex.value.get(name),
    option: (id) => optionIndex.value.get(id),
    migrations: () => migrations.value,
  };
}

function built(chain: readonly CheckedClass[]): Built {
  const properties = new Map<string, { owner: CheckedClass; validate: Validate }>();
  for (const cls of chain) {
    for (const [name, validate] of Object.entries(cls.validate)) {
      properties.set(name, { owner: cls, validate });
    }
  }
  const def = chain.at(-1)?.def ?? CORE_BOARD;
  return { def, chain, properties };
}

// The board taken as a record, by which code that does not import the types of
// its class's properties reads them.
export function recordOf(board: Board): Readonly<Record<string, unknown>> {
  return board as unknown as Readonly<Record<string, unknown>>;
}

// The properties of the board beside core's, by name.
export function propertiesOf(board: Board): Readonly<Record<string, unknown>> {
  return Object.fromEntries(
    Object.entries(recordOf(board)).filter(([name]) => !CORE_PROPERTIES.includes(name)),
  );
}

// The default of a property, as the class that adds it gives it.
export function defaultOf(built: Built, name: string): Json | undefined {
  const at = built.properties.get(name);
  return at === undefined ? undefined : own(at.owner.def.properties, name)?.default;
}
