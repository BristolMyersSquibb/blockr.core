// What packages register with the store, from their startup modules, before
// it opens a board: board classes, board options and the migrations of their
// block classes' states.
// A board class is registered under its R class name, with the properties it
// adds.
// A board class, a board option id or a block class's migration registered
// twice fails.

import { computed, signal } from "@preact/signals-core";
import type {
  Block,
  Board,
  OptionId,
  RegistryId,
} from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, isObject, own } from "../util/index.ts";
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
  // Converts the properties beside core's, as a JSON board holds them, from
  // the version of this class that the board records to the one R gives,
  // which R's `board_version()` gives, and returns them as the board then
  // holds them, so that what it leaves out is gone.
  // The store calls it as it opens a board whose recorded version of the class
  // is below R's, with that version, for the class and each of its parents,
  // root first.
  // It changes only the properties that its own class adds, which nothing but
  // the package enforces, and may rename, split and merge them.
  migrate?(properties: Readonly<Record<string, Json>>, version: number): Readonly<Record<string, Json>>;
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
  // Its UI module, which core mounts with an OptionHost where the app names no
  // renderer for the board option.
  readonly ui: OptionModule;
  // What a new value does on the page, such as switching the theme.
  onChange?(value: Json, core: CoreApi): void;
  // Converts the value, as a JSON board holds it, from the version of its
  // shape that the board records to the one that the package's method of R's
  // `board_option_version()` gives, and returns it as the board then holds it.
  // The store calls it as it opens a board whose recorded version of the value
  // is below R's, with that version.
  migrate?(value: Json, version: number): Json;
}

// The board options packages define, by id.
export interface Options {
  register(option: Option): () => void;
  registered(): Readonly<Record<OptionId, Option>>;
}

// A block class's migration, which converts a block's state, as a JSON board
// holds it, from the version of the class's state that the block records to
// the one that the class's entry in the block registry gives, which R's
// `new_block_class()` takes.
// A package's startup module registers it under the class's registry id, and
// the store calls it as it opens a board, for each block of the class whose
// version is below the class's, with that version.
// A migration converts what older versions of its class wrote into today's
// shape, so that a board saved and opened again comes back as it was.
// What a board reads where nothing set a value comes from
// what packages register instead, a property's `default`
// and a board option's `default`.
export type Migration = (state: Block["state"], version: number) => Block["state"];

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
  register(def: BoardClass): () => void;
  registered(): readonly BoardClass[];
  readonly options: Options;
  registerMigration(registryId: RegistryId, fn: Migration): () => void;
  builtAs(classes: readonly string[]): Built;
  option(id: OptionId): Checked<Option> | undefined;
  migration(registryId: RegistryId): Migration | undefined;
}

// Core's board class.
// The store builds a board as it where no class in the board's class vector
// is registered, so core's basic front-end draws every board at least.
// It requires the board options that core's basic front-end reads, the
// severities of the conditions it shows and the theme, as today's default
// board options are these two, so that every board gets them as it opens.
export const CORE_BOARD: BoardClass = {
  class: "board",
  options: ["show_conditions", "dark_mode"],
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
  const options = signal<readonly Checked<Option>[]>([]);
  const migrations = signal<ReadonlyMap<RegistryId, Migration>>(new Map());

  const optionIndex = computed(
    () => new Map(options.value.map((option) => [option.def.id, option])),
  );
  const registered = computed(() => [...classes.value.values()].map((cls) => cls.def));
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
    if (def.migrate !== undefined && typeof def.migrate !== "function") {
      throw new Error(`The board class "${name}" has a migrate that is no function.`);
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
    register: (def) => registerClass(def),
    registered: () => registered.value,
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        if (optionIndex.value.has(def.id)) {
          throw new Error(`The board option "${def.id}" is registered already.`);
        }
        // A package that writes its JS without types may give no UI module,
        // or an onChange or a migrate that is no function.
        const ui: unknown = def.ui;
        if (!isObject(ui) || typeof ui.render !== "function") {
          throw new Error(`The board option "${def.id}" has no UI module with a render() function.`);
        }
        const { onChange } = def as { readonly onChange?: unknown };
        if (onChange !== undefined && typeof onChange !== "function") {
          throw new Error(`The onChange of the board option "${def.id}" is no function.`);
        }
        if (def.migrate !== undefined && typeof def.migrate !== "function") {
          throw new Error(`The board option "${def.id}" has a migrate that is no function.`);
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
    registerMigration(registryId, fn) {
      if (migrations.value.has(registryId)) {
        throw new Error(`The block class "${registryId}" has a migration registered already.`);
      }
      migrations.value = new Map([...migrations.value, [registryId, fn]]);
      return () => {
        if (migrations.value.get(registryId) !== fn) return;
        const rest = new Map(migrations.value);
        rest.delete(registryId);
        migrations.value = rest;
      };
    },
    builtAs,
    option: (id) => optionIndex.value.get(id),
    migration: (registryId) => migrations.value.get(registryId),
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
