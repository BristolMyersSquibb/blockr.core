// What packages register with the store, from their startup modules, before
// it opens a board: plugin classes, board options and the migrations of their
// block classes' states.
// A plugin class is registered under its id, `{pkg}-{name}_plugin`, as R names
// the class.
// A board option's id names no package, so the registrations keep beside each
// board option the package whose startup module registered it. Core tells
// them that package.
// A plugin class, a board option id or a block class's migration registered
// twice fails. For a board option id, the error names the package that
// registered it, where core knows it.

import { computed, signal } from "@preact/signals-core";
import type {
  Block,
  Board,
  OptionId,
  PluginClassId,
  PluginId,
  RegistryId,
} from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, isObject, packageOf } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";

// A plugin class, which the package that ships it defines in JS under the
// class's id, as R names the class, so that the definition and R's class meet
// at the id.
// A class sets any mix of its members, or none of `ui`, `frontend` and
// `storage`, as a plugin without a place on the screen does.
// The store reads `state`, `update` and `migrate`, and tells a frontend and a
// storage plugin by `frontend` and `storage`, of which a board holds at most
// one each.
// Core reads the rest.
// What sits inside a plugin's state is the class's own design, and the store
// applies patches to it without knowing that design.
export interface PluginClass {
  // As "blockr.dag-dag_plugin".
  readonly id: PluginClassId;
  // The board options its plugins require.
  readonly options?: readonly OptionId[];
  // What each of its plugins keeps in the board: the schema of the state, with
  // descriptions and examples for a model, against which the store checks the
  // state after every change, and the state that a plugin of the app's
  // opening board starts with on a board that opens without it.
  // The plugins of a class without one each keep an empty object.
  readonly state?: { readonly schema: JsonSchema; readonly default: Json };
  // The patches to the state of the plugin `plugin` that follow from a change,
  // such as removing a removed block's position from the DAG's positions.
  // The store calls it once for each plugin of the class that the board holds
  // before the change, with every change it applies, and applies what it
  // returns in the same step.
  // It gets the delta, as expanded so far, and the board before the delta,
  // so that it sees what the delta removes.
  // It reads nothing else: it calls no R and reads nothing on screen, so that
  // every peer that applies a change computes the same patches.
  // It mints an id only into the step it is part of.
  update?(delta: Delta, board: Board, plugin: PluginId): readonly Patch[];
  // Converts a plugin's state, as a JSON board holds it, from the version of
  // the class that the board records to the one that R's `plugin_version()`
  // gives, and returns it as the board then holds it, so that what it leaves
  // out is gone.
  // The store calls it as it opens a board whose recorded version of the class
  // is below R's, with that version, for each plugin of the class.
  migrate?(state: Json, version: number): Json;
  // The module that every frontend places, once for each plugin of the class.
  readonly ui?: string;
  // A frontend's: the module that draws the page.
  readonly frontend?: string;
  // A storage plugin's: what every frontend's save and open controls call.
  readonly storage?: Storage;
  // What runs while the open board holds the plugin, until the signal aborts.
  // It may return a cleanup, which core runs as the signal aborts.
  start?(args: { readonly core: CoreApi; readonly plugin: PluginId; readonly signal: AbortSignal }): (() => void) | undefined;
}

// What a storage plugin saves boards and opens them through.
export interface Storage {
  // Saves the JSON board's text, as the store's save() writes it.
  save(board: string): Promise<void>;
  // The text of the board that the user picks, or null where they pick none.
  open(): Promise<string | null>;
}

// The plugin classes that packages define, by id.
export interface Plugins {
  register(def: PluginClass): () => void;
  registered(): Readonly<Record<PluginClassId, PluginClass>>;
}

// A board option, whose value the board holds under its id, while the package
// that ships it defines the rest. Where something needs the board option and
// the board lacks its value, core adds it to the board with its `default`: a
// value, or what the function that is its default resolves with.
export interface Option {
  readonly id: OptionId;
  readonly label: string;
  readonly category: string;
  readonly schema: JsonSchema;
  readonly default: Json | (() => Promise<Json>);
  // Its UI module, which core mounts with an OptionHost where the app names no
  // renderer for the board option.
  readonly ui: OptionModule;
  // What a new value does on the page, such as switching the theme. Core
  // calls it as each board opens, with the board's value, and again as a delta
  // sets the value. Where the board lacks the value, core adds the board
  // option with its default first.
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
// what packages register instead, a plugin class's default state
// and a board option's `default`.
export type Migration = (state: Block["state"], version: number) => Block["state"];

// A registration with the validator of its schema.
export interface Checked<T> {
  readonly def: T;
  readonly validate: Validate;
}

// A board option's registration, with the package whose startup module
// registered it: blockr.core for core's own board options, and null for one
// that no startup module's initialize() registered, such as one that a test
// registers.
interface RegisteredOption extends Checked<Option> {
  readonly package: string | null;
}

export interface Registrations {
  readonly plugins: Plugins;
  readonly options: Options;
  registerMigration(registryId: RegistryId, fn: Migration): () => void;
  // A plugin class with the validator of its state's schema, which allows any
  // state where the class gives none.
  plugin(id: PluginClassId): Checked<PluginClass> | undefined;
  option(id: OptionId): Checked<Option> | undefined;
  migration(registryId: RegistryId): Migration | undefined;
}

// The board options that core's basic frontend requires, the severities of
// the conditions it shows and the theme, which every board gets with their
// defaults as it opens, where it lacks them, since core's basic frontend draws
// every board.
export const BASIC_OPTIONS: readonly OptionId[] = ["show_conditions", "dark_mode"];

// The registrations of one opening of a board. Each board option comes from
// the package that `registering` gives at the moment the board option is
// registered. Core gives the package of the startup module whose initialize()
// it awaits, and blockr.core while it registers its own board options.
export function createRegistrations(registering: () => string | null = () => null): Registrations {
  const classes = signal<ReadonlyMap<PluginClassId, Checked<PluginClass>>>(new Map());
  const options = signal<readonly RegisteredOption[]>([]);
  const migrations = signal<ReadonlyMap<RegistryId, Migration>>(new Map());

  const optionIndex = computed(
    () => new Map(options.value.map((option) => [option.def.id, option])),
  );
  const classDefs = computed(() =>
    Object.fromEntries([...classes.value].map(([id, cls]) => [id, cls.def])),
  );
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

  function registerPlugin(def: PluginClass): () => void {
    const { id } = def;
    if (typeof id !== "string" || !isPluginClassId(id)) {
      throw new Error(
        `The plugin class id ${JSON.stringify(id)} is not named as R names a plugin class, "{pkg}-{name}_plugin".`,
      );
    }
    if (classes.value.has(id)) {
      throw new Error(`The plugin class "${id}" is registered already.`);
    }
    // A package that writes its JS without types may give members of the
    // wrong kind.
    const given = def as unknown as Readonly<Record<string, unknown>>;
    for (const [member, article] of [["update", "an"], ["migrate", "a"], ["start", "a"]] as const) {
      if (given[member] !== undefined && typeof given[member] !== "function") {
        throw new Error(`The plugin class "${id}" has ${article} ${member} that is no function.`);
      }
    }
    for (const member of ["ui", "frontend"]) {
      if (given[member] !== undefined && (typeof given[member] !== "string" || given[member] === "")) {
        throw new Error(`The plugin class "${id}" has a ${member} that is no module's id.`);
      }
    }
    const storage = given.storage;
    if (
      storage !== undefined &&
      (!isObject(storage) || typeof storage.save !== "function" || typeof storage.open !== "function")
    ) {
      throw new Error(`The plugin class "${id}" has a storage without save() and open() functions.`);
    }
    if (
      given.options !== undefined &&
      (!Array.isArray(given.options) || !given.options.every((option) => typeof option === "string"))
    ) {
      throw new Error(`The plugin class "${id}" requires board options that are no array of their ids.`);
    }

    const what = `the state of the plugin class "${id}"`;
    let validate: Validate = () => [];
    const state = given.state;
    if (state !== undefined) {
      if (!isObject(state) || !isObject(state.schema) || !Object.hasOwn(state, "default")) {
        throw new Error(`The plugin class "${id}" gives a state without a schema and a default.`);
      }
      validate = compile(what, state.schema as JsonSchema);
      checkInitial(what, validate, state.default as Json, "default");
    }

    const item: Checked<PluginClass> = { def, validate };
    classes.value = new Map([...classes.value, [id, item]]);
    return () => {
      if (classes.value.get(id) !== item) return;
      const rest = new Map(classes.value);
      rest.delete(id);
      classes.value = rest;
    };
  }

  return {
    plugins: {
      register: registerPlugin,
      registered: () => classDefs.value,
    },
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        const earlier = optionIndex.value.get(def.id);
        if (earlier !== undefined) {
          const by = earlier.package === null ? "" : ` by ${earlier.package}`;
          throw new Error(`The board option "${def.id}" is registered already${by}.`);
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
        const item: RegisteredOption = { def, validate, package: registering() };
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
    plugin: (id) => classes.value.get(id),
    option: (id) => optionIndex.value.get(id),
    migration: (registryId) => migrations.value.get(registryId),
  };
}

// Whether an id is named as R names a plugin class, `{pkg}-{name}_plugin`:
// a package name, a dash, and a name that ends in `_plugin`.
export function isPluginClassId(id: string): boolean {
  const pkg = packageOf(id);
  return pkg !== null && /^.+_plugin$/.test(id.slice(pkg.length + 1));
}
