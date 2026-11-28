// R's replies from the wire protocol's fixtures, and a store holding the board
// of a core app, which R sends in its reply to `hello`, for the tests of the
// store and of what builds on it.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { Fixture, HelloReply, PluginEntry } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import type { JsonPlugin, OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// R's first reply in a fixture.
export function reply(name: string): unknown {
  const file = join(root, "inst", "protocol", "fixtures", `${name}.json`);
  const fixture = JSON.parse(readFileSync(file, "utf8")) as Fixture;
  return fixture.r[0]?.[0];
}

export const hello = reply("hello") as HelloReply;

// The UI module of a board option that a test never mounts, which draws
// nothing.
export const noUi: OptionModule = { render: () => undefined };

// What a test of the mounts gives them as CoreApi, which a board option's
// default that is a function gets: nothing, for a default that reads none of
// it.
export const noCore = (): CoreApi => ({}) as CoreApi;

// Core's board options: n_rows, whose value the fixtures' boards hold, and
// dark_mode, which core's basic frontend requires, with the light theme as its
// default, so that no test reads the theme of the system it runs on.
export const coreOptions: readonly Option[] = [
  {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema: { type: "integer", minimum: 1 },
    default: 50,
    ui: noUi,
  },
  {
    id: "dark_mode",
    label: "Light/dark mode",
    category: "Theme options",
    schema: { enum: ["light", "dark"] },
    default: "light",
    ui: noUi,
  },
];

// A store with core's board options that has opened the board of the reply to
// `hello`, that of the fixture hello unless given, once `setup` has registered
// what a test needs.
export async function openedStore(setup?: (store: Store) => void, replied: HelloReply = hello): Promise<Store> {
  const store = createStore();
  for (const option of coreOptions) store.options.register(option);
  setup?.(store);
  await store.open(replied);
  return store;
}

// R's reply to `hello` without the app's plugins of the classes given, as for
// an app whose rows leave them out, or where another plugin takes their place.
export function withoutPlugins(replied: HelloReply, ...classes: readonly ClassName[]): HelloReply {
  return {
    ...replied,
    plugins: replied.plugins.filter((entry) => !classes.some((cls) => cls.package === entry.package && cls.id === entry.id)),
  };
}

// A plugin class by its package and its id, as a test names one.
interface ClassName {
  readonly package: string;
  readonly id: string;
}

// A plugin that withPlugins() has the app run: its class's name, the state
// that the opening board holds for it, if any, and the board options that it
// reads, if any, as the method of R's `plugin_options()` for its class names
// them.
export interface GivenPlugin {
  readonly name: string;
  readonly state?: Json;
  readonly options?: readonly OptionId[];
}

// Where R serves the module of the plugin class with the id given, as the
// reply of withPlugins() names it, as "blockr.dag/dag_plugin.js".
export function pluginUrl(cls: ClassName): string {
  return `${cls.package}/${cls.id}.js`;
}

// Where R serves the module of the board option with the id given, which the
// package given lists, as the reply of withOptions() names it, as
// "blockr.test/zoom.js".
export function optionUrl(id: OptionId, pkg = "blockr.test"): string {
  return `${pkg}/${id}.js`;
}

// R's reply to `hello`, that of the fixture hello unless given, that names the
// board options given too, each with its module, which the package given lists,
// and the version given of its shape, 1 for one that the versions leave out.
export function withOptions(
  ids: readonly OptionId[],
  replied: HelloReply = hello,
  versions: Readonly<Record<OptionId, number>> = {},
  pkg = "blockr.test",
): HelloReply {
  return {
    ...replied,
    options: {
      ...replied.options,
      ...Object.fromEntries(
        ids.map((id) => [id, { package: pkg, module: optionUrl(id, pkg), version: versions[id] ?? 1 }]),
      ),
    },
  };
}

// R's reply to `hello`, that of the fixture hello unless given, whose app runs
// the plugins given too, by the package and then by the id of each one's
// class, after its own, where the reply names no plugin of the class, and in
// that plugin's place otherwise, with an opening board that holds the state
// given of each, as an app script's board would, and with the versions given
// of their classes, by package and id as well, which the reply's plugins carry
// and the board records beside each state, 1 for a class that they leave out.
// R serves the module of each class at pluginUrl().
export function withPlugins(
  plugins: Readonly<Record<string, Readonly<Record<string, GivenPlugin>>>>,
  replied: HelloReply = hello,
  versions: Readonly<Record<string, Readonly<Record<string, number>>>> = {},
): HelloReply {
  const given = Object.entries(plugins).flatMap(([pkg, byId]) =>
    Object.entries(byId).map(([id, plugin]) => ({ cls: { package: pkg, id }, plugin })),
  );
  const version = (cls: ClassName): number => versions[cls.package]?.[cls.id] ?? 1;
  const entry = ({ cls, plugin }: (typeof given)[number]): PluginEntry => ({
    package: cls.package,
    id: cls.id,
    name: plugin.name,
    version: version(cls),
    ...(plugin.options === undefined ? {} : { options: [...plugin.options] }),
    module: pluginUrl(cls),
  });
  const same = (a: ClassName, b: ClassName): boolean => a.package === b.package && a.id === b.id;
  const states: Record<string, Record<string, JsonPlugin>> = Object.fromEntries(
    Object.entries(replied.board.board.plugins).map(([pkg, byId]) => [pkg, { ...byId }]),
  );
  for (const { cls, plugin } of given) {
    if (plugin.state !== undefined) (states[cls.package] ??= {})[cls.id] = { state: plugin.state, version: version(cls) };
  }
  return {
    ...replied,
    plugins: [
      ...replied.plugins.map((held) => {
        const found = given.find(({ cls }) => same(cls, held));
        return found === undefined ? held : entry(found);
      }),
      ...given.filter(({ cls }) => !replied.plugins.some((held) => same(cls, held))).map(entry),
    ],
    board: {
      ...replied.board,
      board: { ...replied.board.board, plugins: states },
    },
  };
}
