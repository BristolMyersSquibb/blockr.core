// R's replies from the wire protocol's fixtures, and a store holding the board
// of a core app, which R sends in its reply to `hello`, for the tests of the
// store and of what builds on it.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { Fixture, HelloReply } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
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

// Core's board options, as the fixtures' boards hold their values, and
// dark_mode, which core's basic frontend requires too, with the light theme as
// its default, so that no test reads the theme of the system it runs on.
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
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema: { type: "array", items: { enum: ["message", "warning", "error"] } },
    default: ["warning", "error"],
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
export function withoutPlugins(replied: HelloReply, ...classes: readonly PluginClassId[]): HelloReply {
  return {
    ...replied,
    plugins: Object.fromEntries(Object.entries(replied.plugins).filter(([id]) => !classes.includes(id))),
  };
}

// A plugin that withPlugins() has the app run: its class's name, and the state
// that the opening board holds for it, if any.
export interface GivenPlugin {
  readonly name: string;
  readonly state?: Json;
}

// R's reply to `hello`, that of the fixture hello unless given, whose app runs
// the plugins given too, after its own, each under the id of its class, with
// an opening board that holds the state given of each, as an app script's
// board would, and with the versions given of their classes, which the
// reply's plugins carry and the board records, 1 for a class that they leave
// out.
export function withPlugins(
  plugins: Readonly<Record<PluginClassId, GivenPlugin>>,
  replied: HelloReply = hello,
  versions: Readonly<Record<PluginClassId, number>> = {},
): HelloReply {
  const version = (cls: PluginClassId): number => versions[cls] ?? 1;
  const held = Object.entries(plugins).flatMap(([cls, given]) =>
    given.state === undefined ? [] : [[cls, given.state] as const],
  );
  return {
    ...replied,
    plugins: {
      ...replied.plugins,
      ...Object.fromEntries(Object.entries(plugins).map(([cls, given]) => [cls, { name: given.name, version: version(cls) }])),
    },
    board: {
      ...replied.board,
      board: {
        ...replied.board.board,
        versions: {
          ...replied.board.board.versions,
          ...Object.fromEntries(held.map(([cls]) => [cls, version(cls)])),
        },
        plugins: { ...replied.board.board.plugins, ...Object.fromEntries(held) },
      },
    },
  };
}
