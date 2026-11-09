// R's replies from the wire protocol's fixtures, and a store holding the board
// of a core app, which R sends in its reply to `hello`, for the tests of the
// store and of what builds on it.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { Fixture, HelloReply } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import type { Plugin, PluginClassId, PluginId } from "../types/board.ts";
import type { OptionModule } from "../types/hosts.ts";

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

// R's reply to `hello`, that of the fixture hello unless given, with an
// opening board that holds the plugins given too, as an app script's board
// would, and with the versions given of their classes, which R sends and the
// board records, 1 for a class that they leave out.
export function withPlugins(
  plugins: Readonly<Record<PluginId, Plugin>>,
  replied: HelloReply = hello,
  versions: Readonly<Record<PluginClassId, number>> = {},
): HelloReply {
  const classes = [...new Set(Object.values(plugins).map((plugin) => plugin.class))];
  return {
    ...replied,
    plugin_versions: { ...replied.plugin_versions, ...versions },
    board: {
      ...replied.board,
      board: {
        ...replied.board.board,
        versions: {
          ...replied.board.board.versions,
          ...Object.fromEntries(classes.map((cls) => [cls, versions[cls] ?? 1])),
        },
        plugins: { ...replied.board.board.plugins, ...plugins },
      },
    },
  };
}
