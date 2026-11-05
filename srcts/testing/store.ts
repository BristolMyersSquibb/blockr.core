// R's replies from the wire protocol's fixtures, and a store holding the board
// of a core app, which R sends in its reply to `hello`, for the tests of the
// store and of what builds on it.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { Fixture, HelloReply } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
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

// Core's board options, as the fixtures' boards hold their values.
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
];

// A store with core's board options that has opened the board of the reply to
// `hello`, once `setup` has registered what a test needs.
export async function openedStore(setup?: (store: Store) => void): Promise<Store> {
  const store = createStore();
  for (const option of coreOptions) store.options.register(option);
  setup?.(store);
  await store.open(hello);
  return store;
}
