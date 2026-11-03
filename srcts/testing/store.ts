// R's replies from the wire protocol's fixtures, and a store holding the board
// of a core app, which R sends in its reply to `hello`, for the tests of the
// store and of what builds on it.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { Fixture, HelloReply, OpenReply, Reply } from "../protocol/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store, StoreOptions } from "../store/index.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// R's first reply in a fixture.
export function reply(name: string): unknown {
  const file = join(root, "inst", "protocol", "fixtures", `${name}.json`);
  const fixture = JSON.parse(readFileSync(file, "utf8")) as Fixture;
  return fixture.r[0]?.[0];
}

export const hello = reply("hello") as HelloReply;
export const loaded = reply("open") as OpenReply;

// R, as the fixtures give its replies, which records what the store sent.
type Client = StoreOptions["client"];
type Sent = Parameters<Client["request"]>[0];

export function fakeR(answer: Reply = loaded): Client & { sent: Sent[] } {
  const sent: Sent[] = [];
  return {
    sent,
    request(request) {
      sent.push(request);
      return Promise.resolve({ reply: answer, body: null });
    },
  };
}

// Core's board options, as the fixtures' boards hold their values.
export const coreOptions: readonly Option[] = [
  {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema: { type: "integer", minimum: 0 },
    default: 50,
  },
  {
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema: { type: "array", items: { enum: ["message", "warning", "error"] } },
    default: ["warning", "error"],
  },
];

// A store with core's board options that has opened the board of the reply to
// `hello`, once `setup` has registered what a test needs.
export async function openedStore(setup?: (store: Store) => void): Promise<Store> {
  const store = createStore({ client: fakeR() });
  for (const option of coreOptions) store.options.register(option);
  setup?.(store);
  await store.open(hello);
  return store;
}
