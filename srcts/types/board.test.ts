// @vitest-environment node

// Checks the fixtures of boards that app scripts build, in inst/board/fixtures,
// against core's types: the JSON board that R writes for each, which the
// client opens and saves again as it is.
// Each fixture is type-checked as the value of a variable of its type,
// as the fixtures of the wire protocol are, and every id is given,
// so that nothing is drawn at random.
// A board holds the properties of its class beside core's,
// which core's types leave open.

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, expect, test } from "vitest";
import { createStore } from "../store/index.ts";
import type { BoardClass } from "../store/index.ts";
import { coreOptions, hello } from "../testing/store.ts";
import { declaration, typecheck } from "../testing/typecheck.ts";
import type { JsonBoard } from "./board.ts";
import type { RegistryEntry } from "./registry.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const fixtures = join(root, "inst", "board", "fixtures");
const board = join(root, "srcts", "types", "board.ts");

const type = "JsonBoard & { description: string; board: Readonly<Record<string, unknown>> }";

interface Fixture extends JsonBoard {
  description: string;
}

const names = readdirSync(fixtures)
  .filter((file) => file.endsWith(".json"))
  .map((file) => file.slice(0, -".json".length))
  .sort();

function text(name: string): string {
  return readFileSync(join(fixtures, `${name}.json`), "utf8").trimEnd();
}

function read(name: string): Fixture {
  return JSON.parse(text(name)) as Fixture;
}

// A fixture whose block names its class in a field that a block lacks, which
// the check must fail.
const fixture = read("board");
const broken = {
  ...fixture,
  board: {
    ...fixture.board,
    blocks: { ...fixture.board.blocks, a: { ...fixture.board.blocks.a, class: "join_block" } },
  },
};

// Where the module that declares a fixture is, which exists only in memory.
function path(name: string): string {
  return join(root, "srcts", "types", "__fixtures__", `${name}.ts`);
}

let diagnostics: Map<string, string[]>;

beforeAll(() => {
  const modules = new Map<string, string>();

  for (const name of names) {
    modules.set(path(name), declaration(type, board, text(name)));
  }

  modules.set(
    path("broken"),
    declaration(type, board, JSON.stringify(broken, null, 2)),
  );

  diagnostics = typecheck(root, modules);
}, 60_000);

// The block registry's entries of the fixtures' classes, which are defined
// outside a package, so that their registry ids are their names, with the
// inputs that the fixtures' links feed.
function entry(id: string, inputs: readonly string[]): RegistryEntry {
  return {
    id,
    name: id,
    description: "",
    keywords: [],
    category: "",
    icon: "",
    inputs,
    module: "",
    version: 1,
    schema: {},
    state: {},
    arguments: {},
    requests: {},
  };
}

// The board class that blockr.test defines, which the fixture board-class is
// built as.
const grid: BoardClass = {
  class: "grid_board",
  parent: "board",
  properties: {
    notes: { schema: { type: "object" }, default: {} },
    pinned: { schema: { type: "string" }, default: "" },
  },
};

function jsonOf({ format, packages, board }: Fixture): JsonBoard {
  return { format, packages, board };
}

test("there are fixtures", () => {
  expect(names).toContain("board");
  expect(names).toContain("board-class");
});

test.each(names)("%s meets the type of a JSON board", (name) => {
  expect(diagnostics.get(path(name))).toEqual([]);
});

test("the check fails a fixture with a field that its type lacks", () => {
  const found = diagnostics.get(path("broken")) ?? [];
  expect(found.join("\n")).toMatch(/'"class"' does not exist in type 'JsonBlock'/);
});

// R loaded the packages at the versions that the JSON board records, so that
// the store runs no migration.
test.each(names)("the store opens %s and saves it as it is", async (name) => {
  const json = jsonOf(read(name));
  const store = createStore();
  for (const option of coreOptions) store.options.register(option);
  store.register(grid);

  await store.open({
    ...hello,
    packages: json.packages,
    board_classes: {
      ...hello.board_classes,
      grid_board: { package: "blockr.test", front_end: "blockr.test-grid", version: 1 },
    },
    block_registry: {
      letters_block: entry("letters_block", []),
      join_block: entry("join_block", ["x", "y"]),
    },
    board: json,
  });

  expect(store.jsonBoard()).toEqual(json);

  // A collection keeps the order of its entries, as JSON.parse() keeps the
  // order of keys that do not look like integers.
  for (const key of ["blocks", "links", "stacks"] as const) {
    expect(Object.keys(store.board()[key])).toEqual(Object.keys(json.board[key]));
  }
});
