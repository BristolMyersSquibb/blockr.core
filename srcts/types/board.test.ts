// @vitest-environment node

// Checks the fixtures of boards that app scripts build, in inst/board/fixtures,
// against core's types: the JSON that R writes for each, as R sends the board,
// and the file that the client writes for the board it opens that JSON to.
// Each fixture is type-checked as the value of a variable of its type, as the
// fixtures of the wire protocol are, and every id is given, so that the file
// holds the JSON's entries keyed by their ids, in their order.

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, expect, test } from "vitest";
import { declaration, typecheck } from "../testing/typecheck.ts";
import type { Board, OpeningBoard, SavedBoard } from "./board.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const fixtures = join(root, "inst", "board", "fixtures");
const board = join(root, "srcts", "types", "board.ts");

const type = "{ description: string; board: OpeningBoard; file: SavedBoard }";

interface Fixture {
  description: string;
  board: OpeningBoard;
  file: SavedBoard;
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

// A fixture whose board names its class in a field that a board lacks, which
// the check must fail.
const fixture = read("board");
const [first, ...rest] = fixture.board.blocks;
const broken = {
  ...fixture,
  board: { ...fixture.board, blocks: [{ ...first, class: "join_block" }, ...rest] },
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

// The entries of a collection as the board holds them, keyed by the ids that
// R sent with them, which a fixture gives every one of.
function keyed<T extends { id?: string }>(
  entries: readonly T[],
): Record<string, Omit<T, "id">> {
  return Object.fromEntries(
    entries.map(({ id, ...entry }) => {
      if (id === undefined) throw new Error("A fixture gives every id.");
      return [id, entry];
    }),
  );
}

function opened(json: OpeningBoard): Board {
  const { id, name } = json;
  if (id === undefined || name === undefined) {
    throw new Error("A fixture gives the board's id and name.");
  }
  return {
    id,
    front_end: json.front_end,
    name,
    blocks: keyed(json.blocks),
    links: keyed(json.links),
    stacks: keyed(json.stacks),
    options: json.options,
  };
}

test("there are fixtures", () => {
  expect(names).toContain("board");
  expect(names).toContain("front-end-state");
});

test.each(names)("%s meets the types of the board and the saved file", (name) => {
  expect(diagnostics.get(path(name))).toEqual([]);
});

test("the check fails a fixture with a field that its type lacks", () => {
  const found = diagnostics.get(path("broken")) ?? [];
  expect(found.join("\n")).toMatch(/'"class"' does not exist in type 'Block & /);
});

test.each(names)("the file of %s holds its board keyed by id", (name) => {
  const { board: json, file } = read(name);

  expect(file.format).toBe(1);
  expect(file.board).toEqual(opened(json));

  // A collection keeps the order of its entries, as JSON.parse() keeps the
  // order of keys that do not look like integers.
  for (const key of ["blocks", "links", "stacks"] as const) {
    expect(Object.keys(file.board[key])).toEqual(json[key].map((entry) => entry.id));
  }
});
