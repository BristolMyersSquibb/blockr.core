// @vitest-environment node

// Checks the sample outputs of core's output classes, in inst/outputs/fixtures,
// against the types of their classes. Each fixture is type-checked as the
// value of a variable of its type, as the fixtures of the wire protocol are.

import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, expect, test } from "vitest";
import { declaration, typecheck } from "../testing/typecheck.ts";
import type { OutputFixtures } from "./outputs.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const fixtures = join(root, "inst", "outputs", "fixtures");
const outputs = join(root, "srcts", "types", "outputs.ts");

// The type of each fixture, by its file's name.
const types: Record<string, string> = {
  "table-output": "OutputFixtures<TableOutput>",
  "text-output": "OutputFixtures<TextOutput>",
  "plot-output": "OutputFixtures<PlotOutput>",
};

const names = readdirSync(fixtures)
  .filter((file) => file.endsWith(".json"))
  .map((file) => file.slice(0, -".json".length))
  .sort();

function text(name: string): string {
  return readFileSync(join(fixtures, `${name}.json`), "utf8").trimEnd();
}

function read(name: string): OutputFixtures {
  return JSON.parse(text(name)) as OutputFixtures;
}

// A table fixture whose first output misspells a property, which the check
// must fail.
const table = read("table-output");
const [first] = table.outputs;
const broken = {
  ...table,
  outputs: [{ ...first, output: { ...first?.output, totl: 7 } }],
};

// Where the module that declares a fixture is, which exists only in memory.
function path(name: string): string {
  return join(root, "srcts", "types", "__fixtures__", `${name}.ts`);
}

let diagnostics: Map<string, string[]>;

beforeAll(() => {
  const modules = new Map<string, string>();

  for (const name of names) {
    const type = types[name];
    if (type === undefined) continue;
    modules.set(path(name), declaration(type, outputs, text(name)));
  }

  modules.set(
    path("broken"),
    declaration(
      types["table-output"] ?? "",
      outputs,
      JSON.stringify(broken, null, 2),
    ),
  );

  diagnostics = typecheck(root, modules);
}, 60_000);

test("every fixture has a type to be checked against", () => {
  expect(names).toEqual(Object.keys(types).sort());
});

test.each(names)("%s meets the type of its output class", (name) => {
  expect(diagnostics.get(path(name))).toEqual([]);
});

test("the check fails a fixture with a property that its type lacks", () => {
  const found = diagnostics.get(path("broken")) ?? [];
  expect(found.join("\n")).toMatch(/'"totl"' does not exist in type 'TableOutput'/);
});

test("every body of a sample output is a file beside its fixture", () => {
  const files = names.flatMap((name) =>
    read(name).outputs.flatMap((sample) =>
      sample.body_file === undefined ? [] : [sample.body_file],
    ),
  );

  expect(files.length).toBeGreaterThan(0);

  for (const file of files) {
    expect(existsSync(join(fixtures, file)), file).toBe(true);
  }
});
