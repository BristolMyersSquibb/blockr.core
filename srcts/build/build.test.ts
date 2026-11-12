// @vitest-environment node

import { createRequire } from "node:module";
import { mkdir, mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { afterEach, beforeEach, expect, test } from "vitest";
import { IMPORTS, REACT_ENTRIES, importFile } from "../import-map/index.ts";
import { build, idWords, outputTypes, schemaFile, schemas, stylesheet } from "./build.ts";

// The fixtures import and reference core's stylesheet, so that they test core's
// setup of Tailwind CSS.
const core = resolve(dirname(fileURLToPath(import.meta.url)), "../runtime/index.css");

let root: string;

beforeEach(async () => {
  root = await mkdtemp(join(tmpdir(), "blockr-core-build-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

async function write(files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    const file = join(root, path);
    await mkdir(dirname(file), { recursive: true });
    await writeFile(file, content);
  }
}

test("bundles the runtime and each module, each with its stylesheet beside it", async () => {
  await write({
    "srcts/runtime/index.ts": "document.title = 'runtime';\n",
    "srcts/runtime/index.css": `@import "${core}";\n@source "../modules";\n`,
    "srcts/modules/head-block/index.ts": "export const ui = 'bc:flex';\n",
    "srcts/modules/head-block/index.css": `@reference "${core}";\n.head-block { @apply bc:p-2; }\n`,
    "srcts/modules/plain/index.ts": "export const ui = 'plain';\n",
    "inst/js/gone.js": "",
  });

  await build(root);

  const out = join(root, "inst/js");
  // Beside them, the build writes React's entry points and the import map.
  expect((await readdir(out)).filter((file) => !/^react|^import-map/.test(file)).sort()).toEqual([
    "blockr-core.css",
    "blockr-core.js",
    "head-block.css",
    "head-block.js",
    "plain.js",
  ]);

  // Core's stylesheet holds the utilities of every module.
  expect(await readFile(join(out, "blockr-core.css"), "utf8")).toMatch(/^\.bc\\:flex \{/m);

  // A module's stylesheet holds only its own rules.
  const own = await readFile(join(out, "head-block.css"), "utf8");
  expect(own).toMatch(/^\.head-block \{\n {2}padding: /m);
  expect(own).not.toContain(".bc\\:");
  expect(own).not.toMatch(/--bc-[\w-]+:/);
});

test("writes each of React's entry points as an ES module, exporting what the entry point exports, all on one React", async () => {
  await write({ "srcts/runtime/index.ts": "export const runtime = 'core';\n" });

  const written = await build(root);

  const out = join(root, "inst/js");
  const bundles = (await readdir(out)).filter((file) => /^react-[0-9a-f]{8}\.js$/.test(file));
  expect(bundles).toHaveLength(1);
  expect(written).toEqual(
    ["blockr-core.js", "import-map.json", ...bundles, ...REACT_ENTRIES.map(importFile)].map((file) => `inst/js/${file}`).sort(),
  );
  // The bundle keeps React's license comments.
  expect(await readFile(join(out, bundles[0] ?? ""), "utf8")).toContain("@license React");

  // Each entry point exports what React's does in production, bar the two
  // names that its development build alone exports.
  const installed = createRequire(import.meta.url);
  for (const name of REACT_ENTRIES) {
    const module = (await import(pathToFileURL(join(out, importFile(name))).href)) as Record<string, unknown>;
    const exported = Object.keys(installed(name) as object).filter((key) => !["act", "captureOwnerStack"].includes(key));
    expect(Object.keys(module).sort()).toEqual([...exported, "default"].sort());
  }
});

test("builds the SDK, the binding and the controls on their own, which the runtime and the modules import by name", async () => {
  const imports = (up: string) =>
    [
      `import { sdk } from "${up}/react/index.ts";`,
      `import { input } from "${up}/inputs/index.ts";`,
      `import { binding } from "${up}/binding/index.ts";`,
      `import { controls } from "${up}/controls/index.ts";`,
      "export const all = [sdk, input, binding, controls];",
      "",
    ].join("\n");
  await write({
    "srcts/react/index.ts": 'import { binding } from "../binding/index.ts";\nexport const sdk = "the sdk " + binding;\nexport { input } from "../inputs/index.ts";\n',
    "srcts/inputs/index.ts": "export const input = 'the input';\n",
    "srcts/binding/index.ts": "export const binding = 'the binding';\n",
    "srcts/controls/index.ts": 'import { input } from "../inputs/index.ts";\nexport const controls = "the controls " + input;\n',
    "srcts/runtime/index.ts": imports(".."),
    "srcts/modules/head-block/index.ts": imports("../.."),
  });

  await build(root);

  const read = (file: string) => readFile(join(root, "inst/js", file), "utf8");
  for (const file of ["blockr-core.js", "head-block.js"]) {
    const code = await read(file);
    for (const text of ["the sdk", "the input", "the binding", "the controls"]) {
      expect(code).not.toContain(text);
    }
    for (const name of ["@blockr/core/react", "@blockr/core/binding", "@blockr/core/controls"]) {
      expect(code).toContain(`from "${name}"`);
    }
  }
  // The SDK holds the inputs, and imports the binding.
  expect(await read("blockr-core-react.js")).toContain("the input");
  expect(await read("blockr-core-react.js")).toContain('from "@blockr/core/binding"');
  expect(await read("blockr-core-binding.js")).toContain("the binding");
  // The controls take the inputs from the SDK.
  expect(await read("blockr-core-controls.js")).toContain('from "@blockr/core/react"');
  expect(await read("blockr-core-controls.js")).not.toContain("the input");

  // The import map names each import by its file, and the bundle of React,
  // which the entry points of React import, beside them.
  const map = JSON.parse(await read("import-map.json")) as { imports: Record<string, string>; files: string[] };
  expect(map.imports).toEqual(Object.fromEntries(IMPORTS.map((name) => [name, importFile(name)])));
  expect(map.files).toEqual([expect.stringMatching(/^react-[0-9a-f]{8}\.js$/)]);
});

test("keeps core's utilities under its prefix and outside layers, without Preflight", async () => {
  await write({
    "test.css": `@import "${core}";\n@source inline("bc:flex bc:bg-blue-500 flex");\n`,
  });

  const css = await stylesheet(join(root, "test.css"));

  expect(css).toMatch(/^\.bc\\:flex \{/m);
  // Tailwind CSS writes a utility in one rule with others of the same
  // declarations, such as a colour's own with that colour's at an opacity.
  expect(css).toMatch(/^\.bc\\:bg-blue-500(, [^{]+)? \{\n {2}background-color: var\(--bc-color-blue-500\);/m);
  expect(css).toMatch(/@layer theme \{\n {2}:root, :host \{\n[^}]*\n {4}--bc-color-blue-500: /);
  expect(css).not.toMatch(/^\s*\.flex \{/m);
  expect(css).not.toContain("@layer utilities");
  expect(css).not.toContain("@layer base");
  expect(css).not.toContain("box-sizing");
});

test("names the colors of core's UIs as variables of its theme, which follow the text around them", async () => {
  await write({
    "test.css": `@import "${core}";\n@source inline("bc:border-border-default bc:text-text-danger");\n`,
  });

  const css = await stylesheet(join(root, "test.css"));

  // A utility reads the variable, so that a frontend restyles core by setting it.
  expect(css).toMatch(/^\.bc\\:border-border-default(, [^{]+)? \{\n {2}border-color: var\(--bc-color-border-default\);/m);
  expect(css).toContain("--bc-color-border-default: color-mix(in oklab, currentColor 25%, transparent);");
  // A colour with an accent of its own takes the theme's, which the stylesheet
  // then holds too.
  expect(css).toContain("--bc-color-text-danger: var(--bc-color-red-600);");
  expect(css).toMatch(/--bc-color-red-600: oklch\(/);
});

test("draws what floats over the board on the page's background, which the dark theme lightens, as a variable of its theme", async () => {
  await write({ "test.css": `@import "${core}";\n@source inline("bc:bg-bg-raised");\n` });

  const css = await stylesheet(join(root, "test.css"));

  expect(css).toMatch(/^\.bc\\:bg-bg-raised(, [^{]+)? \{\n {2}background-color: var\(--bc-color-bg-raised\);/m);
  // Both of its defaults sit in the theme's layer, so that a frontend that sets
  // the variable outside a layer wins over them.
  const start = css.indexOf("@layer theme {");
  const theme = css.slice(start, css.indexOf("\n}\n", start));
  expect(theme).toMatch(/:root, :host(, [^{]+)? \{[^}]*--bc-color-bg-raised: Canvas;/);
  expect(theme).toMatch(/:root\[data-bs-theme="dark"\] \{[^}]*--bc-color-bg-raised: color-mix\(in oklab, Canvas, currentColor 10%\);/);
  expect(css.replace(theme, "")).not.toContain("--bc-color-bg-raised:");
});

test("takes the face of core's UIs from the theme's sans, which a frontend sets", async () => {
  await write({ "test.css": `@import "${core}";\n@source inline("bc:font-sans");\n` });

  const css = await stylesheet(join(root, "test.css"));

  expect(css).toMatch(/^\.bc\\:font-sans \{\n {2}font-family: var\(--bc-font-sans\);/m);
  // Its default is Tailwind's stack of sans faces, so that core's UIs draw in
  // no serif where the page sets no face.
  expect(css).toMatch(/--bc-font-sans: [^;]*\bsans-serif\b/);
});

test("finds the output types, each but BlockOutput, by their names", () => {
  const source = [
    "export interface BlockOutput { renderer: string }",
    "export interface TableOutput extends BlockOutput { total: number }",
    "export interface TableParams { page?: number }",
    "interface HiddenOutput extends BlockOutput { x: number }",
    "export type AliasOutput = TableOutput;",
  ].join("\n");

  expect(outputTypes("outputs.ts", source)).toEqual(["TableOutput"]);
  expect(schemaFile("TableOutput")).toBe("table-output.schema.json");
  expect(schemaFile("LazyTableOutput")).toBe("lazy-table-output.schema.json");
});

test("writes the schema of each output type and removes those of types gone", async () => {
  await write({
    "srcts/types/json.ts": "export type Json = string | number | Json[];\n",
    "srcts/types/outputs.ts": [
      'import type { Json } from "./json.ts";',
      "export interface BlockOutput { renderer: string }",
      "export interface ListOutput extends BlockOutput {",
      "  items: Json[];",
      "  note?: string;",
      "}",
      "",
    ].join("\n"),
    "inst/outputs/gone-output.schema.json": "{}",
    "inst/outputs/fixtures/list-output.json": "{}",
  });

  expect(await schemas(root)).toEqual(["inst/outputs/list-output.schema.json"]);

  const out = join(root, "inst", "outputs");
  expect((await readdir(out)).sort()).toEqual(["fixtures", "list-output.schema.json"]);

  const schema = JSON.parse(
    await readFile(join(out, "list-output.schema.json"), "utf8"),
  ) as Record<string, unknown>;

  expect(schema).toMatchObject({
    type: "object",
    properties: {
      renderer: { type: "string" },
      items: { type: "array", items: { $ref: "#/definitions/Json" } },
      note: { type: "string" },
    },
    required: ["items", "renderer"],
  });
  expect(schema).not.toHaveProperty("additionalProperties");
  expect(Object.keys(schema.definitions as object)).toEqual(["Json"]);
}, 60_000);

test("writes the schema of a block's state beside its module, allowing no other property", async () => {
  await write({
    "srcts/modules/head-block/index.ts": "export const api = 1;\n",
    "srcts/modules/head-block/state.ts": [
      "export type State = Readonly<{",
      "  /** @asType integer */",
      "  n: number;",
      '  direction?: "head" | "tail";',
      "}>;",
      "",
    ].join("\n"),
    "srcts/modules/none-block/state.ts": "export type State = Readonly<Record<string, never>>;\n",
    "srcts/modules/basic-table/index.ts": "export const api = 1;\n",
  });

  expect(await schemas(root)).toEqual(["inst/js/head-block.schema.json", "inst/js/none-block.schema.json"]);

  const read = async (file: string): Promise<unknown> =>
    JSON.parse(await readFile(join(root, "inst", "js", file), "utf8")) as unknown;

  expect(await read("head-block.schema.json")).toMatchObject({
    type: "object",
    properties: { n: { type: "integer" }, direction: { type: "string", enum: ["head", "tail"] } },
    required: ["n"],
    additionalProperties: false,
  });
  expect(await read("none-block.schema.json")).toMatchObject({ type: "object", properties: {} });
}, 60_000);

test("writes no schemas without the types and modules they come from", async () => {
  expect(await schemas(root)).toEqual([]);
});

test("writes the schema of a JSON board, and removes any other in inst/board", async () => {
  await write({
    "srcts/types/board.ts": [
      "export interface Stack { name: string; blocks: string[] }",
      "export interface JsonBoard {",
      "  format: number;",
      "  board: { stacks: Record<string, Stack>; options: Record<string, number> };",
      "}",
      "",
    ].join("\n"),
    "inst/board/fixtures/board.json": "{}",
    "inst/board/opening-board.schema.json": "{}",
  });

  expect(await schemas(root)).toEqual(["inst/board/json-board.schema.json"]);

  const out = join(root, "inst", "board");
  expect((await readdir(out)).sort()).toEqual(["fixtures", "json-board.schema.json"]);

  const schema = JSON.parse(
    await readFile(join(out, "json-board.schema.json"), "utf8"),
  ) as Record<string, unknown>;

  expect(schema).toMatchObject({
    type: "object",
    properties: {
      format: { type: "number" },
      board: {
        type: "object",
        properties: {
          stacks: {
            type: "object",
            additionalProperties: { $ref: "#/definitions/Stack" },
          },
          options: { type: "object", additionalProperties: { type: "number" } },
        },
      },
    },
    required: ["format", "board"],
    definitions: {
      Stack: {
        type: "object",
        properties: {
          name: { type: "string" },
          blocks: { type: "array", items: { type: "string" } },
        },
      },
    },
  });
}, 60_000);

test("writes the schemas of core's board options next to its runtime", async () => {
  await write({
    "srcts/options/schemas.ts": [
      "export const optionSchemas = {",
      '  n_rows: { type: "integer", minimum: 0 },',
      '  show_conditions: { type: "array", items: { enum: ["error"] } },',
      "};",
      "",
    ].join("\n"),
  });

  // Each in a file of its own, named by the board option's class in R
  expect(await schemas(root)).toEqual([
    "inst/js/n_rows_option.schema.json",
    "inst/js/show_conditions_option.schema.json",
  ]);

  const read = async (name: string) =>
    JSON.parse(await readFile(join(root, "inst", "js", name), "utf8")) as unknown;

  expect(await read("n_rows_option.schema.json")).toEqual({ type: "integer", minimum: 0 });
  expect(await read("show_conditions_option.schema.json")).toEqual({
    type: "array",
    items: { enum: ["error"] },
  });
});

test("writes the words that ids are made of, one word a line, for R", async () => {
  await write({
    "srcts/store/words.ts": [
      'export const adjectives = ["calm", "keen"];',
      'export const animals = ["heron"];',
      "",
    ].join("\n"),
  });

  expect(await idWords(root)).toEqual(["inst/ids/adjectives.txt", "inst/ids/animals.txt"]);
  expect(await readFile(join(root, "inst", "ids", "adjectives.txt"), "utf8")).toBe("calm\nkeen\n");
  expect(await readFile(join(root, "inst", "ids", "animals.txt"), "utf8")).toBe("heron\n");
});

test("writes no words without the module they come from", async () => {
  expect(await idWords(root)).toEqual([]);
});
