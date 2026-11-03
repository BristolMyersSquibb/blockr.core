// @vitest-environment node

import { mkdir, mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, expect, test } from "vitest";
import { build, outputTypes, schemaFile, schemas, stylesheet } from "./build.ts";

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
  expect((await readdir(out)).sort()).toEqual([
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

test("keeps core's utilities under its prefix and outside layers, without Preflight", async () => {
  await write({
    "test.css": `@import "${core}";\n@source inline("bc:flex bc:bg-blue-500 flex");\n`,
  });

  const css = await stylesheet(join(root, "test.css"));

  expect(css).toMatch(/^\.bc\\:flex \{/m);
  expect(css).toMatch(/^\.bc\\:bg-blue-500 \{\n {2}background-color: var\(--bc-color-blue-500\);/m);
  expect(css).toMatch(/@layer theme \{\n {2}:root, :host \{\n[^}]*\n {4}--bc-color-blue-500: /);
  expect(css).not.toMatch(/^\s*\.flex \{/m);
  expect(css).not.toContain("@layer utilities");
  expect(css).not.toContain("@layer base");
  expect(css).not.toContain("box-sizing");
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
});

test("writes no schemas without output types", async () => {
  expect(await schemas(root)).toEqual([]);
});
