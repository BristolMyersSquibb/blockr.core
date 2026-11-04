// Builds core's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node. Run: npm run build
//
// - Core's runtime, srcts/runtime/index.ts, is bundled into blockr-core.js.
// - A module, srcts/modules/<name>/index.ts, is bundled into <name>.js on its
//   own.
// - An index.css beside either is its stylesheet, built with Tailwind CSS into
//   blockr-core.css or <name>.css.
// - Each output type in srcts/types/outputs.ts gets its JSON Schema in
//   inst/outputs, by which R writes the outputs of its class.
// - The board as R sends it, OpeningBoard in srcts/types/board.ts, gets its
//   JSON Schema in inst/board, by which R writes the board an app script
//   built.
// - The schemas of core's board options, in srcts/options/schemas.ts, go
//   next to core's runtime, into blockr-core.schema.json, by which R writes the
//   value of one that an app script gives.
//
// The build owns inst/js and empties it first, so that nothing is left of a
// module that is gone. In inst/outputs and inst/board, it owns the schemas
// only.

import { existsSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import * as esbuild from "esbuild";
import postcss from "postcss";
import { createGenerator } from "ts-json-schema-generator";
import ts from "typescript";

interface Entry {
  name: string;
  dir: string;
}

async function entries(src: string): Promise<Entry[]> {
  const modules = join(src, "modules");
  const dirs = existsSync(modules)
    ? await readdir(modules, { withFileTypes: true })
    : [];
  return [
    { name: "blockr-core", dir: join(src, "runtime") },
    ...dirs
      .filter((dir) => dir.isDirectory())
      .map((dir) => ({ name: dir.name, dir: join(modules, dir.name) })),
  ];
}

export async function stylesheet(file: string): Promise<string> {
  const plugin = tailwindcss({
    base: dirname(file),
    optimize: { minify: false },
  });
  const result = await postcss([plugin]).process(await readFile(file, "utf8"), {
    from: file,
  });
  return result.css;
}

export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");
  const all = await entries(join(root, "srcts"));

  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });

  const { metafile, warnings } = await esbuild.build({
    absWorkingDir: root,
    entryPoints: all.map((entry) => ({
      in: join(entry.dir, "index.ts"),
      out: entry.name,
    })),
    outdir: out,
    bundle: true,
    format: "esm",
    platform: "browser",
    target: "es2022",
    logLevel: "warning",
    metafile: true,
  });

  if (warnings.length > 0) {
    throw new Error("esbuild warned, as printed above");
  }

  const written = Object.keys(metafile.outputs);

  for (const entry of all) {
    const css = join(entry.dir, "index.css");
    if (!existsSync(css)) continue;
    const file = join(out, `${entry.name}.css`);
    await writeFile(file, await stylesheet(css));
    written.push(relative(root, file));
  }

  return written.sort();
}

// The output types: every exported interface of the file named <Name>Output,
// but BlockOutput, which they all extend.
export function outputTypes(file: string, source: string): string[] {
  const parsed = ts.createSourceFile(file, source, ts.ScriptTarget.Latest);
  return parsed.statements
    .filter(ts.isInterfaceDeclaration)
    .filter((node) =>
      node.modifiers?.some((mod) => mod.kind === ts.SyntaxKind.ExportKeyword),
    )
    .map((node) => node.name.text)
    .filter((name) => name.endsWith("Output") && name !== "BlockOutput");
}

// The file name of a type's schema, as table-output.schema.json for
// TableOutput.
export function schemaFile(type: string): string {
  return `${type.replace(/([a-z0-9])([A-Z])/g, "$1-$2").toLowerCase()}.schema.json`;
}

// Writes the JSON Schema of each output type in srcts/types/outputs.ts into
// inst/outputs, and removes the schema of a type that is gone, then the schema
// of the board as R sends it and those beside core's runtime. A schema leaves
// out no field that a later version may add, since both sides of the wire
// protocol skip fields they do not know.
export async function schemas(root: string): Promise<string[]> {
  return [
    ...(await outputSchemas(root)),
    ...(await boardSchema(root)),
    ...(await runtimeSchemas(root)),
  ].sort();
}

async function outputSchemas(root: string): Promise<string[]> {
  const types = join(root, "srcts", "types", "outputs.ts");
  if (!existsSync(types)) return [];

  const out = join(root, "inst", "outputs");
  const names = outputTypes(types, await readFile(types, "utf8"));
  const files = new Set(names.map(schemaFile));

  await mkdir(out, { recursive: true });

  for (const file of await readdir(out)) {
    if (file.endsWith(".schema.json") && !files.has(file)) {
      await rm(join(out, file));
    }
  }

  const schema = typeSchemas(root, types);
  const written: string[] = [];

  for (const name of names) {
    const file = join(out, schemaFile(name));
    await writeJson(file, schema(name));
    written.push(relative(root, file));
  }

  return written;
}

// The JSON Schema of the board as R sends the one an app script built, with
// its collections as arrays, by which R writes core's objects in it, so that a
// stack's blocks stay an array and an empty collection an object.
async function boardSchema(root: string): Promise<string[]> {
  const types = join(root, "srcts", "types", "board.ts");
  if (!existsSync(types)) return [];

  const file = join(root, "inst", "board", schemaFile("OpeningBoard"));

  await mkdir(dirname(file), { recursive: true });
  await writeJson(file, typeSchemas(root, types)("OpeningBoard"));

  return [relative(root, file)];
}

// The JSON Schema next to core's runtime, laid out like the board, for the
// part of it that core defines: the values of its board options, under
// `options` by id, as a package puts one next to its startup module for the
// front-end's state and its board options.
async function runtimeSchemas(root: string): Promise<string[]> {
  const source = join(root, "srcts", "options", "schemas.ts");
  if (!existsSync(source)) return [];

  const { optionSchemas } = (await import(pathToFileURL(source).href)) as {
    optionSchemas: unknown;
  };
  const file = join(root, "inst", "js", "blockr-core.schema.json");

  await mkdir(dirname(file), { recursive: true });
  await writeJson(file, {
    type: "object",
    properties: { options: { type: "object", properties: optionSchemas } },
  });

  return [relative(root, file)];
}

// The JSON Schema of each type that the file at `path` exports, by the type's
// name, without definitions where it needs none.
function typeSchemas(root: string, path: string) {
  const tsconfig = join(root, "tsconfig.client.json");
  const generator = createGenerator({
    path,
    ...(existsSync(tsconfig) ? { tsconfig } : {}),
    expose: "export",
    topRef: false,
    additionalProperties: true,
    skipTypeCheck: true,
  });

  return (name: string) => {
    const schema = generator.createSchema(name);
    if (Object.keys(schema.definitions ?? {}).length === 0) {
      delete schema.definitions;
    }
    return schema;
  };
}

async function writeJson(file: string, value: unknown): Promise<void> {
  await writeFile(file, `${JSON.stringify(value, null, 2)}\n`);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
  for (const file of [...(await build(root)), ...(await schemas(root))]) {
    console.log(file);
  }
}
