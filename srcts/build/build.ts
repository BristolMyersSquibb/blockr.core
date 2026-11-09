// Builds core's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node. Run: npm run build
//
// - Core's runtime, srcts/runtime/index.ts, is bundled into blockr-core.js.
//   The file starts with the one copy of React, which
//   srcts/runtime/share-react.ts puts on the page, minified, since it changes
//   only with React's version. Core's own code follows unminified, its imports
//   of React mapped to that copy as a module's are.
//   It holds core's React SDK, its binding and its controls, which
//   srcts/runtime/share-core.ts puts on the page beside React.
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js on its own, by the build preset of ./preset.ts, which maps each
//   import of React to the copy that core's runtime puts on the page, so that
//   its bundle holds no React.
//   Its imports of the SDK, the binding and the controls go to the copies on
//   the page too, so that it holds none of them, as a package's module does.
// - An index.css beside either is its stylesheet, built with Tailwind CSS into
//   blockr-core.css or <name>.css.
// - Each output type in srcts/types/outputs.ts gets its JSON Schema in
//   inst/outputs, by which R writes the outputs of its class.
// - A JSON board, JsonBoard in srcts/types/board.ts, gets its JSON Schema in
//   inst/board, by which R writes the board an app script built.
// - The schemas of core's board options, in srcts/options/schemas.ts, go
//   next to core's runtime, into blockr-core.schema.json, by which R writes the
//   value of one that an app script gives.
// - A block class's UI module whose folder holds a state.ts
//   ships the JSON Schema of the block's state beside it,
//   as <name>.schema.json, from the type State that the file exports.
//   R writes the state of the blocks it builds by it.
// - The types that modules are written against, srcts/index.ts, and the stubs
//   of the hosts, srcts/stubs/index.ts, go into inst/types, which a package's
//   build resolves as `@blockr/core`, and so do core's React SDK, srcts/react,
//   its binding, srcts/binding, and the build preset, srcts/build, each as the
//   export of its name, such as `@blockr/core/react`: their declarations, the
//   SDK and the binding bundled for a package's tests, which its build maps to
//   the copies on the page, the preset bundled for a package's build, with
//   React left an import, the stubs bundled for
//   a test runner, and the package.json that names them and the React that
//   the runtime bundles as its peer.
//   The controls, srcts/controls/index.ts, go there as the SDK does,
//   as `@blockr/core/controls`, for a package that writes its modules without
//   React.
//
// - The words that ids are made of, in srcts/store/words.ts, go into inst/ids,
//   one word a line, for R's rand_names() to draw from as the client's
//   mintId() does.
//
// The build owns inst/js and inst/types and empties them first, so that
// nothing is left of a module that is gone. In inst/outputs and inst/board, it
// owns the schemas only, and in inst/ids the files of words.

import { existsSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import * as esbuild from "esbuild";
import postcss from "postcss";
import { createGenerator } from "ts-json-schema-generator";
import ts from "typescript";
import { preset } from "./preset.ts";

interface Entry {
  name: string;
  dir: string;
}

// The file a module's directory starts from, index.ts or, where it draws
// with React, index.tsx.
function entryFile(dir: string): string {
  const tsx = join(dir, "index.tsx");
  return existsSync(tsx) ? tsx : join(dir, "index.ts");
}

// Core's runtime and its modules, each by the name of the file it is bundled
// into and its directory.
async function entries(src: string): Promise<{ runtime: Entry; modules: Entry[] }> {
  const modules = join(src, "modules");
  const dirs = existsSync(modules)
    ? await readdir(modules, { withFileTypes: true })
    : [];
  return {
    runtime: { name: "blockr-core", dir: join(src, "runtime") },
    modules: dirs
      .filter((dir) => dir.isDirectory())
      .map((dir) => ({ name: dir.name, dir: join(modules, dir.name) })),
  };
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

// The one copy of React that core's runtime puts on the page, from
// share-react.ts in the runtime's directory, as a minified script that keeps
// React's license comments, which the runtime's bundle starts with.
async function reactCopy(root: string, dir: string) {
  const entry = join(dir, "share-react.ts");
  if (!existsSync(entry)) return undefined;
  return await esbuild.build({
    absWorkingDir: root,
    entryPoints: [entry],
    bundle: true,
    write: false,
    format: "iife",
    platform: "browser",
    target: preset.target,
    minify: true,
    legalComments: "inline",
    define: preset.define,
    logLevel: "warning",
  });
}

// Maps the imports in core's modules of core's React SDK, its binding and its
// controls, from their sources in srcts, to their names in `@blockr/core`.
// The preset then maps those names to the copies that core's runtime puts on
// the page, so that core's modules hold none of them, as a package's modules
// do not.
function sharedSources(src: string): esbuild.Plugin {
  const names = new Map([
    [join(src, "react", "index.ts"), "@blockr/core/react"],
    [join(src, "binding", "index.ts"), "@blockr/core/binding"],
    [join(src, "controls", "index.ts"), "@blockr/core/controls"],
  ]);
  return {
    name: "blockr.core-shared-sources",
    setup(build) {
      build.onResolve({ filter: /^\.\.?\// }, async (args) => {
        const name = names.get(resolve(args.resolveDir, args.path));
        if (name === undefined) return undefined;
        return await build.resolve(name, { kind: args.kind, resolveDir: args.resolveDir });
      });
    },
  };
}

export async function build(root: string): Promise<string[]> {
  const out = join(root, "inst", "js");
  const src = join(root, "srcts");
  const { runtime, modules } = await entries(src);

  await rm(out, { recursive: true, force: true });
  await mkdir(out, { recursive: true });

  const options = {
    ...preset,
    absWorkingDir: root,
    outdir: out,
    logLevel: "warning",
    metafile: true,
  } satisfies esbuild.BuildOptions;
  // The runtime maps its imports of React to the copy that its bundle starts
  // with, as a module does, and is written behind that copy below.
  // It bundles the SDK, the binding and the controls, which it shares.
  const core = await esbuild.build({
    ...options,
    write: false,
    entryPoints: [{ in: entryFile(runtime.dir), out: runtime.name }],
  });
  const copy = await reactCopy(root, runtime.dir);
  const bundled =
    modules.length === 0
      ? []
      : [
          await esbuild.build({
            ...options,
            plugins: [sharedSources(src), ...preset.plugins],
            entryPoints: modules.map((entry) => ({ in: entryFile(entry.dir), out: entry.name })),
          }),
        ];

  if ([core, ...(copy === undefined ? [] : [copy]), ...bundled].some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  for (const file of core.outputFiles) {
    const start = file.path.endsWith(".js") ? (copy?.outputFiles[0]?.text ?? "") : "";
    await writeFile(file.path, start + file.text);
  }

  const written = [core, ...bundled].flatMap((result) => Object.keys(result.metafile.outputs));

  for (const entry of [runtime, ...modules]) {
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
// of a JSON board, those beside core's runtime and those of the block states
// beside the modules.
// A schema leaves out no field that a later version may add,
// since both sides of the wire protocol skip fields they do not know,
// but for a block's state, which holds its class's state args alone.
export async function schemas(root: string): Promise<string[]> {
  return [
    ...(await outputSchemas(root)),
    ...(await boardSchema(root)),
    ...(await runtimeSchemas(root)),
    ...(await stateSchemas(root)),
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

// The JSON Schema of a JSON board, by which R writes core's objects in the
// board an app script built, so that a stack's blocks stay an array and an
// empty collection an object.
// It removes every other schema in inst/board, such as that of a type that is
// gone.
async function boardSchema(root: string): Promise<string[]> {
  const types = join(root, "srcts", "types", "board.ts");
  if (!existsSync(types)) return [];

  const out = join(root, "inst", "board");
  const file = join(out, schemaFile("JsonBoard"));

  await mkdir(out, { recursive: true });
  for (const name of await readdir(out)) {
    if (name.endsWith(".schema.json") && name !== schemaFile("JsonBoard")) {
      await rm(join(out, name));
    }
  }
  await writeJson(file, typeSchemas(root, types)("JsonBoard"));

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

// The JSON Schema of the block's state that each block class's UI module
// ships beside it,
// from the type State that state.ts in the module's folder exports,
// such as inst/js/head-block.schema.json
// from srcts/modules/head-block/state.ts.
// R reads it when it registers the class,
// and refuses one whose properties are not the class's state args,
// so it allows no other property.
async function stateSchemas(root: string): Promise<string[]> {
  const modules = join(root, "srcts", "modules");
  if (!existsSync(modules)) return [];

  const written: string[] = [];

  for (const dir of await readdir(modules, { withFileTypes: true })) {
    const source = join(modules, dir.name, "state.ts");
    if (!dir.isDirectory() || !existsSync(source)) continue;

    const file = join(root, "inst", "js", `${dir.name}.schema.json`);
    const schema = typeSchemas(root, source, false)("State");

    await mkdir(dirname(file), { recursive: true });
    // A state with no state args has no properties,
    // which the schema still lists, as R reads them.
    await writeJson(file, { ...schema, properties: schema.properties ?? {} });
    written.push(relative(root, file));
  }

  return written;
}

// The JSON Schema of each type that the file at `path` exports, by the type's
// name, without definitions where it needs none.
// An object that its type gives no index signature allows other properties,
// unless `additionalProperties` is false.
function typeSchemas(root: string, path: string, additionalProperties = true) {
  const tsconfig = join(root, "tsconfig.client.json");
  const generator = createGenerator({
    path,
    ...(existsSync(tsconfig) ? { tsconfig } : {}),
    expose: "export",
    topRef: false,
    additionalProperties,
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

// Writes the words that ids are made of, from srcts/store/words.ts, into
// inst/ids, one word a line, adjectives.txt and animals.txt.
export async function idWords(root: string): Promise<string[]> {
  const source = join(root, "srcts", "store", "words.ts");
  if (!existsSync(source)) return [];

  const words = (await import(pathToFileURL(source).href)) as Record<
    "adjectives" | "animals",
    readonly string[]
  >;
  const out = join(root, "inst", "ids");

  await mkdir(out, { recursive: true });

  const written: string[] = [];

  for (const kind of ["adjectives", "animals"] as const) {
    const file = join(out, `${kind}.txt`);
    await writeFile(file, `${words[kind].join("\n")}\n`);
    written.push(relative(root, file));
  }

  return written;
}

async function writeJson(file: string, value: unknown): Promise<void> {
  await writeFile(file, `${JSON.stringify(value, null, 2)}\n`);
}

// The version of a package that core's build resolves.
function versionOf(name: string): string {
  return (createRequire(import.meta.url)(`${name}/package.json`) as { version: string }).version;
}

// The package that inst/types is: the types modules are written against, as
// `@blockr/core`, the stubs of the hosts, as `@blockr/core/stubs`, and core's
// React SDK, its binding and the build preset, as `@blockr/core/react`,
// `@blockr/core/binding` and `@blockr/core/build`.
// Its bundles leave React an import, so it names the React that core's
// runtime bundles, at that version, as its peer.
// So npm installs that React with `@blockr/core` where a package has none, and
// refuses a package that asks for another, so that a package's tests run on
// one copy of the app's React, the stubs drawing core's UIs with it, while a
// package's build maps every import of React, and of the SDK, the binding and
// the controls, to the copies on the page.
export const TYPES_PACKAGE = {
  name: "@blockr/core",
  private: true,
  description:
    "The types that the modules of blockr.core's client are written against, a stub of each host for their tests, the React SDK, its binding and the build preset that they may be built with, and the controls they may draw with, as the R package blockr.core ships them.",
  type: "module",
  exports: {
    ".": { types: "./index.d.ts" },
    "./binding": { types: "./binding/index.d.ts", default: "./binding/index.js" },
    "./build": { types: "./build/index.d.ts", default: "./build/index.js" },
    "./react": { types: "./react/index.d.ts", default: "./react/index.js" },
    "./stubs": { types: "./stubs/index.d.ts", default: "./stubs/index.js" },
    "./controls": { types: "./controls/index.d.ts", default: "./controls/index.js" },
  },
  peerDependencies: { react: versionOf("react"), "react-dom": versionOf("react-dom") },
};

// The modules of srcts that the types package bundles,
// each as the export of its name.
const BUNDLED = ["stubs", "binding", "react", "build", "controls"];

// Writes inst/types: the declarations of srcts/index.ts and of the modules
// that BUNDLED names, each file's beside where its source sits in srcts, those
// modules bundled, and the package.json that names them.
export async function types(root: string): Promise<string[]> {
  const src = join(root, "srcts");
  const entry = join(src, "index.ts");
  const bundled = BUNDLED.map((name) => ({ name, file: join(src, name, "index.ts") })).filter(({ file }) =>
    existsSync(file),
  );
  if (!existsSync(entry)) return [];

  const dir = join(root, "inst", "types");
  await rm(dir, { recursive: true, force: true });
  await mkdir(dir, { recursive: true });

  const config = ts.readConfigFile(join(root, "tsconfig.client.json"), (file) => ts.sys.readFile(file));
  const { options } = ts.parseJsonConfigFileContent(config.config, ts.sys, root);
  const program = ts.createProgram([entry, ...bundled.map(({ file }) => file)], {
    ...options,
    noEmit: false,
    declaration: true,
    emitDeclarationOnly: true,
    rootDir: src,
    outDir: dir,
    incremental: false,
    tsBuildInfoFile: undefined,
  });

  const written: string[] = [];
  const emitted = program.emit(undefined, (file, text) => {
    ts.sys.writeFile(file, text);
    written.push(relative(root, file));
  });
  const problems = ts.getPreEmitDiagnostics(program).concat(emitted.diagnostics);
  if (problems.length > 0) {
    throw new Error(
      ts.formatDiagnostics(problems, {
        getCanonicalFileName: (file) => file,
        getCurrentDirectory: () => root,
        getNewLine: () => "\n",
      }),
    );
  }

  if (bundled.length > 0) {
    const { metafile, warnings } = await esbuild.build({
      absWorkingDir: root,
      entryPoints: bundled.map(({ name, file }) => ({ in: file, out: `${name}/index` })),
      outdir: dir,
      bundle: true,
      format: "esm",
      platform: "neutral",
      target: "es2022",
      jsx: "automatic",
      // React stays an import, which a package's tests resolve to the React
      // that npm installs with `@blockr/core`.
      external: ["react", "react/*", "react-dom", "react-dom/*"],
      logLevel: "warning",
      metafile: true,
    });
    if (warnings.length > 0) {
      throw new Error("esbuild warned, as printed above");
    }
    written.push(...Object.keys(metafile.outputs));
  }

  const manifest = join(dir, "package.json");
  await writeJson(manifest, TYPES_PACKAGE);
  written.push(relative(root, manifest));

  return written.sort();
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
  for (const file of [
    ...(await build(root)),
    ...(await schemas(root)),
    ...(await types(root)),
    ...(await idWords(root)),
  ]) {
    console.log(file);
  }
}
