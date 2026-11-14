// Builds core's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node. Run: npm run build
//
// - Core's runtime, srcts/runtime/index.ts, is bundled into blockr-core.js.
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js on its own, by the build preset of ./preset.ts.
// - Core's React SDK, srcts/react, its binding, srcts/binding, and its
//   controls, srcts/controls, are bundled into files of their own,
//   blockr-core-react.js, blockr-core-binding.js and blockr-core-controls.js,
//   and so is its OpenTelemetry module, srcts/otel, into blockr-core-otel.js,
//   with the parts of OpenTelemetry's SDK that it uses.
// - React's entry points, such as `react` and `react-dom/client`, are written
//   as ES modules of their own, such as react.js and react-dom-client.js,
//   which read one minified bundle of React, react-<hash>.js.
// - The runtime, the modules and the SDK, the binding and the controls leave
//   their imports of React and of each other imports, which the page that R
//   serves resolves through its import map, as inst/js/import-map.json lays
//   it out. So the page runs one copy of each, and so do a package's modules.
//   The runtime leaves its import of the OpenTelemetry module an import as
//   well, so that a page loads the module only where it imports it.
// - An index.css beside either is its stylesheet, built with Tailwind CSS into
//   blockr-core.css or <name>.css.
// - Each output type in srcts/types/outputs.ts gets its JSON Schema in
//   inst/outputs, by which R writes the outputs of its class.
// - A JSON board, JsonBoard in srcts/types/board.ts, gets its JSON Schema in
//   inst/board, by which R writes the board an app script built.
// - The schema of each of core's board options, in srcts/options/schemas.ts,
//   goes beside the board option's module, srcts/modules/<name>-option, into
//   <name>-option.schema.json, where <name> is the board option's id with
//   dashes, by which R writes the value of one that an app script gives.
// - A block class's UI module whose folder holds a state.ts
//   ships the JSON Schema of the block's state beside it,
//   as <name>.schema.json, from the type State that the file exports.
//   R writes the state of the blocks it builds by it.
// - The types that modules are written against, srcts/index.ts, and the stubs
//   of the hosts, srcts/stubs/index.ts, go into inst/types, which a package's
//   build resolves as `@blockr/core`, and so do core's React SDK, srcts/react,
//   its binding, srcts/binding, and the build preset, srcts/build, each as the
//   export of its name, such as `@blockr/core/react`: their declarations, the
//   SDK and the binding bundled for a package's tests, whose build leaves its
//   imports of them to the page's import map, the preset bundled for a
//   package's build, with React left an import, the stubs bundled for
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

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
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
import { IMPORTS, importFile, REACT_ENTRIES } from "../import-map/index.ts";
import type { Import } from "../import-map/index.ts";
import { preset } from "./preset.ts";

// Core's repo, whose node_modules holds the React that the build writes into
// inst/js.
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

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

// The names that each of React's entry points exports in production, which its
// ES module exports as well. A child process requires them, since this process
// may hold React's development build, which exports more.
function reactNames(): Record<string, string[]> {
  const script = `console.log(JSON.stringify(Object.fromEntries(${JSON.stringify(REACT_ENTRIES)}.map((name) => [name, Object.keys(require(name))]))));`;
  const out = execFileSync(process.execPath, ["-e", script], {
    cwd: repo,
    env: { ...process.env, NODE_ENV: "production" },
    encoding: "utf8",
  });
  return JSON.parse(out) as Record<string, string[]>;
}

// Writes React's entry points into `out` as ES modules of their own, each
// exporting the names that the entry point exports, from one minified bundle of
// React and ReactDOM, so that all of them read one React.
// The bundle keeps React's license comments, and its name holds a hash of its
// content, as react-3f2a9c1d.js, so that no browser runs an old React from its
// cache, since the entry points import it by that name.
async function reactModules(out: string): Promise<string[]> {
  const result = await esbuild.build({
    absWorkingDir: repo,
    stdin: {
      contents: REACT_ENTRIES.map((name, i) => `export * as entry${String(i)} from ${JSON.stringify(name)};`).join("\n"),
      resolveDir: repo,
      loader: "js",
    },
    bundle: true,
    write: false,
    format: "esm",
    platform: "browser",
    target: preset.target,
    define: preset.define,
    minify: true,
    legalComments: "inline",
    logLevel: "warning",
  });
  if (result.warnings.length > 0) {
    throw new Error("esbuild warned, as printed above");
  }

  const text = result.outputFiles[0]?.text ?? "";
  const bundle = `react-${createHash("sha256").update(text).digest("hex").slice(0, 8)}.js`;
  await writeFile(join(out, bundle), text);

  const names = reactNames();
  const written = [bundle];
  for (const [i, name] of REACT_ENTRIES.entries()) {
    const exported = (names[name] ?? []).filter((key) => key !== "default" && /^[A-Za-z_$][\w$]*$/.test(key));
    const file = importFile(name);
    await writeFile(
      join(out, file),
      [
        `// ${name}, as an ES module, from the bundle of React that blockr.core ships.`,
        `import { entry${String(i)} as m } from "./${bundle}";`,
        "export default m.default;",
        `export const { ${exported.join(", ")} } = m;`,
        "",
      ].join("\n"),
    );
    written.push(file);
  }
  return written;
}

// Core's libraries for modules, each by its name in the import map and the
// directory of its source in srcts, and its OpenTelemetry module, which the
// runtime imports by its name too.
const LIBRARIES = [
  ["@blockr/core/react", "react"],
  ["@blockr/core/binding", "binding"],
  ["@blockr/core/controls", "controls"],
  ["@blockr/core/otel", "otel"],
] as const;

// Maps the imports in core's sources of core's React SDK, its binding and its
// controls, from their sources in srcts, to their names in the import map,
// which the preset leaves imports.
// The SDK holds the inputs, so an import of srcts/inputs is one of the SDK.
// The build of a library bundles the library's own sources, which `own` names.
function sourceImports(src: string, own?: Import): esbuild.Plugin {
  const libraries: readonly (readonly [Import, string])[] = [...LIBRARIES, ["@blockr/core/react", "inputs"]];
  const names = new Map(
    libraries.filter(([name]) => name !== own).map(([name, dir]) => [join(src, dir, "index.ts"), name] as const),
  );
  return {
    name: "blockr.core-source-imports",
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
  const plugins = (own?: Import) => [sourceImports(src, own), ...preset.plugins];
  const libraries = LIBRARIES.filter(([, dir]) => existsSync(join(src, dir, "index.ts")));

  const results = [
    await esbuild.build({
      ...options,
      plugins: plugins(),
      entryPoints: [{ in: entryFile(runtime.dir), out: runtime.name }],
    }),
    ...(modules.length === 0
      ? []
      : [
          await esbuild.build({
            ...options,
            plugins: plugins(),
            entryPoints: modules.map((entry) => ({ in: entryFile(entry.dir), out: entry.name })),
          }),
        ]),
    ...(await Promise.all(
      libraries.map(([name, dir]) =>
        esbuild.build({
          ...options,
          plugins: plugins(name),
          entryPoints: [{ in: join(src, dir, "index.ts"), out: importFile(name).replace(/\.js$/, "") }],
        }),
      ),
    )),
  ];

  if (results.some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  const written = results.flatMap((result) => Object.keys(result.metafile.outputs));
  const react = await reactModules(out);
  written.push(...react.map((file) => relative(root, join(out, file))));

  // The import map that the page R serves holds, as R reads it: each import by
  // the file in inst/js that it resolves to, and the files that those import in
  // turn, which R serves beside them.
  const imports = Object.fromEntries(
    IMPORTS.filter((name) => existsSync(join(out, importFile(name)))).map((name) => [name, importFile(name)]),
  );
  const map = join(out, "import-map.json");
  await writeJson(map, { imports, files: react.filter((file) => !Object.values(imports).includes(file)) });
  written.push(relative(root, map));

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
// of a JSON board, those of the board options' values and those of the block
// states beside the modules.
// A schema leaves out no property that a later version may add,
// since both sides of the wire protocol skip properties they do not know,
// but for a block's state, which holds its class's state args alone.
export async function schemas(root: string): Promise<string[]> {
  return [
    ...(await outputSchemas(root)),
    ...(await boardSchema(root)),
    ...(await optionSchemas(root)),
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

// The JSON Schema of the value of each of core's board options, beside the
// board option's module, in a file named by the module, as
// n-rows-option.schema.json beside n-rows-option.js for n_rows, as a package
// puts the schemas of its board options and of its plugin classes' state beside
// their modules. R writes the value of a board option that an app script gives
// by it.
async function optionSchemas(root: string): Promise<string[]> {
  const source = join(root, "srcts", "options", "schemas.ts");
  if (!existsSync(source)) return [];

  const { optionSchemas: schemas } = (await import(pathToFileURL(source).href)) as {
    optionSchemas: Readonly<Record<string, unknown>>;
  };
  const out = join(root, "inst", "js");

  await mkdir(out, { recursive: true });

  const written: string[] = [];

  for (const [id, schema] of Object.entries(schemas)) {
    const file = join(out, `${optionModuleName(id)}.schema.json`);
    await writeJson(file, schema);
    written.push(relative(root, file));
  }

  return written;
}

// The name of the module of core's board option with the id given, as
// n-rows-option for n_rows, which core lists in R with the board option.
function optionModuleName(id: string): string {
  return `${id.replaceAll("_", "-")}-option`;
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
function packageVersion(name: string): string {
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
// package's build leaves its imports of React, and of the SDK, the binding
// and the controls, to the page's import map.
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
  peerDependencies: { react: packageVersion("react"), "react-dom": packageVersion("react-dom") },
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
