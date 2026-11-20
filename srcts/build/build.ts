// Builds core's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node. Run: npm run build
//
// - Core's runtime, srcts/runtime/index.ts, is bundled into blockr-core.js.
// - A module, srcts/modules/<name>/index.ts or index.tsx, is bundled into
//   <name>.js on its own, by the build preset of ./preset.ts.
// - Core's React SDK, srcts/react, its dates and times, srcts/dates, its
//   binding, srcts/binding, and its controls, srcts/controls, are bundled into
//   files of their own, blockr-core-react.js, blockr-core-react-dates.js,
//   blockr-core-binding.js and blockr-core-controls.js, and so is its
//   OpenTelemetry module, srcts/otel, into blockr-core-otel.js.
// - The parts of OpenTelemetry's SDK that the OpenTelemetry module uses, which
//   srcts/otel/sdk.ts names, are bundled, minified, into a file of their own,
//   opentelemetry-<hash>.js, which the module imports by that name, so that
//   blockr-core-otel.js holds core's own code alone.
// - OpenTelemetry's API is bundled, minified, into opentelemetry-api.js, where
//   one of the files above imports it, as core's runtime and that bundle of
//   the SDK do. So the page runs one copy of the API, through which the
//   modules of packages record spans as well.
// - React and the packages that core's code imports beside it, the signals of
//   @preact/signals-core and the JSON Schema validator of @cfworker/json-schema,
//   are bundled, minified, into one file, vendor-<hash>.js, since the page
//   loads all of them up front.
//   So are the parts of React Aria that srcts/aria/index.ts names, which the
//   inputs draw with, as the one place in core's code that imports React
//   Aria. The file bundles those parts alone, since the whole of React Aria is
//   several times the size of React.
//   The parts of React Aria that core's dates and times draw with, which
//   srcts/dates/aria.ts names, are bundled, minified, into a file of their
//   own, vendor-dates-<hash>.js, since the page loads them only once a module
//   first imports core's dates.
//   The code of the vendor file goes into a chunk, chunk-<hash>.js, which
//   both files import, so that the page runs one copy of what they share,
//   such as React and React Aria's Button.
//   React's entry points, such as `react` and `react-dom/client`, are written
//   as ES modules of their own, such as react.js and react-dom-client.js,
//   which read React from the vendor file.
//   The runtime, the modules and the SDK, the binding, the controls and the
//   OpenTelemetry module import the other packages from the vendor file by
//   its name, and React Aria where they import srcts/aria/index.ts.
//   Core's dates import the parts of dates and times from their file, where
//   they import srcts/dates/aria.ts.
//   So blockr-core.js holds core's own code alone, and the page runs one copy
//   of each package.
//   The vendor check of ./vendor-check.ts, which core ships to packages beside
//   the preset, fails the build where one of those files would bundle any
//   other package from node_modules.
// - The runtime, the modules and the SDK, its dates, the binding and the
//   controls leave their imports of React, of OpenTelemetry's API and of each
//   other imports, which the page that R serves resolves through its import
//   map, as inst/js/import-map.json lays it out. So the page runs one copy of
//   each, and so do a package's modules.
//   The runtime leaves its import of the OpenTelemetry module an import as
//   well, so that a page loads the module only where it imports it.
// - What each file imports up front, the static imports of ./preload.ts, goes
//   into preload.json, React's entry points included, which import the vendor
//   file, and the vendor file and the file of the dates, which import the
//   chunk.
//   R names those files in the page's head, so that the browser fetches them at
//   once, rather than each once the file that imports it has loaded.
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
//   a test runner, and the package.json that names them.
//   That package.json names React and OpenTelemetry's API as its peers.
//   It names @preact/signals-core and @cfworker/json-schema as its
//   dependencies, since core's code imports them beside React.
//   The bundles leave each package that it names an import, which a
//   package's tests resolve to the copy that npm installs with `@blockr/core`.
//   So the stubs hold core's own code alone.
//   The controls, srcts/controls/index.ts, go there as the SDK does,
//   as `@blockr/core/controls`, for a package that writes its modules without
//   React, and so do the SDK's dates, srcts/dates/index.ts, as
//   `@blockr/core/react/dates`.
// - The licenses of what the files in inst/js, inst/types and inst/ids hold
//   from other projects go into LICENSES in each of these directories.
//   Each has a line for each package from node_modules, with its version, its
//   author where its package.json names one, and the files that hold its code.
//   Each project whose work one of core's own sources holds gets a line too,
//   as COPIED names it, such as the word lists that the words of ids are
//   picked from.
//   The text of a license follows the lines that share it.
//   Tailwind CSS counts as a package of each stylesheet, since it writes them.
//   A package that npm installs without a license file takes the one in
//   srcts/build/licenses under its name.
//   The build fails for a package with neither, so that no code ships without
//   its license.
//
// - The words that ids are made of, in srcts/store/words.ts, go into inst/ids,
//   one word a line, for R's rand_names() to draw from as the client's
//   mintId() does.
//
// The build owns inst/js and inst/types and empties them first, so that
// nothing is left of a module that is gone. In inst/outputs and inst/board, it
// owns the schemas only, and in inst/ids the files of words and LICENSES.

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { basename, dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import * as esbuild from "esbuild";
import postcss from "postcss";
import { createGenerator } from "ts-json-schema-generator";
import ts from "typescript";
import { IMPORTS, importFile, OTEL_API, REACT_ENTRIES } from "../import-map/index.ts";
import type { Import } from "../import-map/index.ts";
import { listing } from "../util/index.ts";
import { staticImports } from "./preload.ts";
import { preset } from "./preset.ts";
import { vendorCheck } from "./vendor-check.ts";

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

// A file that vendored() bundles, by its name, as vendor, and either the
// source that it starts from, `in`, or its code, `contents`, which imports
// what the file bundles.
type VendoredFile = { readonly name: string } & ({ readonly in: string } | { readonly contents: string });

// What vendored() wrote: the name of each file that it was given, by the
// name given, the names of all the files, with the chunks, the metafile of
// the build, which gives what each file imports, and what each file holds
// from other projects, by its path, as licensedSources() gives it.
interface Vendored {
  readonly files: ReadonlyMap<string, string>;
  readonly written: readonly string[];
  readonly metafile: esbuild.Metafile;
  readonly sources: ReadonlyMap<string, string[]>;
}

// The namespace of the files that vendored() is given the code of, which the
// code of one such file imports another by, as `vendored:vendor`.
const VENDORED = "vendored";

// Bundles code that core ships from other projects, minified and with its
// license comments, into files in `out`, one for each of `files`, which it
// resolves from core's repo.
// Where several files hold the same code, esbuild puts it into a chunk of its
// own, as chunk-JF4OGNM7.js, which each of them imports, so that the page
// runs one copy of it.
// The imports that `input` names external stay imports, which the page's
// import map resolves.
// Each file's name holds a hash of its content that esbuild gives it, as
// vendor-JF4OGNM7.js, so that no browser runs an old copy from its cache,
// since what imports the file imports it by that name.
// A file that the import map names, which R serves at a URL that holds a hash
// of the content, is named as given instead.
async function vendored(
  out: string,
  files: readonly VendoredFile[],
  input: Pick<esbuild.BuildOptions, "nodePaths" | "external"> = {},
  hashed = true,
): Promise<Vendored> {
  const code = new Map(files.flatMap((file) => ("contents" in file ? [[file.name, file.contents] as const] : [])));
  const result = await esbuild.build({
    absWorkingDir: repo,
    ...input,
    entryPoints: files.map((file) => ({ in: "in" in file ? file.in : `${VENDORED}:${file.name}`, out: file.name })),
    plugins: [
      {
        name: "blockr.core-vendored",
        setup(build) {
          build.onResolve({ filter: new RegExp(`^${VENDORED}:`) }, (args) => ({
            path: args.path.slice(VENDORED.length + 1),
            namespace: VENDORED,
          }));
          build.onLoad({ filter: /.*/, namespace: VENDORED }, (args) => ({
            contents: code.get(args.path) ?? "",
            resolveDir: repo,
            loader: "js",
          }));
        },
      },
    ],
    bundle: true,
    splitting: files.length > 1,
    outdir: out,
    entryNames: hashed ? "[name]-[hash]" : "[name]",
    chunkNames: "chunk-[hash]",
    metafile: true,
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

  // The name given for each file, by its entry point as the metafile names it.
  const named = new Map(files.map((file) => ["in" in file ? relative(repo, file.in) : `${VENDORED}:${file.name}`, file.name] as const));
  const outputs = Object.entries(result.metafile.outputs);
  return {
    files: new Map(
      outputs.flatMap(([path, { entryPoint }]) => {
        const name = entryPoint === undefined ? undefined : named.get(entryPoint);
        return name === undefined ? [] : [[name, basename(path)] as const];
      }),
    ),
    written: outputs.map(([path]) => basename(path)),
    metafile: result.metafile,
    sources: licensedSources(result.metafile, repo),
  };
}

// The imports that the files of a build leave imports, by their specifiers.
function externalImports(metafile: esbuild.Metafile): string[] {
  return Object.values(metafile.outputs).flatMap((output) =>
    output.imports.flatMap((entry) => (entry.external === true ? [entry.path] : [])),
  );
}

// What each file of a build holds from other projects, by the file's path:
// the directory of each package from node_modules whose code it bundles, and
// each of core's sources that COPIED names. The metafile gives each path from
// `dir`, the working directory of the build, whose srcts holds core's sources.
function licensedSources(metafile: esbuild.Metafile, dir: string): Map<string, string[]> {
  const src = join(dir, "srcts");
  const bundled = new Map<string, string[]>();
  for (const [output, { inputs }] of Object.entries(metafile.outputs)) {
    const sources = new Set(
      Object.keys(inputs).flatMap((input) => {
        const found = /^(.*node_modules\/(?:@[^/]+\/)?[^/]+)\//.exec(input)?.[1];
        if (found !== undefined) return [resolve(dir, found)];
        const file = resolve(dir, input);
        return COPIED.has(relative(src, file)) ? [file] : [];
      }),
    );
    if (sources.size > 0) bundled.set(resolve(dir, output), [...sources]);
  }
  return bundled;
}

// The packages beside React that core's code imports, which the build bundles
// with React into vendor-<hash>.js, rather than into each file that imports
// them.
// A package that core's code starts to import goes here, or the vendor check
// fails the build.
const VENDOR_IMPORTS = ["@preact/signals-core", "@cfworker/json-schema"] as const;

// The files in srcts that name the parts of React Aria that core's code draws
// with: aria/index.ts, whose parts the build bundles into vendor-<hash>.js by
// those names, and dates/aria.ts, whose parts of dates and times it bundles
// into vendor-dates-<hash>.js.
function ariaFiles(src: string): { vendor: string; dates: string } {
  return { vendor: join(src, "aria", "index.ts"), dates: join(src, "dates", "aria.ts") };
}

// What vendorBundle() wrote, with the names of the vendor file and of the file
// of the dates, which it writes only where srcts holds dates/aria.ts.
type VendorFiles = Vendored & { readonly file: string; readonly dates: string | undefined };

// Bundles React, ReactDOM and the packages that VENDOR_IMPORTS names into one
// minified file in `out`, as vendor-JF4OGNM7.js, with the parts of React Aria
// that srcts/aria/index.ts names, where srcts holds it.
// It bundles the parts of dates and times that srcts/dates/aria.ts names,
// where srcts holds it, into a file of their own, as
// vendor-dates-NB2X5AQE.js.
// The vendor file exports each of React's entry points as a namespace of its
// own, entry0 for `react` and so on, which React's ES modules read.
// It exports the names of the other packages as they are, which core's code
// imports, and the names that srcts/aria/index.ts exports, as the file of the
// dates exports those that srcts/dates/aria.ts exports.
// The file of the dates imports the vendor file's code first, as the page
// loads the vendor file before any module imports core's dates.
// So esbuild finds all of the vendor file's code in both files, and puts it
// into one chunk, which both import.
// The file of the dates then holds the parts of dates and times alone, and the
// vendor file the exports of the chunk, under the names that core's code
// imports.
// Without that import, esbuild would split the vendor file's code in two, at
// what the dates share with it, such as React, and split it again for each
// further file of parts that the page loads on demand, such as colours would
// be.
// The page would load all of these pieces as it starts, which together take
// more than the one chunk.
async function vendorBundle(out: string, src: string): Promise<VendorFiles> {
  const aria = ariaFiles(src);
  const vendor = {
    name: "vendor",
    contents: [
      ...REACT_ENTRIES.map((name, i) => `export * as entry${String(i)} from ${JSON.stringify(name)};`),
      ...VENDOR_IMPORTS.map((name) => `export * from ${JSON.stringify(name)};`),
      ...(existsSync(aria.vendor) ? [`export * from ${JSON.stringify(aria.vendor)};`] : []),
    ].join("\n"),
  };
  const dates = existsSync(aria.dates)
    ? [{ name: "vendor-dates", contents: `import "${VENDORED}:vendor";\nexport * from ${JSON.stringify(aria.dates)};\n` }]
    : [];
  const built = await vendored(out, [vendor, ...dates], { nodePaths: [join(repo, "node_modules")] });
  return { ...built, file: built.files.get("vendor") ?? "", dates: built.files.get("vendor-dates") };
}

// Leaves each import of a package that VENDOR_IMPORTS names an import of the
// vendor file, by its name beside the file that imports it, and so each import
// of srcts/aria/index.ts, in `src`, and each import of srcts/dates/aria.ts an
// import of the file of the dates, as vendorBundle() names them.
// The packages have no side effects, as their package.json files say.
// So a file that imports none of their names, as from code of core's that it
// bundles, keeps no import of the vendor file.
function vendorImports(files: Pick<VendorFiles, "file" | "dates">, src: string): esbuild.Plugin {
  const names: readonly string[] = VENDOR_IMPORTS;
  const aria = ariaFiles(src);
  const beside = (file: string) => ({ path: `./${file}`, external: true, sideEffects: false });
  const vendor = beside(files.file);
  const dates = files.dates === undefined ? undefined : beside(files.dates);
  return {
    name: "blockr.core-vendor-imports",
    setup(build) {
      build.onResolve({ filter: /^[^./]/ }, (args) => (names.includes(args.path) ? vendor : undefined));
      build.onResolve({ filter: /^\.\.?\// }, (args) => {
        const path = resolve(args.resolveDir, args.path);
        return path === aria.vendor ? vendor : path === aria.dates ? dates : undefined;
      });
    },
  };
}

// Writes React's entry points into `out` as ES modules of their own, each
// exporting the names that the entry point exports, from the vendor file,
// `bundle`, so that all of them read one React.
async function reactModules(out: string, bundle: string): Promise<string[]> {
  const names = reactNames();
  const written: string[] = [];
  for (const [i, name] of REACT_ENTRIES.entries()) {
    const exported = (names[name] ?? []).filter((key) => key !== "default" && /^[A-Za-z_$][\w$]*$/.test(key));
    const file = importFile(name);
    await writeFile(
      join(out, file),
      [
        `// ${name}, as an ES module, from the one copy of React that blockr.core ships.`,
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
  ["@blockr/core/react/dates", "dates"],
  ["@blockr/core/binding", "binding"],
  ["@blockr/core/controls", "controls"],
  ["@blockr/core/otel", "otel"],
] as const;

// Maps the imports in core's sources of core's React SDK, its dates, its
// binding and its controls, from their sources in srcts, to their names in the
// import map, which the preset leaves imports.
// The SDK holds the inputs and the overlays, so an import of srcts/inputs or
// srcts/overlays is one of the SDK.
// The build of a library bundles the library's own sources, which `own` names.
function sourceImports(src: string, own?: Import): esbuild.Plugin {
  const libraries: readonly (readonly [Import, string])[] = [
    ...LIBRARIES,
    ["@blockr/core/react", "inputs"],
    ["@blockr/core/react", "overlays"],
  ];
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

// Bundles the parts of OpenTelemetry's SDK that srcts/otel/sdk.ts names into
// one file in `out`, as opentelemetry-KZ3MV2QH.js, as it bundles React, and
// returns what vendored() returns, with the file's name, or writes nothing
// where srcts holds no such file.
// The bundle leaves its imports of OpenTelemetry's API imports, so that the
// SDK records through the API that core's runtime and the modules of packages
// use.
async function otelSdk(src: string, out: string): Promise<(Vendored & { readonly file: string }) | undefined> {
  const entry = join(src, "otel", "sdk.ts");
  if (!existsSync(entry)) return undefined;
  const built = await vendored(out, [{ name: "opentelemetry", in: entry }], {
    nodePaths: [join(repo, "node_modules")],
    external: [OTEL_API],
  });
  return { ...built, file: built.files.get("opentelemetry") ?? "" };
}

// Bundles OpenTelemetry's API into the file in `out` that the import map names
// for it, opentelemetry-api.js, with its default export too.
async function otelApi(out: string): Promise<Vendored> {
  const contents = `export * from ${JSON.stringify(OTEL_API)};\nexport { default } from ${JSON.stringify(OTEL_API)};\n`;
  return vendored(out, [{ name: importFile(OTEL_API).replace(/\.js$/, ""), contents }], {}, false);
}

// Leaves the OpenTelemetry module's import of srcts/otel/sdk.ts an import of
// the file that otelSdk() wrote, by its name beside the module.
function vendoredSdk(src: string, file: string): esbuild.Plugin {
  const sdk = join(src, "otel", "sdk.ts");
  return {
    name: "blockr.core-otel-sdk",
    setup(build) {
      build.onResolve({ filter: /^\.\.?\// }, (args) =>
        resolve(args.resolveDir, args.path) === sdk ? { path: `./${file}`, external: true } : undefined,
      );
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
  const vendor = await vendorBundle(out, src);
  const plugins = (own?: Import) => [vendorImports(vendor, src), sourceImports(src, own), ...preset.plugins, vendorCheck];
  const libraries = LIBRARIES.filter(([, dir]) => existsSync(join(src, dir, "index.ts")));
  const otel = await otelSdk(src, out);
  const sdk = otel?.file;

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
          plugins: [...(name === "@blockr/core/otel" && sdk !== undefined ? [vendoredSdk(src, sdk)] : []), ...plugins(name)],
          entryPoints: [{ in: join(src, dir, "index.ts"), out: importFile(name).replace(/\.js$/, "") }],
        }),
      ),
    )),
  ];

  if (results.some((result) => result.warnings.length > 0)) {
    throw new Error("esbuild warned, as printed above");
  }

  const written = results.flatMap((result) => Object.keys(result.metafile.outputs));
  // What each file holds from other projects, by the file's path.
  const bundled = new Map(results.flatMap((result) => [...licensedSources(result.metafile, root)]));
  const react = await reactModules(out, vendor.file);
  written.push(...react.map((file) => relative(root, join(out, file))));
  // OpenTelemetry's API, where core's runtime, a module, a library or the
  // bundle of the SDK imports it.
  const external = [...results, ...(otel === undefined ? [] : [otel])].flatMap((result) => externalImports(result.metafile));
  const api = external.includes(OTEL_API) ? await otelApi(out) : undefined;
  // The files of code from other projects: the vendor file, the file of the
  // dates and their chunk, and the bundles of OpenTelemetry.
  const vendoredFiles = [vendor, ...(otel === undefined ? [] : [otel]), ...(api === undefined ? [] : [api])];
  for (const files of vendoredFiles) {
    written.push(...files.written.map((file) => relative(root, join(out, file))));
    for (const [file, sources] of files.sources) bundled.set(file, sources);
  }

  // The import map that the page R serves holds, as R reads it: each import by
  // the file in inst/js that it resolves to. R serves the files that core's
  // files import by their names, the vendor file and OpenTelemetry's bundle,
  // as files beside them.
  const imports = Object.fromEntries(
    IMPORTS.filter((name) => existsSync(join(out, importFile(name)))).map((name) => [name, importFile(name)]),
  );
  const map = join(out, "import-map.json");
  await writeJson(map, { imports });
  written.push(relative(root, map));

  // What each file imports up front, the files of code from other projects
  // among them, as the vendor file imports the chunk and the bundle of
  // OpenTelemetry's SDK imports its API, and React's entry points, which the
  // build writes itself, the vendor file.
  const preload = join(out, "preload.json");
  const imported: Record<string, readonly string[]> = {
    ...staticImports([...results, ...vendoredFiles].map((result) => result.metafile)),
    ...Object.fromEntries(react.map((file) => [file, [`./${vendor.file}`]])),
  };
  await writeJson(preload, Object.fromEntries(Object.entries(imported).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))));
  written.push(relative(root, preload));

  for (const entry of [runtime, ...modules]) {
    const css = join(entry.dir, "index.css");
    if (!existsSync(css)) continue;
    const file = join(out, `${entry.name}.css`);
    await writeFile(file, await stylesheet(css));
    written.push(relative(root, file));
    bundled.set(file, [join(repo, "node_modules", "tailwindcss")]);
  }

  written.push(...(await licenses(root, out, bundled)));
  return written.sort();
}

// The parts of a package's package.json that LICENSES names.
interface Manifest {
  name: string;
  version: string;
  author?: string | { name?: string };
}

// The sources of core that hold work from other projects, by their paths in
// srcts. For each, the lines that LICENSES names the projects by, and the
// directory in srcts/build/licenses that holds their license.
// The words that ids are made of are picked from the word lists of two
// projects, which share one license text.
const COPIED = new Map([
  [
    "store/words.ts",
    {
      projects: [
        "The word lists of adjective-adjective-animal, by Grant Forrest",
        "The word lists of the R package ids, by Rich FitzJohn",
      ],
      license: "word-lists",
    },
  ],
]);

// The text of each license file in a package's directory, such as LICENSE or
// LICENSE.md, or none where the directory holds none or does not exist.
async function licenseTexts(dir: string): Promise<string[]> {
  if (!existsSync(dir)) return [];
  const files = (await readdir(dir)).filter((file) => /^(licen[cs]e|copying)/i.test(file)).sort();
  return Promise.all(files.map(async (file) => (await readFile(join(dir, file), "utf8")).replace(/\r\n/g, "\n").trimEnd()));
}

// The author that a package's package.json names, without an email or a URL.
function authorName(manifest: Manifest): string | undefined {
  const name = typeof manifest.author === "string" ? manifest.author.replace(/\s*[<(].*$/, "") : manifest.author?.name;
  return name === undefined || name === "" ? undefined : name;
}

// Writes the licenses of what the files in `dir` hold from other projects
// into `dir`, as LICENSES, and returns its path from `root`.
// For each file, `bundled` gives what it holds, by the file's path: the
// directories of packages from node_modules and the sources that COPIED names.
// A package whose directory holds no license file takes the one that
// srcts/build/licenses in core's repo holds under its name, as the packages
// come from core's node_modules, and the build fails for a package with
// neither.
// Where the files hold nothing from other projects, it writes nothing.
export async function licenses(
  root: string,
  dir: string,
  bundled: ReadonlyMap<string, readonly string[]>,
): Promise<string[]> {
  const holders = new Map<string, string[]>();
  for (const [file, sources] of bundled) {
    for (const source of sources) holders.set(source, [...(holders.get(source) ?? []), relative(dir, file)]);
  }
  if (holders.size === 0) return [];

  const src = join(root, "srcts");
  const kept = join(repo, "srcts", "build", "licenses");
  const entries = await Promise.all(
    [...holders].map(async ([source, files]) => {
      const held = listing(files.sort());
      const copied = COPIED.get(relative(src, source));
      if (copied !== undefined) {
        const texts = await licenseTexts(join(kept, copied.license));
        if (texts.length === 0) {
          throw new Error(
            `${relative(root, source)} holds the work of other projects, but srcts/build/licenses/${copied.license} holds no license file.`,
          );
        }
        return copied.projects.map((project) => ({ line: `${project}, in ${held}`, text: texts.join("\n\n") }));
      }
      const manifest = JSON.parse(await readFile(join(source, "package.json"), "utf8")) as Manifest;
      const own = await licenseTexts(source);
      const texts = own.length > 0 ? own : await licenseTexts(join(kept, manifest.name));
      if (texts.length === 0) {
        throw new Error(
          `The code of ${manifest.name} in ${held} comes with no license: neither its directory in node_modules nor srcts/build/licenses/${manifest.name} holds a license file.`,
        );
      }
      const author = authorName(manifest);
      return [
        {
          line: `${manifest.name} ${manifest.version}${author === undefined ? "" : `, by ${author}`}, in ${held}`,
          text: texts.join("\n\n"),
        },
      ];
    }),
  );

  // Each license once, after the lines of the packages that share it, in the
  // order of the packages' names.
  const byLicense = new Map<string, string[]>();
  for (const { line, text } of entries.flat().sort((a, b) => (a.line < b.line ? -1 : 1))) {
    byLicense.set(text, [...(byLicense.get(text) ?? []), line]);
  }
  const file = join(dir, "LICENSES");
  await writeFile(
    file,
    [...byLicense].map(([text, lines]) => `${lines.join("\n")}\n\n${text}\n`).join(`\n${"-".repeat(78)}\n\n`),
  );
  return [relative(root, file)];
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
// inst/ids, one word a line, adjectives.txt and animals.txt, and the license of
// the word lists they are picked from beside them, into LICENSES.
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
  written.push(...(await licenses(root, out, new Map(written.map((file) => [join(root, file), [source]])))));

  return written;
}

async function writeJson(file: string, value: unknown): Promise<void> {
  await writeFile(file, `${JSON.stringify(value, null, 2)}\n`);
}

// The version of a package that core's build resolves, from the package.json
// in core's node_modules, which the exports of a package such as
// OpenTelemetry's API do not name.
function packageVersion(name: string): string {
  return (JSON.parse(readFileSync(join(repo, "node_modules", name, "package.json"), "utf8")) as { version: string }).version;
}

// The package that inst/types is: the types modules are written against, as
// `@blockr/core`, the stubs of the hosts, as `@blockr/core/stubs`, and core's
// React SDK, its dates, its binding and the build preset, as
// `@blockr/core/react`, `@blockr/core/react/dates`, `@blockr/core/binding` and
// `@blockr/core/build`.
// Its bundles leave React an import, so it names the React that core's
// runtime bundles, at that version, as its peer.
// So npm installs that React with `@blockr/core` where a package has none, and
// refuses a package that asks for another, so that a package's tests run on
// one copy of the app's React, the stubs drawing core's UIs with it, while a
// package's build leaves its imports of React, and of the SDK, the binding
// and the controls, to the page's import map.
// It names OpenTelemetry's API, which the page's import map names, as its peer
// too, at the version that core ships, since core's declarations name the
// API's types and its bundles leave the API an import. So a package's modules
// type their spans, and its tests record core's spans and its own, through
// the API of the page.
// Core's code, which the stubs bundle, imports @preact/signals-core and
// @cfworker/json-schema.
// The bundles leave both packages imports, so the package names them as its
// dependencies, at the versions that core's runtime bundles.
// So npm installs them with `@blockr/core`, and the stubs hold core's own code
// alone, as the Conventions of State in JS ask
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1#conventions).
// They are not peers, as React is, since core's declarations name neither.
// So no signal or validator passes between the stubs and a package's code,
// and a package may depend on other versions of the two packages.
// Then npm puts core's versions in the node_modules of `@blockr/core`.
// The SDK draws its inputs with React Aria, whose parts srcts/aria/index.ts
// names, from react-aria-components, which the bundles leave an import too.
// The package names it as its dependency, at the version that core's vendor
// file bundles, since core's declarations name its types, while a package
// draws with React Aria only through the components of the SDK.
// The SDK's dates read the strings of a state with @internationalized/date,
// which srcts/dates/aria.ts names, and which the bundles leave an import as
// well.
// So the package names it as its dependency too, at the version that the
// file of the dates bundles.
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
    "./react/dates": { types: "./dates/index.d.ts", default: "./dates/index.js" },
    "./stubs": { types: "./stubs/index.d.ts", default: "./stubs/index.js" },
    "./controls": { types: "./controls/index.d.ts", default: "./controls/index.js" },
  },
  dependencies: {
    "@preact/signals-core": packageVersion("@preact/signals-core"),
    "@cfworker/json-schema": packageVersion("@cfworker/json-schema"),
    "react-aria-components": packageVersion("react-aria-components"),
    "@internationalized/date": packageVersion("@internationalized/date"),
  },
  peerDependencies: {
    react: packageVersion("react"),
    "react-dom": packageVersion("react-dom"),
    [OTEL_API]: packageVersion(OTEL_API),
  },
};

// The modules of srcts that the types package bundles,
// each as the export of its name.
const BUNDLED = ["stubs", "binding", "react", "dates", "build", "controls"];

// Leaves each import of a package that `names` holds an import, and each
// import of one of its entry points, such as react-aria-components/Button.
// The packages have no side effects, as their package.json files say.
// So a bundle that holds some of core's code, but uses no name of the
// package that the code imports, keeps no import of it.
// Without that, the SDK, the binding and the controls would each import
// @cfworker/json-schema, whose validator they do not use.
function dependencyImports(names: readonly string[]): esbuild.Plugin {
  return {
    name: "blockr.core-dependency-imports",
    setup(build) {
      build.onResolve({ filter: /^[^./]/ }, (args) =>
        names.some((name) => args.path === name || args.path.startsWith(`${name}/`))
          ? { path: args.path, external: true, sideEffects: false }
          : undefined,
      );
    },
  };
}

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
      // The peers that the package.json names, React and OpenTelemetry's API,
      // stay imports, with their subpaths, such as react/jsx-runtime.
      // A package's tests resolve them to those that npm installs with
      // `@blockr/core`.
      external: Object.keys(TYPES_PACKAGE.peerDependencies).flatMap((name) => [name, `${name}/*`]),
      // So do the packages that the package.json names as its dependencies.
      plugins: [dependencyImports(Object.keys(TYPES_PACKAGE.dependencies))],
      logLevel: "warning",
      metafile: true,
    });
    if (warnings.length > 0) {
      throw new Error("esbuild warned, as printed above");
    }
    written.push(...Object.keys(metafile.outputs));
    written.push(...(await licenses(root, dir, licensedSources(metafile, root))));
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
