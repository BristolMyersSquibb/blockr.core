// @vitest-environment node

import { createRequire } from "node:module";
import { mkdir, mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { afterEach, beforeEach, expect, test } from "vitest";
import { IMPORTS, OTEL_API, REACT_ENTRIES, importFile } from "../import-map/index.ts";
import type { Status } from "../scheduler/index.ts";
import { build, idWords, licenses, outputTypes, schemaFile, schemas, stylesheet } from "./build.ts";

// The fixtures import and reference core's stylesheet, so that they test core's
// setup of Tailwind CSS.
const core = resolve(dirname(fileURLToPath(import.meta.url)), "../runtime/index.css");

// Core's repo, whose node_modules holds the packages that the build bundles.
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

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
  // Beside them, the build writes the vendor file, React's entry points, the
  // import map and the files that a page imports as it starts.
  expect((await readdir(out)).filter((file) => !/^react|^vendor-|^import-map|^preload/.test(file)).sort()).toEqual([
    "LICENSES",
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
  const vendor = (await readdir(out)).filter((file) => /^vendor-[0-9a-f]{8}\.js$/.test(file));
  expect(vendor).toHaveLength(1);
  expect(written).toEqual(
    ["LICENSES", "blockr-core.js", "import-map.json", "preload.json", ...vendor, ...REACT_ENTRIES.map(importFile)]
      .map((file) => `inst/js/${file}`)
      .sort(),
  );
  // The vendor file keeps React's license comments.
  expect(await readFile(join(out, vendor[0] ?? ""), "utf8")).toContain("@license React");

  // Each entry point exports what React's does in production, bar the two
  // names that its development build alone exports.
  const installed = createRequire(import.meta.url);
  for (const name of REACT_ENTRIES) {
    const module = (await import(pathToFileURL(join(out, importFile(name))).href)) as Record<string, unknown>;
    const exported = Object.keys(installed(name) as object).filter((key) => !["act", "captureOwnerStack"].includes(key));
    expect(Object.keys(module).sort()).toEqual([...exported, "default"].sort());
  }
});

test("bundles the packages that core's code imports beside React into the vendor file, minified, which the runtime and the modules import by its name", async () => {
  await write({
    "srcts/runtime/index.ts": [
      'import { computed, signal } from "@preact/signals-core";',
      'import { Validator } from "@cfworker/json-schema";',
      'export const valid = (value: unknown) => new Validator({ type: "integer" }).validate(value).valid;',
      "const n = signal(1);",
      "const doubled = computed(() => n.value * 2);",
      "export const double = (value: number) => {",
      "  n.value = value;",
      "  return doubled.value;",
      "};",
      "",
    ].join("\n"),
    "srcts/util/count.ts": 'import { signal } from "@preact/signals-core";\nexport const counter = () => signal(0);\nexport const label = "plain";\n',
    "srcts/modules/head-block/index.ts": 'import { counter } from "../../util/count.ts";\nexport const read = () => counter().value;\n',
    "srcts/modules/plain/index.ts": 'import { label } from "../../util/count.ts";\nexport const ui = label;\n',
  });

  const written = await build(root);

  const out = join(root, "inst/js");
  const vendor = (await readdir(out)).filter((file) => /^vendor-[0-9a-f]{8}\.js$/.test(file));
  expect(vendor).toHaveLength(1);
  const file = vendor[0] ?? "";
  expect(written).toContain(`inst/js/${file}`);
  const read = (name: string) => readFile(join(out, name), "utf8");
  // The packages' code, minified, without the paths of its sources, beside
  // React's.
  const bundled = await read(file);
  expect(bundled).toContain("Cycle detected");
  expect(bundled).toContain("Instance type");
  expect(bundled).toContain("@license React");
  expect(bundled).not.toContain("// node_modules/");
  // The runtime and the module hold core's code alone, and import the packages
  // from the vendor file, by its name beside them.
  for (const name of ["blockr-core.js", "head-block.js"]) {
    const own = await read(name);
    expect(own).toContain(`from "./${file}"`);
    expect(own).not.toContain("Cycle detected");
    expect(own).not.toContain("Instance type");
  }
  // A module that uses none of the packages' names keeps no import of the
  // vendor file, though it bundles code of core's that imports them.
  expect(await read("plain.js")).not.toContain(file);
  // The runtime runs on the packages in the vendor file.
  const runtime = (await import(pathToFileURL(join(out, "blockr-core.js")).href)) as {
    valid: (value: unknown) => boolean;
    double: (value: number) => number;
  };
  expect(runtime.valid(3)).toBe(true);
  expect(runtime.valid("3")).toBe(false);
  expect(runtime.double(4)).toBe(8);
  // The vendor file sits beside the runtime, where R serves it as a file
  // beside a module, so the import map names it nowhere.
  expect(await read("import-map.json")).not.toContain(file);
});

test("fails where one of core's files would bundle a package from node_modules that the vendor file does not hold, through the vendor check", async () => {
  await write({
    "node_modules/@acme/pad/package.json": '{ "name": "@acme/pad", "type": "module", "main": "index.js" }\n',
    "node_modules/@acme/pad/index.js": "export const pad = (s, n) => String(s).padStart(n);\n",
    "srcts/runtime/index.ts": 'import { pad } from "@acme/pad";\nexport const padded = pad("x", 3);\n',
  });

  await expect(build(root)).rejects.toThrow(
    "The build put code of @acme/pad, from node_modules, into inst/js/blockr-core.js. Bundle @acme/pad into the package's vendor file instead, and import it from there.",
  );
});

test("builds the SDK, the binding, the controls and the OpenTelemetry module on their own, which the runtime and the modules import by name", async () => {
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
    "srcts/otel/index.ts": "export const otel = 'the otel module';\n",
    "srcts/runtime/index.ts": `${imports("..")}import { trace } from "@opentelemetry/api";\nimport { version } from "react";\nexport const tracer = trace.getTracer("blockr.core", version);\nexport const otel = () => import("../otel/index.ts");\n`,
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
  // The runtime imports the OpenTelemetry module by its name, as it loads it,
  // which the build bundles on its own.
  expect(await read("blockr-core.js")).toContain('import("@blockr/core/otel")');
  expect(await read("blockr-core.js")).not.toContain("the otel module");
  expect(await read("blockr-core-otel.js")).toContain("the otel module");
  // The runtime imports OpenTelemetry's API by its name too, which the build
  // bundles into a file of its own, with its default export, as it imports it.
  expect(await read("blockr-core.js")).toContain('from "@opentelemetry/api"');
  expect(await read("blockr-core.js")).not.toContain("ProxyTracerProvider");
  const api = (await import(pathToFileURL(join(root, "inst/js", importFile(OTEL_API))).href)) as Record<string, unknown>;
  expect(Object.keys(api)).toEqual(expect.arrayContaining(["context", "default", "propagation", "trace"]));

  // The import map names each import by its file, and nothing else, since R
  // serves the files that those import, such as the vendor file, as files
  // beside them.
  const map = JSON.parse(await read("import-map.json")) as unknown;
  expect(map).toEqual({ imports: Object.fromEntries(IMPORTS.map((name) => [name, importFile(name)])) });

  // The build lists what each file imports up front: an import by name, and a
  // file beside it by its path. The runtime's import of the OpenTelemetry
  // module is left out, which the runtime imports only as it loads it, and
  // each of React's entry points imports the vendor file.
  const vendor = (await readdir(join(root, "inst/js"))).find((file) => /^vendor-[0-9a-f]{8}\.js$/.test(file));
  const preload = JSON.parse(await read("preload.json")) as Record<string, string[]>;
  expect([...(preload["blockr-core.js"] ?? [])].sort()).toEqual(
    ["@blockr/core/react", "@blockr/core/binding", "@blockr/core/controls", OTEL_API, "react"].sort(),
  );
  expect(preload["blockr-core-react.js"]).toEqual(["@blockr/core/binding"]);
  for (const name of REACT_ENTRIES) expect(preload[importFile(name)]).toEqual([`./${vendor ?? ""}`]);
  expect(Object.keys(preload)).toEqual(Object.keys(preload).sort());
});

test("bundles the parts of OpenTelemetry's SDK that the OpenTelemetry module uses, minified, into a file of their own, which the module imports by its name", async () => {
  await write({
    "srcts/otel/sdk.ts": 'export { SeverityNumber } from "@opentelemetry/api";\nexport { BatchLogRecordProcessor } from "@opentelemetry/sdk-logs";\n',
    "srcts/otel/index.ts":
      'import { BatchLogRecordProcessor, SeverityNumber } from "./sdk.ts";\nexport const warn = () => ["the otel module", SeverityNumber.WARN, BatchLogRecordProcessor];\n',
    "srcts/runtime/index.ts": 'export const otel = () => import("../otel/index.ts");\n',
  });

  const written = await build(root);

  const out = join(root, "inst/js");
  const sdk = (await readdir(out)).filter((file) => /^opentelemetry-[0-9a-f]{8}\.js$/.test(file));
  expect(sdk).toHaveLength(1);
  expect(written).toContain(`inst/js/${sdk[0] ?? ""}`);
  const vendored = await readFile(join(out, sdk[0] ?? ""), "utf8");
  // The SDK's code, minified, without the paths of its sources.
  expect(vendored).toContain("maxExportBatchSize");
  expect(vendored).not.toContain("// node_modules/");
  // It imports OpenTelemetry's API by its name, which the import map resolves
  // to the file of the API that the build writes, rather than bundle a copy.
  expect(vendored).toMatch(/from\s*"@opentelemetry\/api"/);
  expect(vendored).not.toContain("UNSPECIFIED");
  expect(written).toContain(`inst/js/${importFile(OTEL_API)}`);
  expect(await readFile(join(out, importFile(OTEL_API)), "utf8")).toContain("UNSPECIFIED");
  // The module holds core's code alone, readable, and imports the SDK by the
  // file's name beside it.
  const own = await readFile(join(out, "blockr-core-otel.js"), "utf8");
  expect(own).toContain("the otel module");
  expect(own).toContain(`from "./${sdk[0] ?? ""}"`);
  expect(own).not.toContain("maxExportBatchSize");
  // R serves the file beside the module, and the API as an import of its own.
  const map = JSON.parse(await readFile(join(out, "import-map.json"), "utf8")) as { imports: Record<string, string> };
  expect(map.imports[OTEL_API]).toBe(importFile(OTEL_API));
});

// The version of a package in core's node_modules, as its package.json gives
// it, which the exports of some packages do not name.
async function version(name: string): Promise<string> {
  return (JSON.parse(await readFile(join(repo, "node_modules", name, "package.json"), "utf8")) as { version: string })
    .version;
}

test("writes the licenses of the code from other projects in its files into LICENSES beside them, each license once", async () => {
  await write({
    "srcts/runtime/index.ts": "export const runtime = 'core';\n",
    "srcts/runtime/index.css": `@import "${core}";\n`,
  });

  const written = await build(root);

  const out = join(root, "inst/js");
  expect(written).toContain("inst/js/LICENSES");
  const vendor = (await readdir(out)).find((file) => /^vendor-[0-9a-f]{8}\.js$/.test(file)) ?? "";
  const text = await readFile(join(out, "LICENSES"), "utf8");
  // React's three packages share their license, whose text follows their lines.
  const react = await readFile(join(repo, "node_modules/react/LICENSE"), "utf8");
  expect(text).toContain(
    [
      `react ${await version("react")}, in ${vendor}`,
      `react-dom ${await version("react-dom")}, in ${vendor}`,
      `scheduler ${await version("scheduler")}, in ${vendor}`,
      "",
      react.trimEnd(),
    ].join("\n"),
  );
  // The vendor file holds the packages beside React too.
  expect(text).toContain(`@preact/signals-core ${await version("@preact/signals-core")}, in ${vendor}\n`);
  expect(text.split("Copyright (c) Meta Platforms, Inc. and affiliates.")).toHaveLength(2);
  // Tailwind CSS writes the stylesheet.
  const tailwind = await readFile(join(repo, "node_modules/tailwindcss/LICENSE"), "utf8");
  expect(text).toContain(`tailwindcss ${await version("tailwindcss")}, in blockr-core.css\n\n${tailwind.trimEnd()}\n`);
});

test("names the word lists that the words of ids are picked from beside each file that holds the words", async () => {
  await write({
    "srcts/store/words.ts": 'export const adjectives = ["calm"];\nexport const animals = ["heron"];\n',
    "srcts/runtime/index.ts": 'import { adjectives, animals } from "../store/words.ts";\nexport const id = `${adjectives[0]}_${animals[0]}`;\n',
  });

  await build(root);

  const license = await readFile(join(repo, "srcts/build/licenses/word-lists/LICENSE"), "utf8");
  expect(await readFile(join(root, "inst/js/LICENSES"), "utf8")).toContain(
    [
      "The word lists of adjective-adjective-animal, by Grant Forrest, in blockr-core.js",
      "The word lists of the R package ids, by Rich FitzJohn, in blockr-core.js",
      "",
      license.trimEnd(),
      "",
    ].join("\n"),
  );
});

test("takes the license of a package that npm installs without one from srcts/build/licenses, and fails for a package with neither", async () => {
  // A package that npm installs without a license file, as
  // @cfworker/json-schema, takes the one that core's repo keeps for it.
  await write({
    "node_modules/@scope/own/package.json": JSON.stringify({
      name: "@scope/own",
      version: "1.0.0",
      author: "Jane Doe <jane@example.com> (https://example.com)",
    }),
    "node_modules/@scope/own/LICENSE": "The license of own\n",
    "node_modules/@cfworker/json-schema/package.json": JSON.stringify({
      name: "@cfworker/json-schema",
      version: "2.0.0",
      author: { name: "Kim Lee" },
    }),
    "node_modules/none/package.json": JSON.stringify({ name: "none", version: "3.0.0" }),
  });
  const out = join(root, "inst/js");
  await mkdir(out, { recursive: true });
  const at = (name: string) => join(root, "node_modules", name);
  const kept = await readFile(join(repo, "srcts/build/licenses/@cfworker/json-schema/LICENSE.md"), "utf8");

  const written = await licenses(
    root,
    out,
    new Map([
      [join(out, "b.js"), [at("@cfworker/json-schema")]],
      [join(out, "a.js"), [at("@scope/own"), at("@cfworker/json-schema")]],
    ]),
  );

  expect(written).toEqual(["inst/js/LICENSES"]);
  expect(await readFile(join(out, "LICENSES"), "utf8")).toBe(
    [
      "@cfworker/json-schema 2.0.0, by Kim Lee, in a.js and b.js",
      "",
      kept.trimEnd(),
      "",
      "-".repeat(78),
      "",
      "@scope/own 1.0.0, by Jane Doe, in a.js",
      "",
      "The license of own",
      "",
    ].join("\n"),
  );

  await expect(licenses(root, out, new Map([[join(out, "a.js"), [at("none")]]]))).rejects.toThrow(
    "The code of none in a.js comes with no license",
  );
  // Where its files hold no code from other projects, a directory gets no
  // LICENSES.
  expect(await licenses(root, join(root, "inst/types"), new Map())).toEqual([]);
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
  // Preflight sets box-sizing on every element, while a utility such as
  // bc:box-border sets it on its own element alone.
  expect(css).not.toMatch(/^\s*\*[^{]*\{[^}]*box-sizing/m);
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
  expect(css).toContain("--bc-color-text-danger: var(--bc-color-red-700);");
  expect(css).toMatch(/--bc-color-red-700: oklch\(/);
});

test("draws the danger and the warning texts in shades that reach 4.5:1 under the light theme, and lighter ones under the dark theme", async () => {
  await write({ "test.css": `@import "${core}";\n@source inline("bc:text-text-danger bc:text-text-warning");\n` });

  const css = await stylesheet(join(root, "test.css"));

  const start = css.indexOf("@layer theme {");
  const theme = css.slice(start, css.indexOf("\n}\n", start));
  // Under the light theme, each takes the lightest shade of the theme's red or
  // amber that reaches 4.5:1 on a card and on the tint of a card's mark.
  expect(theme).toMatch(/:root, :host \{[^}]*--bc-color-text-danger: var\(--bc-color-red-700\);/);
  expect(theme).toMatch(/:root, :host \{[^}]*--bc-color-text-warning: var\(--bc-color-amber-800\);/);
  // Under the dark theme, each takes the value that blockr.ui gives its token
  // there. The rule sits in the theme's layer, so that a frontend that sets the
  // variable outside a layer wins over it.
  expect(theme).toMatch(/:root\[data-bc-theme="dark"\] \{[^}]*--bc-color-text-danger: #f87171;/);
  expect(theme).toMatch(/:root\[data-bc-theme="dark"\] \{[^}]*--bc-color-text-warning: #fbbf24;/);
  expect(css.replace(theme, "")).not.toMatch(/--bc-color-text-(danger|warning):/);
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
  expect(theme).toMatch(/:root\[data-bc-theme="dark"\] \{[^}]*--bc-color-bg-raised: color-mix\(in oklab, Canvas, currentColor 10%\);/);
  expect(css.replace(theme, "")).not.toContain("--bc-color-bg-raised:");
});

test("holds a color for each status of a block, lighter under the dark theme, and for each category of block classes, though no utility uses those", async () => {
  await write({ "test.css": `@import "${core}";\n` });

  const css = await stylesheet(join(root, "test.css"));

  // The colors sit in the theme's layer, so that a frontend that sets one
  // outside a layer wins over them.
  const start = css.indexOf("@layer theme {");
  const theme = css.slice(start, css.indexOf("\n}\n", start));
  expect(css.replace(theme, "")).not.toMatch(/--bc-color-(status|category)-[a-z]+:/);
  // Every status, as the type names them.
  const statuses: Record<Status, null> = { unevaluated: null, stale: null, waiting: null, invalid: null, ready: null, failed: null };
  for (const status of Object.keys(statuses)) {
    expect(theme).toMatch(new RegExp(`:root, :host \\{[^}]*--bc-color-status-${status}: #`));
    expect(theme).toMatch(new RegExp(`:root\\[data-bc-theme="dark"\\] \\{[^}]*--bc-color-status-${status}: #`));
  }
  // No UI of core's draws a category, whose color the dark theme keeps.
  expect(theme).toMatch(/:root, :host \{[^}]*--bc-color-category-uncategorized: #/);
  expect(theme).not.toMatch(/:root\[data-bc-theme="dark"\] \{[^}]*--bc-color-category-/);
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

test("writes the schemas of core's board options beside their modules", async () => {
  await write({
    "srcts/options/schemas.ts": [
      "export const optionSchemas = {",
      '  n_rows: { type: "integer", minimum: 0 },',
      '  show_conditions: { type: "array", items: { enum: ["error"] } },',
      "};",
      "",
    ].join("\n"),
  });

  // Each in a file of its own, beside the board option's module, which core
  // names by the board option's id
  expect(await schemas(root)).toEqual([
    "inst/js/n-rows-option.schema.json",
    "inst/js/show-conditions-option.schema.json",
  ]);

  const read = async (name: string) =>
    JSON.parse(await readFile(join(root, "inst", "js", name), "utf8")) as unknown;

  expect(await read("n-rows-option.schema.json")).toEqual({ type: "integer", minimum: 0 });
  expect(await read("show-conditions-option.schema.json")).toEqual({
    type: "array",
    items: { enum: ["error"] },
  });
});

test("writes the words that ids are made of, one word a line, for R, with the license of their word lists", async () => {
  await write({
    "srcts/store/words.ts": [
      'export const adjectives = ["calm", "keen"];',
      'export const animals = ["heron"];',
      "",
    ].join("\n"),
  });

  expect(await idWords(root)).toEqual(["inst/ids/adjectives.txt", "inst/ids/animals.txt", "inst/ids/LICENSES"]);
  expect(await readFile(join(root, "inst", "ids", "adjectives.txt"), "utf8")).toBe("calm\nkeen\n");
  expect(await readFile(join(root, "inst", "ids", "animals.txt"), "utf8")).toBe("heron\n");
  expect(await readFile(join(root, "inst", "ids", "LICENSES"), "utf8")).toBe(
    [
      "The word lists of adjective-adjective-animal, by Grant Forrest, in adjectives.txt and animals.txt",
      "The word lists of the R package ids, by Rich FitzJohn, in adjectives.txt and animals.txt",
      "",
      (await readFile(join(repo, "srcts/build/licenses/word-lists/LICENSE"), "utf8")).trimEnd(),
      "",
    ].join("\n"),
  );
});

test("writes no words without the module they come from", async () => {
  expect(await idWords(root)).toEqual([]);
});
