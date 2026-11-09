// What a package's build makes of its module with the build preset that core
// ships as `@blockr/core/build`, as it imports the React SDK that core ships as
// `@blockr/core/react`: a module that holds none of React and none of the SDK,
// and that draws with the copies that core's runtime puts on the page.
// So does a module written without React on the controls that core ships as
// `@blockr/core/controls`.

import { readFileSync } from "node:fs";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import { afterEach, beforeEach, expect, test } from "vitest";
import { stubBlockHost, stubOutputHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { mountReact } from "../testing/react.ts";
import type { OutputFixtures, TextOutput } from "../types/outputs.ts";
import { SHARED_ENTRIES, SHARED_GLOBAL } from "../shared-global/index.ts";
import type * as Build from "./index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The build preset as core ships it.
const { preset } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

// Core's runtime as it ships, which puts the one copy of React on the page,
// with core's SDK, its binding and its controls.
await import(pathToFileURL(join(repo, "inst/js/blockr-core.js")).href);

let root: string;

beforeEach(async () => {
  // The package sits under the repo, since vitest imports no module from
  // outside it into a test that runs in a page.
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  // It installs nothing of `@blockr/core`, since its build resolves nothing of
  // it from disk: the preset maps what a module imports of it to the copies on
  // the page.
  root = await mkdtemp(join(scratch, "blockr-core-preset-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

// Bundles a package's module from its source, as its build does with the
// preset, and gives the file it bundled it into.
async function bundle(source: string): Promise<string> {
  const entry = join(root, "srcts", "text-renderer.tsx");
  await mkdir(dirname(entry), { recursive: true });
  await writeFile(entry, source);
  await esbuild.build({
    ...preset,
    absWorkingDir: root,
    entryPoints: [entry],
    outdir: join(root, "inst", "js"),
    logLevel: "silent",
  });
  return join(root, "inst", "js", "text-renderer.js");
}

const textRenderer = `import type { TextOutput } from "@blockr/core";
import { outputRenderer } from "@blockr/core/react";

export const api = 1;

export default outputRenderer<TextOutput>(({ output }) => <pre>{output.text}</pre>);
`;

test("core's runtime puts React, the SDK, the binding and the controls on the page, and nothing else", () => {
  const shared: unknown = Reflect.get(globalThis, SHARED_GLOBAL);

  expect(Object.keys(shared as object).sort()).toEqual([...SHARED_ENTRIES].sort());
  expect(Object.isFrozen(shared)).toBe(true);
});

test("bundles neither React nor the React SDK into a package's module", async () => {
  const code = await readFile(await bundle(textRenderer), "utf8");

  expect(code).not.toContain("function outputRenderer(");
  expect(code).toContain('globalThis["blockr.core-shared"]');
  expect(code).toContain('shared["@blockr/core/react"]');
  // React marks each element it makes with this symbol.
  expect(code).not.toContain("react.transitional.element");
});

test("the module draws with the copies of React and the SDK that core's runtime puts on the page", async () => {
  const fixtures = JSON.parse(
    readFileSync(join(repo, "inst", "outputs", "fixtures", "text-output.json"), "utf8"),
  ) as OutputFixtures<TextOutput>;
  const [sample] = fixtures.outputs;
  if (sample === undefined) throw new Error("R wrote no sample text outputs");

  const module = (await import(pathToFileURL(await bundle(textRenderer)).href)) as unknown;
  const mounted = await mountReact(module, stubOutputHost({ output: { ...sample.output } }));

  expect(mounted.el.querySelector("pre")?.textContent).toBe(sample.output.text);
  mounted.unmount();
  expect(mounted.el.childNodes).toHaveLength(0);
});

const plainModule = `import type { BlockModule } from "@blockr/core";
import { controls } from "@blockr/core/controls";

export const api = 1;

const label: BlockModule<{ label: string }> = {
  render({ el, host, signal }) {
    const ui = controls(host, signal);
    ui.text("label", { label: "Label" });
    el.replaceChildren(ui.el);
    return ui.cleanup;
  },
};

export default label;
`;

test("bundles none of the controls into a package's module written without React, which draws with core's copy", async () => {
  const file = await bundle(plainModule);
  const code = await readFile(file, "utf8");
  expect(code).not.toContain("function controls(");
  expect(code).toContain('globalThis["blockr.core-shared"]');
  expect(code).toContain('shared["@blockr/core/controls"]');
  expect(code).not.toContain("react.transitional.element");

  const module = (await import(pathToFileURL(file).href)) as unknown;
  const mounted = await mountReact(module, stubBlockHost({ states: [{ label: "x" }], requests: [] }));

  expect(mounted.el.querySelector("input")?.value).toBe("x");
  mounted.unmount();
  expect(mounted.el.querySelector("input")).toBeNull();
});

test("core's own block module, as core ships it, draws with the copies that core's runtime puts on the page", async () => {
  const code = readFileSync(join(repo, "inst", "js", "head-block.js"), "utf8");
  expect(code).not.toContain("function reactModule(");

  const fixture = JSON.parse(
    readFileSync(join(repo, "tests/testthat/_snaps/transform-head/head_block.json"), "utf8"),
  ) as BlockFixture;
  const module = (await import(pathToFileURL(join(repo, "inst", "js", "head-block.js")).href)) as unknown;
  const mounted = await mountReact(module, stubBlockHost(fixture));

  expect(mounted.el.querySelector("input")).not.toBeNull();
  mounted.unmount();
  expect(mounted.el.querySelector("input")).toBeNull();
});

test("refuses an entry point of React that core does not share", async () => {
  await expect(
    bundle(`import { renderToString } from "react-dom/server";\n\nexport const html = renderToString(null);\n`),
  ).rejects.toThrow("A module imports react-dom/server, which core does not share.");
});
