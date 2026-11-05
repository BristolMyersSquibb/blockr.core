// What a package's build makes of its module with the build preset that core
// ships as `@blockr/core/build`, as it bundles the React SDK that core ships as
// `@blockr/core/react`: a module that holds none of React, and that draws with
// the copy that core's runtime puts on the page.

import { readFileSync } from "node:fs";
import { mkdir, mkdtemp, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import { afterEach, beforeEach, expect, test } from "vitest";
import { stubOutputHost } from "../stubs/index.ts";
import { mountReact } from "../testing/react.ts";
import type { OutputFixtures, TextOutput } from "../types/outputs.ts";
import type * as Build from "./index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The build preset as core ships it.
const { preset } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

// Core's runtime as it ships, which puts the one copy of React on the page.
await import(pathToFileURL(join(repo, "inst/js/blockr-core.js")).href);

let root: string;

beforeEach(async () => {
  // The package sits under the repo, since vitest imports no module from
  // outside it into a test that runs in a page.
  const scratch = join(repo, "node_modules", ".tmp");
  await mkdir(scratch, { recursive: true });
  root = await mkdtemp(join(scratch, "blockr-core-preset-"));
  // A package's build resolves `@blockr/core` as what core ships.
  await mkdir(join(root, "node_modules", "@blockr"), { recursive: true });
  await symlink(join(repo, "inst", "types"), join(root, "node_modules", "@blockr", "core"), "dir");
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

test("bundles the React SDK into a package's module, but none of React", async () => {
  const code = await readFile(await bundle(textRenderer), "utf8");

  expect(code).toContain("function outputRenderer(");
  expect(code).toContain('globalThis["blockr.core-react"]');
  // React marks each element it makes with this symbol.
  expect(code).not.toContain("react.transitional.element");
});

test("the module draws with the copy of React that core's runtime puts on the page", async () => {
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

test("refuses an entry point of React that core does not share", async () => {
  await expect(
    bundle(`import { renderToString } from "react-dom/server";\n\nexport const html = renderToString(null);\n`),
  ).rejects.toThrow("A module imports react-dom/server, which core does not share.");
});
