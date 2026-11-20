// What a package's build lists of what each of its files imports up front,
// with staticImports() as core ships it beside the build preset, as
// `@blockr/core/build`, from the metafiles of its builds.
// R reads the list to name those files in the head of the page that it serves.

import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as esbuild from "esbuild";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as Build from "./index.ts";

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The build preset and the list as core ships them.
const { preset, staticImports } = (await import(pathToFileURL(join(repo, "inst/types/build/index.js")).href)) as typeof Build;

let root: string;

beforeEach(async () => {
  root = await mkdtemp(join(tmpdir(), "blockr-core-preload-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

test("lists what each file of a package's build imports up front, by its name or as a file beside it", async () => {
  await mkdir(join(root, "srcts"), { recursive: true });
  await writeFile(
    join(root, "srcts/dock.ts"),
    [
      'import { useState } from "react";',
      'import { useBlock } from "@blockr/core/react";',
      'import { dockview } from "./vendor-1a2b3c4d.js";',
      'export const later = () => import("./vendor-9c8d7e6f.js");',
      "export const all = [useState, useBlock, dockview];",
      "",
    ].join("\n"),
  );
  await writeFile(join(root, "srcts/dock.css"), '@import "./vendor-5e6f7a8b.css";\n.dock {\n  color: red;\n}\n');
  await writeFile(join(root, "srcts/plain.ts"), "export const plain = 1;\n");

  // The package leaves its imports of its vendor files imports, as a package
  // imports the libraries that it bundles from its vendor file.
  const options = {
    ...preset,
    absWorkingDir: root,
    outdir: join(root, "inst/js"),
    external: ["./vendor-*"],
    metafile: true,
    logLevel: "silent",
  } satisfies esbuild.BuildOptions;
  const js = await esbuild.build({ ...options, entryPoints: ["srcts/dock.ts", "srcts/plain.ts"] });
  const css = await esbuild.build({ ...options, entryPoints: ["srcts/dock.css"] });

  const listed = staticImports([js.metafile, css.metafile, undefined]);

  // A file that imports nothing up front has no entry, and a dynamic import is
  // left out, which the page loads only as it runs the import.
  expect(Object.keys(listed)).toEqual(["dock.css", "dock.js"]);
  expect(listed["dock.css"]).toEqual(["./vendor-5e6f7a8b.css"]);
  expect([...(listed["dock.js"] ?? [])].sort()).toEqual(["./vendor-1a2b3c4d.js", "@blockr/core/react", "react"]);
});

test("lists the chunk that esbuild splits off the code that several files of a build share, as a file beside each of them", async () => {
  await mkdir(join(root, "srcts"), { recursive: true });
  await writeFile(join(root, "srcts/shared.ts"), "export const shared = () => 'shared';\n");
  await writeFile(join(root, "srcts/a.ts"), 'import { shared } from "./shared.ts";\nexport const a = shared();\n');
  await writeFile(join(root, "srcts/b.ts"), 'import { shared } from "./shared.ts";\nexport const b = shared();\n');

  const { metafile } = await esbuild.build({
    ...preset,
    absWorkingDir: root,
    outdir: join(root, "inst/js"),
    entryPoints: ["srcts/a.ts", "srcts/b.ts"],
    splitting: true,
    chunkNames: "chunk-[hash]",
    metafile: true,
    logLevel: "silent",
  });

  const chunks = Object.keys(metafile.outputs).map((path) => basename(path)).filter((file) => file.startsWith("chunk-"));
  expect(chunks).toHaveLength(1);
  expect(staticImports([metafile])).toEqual({ "a.js": [`./${chunks[0] ?? ""}`], "b.js": [`./${chunks[0] ?? ""}`] });
});
