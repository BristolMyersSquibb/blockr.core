// Builds core's client into inst/js, which the R package ships and git keeps,
// since R CMD check and CRAN run no Node. Run: npm run build
//
// - Core's runtime, srcts/runtime/index.ts, is bundled into blockr-core.js.
// - A module, srcts/modules/<name>/index.ts, is bundled into <name>.js on its
//   own.
// - An index.css beside either is its stylesheet, built with Tailwind CSS into
//   blockr-core.css or <name>.css.
//
// The build owns inst/js and empties it first, so that nothing is left of a
// module that is gone.

import { existsSync } from "node:fs";
import { mkdir, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import tailwindcss from "@tailwindcss/postcss";
import * as esbuild from "esbuild";
import postcss from "postcss";

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

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
  for (const file of await build(root)) console.log(file);
}
