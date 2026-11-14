// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package's build
// bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// It holds esbuild's options for a module: an ES module for the browser, whose
// imports of React and of core's React SDK, its binding and its controls stay
// imports, which the page that R serves resolves through its import map.
// So the module's bundle holds none of them.
// A package's build adds its entry points and where they go, as in
// `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// The file imports nothing but the imports that the import map names, since
// Node runs it as core's build and a package's build load it.

import type { BuildOptions, Plugin } from "esbuild";
import { IMPORTS } from "../import-map/index.ts";

// Leaves each import in a module that the page's import map names an import,
// so that every module imports the one copy of each that blockr.core ships.
// An entry point of React that the map does not name fails the build, rather
// than bundling a second copy of React.
const imports: Plugin = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls|otel))$/ }, (args) => {
      if (!(IMPORTS as readonly string[]).includes(args.path)) {
        return {
          errors: [
            {
              text: `A module imports ${args.path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.`,
            },
          ],
        };
      }
      return { path: args.path, external: true };
    });
  },
};

export const preset = {
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  jsx: "automatic",
  // React and the libraries built on it read this to leave out what they
  // check only while an app is developed.
  define: { "process.env.NODE_ENV": JSON.stringify("production") },
  plugins: [imports],
} satisfies BuildOptions;
