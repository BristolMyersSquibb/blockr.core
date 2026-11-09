// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package's build
// bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// It holds esbuild's options for a module: an ES module for the browser, whose
// imports of React go to the one copy that core's runtime puts on the page, so
// that its bundle holds no React.
// Its imports of core's React SDK, its binding and its controls go to the
// copies that core's runtime puts beside React, so that its bundle holds none
// of core's code either.
// A package's build adds its entry points and where they go, as in
// `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// The file imports nothing but the names of the global, since Node runs it
// as core's build and a package's build load it.

import type { BuildOptions, Plugin } from "esbuild";
import { SHARED_ENTRIES, SHARED_GLOBAL } from "../shared-global/index.ts";

// The namespace in which esbuild loads what a module imports of what core
// shares.
const NAMESPACE = "blockr.core-shared";

// Maps each import in a module of what core shares, React's entry points and
// core's React SDK, its binding and its controls, to the copy that core's
// runtime puts on the page, under SHARED_GLOBAL.
// So every module draws with one copy of React and runs the SDK of the core it
// runs on.
// An entry point of React that core does not share fails the build, rather
// than bundling a second copy of React.
export const shared: Plugin = {
  name: NAMESPACE,
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls))$/ }, (args) => {
      if (!(SHARED_ENTRIES as readonly string[]).includes(args.path)) {
        return { errors: [{ text: `A module imports ${args.path}, which core does not share. It shares ${SHARED_ENTRIES.join(", ")}.` }] };
      }
      return { path: args.path, namespace: NAMESPACE };
    });
    build.onLoad({ filter: /.*/, namespace: NAMESPACE }, (args) => ({
      contents: [
        `const shared = globalThis[${JSON.stringify(SHARED_GLOBAL)}];`,
        `if (shared === undefined) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");`,
        `module.exports = shared[${JSON.stringify(args.path)}];`,
      ].join("\n"),
      loader: "js",
    }));
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
  plugins: [shared],
} satisfies BuildOptions;
