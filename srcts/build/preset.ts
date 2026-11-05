// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package's build
// bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// It holds esbuild's options for a module: an ES module for the browser, whose
// imports of React go to the one copy that core's runtime puts on the page, so
// that its bundle holds no React. A package's build adds its entry points and
// where they go, as in
// `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// The file imports nothing but the names of React's global, since Node runs it
// as core's build and a package's build load it.

import type { BuildOptions, Plugin } from "esbuild";
import { REACT_ENTRIES, REACT_GLOBAL } from "../react-global/index.ts";

// Maps each import of React's entry points in a module to the copy that core's
// runtime puts on the page, under REACT_GLOBAL, so that every module draws
// with that one copy. An entry point that core does not share fails the
// build, rather than bundling a second copy of React.
export const sharedReact: Plugin = {
  name: "shared-react",
  setup(build) {
    build.onResolve({ filter: /^react(-dom)?(\/|$)/ }, (args) => {
      if (!(REACT_ENTRIES as readonly string[]).includes(args.path)) {
        return { errors: [{ text: `A module imports ${args.path}, which core does not share. It shares ${REACT_ENTRIES.join(", ")}.` }] };
      }
      return { path: args.path, namespace: "shared-react" };
    });
    build.onLoad({ filter: /.*/, namespace: "shared-react" }, (args) => ({
      contents: [
        `const shared = globalThis[${JSON.stringify(REACT_GLOBAL)}];`,
        `if (shared === undefined) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");`,
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
  plugins: [sharedReact],
} satisfies BuildOptions;
