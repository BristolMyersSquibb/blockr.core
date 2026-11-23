// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package's build
// bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// It holds esbuild's options for a module: an ES module for the browser, whose
// imports of React, of core's React SDK, its binding and its controls, and of
// OpenTelemetry's API stay imports, which the page that R serves resolves
// through its import map.
// So the module's bundle holds none of them.
// It fails a build that imports React Aria, whose components a module draws
// from the React SDK.
// A library built with the React Compiler imports the compiler's runtime.
// The preset resolves that import to the runtime of the React that the module
// imports.
// A package's build adds its entry points and where they go, as in
// `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// To the builds of its own files, it adds the vendor check of
// ./vendor-check.ts as well, which the preset's plugins leave out.
// The file imports nothing but the imports that the import map names, since
// Node runs it as core's build and a package's build load it.

import type { BuildOptions, Plugin } from "esbuild";
import { IMPORTS, OTEL_API } from "../import-map/index.ts";

// The imports of React Aria: react-aria-components, react-aria and
// react-stately, each with its entry points, such as
// react-aria-components/Button, and every package under @react-aria/ or
// @react-stately/, which libraries built on older versions of React Aria
// import.
// A module draws React Aria's components from the React SDK.
// Core bundles the SDK's copy of React Aria into its vendor file, without the
// preset's plugins (./build.ts).
// A second copy would hold React contexts of its own, which the SDK's
// components do not read.
// An item of the second copy inside the SDK's ComboBox would then find no
// select.
// The packages under @internationalized/, such as @internationalized/date,
// hold no React context, so a module bundles them itself.
// A type that a module imports with `import type` passes, since esbuild drops
// that import before it resolves anything.
const REACT_ARIA = /^(react-aria-components|react-aria|react-stately|@react-(aria|stately)\/[^/]+)(\/.*)?$/;

// The imports of the React Compiler's runtime.
// The code that the compiler writes keeps a component's values between renders
// in a cache, which the runtime's `c` gives it.
// A library that the compiler built for React 17 and 18 as well, such as React
// Flow 13, imports the runtime's own package, react-compiler-runtime.
// That package calls `require("react")`.
// A bundle that leaves React an import cannot run that call.
// So a module that bundled the package would fail as the page loads.
// A library built for React 19 alone imports react/compiler-runtime instead,
// an entry point of React that the page's import map does not name.
// The preset resolves both to COMPILER_RUNTIME_MODULE.
const COMPILER_RUNTIME = /^react[-/]compiler-runtime$/;

// What the preset bundles in place of the React Compiler's runtime: a module
// that reads the runtime that React exports as __COMPILER_RUNTIME.
// Its import of React stays an import, as every other does.
// So a package's module holds no copy of the runtime, and the page runs one
// copy of React.
// React's runtime holds `c` alone.
// The compiler's code imports nothing else, unless the compiler ran with an
// option for debugging it.
// A build of such code fails, with an error that names the import.
const COMPILER_RUNTIME_MODULE = `import { __COMPILER_RUNTIME } from "react";
export const { c } = __COMPILER_RUNTIME;
`;

// Leaves each import in a module that the page's import map names an import,
// so that every module imports the one copy of each that blockr.core ships.
// An entry point of React that the map does not name fails the build, rather
// than bundling a second copy of React.
// OpenTelemetry's API has no side effects, as its package says, so a module
// that imports none of its names, as from code of core's that it bundles,
// keeps no import of it.
// An import of React Aria fails the build as well, with an error that names
// the React SDK (REACT_ARIA).
// An import of the React Compiler's runtime gets COMPILER_RUNTIME_MODULE
// instead (COMPILER_RUNTIME).
// The preset resolves that import before React's entry points, since
// react/compiler-runtime is one of them.
const imports: Plugin = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: COMPILER_RUNTIME }, () => ({ path: "react-compiler-runtime", namespace: "blockr.core" }));
    build.onLoad({ filter: /^react-compiler-runtime$/, namespace: "blockr.core" }, () => ({ contents: COMPILER_RUNTIME_MODULE, loader: "js" }));
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls|otel)|@opentelemetry\/api)$/ }, (args) => {
      if (!(IMPORTS as readonly string[]).includes(args.path)) {
        return {
          errors: [
            {
              text: `A module imports ${args.path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.`,
            },
          ],
        };
      }
      return { path: args.path, external: true, ...(args.path === OTEL_API ? { sideEffects: false } : {}) };
    });
    build.onResolve({ filter: REACT_ARIA }, (args) => ({
      errors: [
        {
          text: `A module imports ${args.path}, which is part of React Aria. A package draws React Aria's components from @blockr/core/react, so that the page runs one copy of React Aria.`,
        },
      ],
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
  plugins: [imports],
} satisfies BuildOptions;
