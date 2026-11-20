// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package's build
// bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react).
// It holds esbuild's options for a module: an ES module for the browser, whose
// imports of React, of core's React SDK, its colours, its binding and its
// controls, and of OpenTelemetry's API stay imports, which the page that R
// serves resolves through its import map.
// So the module's bundle holds none of them.
// It fails a build that imports React Aria, whose components a module draws
// from the React SDK, its colours included.
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

// Leaves each import in a module that the page's import map names an import,
// so that every module imports the one copy of each that blockr.core ships.
// An entry point of React that the map does not name fails the build, rather
// than bundling a second copy of React, and so does an entry point of the SDK
// that the map does not name, such as a part of the SDK that this core lacks.
// OpenTelemetry's API has no side effects, as its package says, so a module
// that imports none of its names, as from code of core's that it bundles,
// keeps no import of it.
// An import of React Aria fails the build as well, with an error that names
// the React SDK (REACT_ARIA).
const imports: Plugin = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react(\/[^/]+)?|binding|controls|otel)|@opentelemetry\/api)$/ }, (args) => {
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
          text: `A module imports ${args.path}, which is part of React Aria. A package draws React Aria's components from @blockr/core/react, and its colours from @blockr/core/react/colors, so that the page runs one copy of React Aria.`,
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
