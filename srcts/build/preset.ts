// The build preset, with which core's build bundles core's modules, and which
// core ships to packages as `@blockr/core/build`, so that a package that
// builds on core bundles its modules alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).
// It extends the preset of blockr.ui, `@blockr/ui/build`, with core's own
// entry points.
// The preset of blockr.ui holds esbuild's options for a module, an ES module
// for the browser, whose imports of React, of blockr.ui's components and of
// React Aria's three packages stay imports, and which bundles the icons of
// `@blockr/ui/icons` that the module imports.
// It fails a build that imports an entry point of React or a part of React
// Aria that the page's import map does not name, or one of Lucide's packages.
// This preset leaves the imports of core's React SDK, its components, its
// binding, its controls and its OpenTelemetry module, and of OpenTelemetry's
// API, imports as well.
// The page that R serves resolves all of them through its one import map.
// So the module's bundle holds none of them.
// A package's build adds its entry points and where they go, as in
// `esbuild.build({ ...preset, entryPoints: ["srcts/head-block.tsx"], outdir: "inst/js" })`.
// To the builds of its own files, it adds the vendor check of
// `@blockr/ui/build` as well, which the preset's plugins leave out.
// The file imports nothing but the preset of blockr.ui and the imports that
// core's import map names, since Node runs it as core's build and a package's
// build load it.

import type { BuildOptions, Plugin } from "esbuild";
import { preset as ui } from "@blockr/ui/build";
import { IMPORTS, OTEL_API } from "../import-map/index.ts";

// The imports that core's import map names, each by its whole name.
const CORE = new RegExp(`^(${IMPORTS.map((name) => name.replace(/[/.]/g, String.raw`\$&`)).join("|")})$`);

// Leaves each import in a module that core's import map names an import, so
// that every module imports the one copy of each that blockr.core ships.
// OpenTelemetry's API has no side effects, as its package says, so a module
// that imports none of its names, as from code of core's that it bundles,
// keeps no import of it.
const imports: Plugin = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: CORE }, (args) => ({
      path: args.path,
      external: true,
      ...(args.path === OTEL_API ? { sideEffects: false } : {}),
    }));
  },
};

export const preset = {
  ...ui,
  plugins: [imports, ...ui.plugins],
} satisfies BuildOptions;
