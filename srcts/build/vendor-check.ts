// The vendor check, a plugin of esbuild that core ships to packages beside the
// build preset, as `@blockr/core/build`.
// It fails a build that puts code from node_modules into one of the files that
// it writes, naming each npm package and the file.
// A package's own files hold its own code alone, unminified, as the Conventions
// of State in JS ask
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1#conventions).
// The libraries that it bundles go, minified, into its vendor file, such as
// core's vendor-<hash>.js, which its own files import them from.
// So a package adds the check to the builds of its own files, as in
// `esbuild.build({ ...preset, plugins: [...preset.plugins, vendorCheck], ... })`.
// The preset's plugins leave it out, since a package builds its vendor file
// with the preset as well, and the check would fail that build.
// The file imports nothing, as ./preset.ts does, since Node runs it as core's
// build and a package's build load it.

import type { Metafile, Plugin } from "esbuild";

export const vendorCheck: Plugin = {
  name: "blockr.core-vendor-check",
  setup(build) {
    build.initialOptions.metafile = true;
    build.onEnd((result) => ({
      errors: bundledPackages(result.metafile).map(({ name, file }) => ({
        text: `The build put code of ${name}, from node_modules, into ${file}. Bundle ${name} into the package's vendor file instead, and import it from there.`,
      })),
    }));
  },
};

// The npm packages whose code a build put into its files, each with the file,
// by its path as the build gives it.
// An input whose code the build left out, such as one that tree shaking
// dropped, counts for none.
function bundledPackages(metafile: Metafile | undefined): { name: string; file: string }[] {
  return Object.entries(metafile?.outputs ?? {}).flatMap(([file, output]) => {
    const names = new Set(
      Object.entries(output.inputs).flatMap(([input, { bytesInOutput }]) => {
        const name = inputPackage(input);
        return name === undefined || bytesInOutput === 0 ? [] : [name];
      }),
    );
    return [...names].map((name) => ({ name, file }));
  });
}

// The npm package of an input from node_modules, as @preact/signals-core for
// node_modules/@preact/signals-core/dist/signals-core.module.js, or undefined
// for an input from elsewhere.
function inputPackage(input: string): string | undefined {
  const at = input.lastIndexOf("node_modules/");
  if (at === -1) return undefined;
  const [first = "", second = ""] = input.slice(at + "node_modules/".length).split("/");
  return first.startsWith("@") ? `${first}/${second}` : first;
}
