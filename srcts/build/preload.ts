// What each file that a build writes imports up front, which core ships to
// packages beside the build preset, as `@blockr/core/build`.
// A build writes it into preload.json beside its files.
// R reads that file to name in the head of the page that it serves what the
// page imports as it starts, beside the modules that it names itself
// (R/served-modules.R).
// A browser fetches a file that such a link names at once, as the page loads.
// It fetches what that file imports only as it runs the file, so R names those
// files too.
// The list holds, for each file that imports others up front, each of them:
// an import by name, which the page's import map resolves, as `react`, and a
// file beside it, by its path from the file, as `./vendor-3f2a9c1d.js`.
// A stylesheet's `@import` of a file beside it counts as well.
// A dynamic import is left out, since the page loads it only as it runs the
// import, as core's runtime loads its OpenTelemetry module.
// So a package writes the file from the metafiles of its builds, as in
// `writeFile("inst/js/preload.json", JSON.stringify(staticImports(results.map((r) => r.metafile))))`.
// The file imports nothing, as ./preset.ts does, since Node runs it as core's
// build and a package's build load it.

import type { Metafile } from "esbuild";

// The static imports of each file that the builds of the metafiles given
// wrote, by the file's name, of the files that import any.
export function staticImports(metafiles: readonly (Metafile | undefined)[]): Record<string, string[]> {
  const files: [string, string[]][] = [];
  for (const metafile of metafiles) {
    for (const [path, output] of Object.entries(metafile?.outputs ?? {})) {
      const imports = output.imports.flatMap((entry) =>
        entry.external === true && (entry.kind === "import-statement" || entry.kind === "import-rule") ? [entry.path] : [],
      );
      if (imports.length > 0) files.push([path.slice(path.lastIndexOf("/") + 1), [...new Set(imports)]]);
    }
  }
  return Object.fromEntries(files.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
}
