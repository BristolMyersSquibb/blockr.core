// Installs into core's node_modules the npm package `@blockr/ui`, which the R
// package blockr.ui ships in its inst/types: blockr.ui's components, the
// helpers of `@blockr/ui/stubs` for tests, Lucide's icons, and the build preset
// `@blockr/ui/build` with its stylesheet `@blockr/ui/preset.css`.
// Core's code draws with the components, its tests act on them through the
// helpers, and its build extends blockr.ui's preset
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-build-preset).
// Run: node srcts/build/ui-types.ts
// The scripts of package.json run it before each script that reads the package,
// as npm runs a `pre` script before the script of the same name.
//
// A package that imports core installs `@blockr/ui` with core's own types,
// through blockr.core::install_js_types(), which follows the R packages that
// core imports.
// Core cannot call that function on itself, since CI installs the R packages
// that core imports, but not core.
// So core asks R where the installed blockr.ui keeps its types, and installs
// them as that function does: with one call of npm, as a copy rather than a
// link into the R library, and running no install script.
// The flag --no-save leaves package.json and package-lock.json as they are.
// So the next npm ci or npm install removes the copy again, and the script
// installs it afresh each time, from the blockr.ui that R has installed then.
// The copy names React and React Aria's three packages as its peers, at the
// versions that blockr.ui's files hold, which core's package.json pins.
// It names Lucide's package and @internationalized/date as its dependencies,
// which npm installs with it.
// The file imports no npm package, only Node's own modules, as the build
// preset does.

import { execFileSync } from "node:child_process";
import { existsSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

// Core's repo, whose node_modules gets the copy.
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The directory in which the installed R package blockr.ui keeps its types,
// as R reports it.
// It fails where R finds no blockr.ui, or a blockr.ui whose types directory
// holds no package.json, as a blockr.ui from before its client does.
export function uiTypesDir(): string {
  const dir = execFileSync("Rscript", ["-e", 'cat(system.file("types", package = "blockr.ui"))'], {
    cwd: repo,
    encoding: "utf8",
  }).trim();
  if (dir === "" || !existsSync(join(dir, "package.json"))) {
    throw new Error(
      "R finds no types of blockr.ui, which core's code imports as @blockr/ui. Install the R package blockr.ui at the version that core's DESCRIPTION pins in Remotes.",
    );
  }
  return dir;
}

// Installs `@blockr/ui` from the directory given into core's node_modules,
// after removing the copy that an earlier call made.
// Otherwise npm would keep the old copy, even once blockr.ui changes.
export function installUiTypes(dir: string): void {
  rmSync(join(repo, "node_modules", "@blockr", "ui"), { recursive: true, force: true });
  execFileSync(
    "npm",
    ["install", "--no-save", "--install-links", "--ignore-scripts", "--no-audit", "--no-fund", dir],
    { cwd: repo, stdio: "inherit" },
  );
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  installUiTypes(uiTypesDir());
}
