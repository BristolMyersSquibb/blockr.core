// @vitest-environment node

// What the files of core that every page loads import up front, as core's
// build lists it in inst/js/preload.json: core's runtime, core's React SDK and
// the module of each of core's board options, which core imports as every page
// starts, to register the board option.
// None of them imports blockr.ui's components or one of React Aria's packages
// up front, directly or through another of core's files. So a page whose UIs
// bind their values with core's hooks, but draw neither with blockr.ui's
// components nor with React Aria's, loads no React Aria
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).
// Core's runtime imports neither entry of core's React SDK up front either,
// and loads each UI that it draws as it first draws it.
// The test reads the files that the build wrote, which git keeps, and CI
// checks that a rebuild changes nothing in them.

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import { importFile } from "../import-map/index.ts";
import { optionSchemas } from "../options/index.ts";
import { optionModuleName } from "./build.ts";

const out = resolve(dirname(fileURLToPath(import.meta.url)), "../../inst/js");

function readJson(file: string): unknown {
  return JSON.parse(readFileSync(resolve(out, file), "utf8"));
}

const preload = readJson("preload.json") as Readonly<Record<string, readonly string[]>>;
const map = (readJson("import-map.json") as { imports: Readonly<Record<string, string>> }).imports;

// The imports of blockr.ui's components and of React Aria's three packages,
// which the page's import map resolves to blockr.ui's files.
const KIT = ["@blockr/ui", "react-aria-components", "react-aria", "react-stately"];

// What the file imports up front, directly or through another of core's files,
// each by its name, as `react`, or by its path beside the file, as
// `./vendor-3f2a9c1d.js`.
// An import that core's import map names leads to core's file for it, and one
// by a path to the file of that path. Any other, as of React or blockr.ui,
// leads to a file of blockr.ui's, which the test follows no further.
function upFront(file: string): Set<string> {
  const seen = new Set<string>();
  const queue = [file];
  for (let next = queue.shift(); next !== undefined; next = queue.shift()) {
    for (const name of preload[next] ?? []) {
      if (seen.has(name)) continue;
      seen.add(name);
      const imported = name.startsWith("./") ? name.slice(2) : map[name];
      if (imported !== undefined) queue.push(imported);
    }
  }
  return seen;
}

const OPTIONS = Object.keys(optionSchemas).map((id) => `${optionModuleName(id)}.js`);

test.each([["blockr-core.js"], [importFile("@blockr/core/react")], ...OPTIONS.map((file) => [file])])(
  "%s imports neither blockr.ui's components nor React Aria up front, directly or through another file",
  (file) => {
    expect(readFileSync(resolve(out, file), "utf8")).not.toBe("");
    expect([...upFront(file)].filter((name) => KIT.includes(name))).toEqual([]);
  },
);

test("core's runtime imports neither entry of core's React SDK up front, since it loads each UI that it draws as it first draws it", () => {
  const imported = upFront("blockr-core.js");
  expect(["@blockr/core/react", "@blockr/core/components"].filter((name) => imported.has(name))).toEqual([]);
});

test("follows the files that a file imports up front, by name and by path", () => {
  // The SDK imports the binding, and the controls import blockr.ui's
  // components, so the test finds them as it should.
  expect(upFront(importFile("@blockr/core/react"))).toContain("@blockr/core/binding");
  expect(upFront(importFile("@blockr/core/controls"))).toContain("@blockr/ui");
  expect(upFront("blockr-core.js")).toContain("@opentelemetry/api");
});
