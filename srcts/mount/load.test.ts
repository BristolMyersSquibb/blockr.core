// Loading modules once per page, with the stylesheets beside them. The page
// of these tests fetches no stylesheet: happy-dom reports each as loaded, or
// as failed where a test says so.

import { afterEach, beforeEach, expect, test } from "vitest";
import { reports } from "../testing/reports.ts";
import { createLoader, ModuleError } from "./index.ts";

interface HappyDom {
  happyDOM: { settings: { handleDisabledFileLoadingAsSuccess: boolean } };
}

function stylesheetsLoad(load: boolean): void {
  (window as unknown as HappyDom).happyDOM.settings.handleDisabledFileLoadingAsSuccess = load;
}

beforeEach(() => {
  stylesheetsLoad(true);
});

afterEach(() => {
  for (const link of document.head.querySelectorAll("link")) link.remove();
});

const head = { module: "blockr.core/head-block.js?v=1", css: "blockr.core/head-block.css?v=2" };

test("puts a module's stylesheet on the page once", async () => {
  const loader = createLoader({ importer: () => Promise.resolve({ api: 1 }) });

  await expect(loader.load(head, "blockr.core")).resolves.toEqual({ api: 1 });
  await loader.load(head, "blockr.core");

  const links = [...document.head.querySelectorAll("link[rel=stylesheet]")];
  expect(links.map((link) => link.getAttribute("href"))).toEqual(["blockr.core/head-block.css?v=2"]);
});

test("reports a stylesheet that fails to load, and loads the module regardless", async () => {
  stylesheetsLoad(false);
  const reported = reports();
  const loader = createLoader({ importer: () => Promise.resolve({ api: 1 }) });

  await expect(loader.load(head, "blockr.core")).resolves.toEqual({ api: 1 });
  reported.restore();

  const errors = reported.errors();
  expect(errors).toHaveLength(1);
  expect(errors[0]).toBeInstanceOf(ModuleError);
  expect(String(errors[0])).toBe(
    "ModuleError: The module blockr.core/head-block.js of blockr.core has a stylesheet that failed to load: blockr.core/head-block.css?v=2 could not be loaded.",
  );
});

test("imports a URL once, and gives every later load of it what the first got, a failure included", async () => {
  const urls: string[] = [];
  const loader = createLoader({
    importer: (url) => {
      urls.push(url);
      return url.endsWith("ok.js") ? Promise.resolve({ api: 1 }) : Promise.reject(new Error("404"));
    },
  });

  const ok = await loader.load({ module: "pkg/ok.js" }, "pkg");
  expect(await loader.load({ module: "pkg/ok.js" }, "pkg")).toBe(ok);
  for (let i = 0; i < 2; i++) {
    await expect(loader.load({ module: "pkg/gone.js" }, null)).rejects.toThrow(
      "The module pkg/gone.js from no package failed to load: 404",
    );
  }
  expect(urls).toEqual(["pkg/ok.js", "pkg/gone.js"]);
});

test("imports no module of a package that R runs at another version than the page loaded, but those it imported already", async () => {
  const urls: string[] = [];
  const versions = new Map<string, { loaded: string; now: string }>();
  const loader = createLoader({
    importer: (url) => {
      urls.push(url);
      return Promise.resolve({ url });
    },
    changed: (pkg) => versions.get(pkg),
  });
  const filter = { module: "blockr.test/filter.js?v=1", css: "blockr.test/filter.css?v=1" };
  const loaded = await loader.load(filter, "blockr.test");

  versions.set("blockr.test", { loaded: "1.0.0", now: "1.1.0" });
  expect(await loader.load(filter, "blockr.test")).toBe(loaded);
  for (const source of [{ module: "blockr.test/filter.js?v=2", css: "blockr.test/filter.css?v=2" }, { module: "blockr.test/arrange.js?v=1" }]) {
    const refused = loader.load(source, "blockr.test");
    await expect(refused).rejects.toBeInstanceOf(ModuleError);
    await expect(refused).rejects.toThrow(
      `The module ${source.module.replace(/\?.*$/, "")} of blockr.test is not loaded: R now runs blockr.test 1.1.0, where the page loaded 1.0.0. Reload the page to load it.`,
    );
  }
  await loader.load(head, "blockr.core");

  expect(urls).toEqual(["blockr.test/filter.js?v=1", "blockr.core/head-block.js?v=1"]);
  const links = [...document.head.querySelectorAll("link[rel=stylesheet]")];
  expect(links.map((link) => link.getAttribute("href"))).toEqual(["blockr.test/filter.css?v=1", "blockr.core/head-block.css?v=2"]);
});
