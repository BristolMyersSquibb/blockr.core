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
