// Core's plain renderer of plot outputs, mounted with the stub of its host on
// the sample plot outputs that R writes for the class, each with the PNG of
// its body.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { mount, stubOutputHost } from "../../stubs/index.ts";
import type { OutputFixtures, PlotOutput } from "../../types/outputs.ts";
import * as basicPlot from "./index.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const dir = join(root, "inst", "outputs", "fixtures");
const fixtures = JSON.parse(readFileSync(join(dir, "plot-output.json"), "utf8")) as OutputFixtures<PlotOutput>;

const [scatter] = fixtures.outputs;
if (scatter?.body_file === undefined) throw new Error("The scatter plot's sample has no body.");
const png = new Blob([readFileSync(join(dir, scatter.body_file))], { type: scatter.body?.type ?? "" });

// The URLs made for the bodies a test shows, and those revoked since.
let made: Blob[];
let revoked: string[];

beforeEach(() => {
  made = [];
  revoked = [];
  vi.spyOn(URL, "createObjectURL").mockImplementation((body) => {
    made.push(body as Blob);
    return `blob:plot-${String(made.length)}`;
  });
  vi.spyOn(URL, "revokeObjectURL").mockImplementation((url) => {
    revoked.push(url);
  });
});

afterEach(() => {
  vi.restoreAllMocks();
});

test("shows the PNG of the body at the size the plot was drawn at", async () => {
  const { el } = await mount(basicPlot, stubOutputHost({ output: { ...scatter.output }, body: png }));
  const img = el.querySelector("img");

  expect(made).toEqual([png]);
  expect(img?.getAttribute("src")).toBe("blob:plot-1");
  expect([img?.width, img?.height]).toEqual([320, 200]);
  expect(img?.alt).toBe("Plot");
  expect(img?.hidden).toBe(false);
  expect(basicPlot.api).toBe(1);
  expect(basicPlot.default.options).toBeUndefined();
});

test("shows the body of each output that core hands it, and lets the URL of the one before go", async () => {
  const host = stubOutputHost({ output: { ...scatter.output }, body: png });
  const mounted = await mount(basicPlot, host);
  const { el } = mounted;
  const img = el.querySelector("img");

  host.setLoading(true);
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("true");
  expect(made).toHaveLength(1);

  const wider = new Blob([png], { type: "image/png" });
  host.setOutput({ ...scatter.output, width: 640, height: 400 }, wider);
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("false");
  expect(img?.getAttribute("src")).toBe("blob:plot-2");
  expect([img?.width, img?.height]).toEqual([640, 400]);
  expect(revoked).toEqual(["blob:plot-1"]);

  host.setError({ severity: "error", class: ["simpleError", "error", "condition"], phase: "output", message: "boom" });
  const alert = el.querySelector<HTMLElement>('[role="alert"]');
  expect(alert?.hidden).toBe(false);
  expect(alert?.textContent).toBe("boom");
  expect(img?.getAttribute("src")).toBe("blob:plot-2");

  mounted.unmount();
  expect(revoked).toEqual(["blob:plot-1", "blob:plot-2"]);
});

test("shows no image for an output that holds no body", async () => {
  const { el } = await mount(basicPlot, stubOutputHost({ output: { ...scatter.output } }));
  const img = el.querySelector("img");
  expect(img?.hidden).toBe(true);
  expect(img?.hasAttribute("src")).toBe(false);
  expect(made).toEqual([]);
});
