// The file browser block's UI, mounted on the stub of its host
// with the fixture that the file browser block's R tests write,
// which records what list_dir answers for the root of the volume "here"
// and for its directory data.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as browser from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("file-browser", "filebrowser_block");

const volumes = { here: "." };
const sales: State = { file_path: "./data/sales.csv", volumes };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(browser, host);
  await settle();
  const entries = (): string[] => texts(view.el, "li button");
  const click = (text: string): void => {
    const found = [...view.el.querySelectorAll("button")].find((button) => button.textContent === text);
    if (found === undefined) throw new Error(`Found no button ${text}.`);
    found.click();
  };
  return { host, view, entries, click };
}

test("starts at the directory of the block's file, and marks the file", async () => {
  const { host, view, entries } = await mounted(sales);

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "data" } }]);
  expect(entries()).toEqual(["..", "sales.csv"]);
  expect(find(view.el, 'li [aria-current="true"]').textContent).toBe("sales.csv");
  expect(view.el.textContent).toContain("Chosen: ./data/sales.csv");
  expect(host.submitted).toEqual([]);
});

test("starts at the root of the first volume for a block with no file, and draws no picker of one volume", async () => {
  const { host, view, entries } = await mounted({ volumes });

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "" } }]);
  expect(entries()).toEqual(["data/", "notes.txt"]);
  expect(view.el.querySelector("select")).toBeNull();
  expect(view.el.textContent).toContain("No file chosen yet.");
});

test("opens the directory the user clicks, and submits the path of the file the user clicks", async () => {
  const { host, entries, click } = await mounted({ volumes });

  await step(() => {
    click("data/");
  });
  await settle();
  expect(entries()).toEqual(["..", "sales.csv"]);

  await step(() => {
    click("sales.csv");
  });
  expect(host.submitted).toEqual([sales]);
});

test("goes up a directory, and back to one the path above the list names", async () => {
  const { view, entries, click } = await mounted(sales);

  await step(() => {
    click("..");
  });
  await settle();
  expect(entries()).toEqual(["data/", "notes.txt"]);
  expect(texts(view.el, "nav button")).toEqual(["here"]);

  await step(() => {
    click("data/");
  });
  await settle();
  expect(texts(view.el, "nav button")).toEqual(["here", "data"]);

  await step(() => {
    click("here");
  });
  await settle();
  expect(entries()).toEqual(["data/", "notes.txt"]);
});

test("gives a volume that is gone up for the first, and says why its listing failed", async () => {
  const { host, view } = await mounted(sales);

  await step(() => {
    host.setState({ volumes: { srv: "/srv" } });
  });
  await settle();

  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "srv", path: "" } });
  expect(find(view.el, '[role="alert"]').textContent).toContain('records no answer of the request "list_dir"');
  expect(host.submitted).toEqual([]);
});

test("reads the volumes and R's listing as they are given, and fails on another shape", () => {
  expect(browser.asVolumes(undefined)).toEqual({});
  expect(browser.asVolumes({ here: "." })).toEqual({ here: "." });
  expect(() => browser.asVolumes({ here: 1 })).toThrow(
    'The state arg volumes is {"here":1}, where the file browser takes the path of each volume by its name.',
  );
  expect(() => browser.asVolumes(["."])).toThrow('The state arg volumes is ["."]');

  expect(browser.asListing(undefined)).toEqual({ dirs: [], files: [] });
  expect(browser.asListing({ dirs: ["data"], files: [] })).toEqual({ dirs: ["data"], files: [] });
  expect(() => browser.asListing({ dirs: "data", files: [] })).toThrow(
    'R answered list_dir with {"dirs":"data","files":[]}, where the file browser takes the dirs and the files that it lists.',
  );
  expect(() => browser.asListing(null)).toThrow("R answered list_dir with null");
});
