// The file browser block's UI, mounted on the stub of its host
// with the fixture that the file browser block's R tests write,
// which records what list_dir answers for the root of the volume "here"
// and for its directory data.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, pressAndRelease, texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import { rowOf, rows, rowTexts } from "../../testing/rows.ts";
import { labelled, picked } from "../../testing/select.ts";
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
  const entries = (): string[] => rowTexts(view.el);
  // Clicks an entry of the list, or a directory above it.
  const click = (text: string): void => {
    const found = [...view.el.querySelectorAll<HTMLElement>('button, [role="row"][data-key]')].find((node) => node.textContent === text);
    if (found === undefined) throw new Error(`Found no button or row ${text}.`);
    found.click();
  };
  return { host, view, entries, click };
}

test("starts at the directory of the block's file, and marks the file", async () => {
  const { host, view, entries } = await mounted(sales);

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "data" } }]);
  expect(entries()).toEqual(["..", "sales.csv"]);
  expect(find(view.el, '[role="row"][data-current]').textContent).toBe("sales.csv");
  expect(view.el.textContent).toContain("Chosen: ./data/sales.csv");
  expect(host.submitted).toEqual([]);
});

test("starts at the root of the first volume for a block with no file, and draws no picker of one volume", async () => {
  const { host, view, entries } = await mounted({ volumes });

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "" } }]);
  expect(entries()).toEqual(["data/", "notes.txt"]);
  expect(view.el.querySelector('[role="combobox"]')).toBeNull();
  expect(texts(view.el, "p")).toEqual([]);
});

test("draws a picker of the volumes where there are several, and lists the root of the volume the user picks", async () => {
  const { host, view } = await mounted({ volumes: { here: ".", srv: "/srv" } });
  const volume = labelled(view.el, "Volume");
  expect(volume.value).toBe("here");

  await picked(volume, step, "srv");
  await settle();

  expect(volume.value).toBe("srv");
  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "srv", path: "" } });
  expect(host.submitted).toEqual([]);
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

test("moves through the entries by the arrow keys, and opens the one the user presses Enter on", async () => {
  const { host, view, entries } = await mounted({ volumes });

  await step(() => {
    rowOf(view.el, "data/").focus();
  });
  await step(() => {
    pressAndRelease(rowOf(view.el, "data/"), "ArrowDown");
  });
  expect(document.activeElement).toBe(rowOf(view.el, "notes.txt"));
  await step(() => {
    pressAndRelease(rowOf(view.el, "notes.txt"), "ArrowUp");
  });
  await step(() => {
    pressAndRelease(rowOf(view.el, "data/"), "Enter");
  });
  await settle();

  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "here", path: "data" } });
  expect(entries()).toEqual(["..", "sales.csv"]);
  await step(() => {
    pressAndRelease(rowOf(view.el, "sales.csv"), "Enter");
  });
  expect(host.submitted).toEqual([sales]);
});

test("says that a directory is empty where it holds no entry, and shows the directory above alone in one below the root", async () => {
  // R lists every directory as empty.
  const empty = async (state: State) => {
    const host = stubBlockHost<State>(fixture, { state });
    host.request = () => Promise.resolve({ dirs: [], files: [] }) as never;
    const view = await mountOnPage(browser, host);
    await settle();
    return view.el;
  };

  const root = await empty({ volumes });
  expect(rowTexts(root)).toEqual([]);
  expect(find(root, '[role="grid"]').textContent).toBe("This directory is empty.");

  const below = await empty(sales);
  expect(rowTexts(below)).toEqual([".."]);
  expect(below.textContent).not.toContain("This directory is empty.");
});

test("says that it is loading while R lists a place, and lists the place once R answers", async () => {
  const host = stubBlockHost<State>(fixture, { state: { volumes } });
  const release = holdRequests(host);
  const view = await mountOnPage(browser, host);
  const entries = (): string[] => rowTexts(view.el);
  expect([entries(), view.el.textContent.includes("Loading…")]).toEqual([[], true]);

  release();
  await settle();
  expect([entries(), view.el.textContent.includes("Loading…")]).toEqual([["data/", "notes.txt"], false]);

  await step(() => {
    rows(view.el)[0]?.click();
  });
  expect([entries(), view.el.textContent.includes("Loading…")]).toEqual([[], true]);

  release();
  await settle();
  expect(entries()).toEqual(["..", "sales.csv"]);
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

test("gives a volume that is gone up for the first, and lists nothing where its listing fails, leaving why to core's notification of R's error reply", async () => {
  const { host, view } = await mounted(sales);

  await step(() => {
    host.setState({ volumes: { srv: "/srv" } });
  });
  await settle();

  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "srv", path: "" } });
  expect(view.el.querySelector('[role="alert"]')).toBeNull();
  expect(view.el.textContent).not.toContain("This directory is empty.");
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
