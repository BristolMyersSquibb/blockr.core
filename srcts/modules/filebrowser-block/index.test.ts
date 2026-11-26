// The file browser block's UI, mounted on the stub of its host
// with the fixture that the file browser block's R tests write,
// which records what list_dir answers for the root of the volume "here"
// and for its directory data.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { find, listed, picked, pressAndRelease, rows, selectButton, selectShows, step, texts } from "@blockr/ui/stubs";
import { stubBlockHost } from "../../stubs/index.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { holdRequests } from "../../testing/requests.ts";
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
});

// The breadcrumbs of the directory in the bar above the list, each with its
// text and whether it is the current directory, as React Aria marks it.
function crumbs(el: HTMLElement): (string | null)[][] {
  return [...el.querySelectorAll('nav [role="link"]')].map((link) => [link.textContent, link.getAttribute("aria-current")]);
}

// The names of the entries that the list shows, in order.
function names(el: ParentNode): string[] {
  return rows(el).map((row) => row.querySelector('[role="rowheader"]')?.textContent ?? "");
}

// The row of the entry of the name given.
function entry(el: ParentNode, name: string): HTMLElement {
  const found = rows(el).find((row) => row.querySelector('[role="rowheader"]')?.textContent === name);
  if (found === undefined) throw new Error(`Found no entry ${name}.`);
  return found;
}

// The button that goes up a directory.
function up(el: ParentNode): HTMLButtonElement {
  return find(el, 'button[aria-label="Up one directory"]', HTMLButtonElement);
}

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(browser, host);
  await settle();
  const entries = (): string[] => names(view.el);
  // Clicks an entry of the list, or a directory of the path above it.
  const click = (text: string): void => {
    const crumb = [...view.el.querySelectorAll<HTMLElement>('nav [role="link"]')].find((node) => node.textContent === text);
    (crumb ?? entry(view.el, text)).click();
  };
  return { host, view, entries, click };
}

test("starts at the directory of the block's file, and marks the file", async () => {
  const { host, view, entries } = await mounted(sales);

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "data" } }]);
  expect(entries()).toEqual(["sales.csv"]);
  expect(find(view.el, '[role="row"][data-current]').querySelector('[role="rowheader"]')?.textContent).toBe("sales.csv");
  expect(crumbs(view.el)).toEqual([
    ["here", null],
    ["data", "page"],
  ]);
  expect(up(view.el).disabled).toBe(false);
  expect(view.el.textContent).toContain("Chosen: ./data/sales.csv");
  expect(host.submitted).toEqual([]);
});

test("starts at the root of the first volume for a block with no file, from which it goes up no further, and draws no picker of one volume", async () => {
  const { host, view, entries } = await mounted({ volumes });

  expect(host.requested).toEqual([{ name: "list_dir", args: { volume: "here", path: "" } }]);
  expect(entries()).toEqual(["data", "notes.txt"]);
  expect(up(view.el).disabled).toBe(true);
  expect(view.el.querySelector('[role="combobox"]')).toBeNull();
  expect(texts(view.el, "p")).toEqual([]);
});

test("lists the directories first, then the files, each with its size where it is a file and the time it changed last", async () => {
  const { view } = await mounted({ volumes });

  expect(texts(view.el, '[role="columnheader"]')).toEqual(["Name", "Size", "Modified"]);
  const cells = rows(view.el).map((row) => texts(row, '[role="rowheader"], [role="gridcell"]'));
  expect(cells).toEqual([
    ["data", "—", browser.modifiedText("2026-11-02T09:14:00Z")],
    ["notes.txt", "6 B", browser.modifiedText("2026-11-19T16:51:00Z")],
  ]);
  // A directory shows its icon in the accent colour, a file in the muted one.
  expect(entry(view.el, "data").querySelector("svg")?.getAttribute("class")).toContain("bc:text-primary");
  expect(entry(view.el, "notes.txt").querySelector("svg")?.getAttribute("class")).toContain("bc:text-muted-foreground");
});

test("draws a picker of the volumes where there are several, and lists the root of the volume the user picks", async () => {
  const { host, view } = await mounted({ volumes: { here: ".", srv: "/srv" } });
  const volume = selectButton(view.el, "Volume");
  expect(selectShows(volume)).toBe("here");
  expect(await listed(volume, step)).toEqual(["here", "srv"]);

  await picked(volume, step, "srv");
  await settle();

  expect(selectShows(volume)).toBe("srv");
  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "srv", path: "" } });
  expect(host.submitted).toEqual([]);
});

test("opens the directory the user clicks, and submits the path of the file the user clicks", async () => {
  const { host, entries, click } = await mounted({ volumes });

  await step(() => {
    click("data");
  });
  await settle();
  expect(entries()).toEqual(["sales.csv"]);

  await step(() => {
    click("sales.csv");
  });
  expect(host.submitted).toEqual([sales]);
});

test("moves through the entries by the arrow keys, and opens the one the user presses Enter on", async () => {
  const { host, view, entries } = await mounted({ volumes });

  await step(() => {
    entry(view.el, "data").focus();
  });
  await step(() => {
    pressAndRelease(entry(view.el, "data"), "ArrowDown");
  });
  expect(document.activeElement).toBe(entry(view.el, "notes.txt"));
  await step(() => {
    pressAndRelease(entry(view.el, "notes.txt"), "ArrowUp");
  });
  await step(() => {
    pressAndRelease(entry(view.el, "data"), "Enter");
  });
  await settle();

  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "here", path: "data" } });
  expect(entries()).toEqual(["sales.csv"]);
  await step(() => {
    pressAndRelease(entry(view.el, "sales.csv"), "Enter");
  });
  expect(host.submitted).toEqual([sales]);
});

test("says that a directory is empty where it holds no entry, at the root of a volume and below it", async () => {
  // R lists every directory as empty.
  const empty = async (state: State) => {
    const host = stubBlockHost<State>(fixture, { state });
    host.request = () => Promise.resolve({ dirs: [], files: [] }) as never;
    const view = await mountOnPage(browser, host);
    await settle();
    return view.el;
  };

  for (const el of [await empty({ volumes }), await empty(sales)]) {
    expect(names(el)).toEqual([]);
    expect(find(el, "tbody").textContent).toBe("This directory is empty.");
  }
});

test("says that it is loading while R lists a place, and lists the place once R answers", async () => {
  const host = stubBlockHost<State>(fixture, { state: { volumes } });
  const release = holdRequests(host);
  const view = await mountOnPage(browser, host);
  const entries = (): string[] => names(view.el);
  expect([entries(), find(view.el, "tbody").textContent]).toEqual([[], "Loading…"]);

  release();
  await settle();
  expect([entries(), view.el.textContent.includes("Loading…")]).toEqual([["data", "notes.txt"], false]);

  await step(() => {
    entry(view.el, "data").click();
  });
  expect([entries(), find(view.el, "tbody").textContent]).toEqual([[], "Loading…"]);

  release();
  await settle();
  expect(entries()).toEqual(["sales.csv"]);
});

test("goes up a directory by the button, and back to one that the path above the list names", async () => {
  const { view, entries, click } = await mounted(sales);

  await step(() => {
    up(view.el).click();
  });
  await settle();
  expect(entries()).toEqual(["data", "notes.txt"]);
  expect(crumbs(view.el)).toEqual([["here", "page"]]);

  await step(() => {
    click("data");
  });
  await settle();
  expect(crumbs(view.el)).toEqual([
    ["here", null],
    ["data", "page"],
  ]);

  await step(() => {
    click("here");
  });
  await settle();
  expect(entries()).toEqual(["data", "notes.txt"]);
});

test("gives a volume that is gone up for the first, and lists nothing where its listing fails, leaving why to core's notification of R's error reply", async () => {
  const { host, view } = await mounted(sales);

  await step(() => {
    host.setState({ volumes: { srv: "/srv" } });
  });
  await settle();

  expect(host.requested.at(-1)).toEqual({ name: "list_dir", args: { volume: "srv", path: "" } });
  expect(view.el.querySelector('[role="alert"]')).toBeNull();
  expect(names(view.el)).toEqual([]);
  expect(view.el.textContent).not.toContain("This directory is empty.");
  expect(host.submitted).toEqual([]);
});

test("shows a size in bytes below a kilobyte, and otherwise in the largest unit that keeps it at 1 or more", () => {
  expect([0, 6, 1023].map((bytes) => browser.sizeText(bytes))).toEqual(["0 B", "6 B", "1023 B"]);
  expect([1024, 12_700, 5 * 1024 ** 2, 3.5 * 1024 ** 3].map((bytes) => browser.sizeText(bytes))).toEqual([
    "1.0 KB",
    "12.4 KB",
    "5.0 MB",
    "3.5 GB",
  ]);
});

test("shows the time an entry changed last in the browser's time zone, to the minute", () => {
  // The time of 08:03 on 20 November 2026 in the time zone that the test runs
  // in, as R sends it, in UTC.
  const at = new Date(2026, 10, 20, 8, 3, 30).toISOString();
  expect(browser.modifiedText(at)).toBe("2026-11-20 08:03");
  expect(browser.modifiedText(null)).toBe("");
  expect(browser.modifiedText("yesterday")).toBe("yesterday");
});

test("reads the volumes and R's listing as they are given, and fails on another shape", () => {
  expect(browser.asVolumes(undefined)).toEqual({});
  expect(browser.asVolumes({ here: "." })).toEqual({ here: "." });
  expect(() => browser.asVolumes({ here: 1 })).toThrow(
    'The state arg volumes is {"here":1}, where the file browser takes the path of each volume by its name.',
  );
  expect(() => browser.asVolumes(["."])).toThrow('The state arg volumes is ["."]');

  expect(browser.asListing(undefined)).toEqual({ dirs: [], files: [] });
  const listing = {
    dirs: [{ name: "data", modified: "2026-11-02T09:14:00Z" }],
    files: [{ name: "broken", size: null, modified: null }],
  };
  expect(browser.asListing(listing)).toEqual(listing);
  expect(() => browser.asListing({ dirs: ["data"], files: [] })).toThrow(
    'R answered list_dir with {"dirs":["data"],"files":[]}, where the file browser takes the dirs and the files that it lists, each with its name and the time it changed last, and each file with its size.',
  );
  expect(() => browser.asListing({ dirs: [], files: [{ name: "a", modified: null }] })).toThrow("R answered list_dir with");
  expect(() => browser.asListing(null)).toThrow("R answered list_dir with null");
});
