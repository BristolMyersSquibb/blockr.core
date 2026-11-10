// Core's own storage plugin, which saves a board as a file that the browser
// downloads and opens a file that the user picks, with the browser's download
// and file picker played by the test.

import { afterEach, describe, expect, test, vi } from "vitest";
import { picking } from "../testing/inputs.ts";
import { coreStorage, fileName } from "./index.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

// The board's file as the store writes it, of a board whose id is given.
function text(id: string): string {
  return `${JSON.stringify({ format: 1, packages: {}, board: { id, name: "Cars" } }, null, 2)}\n`;
}

// The downloads that the browser starts, by their name, with what each holds.
function downloads() {
  const started: { name: string; blob: Blob }[] = [];
  const blobs: Blob[] = [];
  vi.spyOn(URL, "createObjectURL").mockImplementation((blob) => {
    blobs.push(blob as Blob);
    return `blob:${String(blobs.length)}`;
  });
  vi.spyOn(URL, "revokeObjectURL").mockImplementation(() => undefined);
  vi.spyOn(HTMLAnchorElement.prototype, "click").mockImplementation(function (this: HTMLAnchorElement) {
    const blob = blobs[Number(this.href.replace(/^blob:/, "")) - 1];
    if (blob !== undefined) started.push({ name: this.download, blob });
  });
  return started;
}

describe("the board's file", () => {
  test("is named by the board's id and the local time it was saved, as today's", () => {
    expect(fileName({ id: "brisk_otter" }, new Date(2026, 10, 6, 14, 3, 9))).toBe("brisk_otter_2026-11-06_14-03-09.json");
  });
});

describe("core's own storage plugin", () => {
  test("saves the board's text as a download, named by the board's id, or as a board's where the text holds none", async () => {
    const started = downloads();
    const storage = coreStorage();

    await storage.save(text("brisk_otter"));
    await storage.save("not a board");

    expect(started.map((download) => download.name)).toEqual([
      expect.stringMatching(/^brisk_otter_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.json$/),
      expect.stringMatching(/^board_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.json$/),
    ]);
    expect(await started[0]?.blob.text()).toBe(text("brisk_otter"));
    expect(started[0]?.blob.type).toBe("application/json");
  });

  test("opens the text of the file that the user picks, or null where the user picks none, and leaves no picker on the page", async () => {
    const storage = coreStorage();
    const picker = { accept: ".json,application/json", connected: true };

    const picked = picking(new File([text("amber_tapir")], "amber_tapir.json", { type: "application/json" }));
    await expect(storage.open()).resolves.toBe(text("amber_tapir"));
    expect(picked).toEqual([picker]);

    vi.restoreAllMocks();
    const cancelled = picking(undefined);
    await expect(storage.open()).resolves.toBeNull();
    expect(cancelled).toEqual([picker]);
    expect(document.querySelectorAll('input[type="file"]')).toHaveLength(0);
  });
});
