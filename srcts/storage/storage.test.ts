// Core's own storage plugin, whose buttons save a board as a file that the
// browser downloads and open a file that the user picks, on the CoreApi for
// tests, with the browser's download and file picker played by the test.

import { afterEach, describe, expect, test, vi } from "vitest";
import type { StorageControl } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import { picking } from "../testing/inputs.ts";
import { coreStorage, fileName } from "./index.ts";

afterEach(() => {
  vi.restoreAllMocks();
});

// The class of core's own storage plugin, which the app of the fixture hello
// runs.
const FILE_STORAGE = "blockr.core-file_storage_plugin";

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

// The button of the id given among the controls.
function button(controls: readonly StorageControl[], id: string): Extract<StorageControl, { kind: "button" }> {
  const found = controls.find((control) => control.id === id);
  if (found?.kind !== "button") throw new Error(`Found no button ${id}.`);
  return found;
}

describe("the board's file", () => {
  test("is named by the board's id and the local time it was saved, as today's", () => {
    expect(fileName({ id: "brisk_otter" }, new Date(2026, 10, 6, 14, 3, 9))).toBe("brisk_otter_2026-11-06_14-03-09.json");
  });
});

describe("core's own storage plugin", () => {
  test("gives two buttons, Save and Open, and leaves Open out under the load lock alone", async () => {
    const open = await stubCore();
    // Neither button has a tooltip, a shortcut or a hint.
    const given = coreStorage().controls({ core: open.core, plugin: FILE_STORAGE });
    expect(given.map((control) => Object.keys(control))).toEqual([
      ["kind", "id", "label", "run"],
      ["kind", "id", "label", "run"],
    ]);
    expect(given).toMatchObject([
      { kind: "button", id: "save", label: "Save" },
      { kind: "button", id: "open", label: "Open" },
    ]);
    open.stop();

    const ids = async (lock: { board: boolean; load: boolean }): Promise<string[]> => {
      const s = await stubCore({ hello: { ...HELLO, lock } });
      const given = coreStorage().controls({ core: s.core, plugin: FILE_STORAGE }).map((control) => control.id);
      s.stop();
      return given;
    };
    expect(await ids({ board: true, load: false })).toEqual(["save", "open"]);
    expect(await ids({ board: false, load: true })).toEqual(["save"]);
  });

  test("saves the board as a download of the store's text, named by the board's id and the time", async () => {
    const { core, stop } = await stubCore();
    const started = downloads();

    await button(coreStorage().controls({ core, plugin: FILE_STORAGE }), "save").run();

    expect(started.map((download) => download.name)).toEqual([
      expect.stringMatching(/^brisk_otter_\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.json$/),
    ]);
    expect(await started[0]?.blob.text()).toBe(core.store.save());
    expect(started[0]?.blob.type).toBe("application/json");
    stop();
  });

  test("opens the board in the file that the user picks, opens none where the user picks none, and leaves no picker on the page", async () => {
    const s = await stubCore();
    const open = button(coreStorage().controls({ core: s.core, plugin: FILE_STORAGE }), "open");
    const picker = { accept: ".json,application/json", connected: true };

    const cancelled = picking(undefined);
    await open.run();
    expect(cancelled).toEqual([picker]);
    expect(s.core.store.board().name).toBe("Iris review");

    vi.restoreAllMocks();
    const text = JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Opened" } });
    const picked = picking(new File([text], "opened.json", { type: "application/json" }));
    await open.run();
    await s.settled();
    expect(picked).toEqual([picker]);
    // Core starts again for the board, with a CoreApi of its own.
    expect(s.core.store.board().name).toBe("Opened");
    expect(document.querySelectorAll('input[type="file"]')).toHaveLength(0);
    s.stop();
  });

  test("tells the user why it cannot read the file that the user picks, through an error of the code board_open_failed about the plugin, and leaves the board open", async () => {
    const s = await stubCore();
    const file = new File(["{}"], "board.json");
    file.text = () => Promise.reject(new Error("The file could not be read."));
    picking(file);

    await button(coreStorage().controls({ core: s.core, plugin: FILE_STORAGE }), "open").run();
    expect(s.core.notifications.get()).toEqual([
      {
        id: expect.any(String) as string,
        severity: "error",
        message: "The file could not be read.",
        source: { kind: "module", module: { object: { plugin: FILE_STORAGE }, package: "blockr.core" }, code: "board_open_failed" },
        about: { plugin: FILE_STORAGE },
      },
    ]);
    expect(s.core.store.board().name).toBe("Iris review");
    s.stop();
  });

  test("rejects where the store refuses the board in the file that the user picks, which core tells the user of, and leaves the board open", async () => {
    const s = await stubCore();
    picking(new File(["{}"], "board.json"));

    await expect(button(coreStorage().controls({ core: s.core, plugin: FILE_STORAGE }), "open").run()).rejects.toThrow(
      "The store refused to open the board",
    );
    // Core raised the notification, and the plugin raised none.
    expect(s.core.notifications.get()).toEqual([
      {
        id: expect.any(String) as string,
        severity: "error",
        message: expect.stringContaining("The store refused to open the board") as string,
        source: { kind: "core", code: "board_open_failed" },
      },
    ]);
    expect(s.core.store.board().name).toBe("Iris review");
    s.stop();
  });
});
