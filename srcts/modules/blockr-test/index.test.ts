// The object through which end-to-end tests read the board, each block's
// status, which block has focus, what is on screen and the notifications, and
// through which they have the frontend show a block or a plugin.
// It runs on the CoreApi for tests, with R played from the protocol's
// fixtures, and on the page that core starts, as the script that R writes for
// the page in Shiny's test mode reads it.

import { describe, expect, test } from "vitest";
import { startPage } from "../../page/index.ts";
import type { Transport } from "../../protocol/index.ts";
import { HELLO, READY, stubCore } from "../../stubs/index.ts";
import type { Notification } from "../../types/core-api.ts";
import { interned } from "../../util/index.ts";
import { blockrTest } from "./index.ts";

// A plugin that the app of the fixture hello runs, which raises the
// notifications of the tests.
const EDIT = interned("blockr.core", "edit_board_plugin");

describe("blockrTest", () => {
  test("gives copies of the open board and of the status of each of its blocks", async () => {
    const s = await stubCore();
    s.core.scheduler.demand("test", ["b"]);
    await s.settled();
    const read = blockrTest(() => s.core);

    // Copies, down to the objects inside, so that nothing that a test does
    // to them reaches the page.
    const board = read.board();
    expect(board).toEqual(s.core.store.board());
    expect(board).not.toBe(s.core.store.board());
    expect(board?.blocks.b).not.toBe(s.core.store.board().blocks.b);

    const statuses = read.statuses();
    expect(Object.keys(statuses)).toEqual(Object.keys(s.core.store.board().blocks));
    expect(statuses.b).toEqual(s.core.scheduler.status("b"));
    expect(statuses.b).not.toBe(s.core.scheduler.status("b"));
    expect(statuses.b?.status).toBe("ready");
    expect(statuses.c?.status).toBe("unevaluated");
    s.stop();
  });

  test("gives the block that has focus, or null for none", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);

    // No block has focus as the board opens.
    expect(read.focus()).toBeNull();

    s.core.focus.set("b");
    expect(read.focus()).toBe("b");

    s.core.focus.set(null);
    expect(read.focus()).toBeNull();
    s.stop();
  });

  test("gives a copy of the blocks and plugins on screen, once the frontend sets them", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);

    // Without an element to draw in, the stub draws no frontend.
    // So nothing has set what is on screen.
    expect(read.onScreen()).toBeNull();

    // As a frontend sets it.
    s.core.onScreen.set({ blocks: ["a", "b"], plugins: [EDIT] });
    const shown = read.onScreen();
    expect(shown).toEqual({ blocks: ["a", "b"], plugins: [EDIT] });
    expect(shown).not.toBe(s.core.onScreen.get());
    expect(shown?.blocks).not.toBe(s.core.onScreen.get()?.blocks);
    s.stop();
  });

  test("gives copies of the notifications that hold, in the order they came", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);
    expect(read.notifications()).toEqual([]);

    const first = s.core.notifications.notify(EDIT, { severity: "warning", code: "first", message: "First." });
    s.core.notifications.notify(EDIT, { severity: "error", code: "second", message: "Second.", about: { block: "b" } });

    const held = read.notifications();
    expect(held).toEqual(s.core.notifications.get());
    expect(held.map((notification) => notification.message)).toEqual(["First.", "Second."]);
    expect(held).not.toBe(s.core.notifications.get());
    expect(held[1]).not.toBe(s.core.notifications.get()[1]);
    expect(held[1]?.about).not.toBe(s.core.notifications.get()[1]?.about);

    // A notification that no longer holds is gone.
    first();
    expect(read.notifications().map((notification) => notification.message)).toEqual(["Second."]);
    s.stop();
  });

  test("has the frontend show a block, which gives the block focus", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);

    await read.show({ block: "b" });
    expect(read.focus()).toBe("b");

    // CoreApi's show() refuses a block that the board does not hold.
    await expect(read.show({ block: "z" })).rejects.toThrow('The board holds no block "z" to show.');
    expect(read.focus()).toBe("b");
    s.stop();
  });

  test("reads the board that opens later, through the CoreApi that core makes for it", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);
    s.core.focus.set("b");
    s.core.onScreen.set({ blocks: ["b"], plugins: [] });
    s.core.notifications.notify(EDIT, { severity: "message", code: "saved", message: "Saved." });

    await s.core.store.open(JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Opened" } }));
    expect(read.board()?.name).toBe("Opened");

    // The board that opens starts with no block in focus and with nothing on
    // screen, until its frontend sets them.
    // The notifications of the board before are gone.
    expect(read.focus()).toBeNull();
    expect(read.onScreen()).toBeNull();
    expect(read.notifications()).toEqual([]);
    s.stop();
  });

  test("gives nothing and shows nothing while core starts", async () => {
    const none = blockrTest(() => undefined);
    expect(none.board()).toBeNull();
    expect(none.statuses()).toEqual({});
    expect(none.focus()).toBeNull();
    expect(none.onScreen()).toBeNull();
    expect(none.notifications()).toEqual([]);
    await expect(none.show({ block: "b" })).rejects.toThrow("No board is open yet");

    // The store holds no board while core starts, before the board opens.
    // So the object reads nothing of the CoreApi, and asks it to show nothing.
    const saved: Notification = {
      id: "saved",
      severity: "message",
      message: "Saved.",
      source: { kind: "module", module: { object: { plugin: EDIT } }, code: "saved" },
    };
    const asked: unknown[] = [];
    const starting = blockrTest(() => ({
      store: {
        hasBoard: () => false,
        board: () => {
          throw new Error("The store holds no board yet.");
        },
      },
      scheduler: { status: () => READY },
      focus: { get: () => "b" },
      onScreen: { get: () => ({ blocks: ["b"], plugins: [] }) },
      notifications: { get: () => [saved] },
      show: (object) => {
        asked.push(object);
        return Promise.resolve();
      },
    }));
    expect(starting.board()).toBeNull();
    expect(starting.statuses()).toEqual({});
    expect(starting.focus()).toBeNull();
    expect(starting.onScreen()).toBeNull();
    expect(starting.notifications()).toEqual([]);
    await expect(starting.show({ block: "b" })).rejects.toThrow("No board is open yet");
    expect(asked).toEqual([]);
  });
});

describe("the page", () => {
  // A transport on which R starts no session, so that core starts no further
  // than the note that the board is loading.
  const silent: Transport = { send: () => Promise.resolve(), listen: () => undefined };

  test("gives blockrTest the core that it starts, as the script in test mode does", async () => {
    const started = startPage({ el: document.createElement("div"), transport: silent });

    // As page_script() in R/utils-serve.R writes it.
    const read = blockrTest(() => started.core());
    expect(read.board()).toBeNull();
    expect(read.statuses()).toEqual({});
    expect(read.focus()).toBeNull();
    expect(read.onScreen()).toBeNull();
    expect(read.notifications()).toEqual([]);
    await expect(read.show({ block: "b" })).rejects.toThrow("No board is open yet");

    started.ready.catch(() => undefined);
    started.stop();
  });
});
