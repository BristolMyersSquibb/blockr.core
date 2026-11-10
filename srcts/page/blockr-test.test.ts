// The object through which end-to-end tests read the board and each block's
// status, on the CoreApi for tests, with R played from the protocol's
// fixtures, and the page, which puts it on `window` in Shiny's test mode.

import { afterEach, describe, expect, test } from "vitest";
import type { Transport } from "../protocol/index.ts";
import { HELLO, READY, stubCore } from "../stubs/index.ts";
import { blockrTest } from "./blockr-test.ts";
import { startPage } from "./page.ts";

afterEach(() => {
  delete window.blockrTest;
});

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

  test("reads the board that opens later, through the CoreApi that core makes for it", async () => {
    const s = await stubCore();
    const read = blockrTest(() => s.core);

    await s.core.store.open(JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Opened" } }));
    expect(read.board()?.name).toBe("Opened");
    s.stop();
  });

  test("gives no board and no statuses while core starts", () => {
    expect(blockrTest(() => undefined).board()).toBeNull();
    expect(blockrTest(() => undefined).statuses()).toEqual({});

    // The store holds no board while the startup modules initialize.
    const starting = blockrTest(() => ({
      store: {
        hasBoard: () => false,
        board: () => {
          throw new Error("The store holds no board yet.");
        },
      },
      scheduler: { status: () => READY },
    }));
    expect(starting.board()).toBeNull();
    expect(starting.statuses()).toEqual({});
  });
});

describe("the page", () => {
  // A transport on which R starts no session, so that core starts no further
  // than the note that the board is loading.
  const silent: Transport = { send: () => Promise.resolve(), listen: () => undefined };

  test("puts blockrTest on window in test mode only", () => {
    const plain = startPage({ el: document.createElement("div"), transport: silent });
    expect(window.blockrTest).toBeUndefined();

    const testing = startPage({ el: document.createElement("div"), transport: silent, test: true });
    expect(window.blockrTest?.board()).toBeNull();
    expect(window.blockrTest?.statuses()).toEqual({});

    for (const started of [plain, testing]) {
      started.ready.catch(() => undefined);
      started.stop();
    }
  });
});
