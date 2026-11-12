// The object through which end-to-end tests read the board and each block's
// status, on the CoreApi for tests, with R played from the protocol's
// fixtures, and on the page that core starts, as the script that R writes for
// the page in Shiny's test mode reads it.

import { describe, expect, test } from "vitest";
import { startPage } from "../../page/index.ts";
import type { Transport } from "../../protocol/index.ts";
import { HELLO, READY, stubCore } from "../../stubs/index.ts";
import { blockrTest } from "./index.ts";

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

    // The store holds no board while core starts, before the board opens.
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

  test("gives blockrTest the core that it starts, as the script in test mode does", () => {
    const started = startPage({ el: document.createElement("div"), transport: silent });

    // As page_script() in R/utils-serve.R writes it.
    const read = blockrTest(() => started.core());
    expect(read.board()).toBeNull();
    expect(read.statuses()).toEqual({});

    started.ready.catch(() => undefined);
    started.stop();
  });
});
