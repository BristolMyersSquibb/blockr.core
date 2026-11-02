// @vitest-environment node

// The store, with R's replies taken from the fixtures of the wire protocol.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, test, vi } from "vitest";
import type {
  ErrorReply,
  Fixture,
  HelloReply,
  OpenReply,
  Reply,
} from "../protocol/messages.ts";
import type { Transport } from "../protocol/transport.ts";
import type { Block, Board, OpeningBoard, SavedBoard } from "../types/board.ts";
import { ROUNDS } from "./expand.ts";
import { createStore, isId, sentenceCase, StoreError } from "./index.ts";
import type {
  Change,
  Delta,
  Migration,
  Option,
  StateKey,
  Store,
} from "./index.ts";

// The DAG keeps a position for each block in the front-end's state, as a
// front-end types its keys there.
declare module "../types/board.ts" {
  interface FrontEndState {
    extensions: Readonly<
      Record<
        string,
        { readonly positions?: Readonly<Record<string, Position>> } & Readonly<
          Record<string, unknown>
        >
      >
    >;
  }
}

interface Position {
  readonly x: number;
  readonly y: number;
}

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// R's first reply in a fixture.
function reply(name: string): unknown {
  const file = join(root, "inst", "protocol", "fixtures", `${name}.json`);
  const fixture = JSON.parse(readFileSync(file, "utf8")) as Fixture;
  return fixture.r[0]?.[0];
}

const hello = reply("hello") as HelloReply;
const loaded = reply("open") as OpenReply;

// R, as the fixtures give its replies, which records what the store sent.
type Sent = Parameters<Transport["send"]>[0];

function fakeR(answer: Reply = loaded): Transport & { sent: Sent[] } {
  const sent: Sent[] = [];
  return {
    sent,
    send(request) {
      sent.push(request);
      return Promise.resolve(answer);
    },
  };
}

// Core's board options, as the fixtures' boards hold their values.
const coreOptions: Option[] = [
  {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema: { type: "integer", minimum: 0, default: 50 },
  },
  {
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema: {
      type: "array",
      items: { enum: ["message", "warning", "error"] },
      default: ["warning", "error"],
    },
  },
];

const path = ["extensions", "blockr.dag-dag", "positions"];

function positionsOf(board: Board): Readonly<Record<string, Position>> {
  return board.front_end.state.extensions?.["blockr.dag-dag"]?.positions ?? {};
}

// The DAG's positions, a removed block's going with it.
const positions: StateKey = {
  path,
  kind: "collection",
  schema: {
    type: "object",
    properties: { x: { type: "number" }, y: { type: "number" } },
    required: ["x", "y"],
    additionalProperties: false,
  },
  expand(delta, board) {
    const held = positionsOf(board);
    const gone = (delta.blocks?.rm ?? []).filter((id) => Object.hasOwn(held, id));
    return gone.length > 0 ? { rm: gone } : undefined;
  },
  check: (value, board) =>
    Object.keys(value as object)
      .filter((id) => !Object.hasOwn(board.blocks, id))
      .map((id) => `There is no block "${id}" to place.`),
};

function newStore(transport: Transport = fakeR()): Store {
  const store = createStore({ transport });
  for (const option of coreOptions) store.options.register(option);
  return store;
}

async function opened(
  setup?: (store: Store) => void,
  transport: Transport = fakeR(),
  replied: HelloReply = hello,
): Promise<Store> {
  const store = newStore(transport);
  setup?.(store);
  await store.open(replied);
  return store;
}

function changes(store: Store): Change[] {
  const seen: Change[] = [];
  store.subscribe((change) => {
    seen.push(change);
  });
  return seen;
}

function head(n = 6, name = "Head"): Block {
  return {
    registry_id: "blockr.core-head_block",
    state: { n, direction: "head" },
    name,
    attrs: {},
  };
}

function file(
  board: Partial<Board>,
  packages: Readonly<Record<string, string>> = { "blockr.core": "0.2.0" },
): string {
  const saved: SavedBoard = {
    format: 1,
    packages,
    board: {
      id: "amber_tapir",
      front_end: { id: "blockr.core-basic", state: {}, attrs: {} },
      name: "Cars",
      blocks: {
        a: {
          registry_id: "blockr.core-dataset_block",
          state: { dataset: "mtcars", package: "datasets" },
          name: "Cars",
          attrs: {},
        },
        b: head(),
      },
      links: { ab: { from: "a", to: "b", input: "data" } },
      stacks: { s1: { name: "Input", blocks: ["a", "b"] } },
      options: { n_rows: 50, show_conditions: ["warning", "error"] },
      ...board,
    },
  };
  return JSON.stringify(saved);
}

describe("opening a board", () => {
  test("opens the board R sent, keying its collections and minting the ids it lacks", async () => {
    const store = newStore();
    const seen = changes(store);
    const dropped = await store.open(hello);
    const board = store.board();

    expect(dropped).toEqual([]);
    expect(isId(board.id)).toBe(true);
    expect(board.name).toBe("Iris review");
    expect(Object.keys(board.blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
    expect(board.blocks.a).toEqual({
      registry_id: "blockr.core-dataset_block",
      state: { dataset: "iris", package: "datasets" },
      name: "Iris",
      attrs: {},
    });

    const links = Object.keys(board.links);
    expect(links.slice(0, 6)).toEqual(["ab", "ac", "bc", "ad", "be", "de"]);
    expect(links[6]).toMatch(/^[a-z]+_[a-z]+$/);
    expect(board.links[links[6] ?? ""]).toEqual({ from: "a", to: "f", input: "data" });
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"] } });

    expect(store.blockRegistry()).toBe(hello.block_registry);
    expect(store.packages()).toEqual({ "blockr.core": "0.2.0" });
    expect(seen).toEqual([{ opened: board, dropped: [] }]);
    expect(store.canUndo()).toBe(false);
  });

  test("names a board without a name after its id, and writes in no default of a board option", async () => {
    const store = newStore();
    await store.open(reply("hello-block") as HelloReply);
    const board = store.board();

    expect(board.name).toBe(sentenceCase(board.id));
    expect(Object.keys(board.blocks)).toHaveLength(1);
    expect(isId(Object.keys(board.blocks)[0])).toBe(true);
    expect(board.options).toEqual({ n_rows: 50 });
  });

  test("opens the saved file that serve() was given without asking R again", async () => {
    const transport = fakeR();
    const store = newStore(transport);
    await store.open(reply("hello-file") as HelloReply);

    expect(transport.sent).toEqual([]);
    expect(store.board().id).toBe("amber_tapir");
    expect(Object.keys(store.board().links)).toEqual(["ab"]);
  });

  test("has R load the packages a saved file names, and drops the blocks whose class the block registry lacks", async () => {
    const transport = fakeR();
    const store = await opened(undefined, transport);
    const seen = changes(store);

    const dropped = await store.open(
      file(
        {
          blocks: {
            a: {
              registry_id: "blockr.core-dataset_block",
              state: { dataset: "mtcars", package: "datasets" },
              name: "Cars",
              attrs: {},
            },
            x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter", attrs: {} },
            b: head(),
          },
          links: {
            ax: { from: "a", to: "x", input: "data" },
            xb: { from: "x", to: "b", input: "data" },
          },
          stacks: { s1: { name: "Input", blocks: ["a", "x", "b"] } },
        },
        { "blockr.core": "0.2.0", "blockr.dplyr": "0.3.0" },
      ),
    );

    expect(transport.sent).toEqual([{ kind: "open", packages: ["blockr.core", "blockr.dplyr"] }]);
    expect(store.blockRegistry()).toBe(loaded.block_registry);
    expect(store.packages()).toBe(loaded.packages);
    expect(dropped).toEqual([
      { block: "x", registry_id: "blockr.dplyr-filter_block", package: "blockr.dplyr" },
    ]);

    const board = store.board();
    expect(board.id).toBe("amber_tapir");
    expect(Object.keys(board.blocks)).toEqual(["a", "b"]);
    expect(board.links).toEqual({});
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"] } });
    expect(seen).toEqual([{ opened: board, dropped }]);
  });

  test("refuses a file above the format it knows, naming both, and a file of today's stack", async () => {
    const store = await opened();
    const board = store.board();

    await expect(store.open(JSON.stringify({ format: 2, packages: {}, board: {} }))).rejects.toThrow(
      /format is 2, while this client reads format 1 at most/,
    );
    await expect(store.open(JSON.stringify({ object: "board", payload: {} }))).rejects.toThrow(
      /no format, as a file of today's stack has none/,
    );
    await expect(store.open("{")).rejects.toThrow(StoreError);
    expect(store.board()).toBe(board);
  });

  test("refuses to open a board with an id made of digits alone, naming it", async () => {
    const store = await opened();
    await expect(store.open(file({ blocks: { 12: head() }, links: {}, stacks: {} }))).rejects.toThrow(
      /block id "12" is empty or made of digits alone/,
    );
  });

  test("refuses to open a board that fails a check, changing nothing", async () => {
    const store = await opened();
    const board = store.board();
    const seen = changes(store);

    await expect(store.open(file({ links: { ab: { from: "a", to: "b", input: "x" } } }))).rejects.toThrow(
      /feeds the input "x" of block "b", whose class has no such input/,
    );
    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
  });

  test("rejects where R refuses to load the packages", async () => {
    const store = await opened(undefined, fakeR(reply("open-locked") as ErrorReply));
    await expect(store.open(file({}))).rejects.toThrow(/The board is locked, so it opens no other board/);
  });

  test("runs what has to run once R has loaded the packages, before it opens the board", async () => {
    const replied = {
      ...hello,
      board: {
        ...hello.board,
        front_end: {
          id: "blockr.core-basic",
          state: { extensions: { "blockr.dag-dag": { positions: { a: { x: 0, y: 0 } } } } },
          attrs: {},
        },
      },
    } as HelloReply;
    const order: string[] = [];
    const store = createStore({
      transport: fakeR(),
      loaded(reply, frontEnd) {
        order.push(`loaded ${frontEnd}`);
        expect(reply).toBe(replied);
        store.register(positions);
      },
    });
    store.subscribe(() => order.push("opened"));
    for (const option of coreOptions) store.options.register(option);

    await store.open(replied);

    expect(order).toEqual(["loaded blockr.core-basic", "opened"]);
    expect(positionsOf(store.board())).toEqual({ a: { x: 0, y: 0 } });
  });

  test("opens a board made for a front-end that R serves no module for as one made for core's, dropping what it does not know", async () => {
    const store = await opened();
    const dropped = await store.open(
      file({
        front_end: {
          id: "blockr.dock-dock",
          state: {
            views: { main: { panels: ["a"] } },
            extensions: { "blockr.dag-dag": { positions: { a: { x: 0, y: 0 } } } },
          },
          attrs: { layout_locked: true },
        },
        blocks: {
          a: {
            registry_id: "blockr.core-dataset_block",
            state: { dataset: "mtcars", package: "datasets" },
            name: "Cars",
            attrs: { visible: ["outputs"], color: "red" },
          },
          b: head(),
        },
        options: { n_rows: 20, view_tabs: true },
      }),
    );

    expect(dropped).toEqual([{ front_end: "blockr.dock-dock", package: "blockr.dock" }]);

    const board = store.board();
    expect(board.front_end).toEqual({ id: "blockr.core-basic", state: {}, attrs: {} });
    expect(board.blocks.a?.attrs).toEqual({ visible: ["outputs"] });
    expect(board.options).toEqual({ n_rows: 20 });
  });

  test("drops front-end state that no package registered, without naming it", async () => {
    const store = await opened((store) => {
      store.register({ ...positions, path: ["extensions", "blockr.dag-dag", "layout"] });
    });
    const dropped = await store.open(
      file({
        front_end: {
          id: "blockr.core-basic",
          state: {
            extensions: {
              "blockr.dag-dag": { positions: { a: { x: 0, y: 0 } } },
              "blockr.md-md": { content: "" },
            },
          },
          attrs: {},
        },
      }),
    );

    expect(dropped).toEqual([]);
    expect(store.board().front_end.state).toEqual({ extensions: { "blockr.dag-dag": { layout: {} } } });
  });

  test("keeps no empty object on the way to nothing", async () => {
    const store = await opened();
    const dropped = await store.open(
      file({ front_end: { id: "blockr.core-basic", state: { extensions: {} }, attrs: {} } }),
    );
    expect(dropped).toEqual([]);
    expect(store.board().front_end.state).toEqual({});
  });

  test("gives each registered key the board lacks its initial value or an empty collection", async () => {
    const store = await opened((store) => {
      store.register(positions);
      store.register({
        path: ["extensions", "blockr.dag-dag", "direction"],
        kind: "value",
        schema: { enum: ["LR", "TB"] },
        initial: "LR",
      });
    });

    expect(store.board().front_end.state).toEqual({
      extensions: { "blockr.dag-dag": { positions: {}, direction: "LR" } },
    });
  });

  test("opening a board starts a new undo history", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    expect(store.canUndo()).toBe(true);

    await store.open(hello);
    expect(store.canUndo()).toBe(false);
    expect(store.board().name).toBe("Iris review");
  });

  test("ends at the same board whichever way the board comes in", async () => {
    // A board that gives every id, so that nothing is minted.
    const replied = reply("hello-locked") as HelloReply & { board: OpeningBoard };
    const answer: OpenReply = {
      id: "2",
      status: "ok",
      packages: replied.packages,
      block_registry: replied.block_registry,
      front_ends: replied.front_ends,
      startup_modules: replied.startup_modules,
      modules: replied.modules,
      renderers: replied.renderers,
      blockr_options: replied.blockr_options,
    };
    const fresh = () => {
      const store = newStore(fakeR(answer));
      store.register(positions);
      return store;
    };

    // As an app script built it.
    const script = fresh();
    await script.open(replied);
    const board = script.board();

    // As the file the client writes for it, which serve() is given or the
    // client opens.
    const saved: SavedBoard = { format: 1, packages: replied.packages, board };
    const viaR = fresh();
    await viaR.open({ ...replied, board: undefined, file: saved });
    const viaClient = fresh();
    await viaClient.open(JSON.stringify(saved));

    // As a user builds it on an empty board.
    const built = fresh();
    await built.open({
      ...replied,
      board: { ...replied.board, blocks: [], links: [], stacks: [] },
    });
    for (const [id, block] of Object.entries(board.blocks)) {
      built.apply({ blocks: { add: { [id]: block } } });
    }
    built.apply({ links: { add: board.links } });

    expect(viaR.board()).toEqual(board);
    expect(viaClient.board()).toEqual(board);
    expect(built.board()).toEqual(board);
  });

  test("drops a block whose class the block registry lacks as a delta removes it, with what keys keep about it", async () => {
    const store = await opened((store) => store.register(positions));
    const dropped = await store.open(
      file({
        front_end: {
          id: "blockr.core-basic",
          state: {
            extensions: {
              "blockr.dag-dag": {
                positions: { a: { x: 0, y: 0 }, x: { x: 1, y: 1 }, b: { x: 2, y: 2 } },
              },
            },
          },
          attrs: {},
        },
        blocks: {
          a: {
            registry_id: "blockr.core-dataset_block",
            state: { dataset: "mtcars", package: "datasets" },
            name: "Cars",
            attrs: {},
          },
          x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter", attrs: { color: "red" } },
          b: head(),
        },
        links: {
          ax: { from: "a", to: "x", input: "data" },
          xb: { from: "x", to: "b", input: "data" },
        },
        stacks: { s1: { name: "Input", blocks: ["a", "x", "b"] } },
      }),
    );

    expect(dropped).toEqual([
      { block: "x", registry_id: "blockr.dplyr-filter_block", package: "blockr.dplyr" },
    ]);
    const board = store.board();
    expect(positionsOf(board)).toEqual({ a: { x: 0, y: 0 }, b: { x: 2, y: 2 } });
    expect(board.links).toEqual({});
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"] } });
  });

  test("drops an attribute or a board option value that its schema refuses, so that it reads as its initial value or default", async () => {
    const store = await opened();
    const dropped = await store.open(
      file({
        blocks: {
          a: {
            registry_id: "blockr.core-dataset_block",
            state: { dataset: "mtcars", package: "datasets" },
            name: "Cars",
            attrs: { visible: ["header"] },
          },
          b: head(),
        },
        options: { n_rows: "fifty", show_conditions: ["warning", "error"] },
      }),
    );

    expect(dropped).toEqual([]);
    expect(store.board().blocks.a?.attrs).toEqual({});
    expect(store.board().options).toEqual({ show_conditions: ["warning", "error"] });
  });

  test("holds no board until it opens one", () => {
    const store = newStore();
    expect(() => store.board()).toThrow("The store holds no board until it opens one.");
    expect(() => {
      store.apply({ name: "Built" });
    }).toThrow("The store rejected the delta\n- The store holds no board until it opens one.");
    expect(store.canUndo()).toBe(false);
  });
});

describe("migrations", () => {
  const replied: OpenReply = {
    ...loaded,
    packages: { ...loaded.packages, "blockr.dag": "0.3.0" },
  };

  // An app that loaded blockr.dag and blockr.io beside core, whose packages
  // register their migrations.
  function migrating(migrations: Record<string, Migration>): Promise<Store> {
    return opened(
      (store) => {
        store.register(positions);
        for (const [pkg, fn] of Object.entries(migrations)) store.registerMigration(pkg, fn);
      },
      fakeR(replied),
      { ...hello, packages: replied.packages },
    );
  }

  const old = file(
    {
      front_end: {
        id: "blockr.core-basic",
        state: { extensions: { "blockr.dag-dag": { layout: { a: [1, 2], b: [3, 4] } } } },
        attrs: {},
      },
      blocks: {
        a: {
          registry_id: "blockr.core-dataset_block",
          state: { dataset: "mtcars", package: "datasets" },
          name: "Cars",
          attrs: {},
        },
        b: { ...head(), state: { n: "6", direction: "head" } },
      },
    },
    { "blockr.core": "0.1.9", "blockr.dag": "0.2.0" },
  );

  test("runs the migration of every package whose saved version differs from the one R loaded, one after another", async () => {
    // The DAG renames its layout to positions.
    const dag = vi.fn<Migration>((saved) => {
      const layout =
        (saved.front_end.state as { extensions: Record<string, { layout?: Record<string, number[]> }> })
          .extensions["blockr.dag-dag"]?.layout ?? {};
      return {
        ...saved,
        front_end: {
          state: {
            extensions: {
              "blockr.dag-dag": {
                positions: Object.fromEntries(
                  Object.entries(layout).map(([id, [x = 0, y = 0]]) => [id, { x, y }]),
                ),
              },
            },
          },
        },
      };
    });
    // Core's head block once kept its n as a string.
    const core = vi.fn<Migration>((saved) => ({
      ...saved,
      blocks: Object.fromEntries(
        Object.entries(saved.blocks).map(([id, block]) => [
          id,
          block.registry_id === "blockr.core-head_block"
            ? { ...block, state: { ...block.state, n: Number(block.state.n) } }
            : block,
        ]),
      ),
    }));
    const io = vi.fn<Migration>((saved) => saved);

    const store = await migrating({ "blockr.dag": dag, "blockr.core": core, "blockr.io": io });
    const dropped = await store.open(old);

    expect(dag).toHaveBeenCalledOnce();
    expect(dag.mock.calls[0]?.[1]).toBe("0.2.0");
    expect(core.mock.calls[0]?.[1]).toBe("0.1.9");
    expect(io.mock.calls[0]?.[1]).toBeNull();

    // Each migration takes the file as the one before it left it.
    const fromDag = dag.mock.results[0]?.value as unknown;
    expect(core.mock.calls[0]?.[0]).toEqual(fromDag);
    expect(Object.isFrozen(core.mock.calls[0]?.[0].blocks)).toBe(true);

    const board = store.board();
    expect(positionsOf(board)).toEqual({ a: { x: 1, y: 2 }, b: { x: 3, y: 4 } });
    expect(board.blocks.b?.state).toEqual({ n: 6, direction: "head" });
    expect(dropped).toEqual([]);
  });

  test("refuses to open a board that a migration fails on", async () => {
    const dag = vi.fn<Migration>(() => {
      throw new Error("The layout is unreadable.");
    });
    const store = await migrating({ "blockr.dag": dag });
    await expect(store.open(old)).rejects.toThrow("The migration of blockr.dag failed: The layout is unreadable.");
  });

  test("runs no migration of a package saved at the version R loaded", async () => {
    const dag = vi.fn<Migration>((saved) => saved);
    const store = await migrating({ "blockr.dag": dag });
    await store.open(file({}, { "blockr.core": "0.2.0", "blockr.dag": "0.3.0" }));
    expect(dag).not.toHaveBeenCalled();
  });

  test("runs no migration for the board an app script built", async () => {
    const dag = vi.fn<Migration>((saved) => saved);
    await migrating({ "blockr.dag": dag });
    expect(dag).not.toHaveBeenCalled();
  });
});

describe("applying a delta", () => {
  test("commits a delta, sharing with the old board what it did not change, and announces it", async () => {
    const store = await opened();
    const before = store.board();
    const seen = changes(store);
    let during: Board | undefined;
    store.subscribe(() => {
      during = store.board();
    });

    const delta: Delta = {
      blocks: { add: { g: head(3) } },
      links: { add: { dg: { from: "d", to: "g", input: "data" } } },
    };
    store.apply(delta);
    const after = store.board();

    expect(after).not.toBe(before);
    expect(after.blocks.a).toBe(before.blocks.a);
    expect(after.stacks).toBe(before.stacks);
    expect(after.options).toBe(before.options);
    expect(after.front_end).toBe(before.front_end);
    expect(Object.keys(after.blocks).at(-1)).toBe("g");
    expect(after.links.dg).toEqual({ from: "d", to: "g", input: "data" });
    expect(seen).toEqual([{ delta, origin: "user" }]);
    expect(during).toBe(after);
    expect(store.board()).toBe(store.board());
  });

  test("names the origin that it is given", async () => {
    const store = await opened();
    const seen = changes(store);
    store.apply({ name: "Renamed" }, "assistant");
    expect(seen).toEqual([{ delta: { name: "Renamed" }, origin: "assistant" }]);
  });

  test("removes a removed block's links and its place in its stack in the same delta", async () => {
    const store = await opened();
    const seen = changes(store);
    store.apply({ blocks: { rm: ["b"] } });

    expect(seen).toEqual([
      {
        delta: {
          blocks: { rm: ["b"] },
          links: { rm: ["ab", "bc", "be"] },
          stacks: { mod: { s1: { blocks: ["a"] } } },
        },
        origin: "user",
      },
    ]);
    const board = store.board();
    expect(Object.keys(board.blocks)).toEqual(["a", "c", "d", "e", "f"]);
    expect(Object.values(board.links).some((link) => link.from === "b" || link.to === "b")).toBe(false);
    expect(board.stacks.s1?.blocks).toEqual(["a"]);
  });

  test("expands a delta through each key's expand, and one undo takes all of it back", async () => {
    const store = await opened((store) => store.register(positions));
    store.apply({
      front_end: { state: [{ path, change: { add: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } } }] },
    });
    const before = store.board();
    const seen = changes(store);

    store.apply({ blocks: { rm: ["b"] } });
    expect(seen[0]).toMatchObject({ delta: { front_end: { state: [{ path, change: { rm: ["b"] } }] } } });
    expect(positionsOf(store.board())).toEqual({ a: { x: 0, y: 0 } });

    store.undo();
    expect(store.board()).toEqual(before);
    expect(Object.keys(store.board().blocks)).toEqual(Object.keys(before.blocks));
    expect(Object.keys(store.board().links)).toEqual(Object.keys(before.links));
    expect(Object.keys(positionsOf(store.board()))).toEqual(["a", "b"]);
  });

  test("rejects a delta that keeps expanding, naming the keys that kept adding", async () => {
    const count: StateKey = {
      path: ["count"],
      kind: "value",
      schema: { type: "integer" },
      initial: 0,
      expand(delta) {
        const change = delta.front_end?.state?.find((other) => other.path[0] === "count")?.change;
        const at = change !== undefined && "set" in change ? (change.set as number) : 0;
        return { set: at + 1 };
      },
    };
    const store = await opened((store) => store.register(count));
    expect(() => {
      store.apply({ name: "Renamed" });
    }).toThrow(new RegExp(`kept expanding after ${String(ROUNDS)} rounds, through count`));
  });

  const refusals: [string, Delta, RegExp][] = [
    ["a link from a block the board lacks", { links: { add: { zb: { from: "z", to: "e", input: "" } } } }, /comes from block "z", which the board lacks/],
    ["a link into an input the class lacks", { links: { add: { ab2: { from: "a", to: "b", input: "x" } } } }, /feeds the input "x" of block "b", whose class has no such input/],
    ["a second link into a fixed input", { links: { add: { db: { from: "d", to: "b", input: "data" } } } }, /input "data" of block "b" takes the links "ab" and "db", where it takes one/],
    ["a cycle", { links: { rm: ["ad"], add: { ed: { from: "e", to: "d", input: "data" } } } }, /links form a cycle through the blocks/],
    ["a stack holding a block the board lacks", { stacks: { add: { s2: { name: "More", blocks: ["z"] } } } }, /stack "s2" holds block "z", which the board lacks/],
    ["a block in two stacks", { stacks: { add: { s2: { name: "More", blocks: ["a"] } } } }, /block "a" sits in the stacks "s1" and "s2", where it sits in one/],
    ["a board option value its schema refuses", { options: { set: { n_rows: -1 } } }, /value of the board option "n_rows" must be >= 0/],
    ["a value of a board option no package registered", { options: { set: { nope: 1 } } }, /board option "nope", which no package registered/],
    ["an attribute no package registered", { blocks: { mod: { a: { attrs: { set: { color: "red" } } } } } }, /block "a" has the attribute "color", which no package registered/],
    ["an attribute its schema refuses", { blocks: { mod: { a: { attrs: { set: { visible: ["header"] } } } } } }, /attribute "visible" of block "a" \/0 must be equal to one of the allowed values/],
    ["a block of a class the block registry lacks", { blocks: { add: { g: { ...head(), registry_id: "blockr.foo-bar" } } } }, /class "blockr.foo-bar", which the block registry lacks/],
    ["an id made of digits alone", { blocks: { add: { 7: head() } } }, /block id "7" is empty or made of digits alone/],
    ["removing a block the board lacks", { blocks: { rm: ["z"] } }, /board holds no block "z" to remove/],
    ["adding a block the board holds", { blocks: { add: { a: head() } } }, /board holds block "a" already/],
    ["changing a block the board lacks", { blocks: { mod: { z: { name: "Z" } } } }, /board holds no block "z" to change/],
    ["setting and unsetting one state arg", { blocks: { mod: { b: { state: { n: 1 }, unset: ["n"] } } } }, /both sets and unsets "n"/],
    ["setting and unsetting one board option", { options: { set: { n_rows: 5 }, unset: ["n_rows"] } }, /both sets and unsets the board option "n_rows"/],
    ["a key that no package registered", { front_end: { state: [{ path: ["views"], change: { add: {} } }] } }, /No key of the front-end's state is registered at views/],
    ["a block that is no block", { blocks: { add: { g: { ...head(), name: 3 } } } } as unknown as Delta, /block "g" has no string as its name/],
    ["a link holding more than core keeps", { links: { add: { dg: { from: "d", to: "f", input: "data", color: "red" } } } } as unknown as Delta, /link "dg" holds "color", which core does not keep/],
  ];

  test.each(refusals)("rejects %s, changing nothing", async (_, delta, failure) => {
    const store = await opened();
    const board = store.board();
    const seen = changes(store);

    expect(() => {
      store.apply(delta);
    }).toThrow(failure);
    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
    expect(store.canUndo()).toBe(false);
  });

  test("checks each key's schema and check", async () => {
    const store = await opened((store) => store.register(positions));
    const add = (entries: Record<string, unknown>): Delta =>
      ({ front_end: { state: [{ path, change: { add: entries } }] } }) as Delta;

    expect(() => {
      store.apply(add({ a: { x: "left", y: 0 } }));
    }).toThrow(/entry "a" of extensions\/blockr.dag-dag\/positions \/x must be number/);
    expect(() => {
      store.apply(add({ z: { x: 0, y: 0 } }));
    }).toThrow(/extensions\/blockr.dag-dag\/positions: There is no block "z" to place/);
  });

  test("applies several deltas in order as one step, or none of them", async () => {
    const store = await opened();
    const before = store.board();
    const seen = changes(store);

    store.apply([
      { blocks: { add: { g: head(3) } } },
      { links: { add: { dg: { from: "d", to: "g", input: "data" } } } },
    ]);
    expect(seen).toHaveLength(2);
    expect(store.board().links.dg).toBeDefined();

    const after = store.board();
    expect(() => {
      store.apply([
        { blocks: { add: { h: head(3) } } },
        { links: { add: { zh: { from: "z", to: "h", input: "data" } } } },
      ]);
    }).toThrow(/Delta 2: The link "zh" comes from block "z"/);
    expect(store.board()).toBe(after);

    store.undo();
    expect(store.board()).toEqual(before);
    expect(store.canUndo()).toBe(false);
  });

  test("removes before it adds, so a delta may reuse an id it drops, and changes last", async () => {
    const store = await opened();
    store.apply({
      blocks: {
        rm: ["f"],
        add: { f: head(2), g: head(3) },
        mod: { g: { name: "Tail" } },
      },
    });
    expect(store.board().blocks.f).toEqual(head(2));
    expect(store.board().blocks.g?.name).toBe("Tail");
  });

  test("places an added link before the one that `before` names", async () => {
    const store = await opened();
    store.apply({
      blocks: { add: { g: head(3) } },
      links: { add: { ge: { from: "g", to: "e", input: "" } }, before: { ge: "de" } },
    });
    const links = Object.keys(store.board().links);
    expect(links.indexOf("ge")).toBe(links.indexOf("de") - 1);
  });

  test("throws when a subscriber applies a delta", async () => {
    const store = await opened();
    let caught: unknown;
    store.subscribe((change) => {
      if (!("delta" in change)) return;
      try {
        store.apply({ name: "Again" });
      } catch (error) {
        caught = error;
      }
    });
    store.apply({ name: "Renamed" });
    expect(String(caught)).toMatch(/applying or announcing a change/);
    expect(store.board().name).toBe("Renamed");
  });

  test("tells every subscriber of a change though one throws, and reports what it threw", async () => {
    const store = await opened();
    const reported: (() => void)[] = [];
    const queue = vi.spyOn(globalThis, "queueMicrotask").mockImplementation((fn) => {
      reported.push(fn);
    });
    const told: string[] = [];
    const failing = store.subscribe(() => {
      throw new Error("A subscriber failed.");
    });
    const telling = store.subscribe(() => told.push("second"));

    store.apply({ name: "Renamed" });
    queue.mockRestore();
    expect(told).toEqual(["second"]);
    expect(reported).toHaveLength(1);
    expect(() => {
      reported[0]?.();
    }).toThrow("A subscriber failed.");

    failing();
    telling();
    store.apply({ name: "Again" }, "assistant");
    expect(told).toEqual(["second"]);
  });

  test("applies nothing and keeps no step for no deltas", async () => {
    const store = await opened();
    const board = store.board();
    store.apply([]);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });
});

describe("edit ids", () => {
  test("change for a block with its own edits only", async () => {
    const store = await opened();
    const b = store.editId({ block: "b" });
    expect(b).toBeTypeOf("number");

    store.apply({ blocks: { mod: { c: { name: "Joined" } } } });
    expect(store.editId({ block: "b" })).toBe(b);

    store.apply({ blocks: { mod: { b: { state: { n: 7 } } } } });
    expect(store.editId({ block: "b" })).not.toBe(b);
  });

  test("are the number of the step that last changed what they belong to", async () => {
    const store = await opened();
    const opening = store.editId({ block: "a" });
    expect(store.editId({ block: "b" })).toBe(opening);

    store.apply({ blocks: { mod: { a: { name: "Data" }, c: { name: "Joined" } } } });
    const step = store.editId({ block: "a" });
    expect(step).toBeGreaterThan(opening ?? Infinity);
    expect(store.editId({ block: "c" })).toBe(step);
    expect(store.editId({ block: "b" })).toBe(opening);
  });

  test("refuse a delta built on an edit that another has replaced", async () => {
    const store = await opened();
    const edit = store.editId({ block: "b" });
    store.apply({ blocks: { mod: { b: { edit, state: { n: 7 } } } } });

    expect(() => {
      store.apply({ blocks: { mod: { b: { edit, state: { n: 8 } } } } });
    }).toThrow(/built on edit \d+ of block "b", which another edit has replaced with edit \d+/);
    expect(store.board().blocks.b?.state.n).toBe(7);
  });

  test("of a step's deltas are those the board had as the step started", async () => {
    const store = await opened();
    const edit = store.editId({ block: "b" });
    store.apply([
      { blocks: { mod: { b: { edit, state: { n: 7 } } } } },
      { blocks: { mod: { b: { edit, name: "Top" } } } },
    ]);
    expect(store.board().blocks.b).toMatchObject({ state: { n: 7 }, name: "Top" });
  });

  test("are kept for each entry and value of the front-end's state", async () => {
    const store = await opened((store) => {
      store.register(positions);
      store.register({
        path: ["extensions", "blockr.dag-dag", "direction"],
        kind: "value",
        schema: { enum: ["LR", "TB"] },
        initial: "LR",
      });
    });
    const direction = ["extensions", "blockr.dag-dag", "direction"];
    expect(store.editId({ path: direction })).toBeTypeOf("number");

    store.apply({
      front_end: { state: [{ path, change: { add: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } } }] },
    });
    const a = store.editId({ path, entry: "a" });
    store.apply({ front_end: { state: [{ path, change: { mod: { b: { set: { x: 2 } } } } }] } });
    expect(store.editId({ path, entry: "a" })).toBe(a);

    const edit = store.editId({ path: direction });
    store.apply({ front_end: { state: [{ path: direction, change: { edit, set: "TB" } }] } });
    expect(() => {
      store.apply({ front_end: { state: [{ path: direction, change: { edit, set: "LR" } }] } });
    }).toThrow(/built on edit \d+ of the value of extensions\/blockr.dag-dag\/direction/);
  });
});

describe("undo", () => {
  test("reverts the origin's last step, and redo applies it again", async () => {
    const store = await opened();
    const seen = changes(store);
    store.apply({ name: "Renamed" });
    expect([store.canUndo(), store.canRedo()]).toEqual([true, false]);

    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect([store.canUndo(), store.canRedo()]).toEqual([false, true]);

    store.redo();
    expect(store.board().name).toBe("Renamed");
    expect(seen).toEqual([
      { delta: { name: "Renamed" }, origin: "user" },
      { delta: { name: "Iris review" }, origin: "user" },
      { delta: { name: "Renamed" }, origin: "user" },
    ]);
  });

  test("puts a removed block back whole and in its place, with its links and its place in its stack", async () => {
    const store = await opened();
    const before = store.board();
    store.apply({ blocks: { rm: ["b"] } });
    store.undo();

    const after = store.board();
    expect(after).toEqual(before);
    expect(after.blocks.b).toBe(before.blocks.b);
    expect(Object.keys(after.blocks)).toEqual(Object.keys(before.blocks));
    expect(Object.keys(after.links)).toEqual(Object.keys(before.links));
  });

  test("removes again what a step set where there was none", async () => {
    const store = await opened((store) => {
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
      });
    });
    const before = store.board();

    store.apply({
      blocks: { mod: { d: { state: { extra: 1 } }, a: { attrs: { set: { visible: ["outputs"] } } } } },
      options: { set: { seed: 42 } },
    });
    store.undo();

    expect(store.board()).toEqual(before);
    expect(Object.hasOwn(store.board().blocks.d?.state ?? {}, "extra")).toBe(false);
    expect(Object.hasOwn(store.board().options, "seed")).toBe(false);
  });

  test("removes a value of front-end state again where its step set one that had none, with a removal that survives JSON", async () => {
    const direction = ["extensions", "blockr.dag-dag", "direction"];
    const store = await opened((store) => {
      store.register({ path: direction, kind: "value", schema: { enum: ["LR", "TB"] } });
    });
    const before = store.board();
    const seen = changes(store);

    store.apply({ front_end: { state: [{ path: direction, change: { set: "TB" } }] } });
    store.undo();

    expect(store.board()).toEqual(before);
    const undo = seen[1];
    expect(undo && "delta" in undo ? JSON.parse(JSON.stringify(undo.delta)) : undefined).toEqual({
      front_end: { state: [{ path: direction, change: { unset: true } }] },
    });
  });

  test("undoes each origin's steps apart", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    store.apply({ options: { set: { n_rows: 20 } } }, "assistant");

    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.board().options.n_rows).toBe(20);
    expect(store.canUndo("assistant")).toBe(true);

    store.undo("assistant");
    expect(store.board().options.n_rows).toBe(50);
  });

  test("sets back what its step changed over what another origin changed since, as the last writer", async () => {
    const store = await opened();
    store.apply({ blocks: { mod: { b: { state: { n: 7 } } } } });
    store.apply({ blocks: { mod: { b: { state: { n: 8 } } } } }, "assistant");
    const seen = changes(store);

    store.undo();
    expect(store.board().blocks.b?.state.n).toBe(6);
    expect(seen).toEqual([{ delta: { blocks: { mod: { b: { state: { n: 6 } } } } }, origin: "user" }]);
    expect(store.canUndo("assistant")).toBe(true);
  });

  test("undoes and redoes as steps of their own, so that edit ids only grow", async () => {
    const store = await opened();
    const opening = store.editId({ block: "b" }) ?? 0;
    store.apply({ blocks: { mod: { b: { state: { n: 7 } } } } });
    store.apply({ blocks: { mod: { b: { state: { n: 8 } } } } });

    store.undo();
    const undone = store.editId({ block: "b" }) ?? 0;
    store.undo();
    expect(store.board().blocks.b?.state.n).toBe(6);
    expect(store.editId({ block: "b" })).toBeGreaterThan(undone);
    expect(undone).toBeGreaterThan(opening);

    store.redo();
    store.redo();
    expect(store.board().blocks.b?.state.n).toBe(8);
  });

  test("redo brings back what was there as the step was undone, whatever another origin changed in between", async () => {
    const store = await opened();
    store.apply({ stacks: { mod: { s1: { blocks: ["a"] } } } });
    store.apply({ stacks: { mod: { s1: { blocks: ["a", "c"] } } } }, "assistant");
    const before = store.board();

    store.undo();
    expect(store.board().stacks.s1?.blocks).toEqual(["a", "b"]);

    store.redo();
    expect(store.board()).toEqual(before);
  });

  test("does nothing where what its step changed is gone", async () => {
    const store = await opened();
    store.apply({ blocks: { mod: { d: { name: "Narrow" } } } });
    store.apply({ blocks: { rm: ["d"] } }, "assistant");
    const board = store.board();
    const seen = changes(store);

    store.undo();
    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
    expect(store.canUndo()).toBe(false);

    store.redo();
    expect(store.board()).toBe(board);
  });

  test("takes an added block back with what another origin has linked to it since", async () => {
    const store = await opened();
    store.apply({ blocks: { add: { g: head(3) } } });
    store.apply({ links: { add: { dg: { from: "d", to: "g", input: "data" } } } }, "assistant");

    store.undo();
    expect(Object.hasOwn(store.board().blocks, "g")).toBe(false);
    expect(Object.hasOwn(store.board().links, "dg")).toBe(false);
  });

  test("puts a removed block back without its links to a block that is gone since", async () => {
    const store = await opened();
    store.apply({ blocks: { rm: ["b"] } });
    store.apply({ blocks: { rm: ["e"] } }, "assistant");

    store.undo();
    const board = store.board();
    expect(Object.hasOwn(board.blocks, "b")).toBe(true);
    expect(board.links.ab).toEqual({ from: "a", to: "b", input: "data" });
    expect(board.links.bc).toEqual({ from: "b", to: "c", input: "y" });
    expect(Object.hasOwn(board.links, "be")).toBe(false);
    expect(board.stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("a new step clears the redo steps of its own origin only", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    store.undo();
    store.apply({ options: { set: { n_rows: 20 } } }, "assistant");
    expect(store.canRedo()).toBe(true);

    store.apply({ name: "Again" });
    expect(store.canRedo()).toBe(false);
  });

  test("steps never merge", async () => {
    const store = await opened();
    store.apply({ name: "A" });
    store.apply({ name: "B" });
    store.undo();
    expect(store.board().name).toBe("A");
    store.undo();
    expect(store.board().name).toBe("Iris review");
  });

  test("redoes the deltas of a step in order, and announces each", async () => {
    const store = await opened();
    store.apply([
      { blocks: { add: { g: head(3) } } },
      { links: { add: { dg: { from: "d", to: "g", input: "data" } } } },
    ]);
    const after = store.board();
    store.undo();
    const seen = changes(store);

    store.redo();
    expect(store.board()).toEqual(after);
    expect(seen.map((change) => ("delta" in change ? Object.keys(change.delta) : []))).toEqual([
      ["blocks"],
      ["links"],
    ]);
  });
});

describe("registering", () => {
  test("core registers its attribute visible", () => {
    const store = newStore();
    expect(store.registered()).toContainEqual(
      expect.objectContaining({ name: "visible", on: "block" }),
    );
  });

  test("a key, an attribute or a board option id registered twice fails", () => {
    const store = newStore();
    store.register(positions);

    expect(() => store.register({ ...positions })).toThrow(
      "The key of the front-end state at extensions/blockr.dag-dag/positions is registered already.",
    );
    expect(() => store.register({ ...positions, path: ["extensions"] })).toThrow(
      "The key of the front-end state at extensions overlaps the one at extensions/blockr.dag-dag/positions, which is registered already.",
    );
    expect(() => store.register({ name: "visible", on: "block", schema: {} })).toThrow(
      'The attribute "visible" of blocks is registered already.',
    );
    expect(() =>
      store.options.register({ id: "n_rows", label: "Rows", category: "Table options", schema: {} }),
    ).toThrow(
      'The board option "n_rows" is registered already.',
    );
    expect(() => {
      store.registerMigration("blockr.foo", (saved) => saved);
      store.registerMigration("blockr.foo", (saved) => saved);
    }).toThrow("blockr.foo registers a migration, which it registered already.");
  });

  test("registered() returns the same list until a registration changes it", () => {
    const store = newStore();
    const before = store.registered();
    expect(store.registered()).toBe(before);

    const unregister = store.register(positions);
    const during = store.registered();
    expect(during).not.toBe(before);
    expect(during).toContain(positions);

    unregister();
    expect(store.registered()).toEqual(before);
    expect(() => store.register(positions)).not.toThrow();
  });

  test("a schema that Ajv cannot compile, or a default its schema refuses, fails", () => {
    const store = newStore();
    expect(() => store.register({ ...positions, schema: { type: "nope" } })).toThrow(
      /Ajv cannot compile the schema of the key of the front-end state at extensions\/blockr.dag-dag\/positions/,
    );
    expect(() =>
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer", default: "one" },
      }),
    ).toThrow('The schema of the board option "seed" refuses its default: must be integer.');
  });
});

test("the board's types are read-only", () => {
  // Never called: the compiler checks that each change fails.
  function change(board: Board, block: Block, stacked: Board["stacks"][string]): void {
    // @ts-expect-error: a board never changes in place.
    board.name = "Renamed";
    // @ts-expect-error: nor do its collections.
    board.blocks.a = head();
    // @ts-expect-error: nor what they hold.
    stacked.blocks[0] = "c";
    // @ts-expect-error: nor a block's state.
    block.state.n = 1;
  }
  expect(change).toBeTypeOf("function");
});
