// @vitest-environment node

// The store, with R's replies taken from the fixtures of the wire protocol.

import { describe, expect, test, vi } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import { coreOptions, hello, noUi, reply } from "../testing/store.ts";
import type { Block, Board } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { ROUNDS } from "./expand.ts";
import { createStore, StoreError } from "./index.ts";
import type {
  BoardClass,
  Change,
  Delta,
  Migration,
  Option,
  Patch,
  Store,
} from "./index.ts";

// A board of a class like the dock's, which lists the panels of
// each view and keeps what the dock's extensions carry by their ids,
// such as the DAG's positions, as the dock types its board.
interface DockBoard extends Board {
  readonly views: Readonly<Record<string, { readonly panels: readonly string[] }>>;
  readonly extensions: Readonly<
    Record<string, { readonly positions?: Readonly<Record<string, Position>> }>
  >;
}

interface Position {
  readonly x: number;
  readonly y: number;
}

const position: JsonSchema = {
  type: "object",
  properties: { x: { type: "number" }, y: { type: "number" } },
  required: ["x", "y"],
  additionalProperties: false,
};

// The dock's board class.
// Its update drops a removed block's panels, the last first,
// so that each index still names the panel it names on the board,
// and its position.
const dock: BoardClass = {
  class: "dock_board",
  parent: "board",
  properties: {
    views: {
      schema: {
        type: "object",
        additionalProperties: {
          type: "object",
          properties: { panels: { type: "array", items: { type: "string" } } },
          required: ["panels"],
          additionalProperties: false,
        },
      },
      default: {},
    },
    extensions: {
      schema: {
        type: "object",
        properties: {
          "blockr.dag-dag": {
            type: "object",
            properties: { positions: { type: "object", additionalProperties: position } },
          },
        },
      },
      default: {},
    },
  },
  update(delta, board) {
    const gone = new Set(delta.blocks?.rm ?? []);
    const { views, extensions } = board as DockBoard;
    const patches: Patch[] = [];
    for (const [view, { panels }] of Object.entries(views)) {
      for (let i = panels.length - 1; i >= 0; i--) {
        if (gone.has(panels[i] ?? "")) {
          patches.push({ op: "remove", path: `/views/${view}/panels/${String(i)}` });
        }
      }
    }
    for (const id of Object.keys(extensions["blockr.dag-dag"]?.positions ?? {})) {
      if (gone.has(id)) {
        patches.push({ op: "remove", path: `/extensions/blockr.dag-dag/positions/${id}` });
      }
    }
    return patches;
  },
};

// A class that extends the dock's with notes on blocks, by block id,
// a removed block's going with it.
const noted: BoardClass = {
  class: "noted_board",
  parent: "dock_board",
  properties: {
    notes: { schema: { type: "object", additionalProperties: { type: "string" } }, default: {} },
  },
  update(delta, board) {
    const notes = (board as DockBoard & { notes: Readonly<Record<string, string>> }).notes;
    return (delta.blocks?.rm ?? [])
      .filter((id) => Object.hasOwn(notes, id))
      .map((id) => ({ op: "remove", path: `/notes/${id}` }));
  },
};

function viewsOf(board: Board): DockBoard["views"] {
  return (board as DockBoard).views;
}

function positionsOf(board: Board): Readonly<Record<string, Position>> {
  return (board as DockBoard).extensions["blockr.dag-dag"]?.positions ?? {};
}

function newStore(): Store {
  const store = createStore();
  for (const option of coreOptions) store.options.register(option);
  return store;
}

async function opened(
  setup?: (store: Store) => void,
  replied: HelloReply = hello,
): Promise<Store> {
  const store = newStore();
  setup?.(store);
  await store.open(replied);
  return store;
}

// A store that has R's reply to `hello`, with which it opens a JSON board's
// text, once what `setup` registers is registered. It holds no board yet, since
// a store opens one board.
function unopened(
  setup?: (store: Store) => void,
  replied: HelloReply = hello,
): Store {
  const store = createStore({ hello: replied });
  for (const option of coreOptions) store.options.register(option);
  setup?.(store);
  return store;
}

// The reply to `hello` of a dock app, which loaded blockr.dock, whose method
// of R's `board_front_end()` names the dock's front-end,
// and whose opening board is a dock board with a view of the panels of a and b,
// and with their positions.
const docking = {
  ...hello,
  packages: { ...hello.packages, "blockr.dock": "0.2.1" },
  board_classes: {
    ...hello.board_classes,
    dock_board: { package: "blockr.dock", front_end: "blockr.dock-dock", version: 1 },
  },
  board: {
    ...hello.board,
    board: {
      ...hello.board.board,
      class: ["dock_board", "board"],
      views: { main: { panels: ["a", "b"] } },
      extensions: {
        "blockr.dag-dag": { positions: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } },
      },
    },
  },
} as unknown as HelloReply;

// A store that holds the board of a dock app, once the dock's class and what
// `setup` registers are registered.
function docked(setup?: (store: Store) => void): Promise<Store> {
  return opened(
    (store) => {
      store.register(dock);
      setup?.(store);
    },
    docking,
  );
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
  };
}

// The text of a JSON board of a core board, with what `board` gives in place of
// its parts, a board class's properties among them.
// Each block records version 1 of its class's state unless it gives another,
// the board version 1 of core's class unless it gives versions, and each board
// option version 1 of its value unless `optionVersions` gives another.
function file(
  board: Partial<Board> & Readonly<Record<string, unknown>>,
  packages: Readonly<Record<string, string>> = { "blockr.core": "0.2.0" },
  optionVersions: Readonly<Record<string, number>> = {},
): string {
  const given = {
    id: "amber_tapir",
    class: ["board"],
    name: "Cars",
    blocks: {
      a: {
        registry_id: "blockr.core-dataset_block",
        state: { dataset: "mtcars", package: "datasets" },
        name: "Cars",
      },
      b: head(),
    } as Readonly<Record<string, Block & { version?: number }>>,
    links: { ab: { from: "a", to: "b", input: "data" } },
    stacks: { s1: { name: "Input", blocks: ["a", "b"], color: null } },
    options: { n_rows: 50, show_conditions: ["warning", "error"] },
    ...board,
  };
  const versions = (board.versions as Readonly<Record<string, number>> | undefined) ?? { board: 1 };
  const entries: readonly [string, Block & { version?: number }][] = Object.entries(given.blocks);
  const blocks = Object.fromEntries(entries.map(([id, block]) => [id, { version: 1, ...block }]));
  const options = Object.fromEntries(
    Object.entries(given.options).map(([id, value]) => [
      id,
      { value, version: optionVersions[id] ?? 1 },
    ]),
  );
  return JSON.stringify({ format: 1, packages, board: { ...given, versions, blocks, options } });
}

describe("opening a board", () => {
  test("opens the JSON board R sent, with every id that R drew for it", async () => {
    const store = newStore();
    const seen = changes(store);
    const dropped = await store.open(hello);
    const board = store.board();

    expect(dropped).toEqual([]);
    expect(board.id).toBe("brisk_otter");
    expect(board.class).toEqual(["board"]);
    expect(board.name).toBe("Iris review");
    expect(Object.keys(board.blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
    expect(board.blocks.a).toEqual({
      registry_id: "blockr.core-dataset_block",
      state: { dataset: "iris", package: "datasets" },
      name: "Iris",
    });

    expect(Object.keys(board.links)).toEqual(["ab", "ac", "bc", "ad", "be", "de", "keen_finch"]);
    expect(board.links.keen_finch).toEqual({ from: "a", to: "f", input: "data" });
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"], color: null } });

    expect(store.blockRegistry()).toBe(hello.block_registry);
    expect(store.packages()).toEqual({ "blockr.core": "0.2.0" });
    expect(seen).toEqual([{ opened: board, dropped: [] }]);
    expect(store.canUndo()).toBe(false);
  });

  test("writes in no default of a board option that the board's class does not require", async () => {
    const store = newStore();
    await store.open(reply("hello-block") as HelloReply);
    const board = store.board();

    expect(board.name).toBe("Quick wren");
    expect(Object.keys(board.blocks)).toEqual(["tidy_crab"]);
    // It writes in those that core's board class requires.
    expect(board.options).toEqual({ n_rows: 50, show_conditions: ["warning", "error"], dark_mode: "light" });
  });

  test("opens the JSON board that serve() was given, which R passed on as it is", async () => {
    const store = newStore();
    await store.open(reply("hello-file") as HelloReply);

    expect(store.board().id).toBe("amber_tapir");
    expect(Object.keys(store.board().links)).toEqual(["ab"]);
  });

  test("opens a JSON board with the block registry of the reply to hello, dropping each block whose class it lacks", async () => {
    const store = unopened();
    const seen = changes(store);

    const dropped = await store.open(
      file(
        {
          blocks: {
            a: {
              registry_id: "blockr.core-dataset_block",
              state: { dataset: "mtcars", package: "datasets" },
              name: "Cars",
            },
            x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter" },
            b: head(),
          },
          links: {
            ax: { from: "a", to: "x", input: "data" },
            xb: { from: "x", to: "b", input: "data" },
          },
          stacks: { s1: { name: "Input", blocks: ["a", "x", "b"], color: null } },
        },
        { "blockr.core": "0.2.0", "blockr.dplyr": "0.3.0" },
      ),
    );

    expect(store.blockRegistry()).toBe(hello.block_registry);
    expect(store.packages()).toBe(hello.packages);
    expect(dropped).toEqual([
      { package: "blockr.dplyr", version: "0.3.0" },
      { block: "x", registry_id: "blockr.dplyr-filter_block", package: "blockr.dplyr" },
    ]);

    const board = store.board();
    expect(board.id).toBe("amber_tapir");
    expect(Object.keys(board.blocks)).toEqual(["a", "b"]);
    expect(board.links).toEqual({});
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"], color: null } });
    expect(seen).toEqual([{ opened: board, dropped }]);
  });

  test("refuses a file above the format it knows, naming both, and a file of today's stack", async () => {
    const store = unopened();

    await expect(store.open(JSON.stringify({ format: 2, packages: {}, board: {} }))).rejects.toThrow(
      /format is 2, while this client reads format 1 at most/,
    );
    await expect(store.open(JSON.stringify({ object: "board", payload: {} }))).rejects.toThrow(
      /no format, as a file of today's stack has none/,
    );
    await expect(store.open("{")).rejects.toThrow(StoreError);
    expect(store.hasBoard()).toBe(false);
  });

  test("refuses to open a board with an id made of digits alone, naming it", async () => {
    const store = unopened();
    await expect(store.open(file({ blocks: { 12: head() }, links: {}, stacks: {} }))).rejects.toThrow(
      /block id "12" is empty or made of digits alone/,
    );
  });

  test("refuses to open a board that fails a check, changing nothing", async () => {
    const store = unopened();
    const seen = changes(store);

    await expect(store.open(file({ links: { ab: { from: "a", to: "b", input: "x" } } }))).rejects.toThrow(
      /feeds the input "x" of block "b", whose class has no such input/,
    );
    await expect(
      store.open(file({ stacks: { s1: { name: "Input", blocks: ["a", "b"], color: "#66c2a5ff" } } })),
    ).rejects.toThrow(/stack "s1" has the colour "#66c2a5ff", which is no hex colour/);
    await expect(
      store.open(file({ stacks: { s1: { name: "Input", blocks: ["a", "b"] } } as unknown as Board["stacks"] })),
    ).rejects.toThrow(/stack "s1" has no string or null as its color/);
    expect(store.hasBoard()).toBe(false);
    expect(seen).toEqual([]);
  });

  test("opens a board whose stacks have hex colours, in either case, or none", async () => {
    const store = unopened();
    const stacks = {
      s1: { name: "Input", blocks: ["a"], color: "#66c2a5" },
      s2: { name: "Head", blocks: ["b"], color: "#FC8D62" },
      s3: { name: "Empty", blocks: [], color: null },
    };

    await store.open(file({ stacks }));

    expect(store.board().stacks).toStrictEqual(stacks);
  });

  test("refuses a JSON board on a locked board, and before R has answered hello", async () => {
    const store = unopened(undefined, reply("hello-locked") as HelloReply);

    await expect(store.open(file({}))).rejects.toThrow(/The board is locked, so it opens no other board/);
    await expect(store.check(file({}))).rejects.toThrow(/The board is locked, so it opens no other board/);
    expect(store.hasBoard()).toBe(false);

    await expect(newStore().open(file({}))).rejects.toThrow(
      /The store opens a JSON board only once R has answered `hello`/,
    );
  });

  test("runs what has to run before it opens a board, given the reply to hello and the board's class vector", async () => {
    const order: string[] = [];
    // A store that opens R's board, or a JSON board's text with R's reply.
    const make = (given?: HelloReply): Store => {
      const store = createStore({
        ...(given === undefined ? {} : { hello: given }),
        loaded(reply, classes) {
          order.push(`loaded ${classes.join(" ")}`);
          expect(reply).toBe(docking);
          store.register(dock);
          expect(store.boardClass(classes)).toBe(dock);
        },
      });
      store.subscribe(() => order.push("opened"));
      for (const option of coreOptions) store.options.register(option);
      return store;
    };

    await make().open(docking);
    const store = make(docking);
    await store.open(file({ class: ["dock_board", "board"], views: {}, extensions: {} }));

    expect(order).toEqual(["loaded dock_board board", "opened", "loaded dock_board board", "opened"]);
    expect(viewsOf(store.board())).toEqual({});
  });

  test("builds a board as the first class in its class vector that a package defines, dropping what the classes it skips add", async () => {
    const store = unopened((store) => store.register(dock));
    const dropped = await store.open(
      file({
        class: ["grid_board", "dock_board", "board"],
        views: { main: { panels: ["a"] } },
        extensions: {},
        panels: { p1: ["a"] },
      }),
    );

    expect(dropped).toEqual([]);
    const board = store.board();
    expect(board.class).toEqual(["grid_board", "dock_board", "board"]);
    expect(viewsOf(board)).toEqual({ main: { panels: ["a"] } });
    expect(Object.hasOwn(board, "panels")).toBe(false);
  });

  test("builds a board whose classes no package defines as core's, naming the packages that R could not load, and dropping what no package registered", async () => {
    const store = unopened();
    const dropped = await store.open(
      file(
        {
          class: ["dock_board", "board"],
          views: { main: { panels: ["a"] } },
          extensions: { "blockr.dag-dag": { positions: { a: { x: 0, y: 0 } } } },
          blocks: {
            a: {
              registry_id: "blockr.core-dataset_block",
              state: { dataset: "mtcars", package: "datasets" },
              name: "Cars",
            },
            b: head(),
          },
          options: { n_rows: 20, view_tabs: true },
        },
        { "blockr.core": "0.2.0", "blockr.dock": "0.2.1" },
      ),
    );

    expect(dropped).toEqual([{ package: "blockr.dock", version: "0.2.1" }]);

    // The board keeps every class, so that a dock app builds it as a dock
    // board again.
    const board = store.board();
    expect(board.class).toEqual(["dock_board", "board"]);
    expect(Object.keys(board)).toEqual(["id", "class", "name", "blocks", "links", "stacks", "options"]);
    expect(board.options).toEqual({ n_rows: 20, show_conditions: ["warning", "error"], dark_mode: "light" });
  });

  test("drops a property that its class does not add, without naming it", async () => {
    const store = unopened((store) => store.register(dock));
    const dropped = await store.open(
      file({ class: ["dock_board", "board"], views: {}, extensions: {}, zoom: 2 }),
    );

    expect(dropped).toEqual([]);
    expect(Object.hasOwn(store.board(), "zoom")).toBe(false);
  });

  test("gives each property of its class that the board lacks its default", async () => {
    const store = unopened((store) => store.register(dock));
    await store.open(file({ class: ["dock_board", "board"], views: { main: { panels: [] } } }));

    expect(viewsOf(store.board())).toEqual({ main: { panels: [] } });
    expect((store.board() as DockBoard).extensions).toEqual({});
  });

  test("gives the board options that its class and its parents require their defaults where it lacks them, as no step", async () => {
    const read = vi.fn(() => Promise.resolve(25));
    const store = unopened((store) => {
      store.options.register({
        id: "view_tabs",
        label: "View tabs",
        category: "Dock options",
        schema: { type: "boolean" },
        default: true,
        ui: noUi,
      });
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: read,
        ui: noUi,
      });
      store.register({ ...dock, options: ["view_tabs", "n_rows"] });
      store.register({ ...noted, options: ["page_rows"] });
    });

    await store.open(file({ class: ["noted_board", "board"], options: { n_rows: 20 } }));

    // Core's board class, the root of the chain, requires show_conditions and
    // dark_mode.
    expect(store.board().options).toEqual({ n_rows: 20, view_tabs: true, page_rows: 25, show_conditions: ["warning", "error"], dark_mode: "light" });
    expect(read).toHaveBeenCalledOnce();
    expect(store.canUndo()).toBe(false);
  });

  test("refuses a board whose class requires a board option that no package defines, naming both, before it calls a default", async () => {
    const read = vi.fn(() => Promise.resolve(25));
    const store = unopened((store) => {
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: read,
        ui: noUi,
      });
      store.register({ ...dock, options: ["view_tabs", "page_rows"] });
    });

    await expect(store.open(file({ class: ["dock_board", "board"] }))).rejects.toThrow(
      'The store refused to open the board\n- The board class "dock_board" requires the board option "view_tabs", which no loaded package defines.',
    );
    expect(read).not.toHaveBeenCalled();
    expect(store.hasBoard()).toBe(false);
  });

  test("refuses a board where the default of a board option that its class requires fails or does not fit its schema", async () => {
    const store = unopened((store) => {
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: () => Promise.reject(new Error("R has no page_rows.")),
        ui: noUi,
      });
      store.options.register({
        id: "view_tabs",
        label: "View tabs",
        category: "Dock options",
        schema: { type: "boolean" },
        default: () => Promise.resolve("yes"),
        ui: noUi,
      });
      store.register({ ...dock, options: ["page_rows", "view_tabs"] });
    });

    const refused = store.open(file({ class: ["dock_board", "board"] }));
    await expect(refused).rejects.toThrow(
      'The default of the board option "page_rows", which the board class "dock_board" requires, failed: R has no page_rows.',
    );
    await expect(refused).rejects.toThrow(
      'The default of the board option "view_tabs", which the board class "dock_board" requires, fails its schema: Instance type "string" is invalid. Expected "boolean".',
    );
    expect(store.hasBoard()).toBe(false);
  });

  test("refuses a board whose class is no class vector", async () => {
    const store = unopened();

    await expect(store.open(file({ class: [] }))).rejects.toThrow(
      /The board's class is no array of class names/,
    );
    await expect(store.open(file({ class: "board" } as unknown as Partial<Board>))).rejects.toThrow(
      StoreError,
    );
  });

  test("refuses a board whose property fails its schema", async () => {
    const store = unopened((store) => store.register(dock));
    await expect(
      store.open(file({ class: ["dock_board", "board"], views: { main: { panels: "a" } } })),
    ).rejects.toThrow(/The property "views" fails its schema at \/main\/panels: Instance type "string" is invalid\. Expected "array"\./);
  });

  test("opens one board, and refuses another once it holds one or while it opens one", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    const refusal =
      "The store refused to open the board\n- The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again.";

    await expect(store.open(hello)).rejects.toThrow(refusal);
    await expect(store.open(file({}))).rejects.toThrow(refusal);
    expect(store.board().name).toBe("Renamed");
    expect(store.canUndo()).toBe(true);

    const other = newStore();
    const first = other.open(hello);
    await expect(other.open(hello)).rejects.toThrow(refusal);
    await first;
    expect(other.board().name).toBe("Iris review");
  });

  test("opens a board once it has refused one, which it does not hold", async () => {
    const store = unopened();
    await expect(store.open(file({ class: [] }))).rejects.toThrow(StoreError);

    await store.open(file({}));
    expect(store.board().name).toBe("Cars");
  });

  test("ends at the same board whichever way the board comes in, and saves the JSON board R sent as it is, with the board options its class requires", async () => {
    // Unlocked, so that the store opens a JSON board on it.
    const replied: HelloReply = { ...(reply("hello-locked") as HelloReply), lock: null };

    // As R sent it.
    const viaR = newStore();
    await viaR.open(replied);
    const board = viaR.board();
    // With dark_mode at its default, which core's board class requires and the
    // board lacks.
    const { options } = replied.board.board;
    expect(viaR.jsonBoard()).toEqual({
      ...replied.board,
      board: { ...replied.board.board, options: { ...options, dark_mode: { value: "light", version: 1 } } },
    });

    // As the client saves it, which the user opens.
    const viaClient = unopened(undefined, replied);
    await viaClient.open(viaR.save());

    // As a user builds it on an empty board.
    const built = newStore();
    await built.open({
      ...replied,
      board: {
        ...replied.board,
        board: {
          ...board,
          versions: replied.board.board.versions,
          blocks: {},
          links: {},
          stacks: {},
          options: replied.board.board.options,
        },
      },
    });
    for (const [id, block] of Object.entries(board.blocks)) {
      built.apply({ blocks: { add: { [id]: block } } });
    }
    built.apply({ links: { add: board.links } });

    expect(viaClient.board()).toEqual(board);
    expect(built.board()).toEqual(board);
  });

  test("drops a block whose class the block registry lacks as a delta removes it, with what its board class keeps about it", async () => {
    const store = unopened((store) => store.register(dock));
    const dropped = await store.open(
      file({
        class: ["dock_board", "board"],
        views: { main: { panels: ["a", "x", "b"] } },
        extensions: {
          "blockr.dag-dag": {
            positions: { a: { x: 0, y: 0 }, x: { x: 1, y: 1 }, b: { x: 2, y: 2 } },
          },
        },
        blocks: {
          a: {
            registry_id: "blockr.core-dataset_block",
            state: { dataset: "mtcars", package: "datasets" },
            name: "Cars",
          },
          x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter" },
          b: head(),
        },
        links: {
          ax: { from: "a", to: "x", input: "data" },
          xb: { from: "x", to: "b", input: "data" },
        },
        stacks: { s1: { name: "Input", blocks: ["a", "x", "b"], color: null } },
      }),
    );

    expect(dropped).toEqual([
      { block: "x", registry_id: "blockr.dplyr-filter_block", package: "blockr.dplyr" },
    ]);
    const board = store.board();
    expect(viewsOf(board)).toEqual({ main: { panels: ["a", "b"] } });
    expect(positionsOf(board)).toEqual({ a: { x: 0, y: 0 }, b: { x: 2, y: 2 } });
    expect(board.links).toEqual({});
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"], color: null } });
  });

  test("drops a board option value that its schema refuses, so that it reads as its default", async () => {
    const store = unopened();
    const dropped = await store.open(
      file({ options: { n_rows: "fifty", show_conditions: ["warning", "error"] } }),
    );

    expect(dropped).toEqual([]);
    expect(store.board().options).toEqual({ show_conditions: ["warning", "error"], dark_mode: "light" });
  });

  test("checks a JSON board's text as it would open it, holding nothing and running nothing that has to run before an opening", async () => {
    const loaded = vi.fn();
    const store = createStore({ loaded });
    for (const option of coreOptions) store.options.register(option);
    store.register(dock);
    await store.open(docking);
    loaded.mockClear();
    const board = store.board();
    const seen = changes(store);

    const dropped = await store.check(
      file(
        {
          class: ["dock_board", "board"],
          views: { main: { panels: ["a", "x"] } },
          blocks: { a: head(), x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter" } },
          links: {},
          stacks: {},
        },
        { "blockr.core": "0.2.0", "blockr.dplyr": "0.3.0" },
      ),
    );

    expect(dropped).toEqual([
      { package: "blockr.dplyr", version: "0.3.0" },
      { block: "x", registry_id: "blockr.dplyr-filter_block", package: "blockr.dplyr" },
    ]);
    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
    expect(loaded).not.toHaveBeenCalled();
  });

  test("checks a JSON board's text with what is registered, rejecting with what would refuse it", async () => {
    const store = await opened((store) => store.register({ ...dock, options: ["view_tabs"] }));
    const board = store.board();

    await expect(store.check("{")).rejects.toThrow(StoreError);
    await expect(store.check(file({ class: ["dock_board", "board"] }))).rejects.toThrow(
      'The board class "dock_board" requires the board option "view_tabs", which no loaded package defines.',
    );
    expect(store.board()).toBe(board);

    // The head block's class at version 2 of its state, which migrates the
    // state that the file records at version 1.
    const head = hello.block_registry["blockr.core-head_block"];
    if (head === undefined) throw new Error("The fixture has no head block.");
    const migrating = unopened(
      (store) =>
        store.registerMigration("blockr.core-head_block", () => {
          throw new Error("The n is unreadable.");
        }),
      { ...hello, block_registry: { ...hello.block_registry, "blockr.core-head_block": { ...head, version: 2 } } },
    );
    await expect(migrating.check(file({}))).rejects.toThrow(
      'The migration of the block class "blockr.core-head_block" failed: The n is unreadable.',
    );
    expect(migrating.hasBoard()).toBe(false);
  });

  test("holds no board until it opens one", async () => {
    const store = newStore();
    expect(store.hasBoard()).toBe(false);
    expect(() => store.board()).toThrow("The store holds no board until it opens one.");
    expect(() => {
      store.apply({ name: "Built" });
    }).toThrow("The store rejected the delta\n- The store holds no board until it opens one.");
    expect(() => store.save()).toThrow("The store holds no board until it opens one.");
    expect(store.canUndo()).toBe(false);

    await store.open(hello);
    expect(store.hasBoard()).toBe(true);
  });
});

describe("a store that ends", () => {
  // Why an ended store refuses a change.
  const ended = "Another board opened in a new store, so this store changes its board no more.";

  test("drops its subscribers and refuses every change once its signal aborts", async () => {
    const controller = new AbortController();
    const store = createStore({ signal: controller.signal });
    for (const option of coreOptions) store.options.register(option);
    await store.open(hello);
    store.apply({ name: "Renamed" });
    const seen = changes(store);

    controller.abort();
    const late = changes(store);

    expect(() => {
      store.apply({ name: "Again" });
    }).toThrow(`The store rejected the delta\n- ${ended}`);
    expect(() => {
      store.undo();
    }).toThrow(`The store refused to undo the step\n- ${ended}`);
    await expect(store.open(file({}))).rejects.toThrow(ended);
    await expect(store.check(file({}))).rejects.toThrow(ended);
    expect(store.board().name).toBe("Renamed");
    expect(seen).toEqual([]);
    expect(late).toEqual([]);
  });

  test("refuses the board that it opens as its signal aborts", async () => {
    const controller = new AbortController();
    const store = createStore({
      signal: controller.signal,
      loaded: () => {
        controller.abort();
      },
    });
    for (const option of coreOptions) store.options.register(option);

    await expect(store.open(hello)).rejects.toThrow(ended);
    expect(store.hasBoard()).toBe(false);
  });
});

describe("saving a board", () => {
  test("writes the board it holds as the text of its JSON board, which another store opens to the same board", async () => {
    const store = await docked();
    store.apply([
      { name: "Renamed" },
      { blocks: { mod: { b: { state: { n: 3 } } } } },
      { patches: [{ op: "replace", path: "/views/main/panels", value: ["b", "a"] }] },
    ]);

    const text = store.save();
    expect(text).toBe(`${JSON.stringify(store.jsonBoard(), null, 2)}\n`);

    const other = unopened((store) => store.register(dock), docking);
    await other.open(text);
    expect(other.board()).toEqual(store.board());
    expect(other.board().name).toBe("Renamed");
    expect(other.board().blocks.b?.state).toEqual({ n: 3, direction: "head" });
    expect(viewsOf(other.board())).toEqual({ main: { panels: ["b", "a"] } });
  });
});

describe("migrations", () => {
  // A dock app whose dock board class is at version 3 of its properties and
  // whose head block class is at version 2 of its state, and whose opening
  // board R wrote at those versions.
  const replied: HelloReply = {
    ...docking,
    board_classes: {
      ...docking.board_classes,
      dock_board: { package: "blockr.dock", front_end: "blockr.dock-dock", version: 3 },
    },
    block_registry: Object.fromEntries(
      Object.entries(docking.block_registry).map(([id, entry]) => [
        id,
        id === "blockr.core-head_block" ? { ...entry, version: 2 } : entry,
      ]),
    ),
    board: {
      ...docking.board,
      board: {
        ...docking.board.board,
        versions: { dock_board: 3, board: 1 },
        blocks: Object.fromEntries(
          Object.entries(docking.board.board.blocks).map(([id, block]) => [
            id,
            block.registry_id === "blockr.core-head_block" ? { ...block, version: 2 } : block,
          ]),
        ),
      },
    },
  };

  // The dock's board class, whose versions before 3 kept the panels of a view
  // under `layout`.
  const migrating: BoardClass = {
    ...dock,
    migrate(properties, version) {
      const views = properties.views as Readonly<Record<string, { layout?: string[] }>>;
      if (version >= 3) return properties;
      return {
        ...properties,
        views: Object.fromEntries(
          Object.entries(views).map(([id, view]) => [id, { panels: view.layout ?? [] }]),
        ),
      };
    },
  };

  // A board of the dock app that the dock's version 2 and the head block's
  // version 1 saved, when the head block kept its n as a string.
  const old = file({
    class: ["dock_board", "board"],
    versions: { dock_board: 2, board: 1 },
    views: { main: { layout: ["a", "b"] } },
    extensions: {},
    blocks: {
      a: {
        registry_id: "blockr.core-dataset_block",
        state: { dataset: "mtcars", package: "datasets" },
        name: "Cars",
      },
      b: { ...head(), state: { n: "6", direction: "head" } },
    },
  });

  function store(head?: Migration): Store {
    return unopened((store) => {
      store.register(migrating);
      if (head !== undefined) store.registerMigration("blockr.core-head_block", head);
    }, replied);
  }

  test("migrates what a JSON board holds of an older version, by the version it records", async () => {
    const head = vi.fn<Migration>((state) => ({ ...state, n: Number(state.n) }));
    const opening = store(head);
    const dropped = await opening.open(old);

    expect(head).toHaveBeenCalledOnce();
    expect(head.mock.calls[0]?.[1]).toBe(1);
    expect(Object.isFrozen(head.mock.calls[0]?.[0])).toBe(true);

    const board = opening.board();
    expect(viewsOf(board)).toEqual({ main: { panels: ["a", "b"] } });
    expect(board.blocks.b?.state).toEqual({ n: 6, direction: "head" });
    expect(dropped).toEqual([]);

    // The board saves at today's versions.
    const saved = opening.jsonBoard();
    expect(saved.board.versions).toEqual({ dock_board: 3, board: 1 });
    expect(saved.board.blocks.b?.version).toBe(2);
    expect(saved.board.blocks.a?.version).toBe(1);
  });

  test("runs no migration of what a JSON board holds at today's version", async () => {
    const head = vi.fn<Migration>((state) => state);
    const viaR = store();
    await viaR.open(replied);
    await store(head).open(viaR.save());
    expect(head).not.toHaveBeenCalled();
  });

  test("refuses to open a board that a migration fails on, or that holds a version above today's", async () => {
    const opening = store(() => {
      throw new Error("The n is unreadable.");
    });
    await expect(opening.open(old)).rejects.toThrow(
      'The migration of the block class "blockr.core-head_block" failed: The n is unreadable.',
    );

    const newer = file({
      class: ["dock_board", "board"],
      versions: { dock_board: 4, board: 1 },
      views: { main: { panels: ["a", "b"] } },
      extensions: {},
    });
    await expect(store().open(newer)).rejects.toThrow(
      'The board records version 4 of the board class "dock_board", while R gives version 3.',
    );
  });

  test("refuses a JSON board without the versions it records", async () => {
    const opening = store();
    const json = JSON.parse(old) as { board: Record<string, unknown> };
    await expect(
      opening.open(JSON.stringify({ ...json, board: { ...json.board, versions: { dock_board: 0 } } })),
    ).rejects.toThrow("The board records its classes' versions as no whole numbers from 1 by class.");
    await expect(
      opening.open(file({ blocks: { a: { ...head(), version: "1" } as unknown as Block } })),
    ).rejects.toThrow('The block "a" records no version of its class\'s state.');
    await expect(
      opening.open(
        JSON.stringify({ ...json, board: { ...json.board, options: { n_rows: 50, seed: { value: 1 } } } }),
      ),
    ).rejects.toThrow(
      [
        'The board option "n_rows" is recorded as no value with the version of its shape.',
        'The board option "seed" is recorded as no value with the version of its shape.',
      ].join("\n- "),
    );
    await expect(
      opening.open(JSON.stringify({ ...json, board: { ...json.board, links: [], options: null } })),
    ).rejects.toThrow("The board holds its links and options in no object by id.");
  });
});

describe("migrating board option values", () => {
  // A board option whose versions before 2 held its value as a string.
  const fromString = (value: Json, version: number): Json => (version < 2 ? Number(value) : value);
  const seed: Option = {
    id: "seed",
    label: "Seed",
    category: "Board options",
    schema: { type: "integer" },
    default: 0,
    ui: noUi,
    migrate: fromString,
  };

  // A core app whose package gives the seed version 2 of its value.
  const replied: HelloReply = { ...hello, option_versions: { seed: 2 } };

  function store(option: Option = seed): Store {
    return unopened((store) => store.options.register(option), replied);
  }

  test("migrates a value of an older version by the board option's migrate, and saves it at today's", async () => {
    const migrate = vi.fn(fromString);
    const opening = store({ ...seed, migrate });
    await opening.open(file({ options: { seed: "42", n_rows: 50 } }, undefined, { seed: 1 }));

    expect(migrate).toHaveBeenCalledOnce();
    expect(migrate.mock.calls[0]).toEqual(["42", 1]);
    expect(opening.board().options).toEqual({ seed: 42, n_rows: 50, show_conditions: ["warning", "error"], dark_mode: "light" });

    const saved = opening.jsonBoard();
    expect(saved.board.options).toEqual({
      seed: { value: 42, version: 2 },
      n_rows: { value: 50, version: 1 },
      show_conditions: { value: ["warning", "error"], version: 1 },
      dark_mode: { value: "light", version: 1 },
    });
  });

  test("keeps a value without a migration, and drops one that then fails its schema", async () => {
    const opening = store({ ...seed, migrate: undefined });
    await opening.open(file({ options: { seed: 7 } }, undefined, { seed: 1 }));
    expect(opening.board().options).toEqual({ seed: 7, show_conditions: ["warning", "error"], dark_mode: "light" });

    const unfit = store({ ...seed, migrate: undefined });
    await unfit.open(file({ options: { seed: "seven" } }, undefined, { seed: 1 }));
    expect(unfit.board().options).toEqual({ show_conditions: ["warning", "error"], dark_mode: "light" });
  });

  test("refuses a board that holds a value above today's version, or that a migration fails on", async () => {
    const opening = store();
    await expect(
      opening.open(file({ options: { seed: 1 } }, undefined, { seed: 3 })),
    ).rejects.toThrow('The board records version 3 of the value of the board option "seed", while R gives version 2.');

    const failing = store({
      ...seed,
      migrate: () => {
        throw new Error("The seed is unreadable.");
      },
    });
    await expect(
      failing.open(file({ options: { seed: "x" } }, undefined, { seed: 1 })),
    ).rejects.toThrow('The migration of the board option "seed" failed: The seed is unreadable.');

    const empty = store({ ...seed, migrate: () => undefined as unknown as Json });
    await expect(
      empty.open(file({ options: { seed: "x" } }, undefined, { seed: 1 })),
    ).rejects.toThrow('The migration of the board option "seed" returned nothing.');
  });

  test("takes a board option that R gives no version as at version 1", async () => {
    const opening = unopened((store) => store.options.register(seed));
    await expect(
      opening.open(file({ options: { seed: 1 } }, undefined, { seed: 2 })),
    ).rejects.toThrow('The board records version 2 of the value of the board option "seed", while R gives version 1.');
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
    expect(after.class).toBe(before.class);
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

  test("expands a delta through the update of the board's class, and one undo takes all of it back", async () => {
    const store = await docked();
    const before = store.board();
    const seen = changes(store);

    store.apply({ blocks: { rm: ["b"] } });
    expect(seen[0]).toMatchObject({
      delta: {
        patches: [
          { op: "remove", path: "/views/main/panels/1" },
          { op: "remove", path: "/extensions/blockr.dag-dag/positions/b" },
        ],
      },
    });
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
    expect(positionsOf(store.board())).toEqual({ a: { x: 0, y: 0 } });

    store.undo();
    expect(store.board()).toEqual(before);
    expect(Object.keys(store.board().links)).toEqual(Object.keys(before.links));
  });

  test("runs the update of the board's class and of each of its parents, each for its own properties", async () => {
    const store = unopened((store) => {
      store.register(dock);
      store.register(noted);
    });
    await store.open(
      file({
        class: ["noted_board", "board"],
        views: { main: { panels: ["a", "b"] } },
        notes: { a: "Raw", b: "First rows" },
      }),
    );

    store.apply({ blocks: { rm: ["b"] } });
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
    expect((store.board() as Board & { notes: unknown }).notes).toEqual({ a: "Raw" });
  });

  test("leaves out what an update returns at a path that the delta's own patches change", async () => {
    // A class that notes every block added, in place of a note the delta gives.
    const store = unopened((store) => {
      store.register(dock);
      store.register({
        ...noted,
        update: (delta) =>
          Object.keys(delta.blocks?.add ?? {}).map((id) => ({
            op: "add",
            path: `/notes/${id}`,
            value: "New",
          })),
      });
    });
    await store.open(file({ class: ["noted_board", "board"] }));

    store.apply({
      blocks: { add: { g: head(3), h: head(4) } },
      patches: [{ op: "add", path: "/notes/g", value: "Pasted" }],
    });
    expect((store.board() as Board & { notes: unknown }).notes).toEqual({ g: "Pasted", h: "New" });
  });

  test("rejects a delta that keeps expanding, naming the board classes that kept adding", async () => {
    const counting: BoardClass = {
      class: "counting_board",
      parent: "board",
      properties: { count: { schema: { type: "integer" }, default: 0 } },
      update(delta) {
        const last = delta.patches?.filter(({ path }) => path === "/count").at(-1);
        return [{ op: "replace", path: "/count", value: Number(last?.value ?? 0) + 1 }];
      },
    };
    const store = unopened((store) => store.register(counting));
    await store.open(file({ class: ["counting_board", "board"] }));

    expect(() => {
      store.apply({ name: "Renamed" });
    }).toThrow(new RegExp(`kept expanding after ${String(ROUNDS)} rounds, through the board class "counting_board"`));
  });

  test("rejects a delta that an update follows with a patch outside the properties its class adds", async () => {
    const store = unopened((store) => {
      store.register(dock);
      store.register({ ...noted, update: () => [{ op: "replace", path: "/views", value: {} }] });
    });
    await store.open(file({ class: ["noted_board", "board"] }));

    expect(() => {
      store.apply({ name: "Renamed" });
    }).toThrow(
      'The board class "noted_board" updated the delta with a patch at "/views", outside the properties it adds.',
    );
  });

  const refusals: [string, Delta, RegExp][] = [
    ["a link from a block the board lacks", { links: { add: { zb: { from: "z", to: "e", input: "" } } } }, /comes from block "z", which the board lacks/],
    ["a link into an input the class lacks", { links: { add: { ab2: { from: "a", to: "b", input: "x" } } } }, /feeds the input "x" of block "b", whose class has no such input/],
    ["a second link into a fixed input", { links: { add: { db: { from: "d", to: "b", input: "data" } } } }, /input "data" of block "b" takes the links "ab" and "db", where it takes one/],
    ["a cycle", { links: { rm: ["ad"], add: { ed: { from: "e", to: "d", input: "data" } } } }, /links form a cycle through the blocks/],
    ["a stack holding a block the board lacks", { stacks: { add: { s2: { name: "More", blocks: ["z"], color: null } } } }, /stack "s2" holds block "z", which the board lacks/],
    ["a block in two stacks", { stacks: { add: { s2: { name: "More", blocks: ["a"], color: null } } } }, /block "a" sits in the stacks "s1" and "s2", where it sits in one/],
    ["a board option value its schema refuses", { options: { set: { n_rows: -1 } } }, /value of the board option "n_rows" fails its schema: -1 is less than 1\./],
    ["a value of a board option no package registered", { options: { set: { nope: 1 } } }, /board option "nope", which no package registered/],
    ["a block of a class the block registry lacks", { blocks: { add: { g: { ...head(), registry_id: "blockr.foo-bar" } } } }, /class "blockr.foo-bar", which the block registry lacks/],
    ["an id made of digits alone", { blocks: { add: { 7: head() } } }, /block id "7" is empty or made of digits alone/],
    ["removing a block the board lacks", { blocks: { rm: ["z"] } }, /board holds no block "z" to remove/],
    ["adding a block the board holds", { blocks: { add: { a: head() } } }, /board holds block "a" already/],
    ["changing a block the board lacks", { blocks: { mod: { z: { name: "Z" } } } }, /board holds no block "z" to change/],
    ["setting and unsetting one state arg", { blocks: { mod: { b: { state: { n: 1 }, unset: ["n"] } } } }, /both sets and unsets "n"/],
    ["setting and unsetting one board option", { options: { set: { n_rows: 5 }, unset: ["n_rows"] } }, /both sets and unsets the board option "n_rows"/],
    ["a patch of a property that the board's class does not add", { patches: [{ op: "add", path: "/views/main", value: {} }] }, /The patch 1, add at "\/views\/main", changes "views", which the board class "board" does not add\./],
    ["a patch of core's properties", { patches: [{ op: "replace", path: "/name", value: "Renamed" }] }, /changes "name", which the board class "board" does not add/],
    ["a patch that is no patch", { patches: [{ op: "move", path: "views" }] } as unknown as Delta, /The patch 1 has the op "move", where it takes "add", "remove" or "replace", has the path "views", which is no JSON Pointer below the board's top\./],
    ["a block that is no block", { blocks: { add: { g: { ...head(), name: 3 } } } } as unknown as Delta, /block "g" has no string as its name/],
    ["a link holding more than core keeps", { links: { add: { dg: { from: "d", to: "f", input: "data", color: "red" } } } } as unknown as Delta, /link "dg" holds "color", which core does not keep/],
    ["a stack whose colour is no hex colour", { stacks: { add: { s2: { name: "More", blocks: [], color: "red" } } } }, /stack "s2" has the colour "red", which is no hex colour of six digits, such as "#66c2a5"\./],
    ["a stack whose colour is a hex colour in short", { stacks: { add: { s2: { name: "More", blocks: [], color: "#fc8" } } } }, /stack "s2" has the colour "#fc8", which is no hex colour of six digits/],
    ["a stack whose colour is no string", { stacks: { mod: { s1: { color: 1 } } } } as unknown as Delta, /stack "s1" has no string or null as its color/],
    ["a stack without a colour", { stacks: { add: { s2: { name: "More", blocks: [] } } } } as unknown as Delta, /stack "s2" has no string or null as its color/],
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

  test("checks the property that a patch changes against its schema, and applies no patch that does not find its path", async () => {
    const store = await docked();
    const board = store.board();

    expect(() => {
      store.apply({ patches: [{ op: "add", path: "/extensions/blockr.dag-dag/positions/c", value: { x: "left", y: 0 } }] });
    }).toThrow(/The property "extensions" fails its schema at \/blockr.dag-dag\/positions\/c\/x: Instance type "string" is invalid\. Expected "number"\./);
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/views/side/panels", value: [] }] });
    }).toThrow('The patch 1, replace at "/views/side/panels", finds no "side" in "/views".');
    expect(() => {
      store.apply({ patches: [{ op: "remove", path: "/views" }] });
    }).toThrow('The patch 1 removes the property "views", which a board of the class "dock_board" always holds.');
    expect(store.board()).toBe(board);
  });

  test("applies patches in their order, after core's parts", async () => {
    const store = await docked();

    store.apply({
      blocks: { add: { g: head(3) } },
      patches: [
        { op: "add", path: "/views/side", value: { panels: [] } },
        { op: "add", path: "/views/side/panels/-", value: "g" },
        { op: "add", path: "/views/side/panels/0", value: "a" },
        { op: "replace", path: "/views/main/panels/1", value: "g" },
      ],
    });
    expect(viewsOf(store.board())).toEqual({
      main: { panels: ["a", "g"] },
      side: { panels: ["a", "g"] },
    });
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

  test("places an added link where the link that `before` names was, where the same delta removes that link", async () => {
    const store = await opened();
    const order = Object.keys(store.board().links);

    store.apply({
      blocks: { add: { g: head(3) } },
      links: { rm: ["be"], add: { ge: { from: "g", to: "e", input: "" } }, before: { ge: "be" } },
    });
    expect(Object.keys(store.board().links)).toEqual(order.map((id) => (id === "be" ? "ge" : id)));

    store.undo();
    expect(Object.keys(store.board().links)).toEqual(order);

    const last = order.at(-1) ?? "";
    store.apply({
      links: { rm: [last], add: { af: { from: "a", to: "f", input: "data" } }, before: { af: last } },
    });
    expect(Object.keys(store.board().links).at(-1)).toBe("af");
  });

  test("refuses a link to go before one that the board lacks", async () => {
    const store = await opened();
    expect(() => {
      store.apply({ links: { add: { ce: { from: "c", to: "e", input: "" } }, before: { ce: "zz" } } });
    }).toThrow('The board holds no link "zz" for link "ce" to go before.');
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

  test("makes no step for a delta that changes nothing, and tells no one", async () => {
    const store = await docked();
    const board = store.board();
    const edits = [store.editId({ block: "b" }), store.editId({ property: "views" })];
    const seen = changes(store);

    store.apply(
      [
        { name: "Iris review" },
        { blocks: { mod: { b: { state: { n: 6 }, unset: ["nothing"], name: "Head" } } } },
        { links: { mod: { ab: { input: "data" } } }, stacks: { mod: { s1: { blocks: ["a", "b"] } } } },
        { options: { set: { n_rows: 50 }, unset: ["zoom"] } },
        {
          patches: [
            { op: "replace", path: "/views/main/panels/0", value: "a" },
            { op: "add", path: "/views/main", value: { panels: ["a", "b"] } },
          ],
        },
      ],
      "assistant",
    );

    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
    expect(store.canUndo("assistant")).toBe(false);
    expect([store.editId({ block: "b" }), store.editId({ property: "views" })]).toEqual(edits);
  });

  test("keeps in a step only what it changes, so that its undo sets back nothing else", async () => {
    const store = await opened();

    store.apply({ blocks: { mod: { b: { state: { n: 6, direction: "tail" } } } } });
    store.apply({ blocks: { mod: { b: { state: { n: 10 } } } } }, "assistant");
    store.undo();

    expect(store.board().blocks.b?.state).toEqual({ n: 10, direction: "head" });
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

  test("are kept for each property of the board's class, which a patch names", async () => {
    const store = await docked();
    const views = store.editId({ property: "views" });
    const extensions = store.editId({ property: "extensions" });
    expect(views).toBeTypeOf("number");

    store.apply({ patches: [{ op: "add", path: "/views/side", value: { panels: [] } }] });
    expect(store.editId({ property: "extensions" })).toBe(extensions);
    const edit = store.editId({ property: "views" });
    expect(edit).toBeGreaterThan(views ?? Infinity);

    store.apply({ patches: [{ op: "replace", path: "/views/side/panels", value: ["a"], edit }] });
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/views/side/panels", value: ["b"], edit }] });
    }).toThrow(/built on edit \d+ of the property "views", which another edit has replaced with edit \d+\.$/);
    expect(viewsOf(store.board()).side).toEqual({ panels: ["a"] });
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
    const store = await opened();
    const before = store.board();

    store.apply({ blocks: { mod: { d: { state: { extra: 1 } }, a: { name: "Renamed" } } } });
    store.undo();

    expect(store.board()).toEqual(before);
    expect(Object.hasOwn(store.board().blocks.d?.state ?? {}, "extra")).toBe(false);
  });

  test("leaves the board option values that its step set or removed, and so does redo, while both take back and apply again the rest of the step", async () => {
    const store = await opened((store) => {
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
        default: 0,
        ui: noUi,
      });
    });
    const seen = changes(store);

    store.apply([
      { options: { set: { n_rows: 20, seed: 42 } } },
      { blocks: { mod: { b: { state: { n: 7 } } } }, options: { unset: ["show_conditions"] }, name: "Renamed" },
    ]);
    const { options } = store.board();
    expect(options).toEqual({ n_rows: 20, seed: 42, dark_mode: "light" });

    store.undo();
    expect([store.board().blocks.b?.state.n, store.board().name]).toEqual([6, "Iris review"]);
    expect(store.board().options).toBe(options);

    store.redo();
    expect([store.board().blocks.b?.state.n, store.board().name]).toEqual([7, "Renamed"]);
    expect(store.board().options).toBe(options);

    // What the undo and the redo announce holds no board option values.
    expect(seen.slice(2)).toEqual([
      { delta: { blocks: { mod: { b: { state: { n: 6 } } } }, name: "Iris review" }, origin: "user" },
      { delta: { blocks: { mod: { b: { state: { n: 7 } } } }, name: "Renamed" }, origin: "user" },
    ]);
  });

  test("a step that sets or removes only board option values is no undo step, and leaves the origin's undo and redo steps as they are", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    store.apply({ name: "Again" });
    store.undo();
    const seen = changes(store);

    const delta: Delta = { options: { set: { n_rows: 20 }, unset: ["show_conditions"] } };
    store.apply(delta);
    expect(seen).toEqual([{ delta, origin: "user" }]);
    expect([store.canUndo(), store.canRedo()]).toEqual([true, true]);

    store.redo();
    expect(store.board().name).toBe("Again");
    store.undo();
    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.canUndo()).toBe(false);
    expect(store.board().options).toEqual({ n_rows: 20, dark_mode: "light" });
  });

  test("sets back a stack's colour, null for none, by steps that survive JSON", async () => {
    const store = await opened();
    const seen = changes(store);
    store.apply({ stacks: { mod: { s1: { color: "#66c2a5" } } } });
    store.apply({ stacks: { mod: { s1: { color: null } } } });
    expect(store.board().stacks.s1?.color).toBeNull();

    store.undo();
    expect(store.board().stacks.s1?.color).toBe("#66c2a5");
    store.undo();
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a", "b"], color: null });

    const undone = seen.slice(2);
    expect(undone).toEqual([
      { delta: { stacks: { mod: { s1: { color: "#66c2a5" } } } }, origin: "user" },
      { delta: { stacks: { mod: { s1: { color: null } } } }, origin: "user" },
    ]);
    expect(JSON.parse(JSON.stringify(undone))).toEqual(undone);
  });

  test("takes back a step's patches by those that invert them, in the order that does, which survive JSON", async () => {
    const store = await docked();
    const before = store.board();
    const seen = changes(store);

    store.apply({
      patches: [
        { op: "add", path: "/views/side", value: { panels: ["a"] } },
        { op: "add", path: "/views/main/panels/0", value: "c" },
        { op: "replace", path: "/extensions/blockr.dag-dag/positions/a", value: { x: 5, y: 5 } },
      ],
    });
    store.undo();

    expect(store.board()).toEqual(before);
    const undo = seen[1];
    expect(undo && "delta" in undo ? JSON.parse(JSON.stringify(undo.delta)) : undefined).toEqual({
      patches: [
        { op: "replace", path: "/extensions/blockr.dag-dag/positions/a", value: { x: 0, y: 0 } },
        { op: "remove", path: "/views/main/panels/0" },
        { op: "remove", path: "/views/side" },
      ],
    });
  });

  test("puts a member that a patch removed back in its place, before the member that followed it", async () => {
    const store = await docked();
    const order = () => Object.keys(viewsOf(store.board()));
    store.apply({
      patches: [
        { op: "add", path: "/views/side", value: { panels: [] } },
        { op: "add", path: "/views/data", value: { panels: [] } },
      ],
    });
    expect(order()).toEqual(["main", "side", "data"]);

    store.apply({
      patches: [
        { op: "remove", path: "/views/side" },
        { op: "remove", path: "/views/main" },
      ],
    });
    expect(order()).toEqual(["data"]);
    store.undo();
    expect(order()).toEqual(["main", "side", "data"]);

    // Redo removes them again, and the undo after it puts them back again.
    store.redo();
    store.undo();
    expect(order()).toEqual(["main", "side", "data"]);
  });

  test("puts a removed member back before the member that followed it where another origin changed the object since, and last where that member is gone", async () => {
    const store = await docked();
    const order = () => Object.keys(viewsOf(store.board()));
    store.apply(
      {
        patches: [
          { op: "add", path: "/views/side", value: { panels: [] } },
          { op: "add", path: "/views/data", value: { panels: [] } },
        ],
      },
      "setup",
    );

    store.apply({ patches: [{ op: "remove", path: "/views/side" }] });
    store.apply({ patches: [{ op: "add", path: "/views/extra", value: { panels: [] } }] }, "assistant");
    store.undo();
    expect(order()).toEqual(["main", "side", "data", "extra"]);

    store.apply({ patches: [{ op: "remove", path: "/views/side" }] });
    store.apply({ patches: [{ op: "remove", path: "/views/data" }] }, "assistant");
    store.undo();
    expect(order()).toEqual(["main", "extra", "side"]);
  });

  test("sets back a property over what another origin changed since, and leaves out the patches whose path is gone", async () => {
    const store = await docked();
    store.apply({
      patches: [
        { op: "add", path: "/views/side", value: { panels: ["a"] } },
        { op: "replace", path: "/views/main/panels", value: ["b", "a"] },
      ],
    });
    store.apply(
      {
        patches: [
          { op: "remove", path: "/views/side" },
          { op: "replace", path: "/views/main/panels", value: ["a"] },
        ],
      },
      "assistant",
    );

    store.undo();
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a", "b"] } });

    // Redo brings back the board as it was before the undo.
    store.redo();
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
  });

  test("undoes each origin's steps apart", async () => {
    const store = await opened();
    store.apply({ name: "Renamed" });
    store.apply({ stacks: { mod: { s1: { name: "Inputs" } } } }, "assistant");

    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.board().stacks.s1?.name).toBe("Inputs");
    expect(store.canUndo("assistant")).toBe(true);

    store.undo("assistant");
    expect(store.board().stacks.s1?.name).toBe("Input");
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
    store.apply({ stacks: { mod: { s1: { name: "Inputs" } } } }, "assistant");
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
  test("core registers its board class board", () => {
    const store = newStore();
    expect(store.registered()).toEqual([{ class: "board", options: ["show_conditions", "dark_mode"], properties: {} }]);
  });

  test("a board class, a board option id or a block class's migration registered twice fails", () => {
    const store = newStore();
    store.register(dock);

    expect(() => store.register({ ...dock })).toThrow('The board class "dock_board" is registered already.');
    expect(() =>
      store.options.register({ id: "n_rows", label: "Rows", category: "Table options", schema: {}, default: 50, ui: noUi }),
    ).toThrow(
      'The board option "n_rows" is registered already.',
    );
    expect(() => {
      store.registerMigration("blockr.foo-bar_block", (state) => state);
      store.registerMigration("blockr.foo-bar_block", (state) => state);
    }).toThrow('The block class "blockr.foo-bar_block" has a migration registered already.');
  });

  test("a board class extends a registered class, and adds no property that core or its parents hold", () => {
    const store = newStore();

    expect(() => store.register({ ...dock, parent: "grid_board" })).toThrow(
      'The board class "dock_board" extends "grid_board", which no package registered.',
    );
    expect(() => store.register({ ...dock, parent: undefined })).toThrow(
      'The board class "dock_board" names no parent, which every board class but core\'s "board" does.',
    );
    expect(() => store.register({ ...dock, properties: { name: { schema: {}, default: "" } } })).toThrow(
      'The board class "dock_board" adds the property "name", which every board holds as core\'s.',
    );

    store.register(dock);
    expect(() =>
      store.register({ ...noted, properties: { views: { schema: {}, default: {} } } }),
    ).toThrow('The board class "noted_board" adds the property "views", which its parent "dock_board" adds already.');
  });

  test("gives the class that a board of a class vector is built as", () => {
    const store = newStore();
    store.register(dock);

    expect(store.boardClass(["grid_board", "dock_board", "board"])).toBe(dock);
    expect(store.boardClass(["grid_board"]).class).toBe("board");

    // A class whose parent is no longer registered is skipped.
    const unregister = store.register(noted);
    expect(store.boardClass(["noted_board", "board"])).toBe(noted);
    store.register({ ...noted, class: "sticky_board", parent: "noted_board", properties: {} });
    unregister();
    expect(store.boardClass(["sticky_board", "dock_board"])).toBe(dock);
  });

  test("gives the front-end that R names for the class a board is built as, or for its nearest parent", async () => {
    const store = await docked((store) => store.register(noted));

    expect(store.frontEnd(["dock_board", "board"])).toBe("blockr.dock-dock");
    // R names no front-end for noted_board, which its parent's draws.
    expect(store.frontEnd(["noted_board", "board"])).toBe("blockr.dock-dock");
    expect(store.frontEnd(["grid_board"])).toBe("blockr.core-basic");

    expect(() => newStore().frontEnd(["board"])).toThrow(
      "The store knows the front-ends of board classes only once R has answered `hello`.",
    );
  });

  test("saves a board with the package of each class it is built as that R names, and of each block's class", async () => {
    const store = unopened((store) => {
      store.register(dock);
      store.register(noted);
    }, docking);
    await store.open(
      JSON.stringify({ ...docking.board, board: { ...docking.board.board, class: ["noted_board", "dock_board", "board"] } }),
    );

    // R names no front-end for noted_board, which so comes from no package.
    expect(store.jsonBoard().packages).toEqual({ "blockr.core": "0.2.0", "blockr.dock": "0.2.1" });
  });

  test("registered() returns the same list until a registration changes it", () => {
    const store = newStore();
    const before = store.registered();
    expect(store.registered()).toBe(before);

    const unregister = store.register(dock);
    const during = store.registered();
    expect(during).not.toBe(before);
    expect(during).toContain(dock);

    unregister();
    expect(store.registered()).toEqual(before);
    expect(() => store.register(dock)).not.toThrow();
  });

  test("a board option without a UI module, or with an onChange that is no function, fails", () => {
    const store = newStore();
    const seed = { id: "seed", label: "Seed", category: "Board options", schema: {}, default: 1 };
    // As a package that writes its JS without types may register them.
    const register = (def: unknown) => store.options.register(def as Option);

    expect(() => register(seed)).toThrow('The board option "seed" has no UI module with a render() function.');
    expect(() => register({ ...seed, ui: {} })).toThrow(
      'The board option "seed" has no UI module with a render() function.',
    );
    expect(() => register({ ...seed, ui: noUi, onChange: "dark" })).toThrow(
      'The onChange of the board option "seed" is no function.',
    );
    expect(() => register({ ...seed, ui: noUi, onChange: () => undefined })).not.toThrow();
  });

  test("a default that its schema refuses fails", () => {
    const store = newStore();
    expect(() =>
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
        default: "one",
        ui: noUi,
      }),
    ).toThrow(
      'The default of the board option "seed" fails its schema: Instance type "string" is invalid. Expected "integer".',
    );
    expect(() =>
      store.register({ ...dock, properties: { views: { schema: { type: "object" }, default: [] } } }),
    ).toThrow(
      'The default of the property "views" of the board class "dock_board" fails its schema: Instance type "array" is invalid. Expected "object".',
    );
  });

  test("a board class's update or migrate, or a board option's migrate, that is no function fails", () => {
    const store = newStore();
    expect(() => store.register({ ...dock, update: "views" } as unknown as BoardClass)).toThrow(
      'The board class "dock_board" has an update that is no function.',
    );
    expect(() => store.register({ ...dock, migrate: 3 } as unknown as BoardClass)).toThrow(
      'The board class "dock_board" has a migrate that is no function.',
    );
    expect(() =>
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
        default: 0,
        ui: noUi,
        migrate: "yes",
      } as unknown as Option),
    ).toThrow('The board option "seed" has a migrate that is no function.');
  });

  test("leaves a default that is a function to whatever needs the board option first", () => {
    const store = newStore();
    const read = vi.fn(() => Promise.resolve<number>(25));
    store.options.register({
      id: "page_rows",
      label: "Rows a page shows",
      category: "Table options",
      schema: { type: "integer" },
      default: read,
      ui: noUi,
    });
    expect(read).not.toHaveBeenCalled();
    expect(store.options.registered().page_rows?.default).toBe(read);
  });

  test("reads the draft a schema declares", () => {
    const store = newStore();
    store.options.register({
      id: "seed",
      label: "Seed",
      category: "Board options",
      schema: { $schema: "http://json-schema.org/draft-07/schema#", type: "integer" },
      default: 1,
      ui: noUi,
    });
    expect(store.options.registered().seed?.default).toBe(1);
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
