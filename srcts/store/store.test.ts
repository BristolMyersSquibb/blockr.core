// @vitest-environment node

// The store, with R's replies taken from the fixtures of the wire protocol.

import { describe, expect, test, vi } from "vitest";
import type { ErrorReply, HelloReply, OpenReply } from "../protocol/index.ts";
import { coreOptions, fakeR, hello, loaded, reply } from "../testing/store.ts";
import type { Block, Board, OpeningBoard, SavedBoard } from "../types/board.ts";
import type { JsonSchema } from "../types/json.ts";
import { ROUNDS } from "./expand.ts";
import { createStore, isId, sentenceCase, StoreError } from "./index.ts";
import type {
  BoardClass,
  Change,
  Delta,
  Migration,
  Patch,
  Store,
  StoreOptions,
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

type Client = StoreOptions["client"];

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
  frontEnd: "blockr.dock-dock",
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
  frontEnd: "blockr.dock-dock",
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

function newStore(client: Client = fakeR()): Store {
  const store = createStore({ client });
  for (const option of coreOptions) store.options.register(option);
  return store;
}

async function opened(
  setup?: (store: Store) => void,
  client: Client = fakeR(),
  replied: HelloReply = hello,
): Promise<Store> {
  const store = newStore(client);
  setup?.(store);
  await store.open(replied);
  return store;
}

// The reply to `hello` of a dock app,
// whose opening board is a dock board with a view of the panels of a and b,
// and with their positions.
const docking = {
  ...hello,
  board: {
    ...hello.board,
    class: ["dock_board", "board"],
    views: { main: { panels: ["a", "b"] } },
    extensions: {
      "blockr.dag-dag": { positions: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } },
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
    fakeR(),
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
    attrs: {},
  };
}

// The text of a saved file of a core board, with what `board` gives in place of
// its parts, a board class's properties among them.
function file(
  board: Partial<Board> & Readonly<Record<string, unknown>>,
  packages: Readonly<Record<string, string>> = { "blockr.core": "0.2.0" },
): string {
  const saved = {
    format: 1,
    packages,
    board: {
      id: "amber_tapir",
      class: ["board"],
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
    expect(board.class).toEqual(["board"]);
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

  test("names a board without a name after its id, and writes in no default of a board option that its class does not require", async () => {
    const store = newStore();
    await store.open(reply("hello-block") as HelloReply);
    const board = store.board();

    expect(board.name).toBe(sentenceCase(board.id));
    expect(Object.keys(board.blocks)).toHaveLength(1);
    expect(isId(Object.keys(board.blocks)[0])).toBe(true);
    expect(board.options).toEqual({ n_rows: 50 });
  });

  test("opens the saved file that serve() was given without asking R again", async () => {
    const client = fakeR();
    const store = newStore(client);
    await store.open(reply("hello-file") as HelloReply);

    expect(client.sent).toEqual([]);
    expect(store.board().id).toBe("amber_tapir");
    expect(Object.keys(store.board().links)).toEqual(["ab"]);
  });

  test("has R load the packages a saved file names, and drops the blocks whose class the block registry lacks", async () => {
    const client = fakeR();
    const store = await opened(undefined, client);
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

    expect(client.sent).toEqual([{ kind: "open", packages: ["blockr.core", "blockr.dplyr"] }]);
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

  test("runs what has to run once R has loaded the packages, given the board's class vector, before it opens the board", async () => {
    const order: string[] = [];
    const store = createStore({
      client: fakeR(),
      loaded(reply, classes) {
        order.push(`loaded ${classes.join(" ")}`);
        expect(reply).toBe(docking);
        store.register(dock);
        expect(store.boardClass(classes)).toBe(dock);
      },
    });
    store.subscribe(() => order.push("opened"));
    for (const option of coreOptions) store.options.register(option);

    await store.open(docking);

    expect(order).toEqual(["loaded dock_board board", "opened"]);
    expect(positionsOf(store.board())).toEqual({ a: { x: 0, y: 0 }, b: { x: 1, y: 1 } });
  });

  test("builds a board as the first class in its class vector that a package defines, naming the classes it skips and dropping what they add", async () => {
    const store = await opened((store) => store.register(dock));
    const dropped = await store.open(
      file({
        class: ["grid_board", "dock_board", "board"],
        views: { main: { panels: ["a"] } },
        extensions: {},
        panels: { p1: ["a"] },
      }),
    );

    expect(dropped).toEqual([{ class: "dock_board", skipped: ["grid_board"] }]);
    const board = store.board();
    expect(board.class).toEqual(["grid_board", "dock_board", "board"]);
    expect(viewsOf(board)).toEqual({ main: { panels: ["a"] } });
    expect(Object.hasOwn(board, "panels")).toBe(false);
  });

  test("builds a board whose classes no package defines as core's, dropping what they add and what no package registered", async () => {
    const store = await opened();
    const dropped = await store.open(
      file({
        class: ["dock_board", "board"],
        views: { main: { panels: ["a"] } },
        extensions: { "blockr.dag-dag": { positions: { a: { x: 0, y: 0 } } } },
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

    expect(dropped).toEqual([{ class: "board", skipped: ["dock_board"] }]);

    // The board keeps every class, so that a dock app builds it as a dock
    // board again.
    const board = store.board();
    expect(board.class).toEqual(["dock_board", "board"]);
    expect(Object.keys(board)).toEqual(["id", "class", "name", "blocks", "links", "stacks", "options"]);
    expect(board.blocks.a?.attrs).toEqual({ visible: ["outputs"] });
    expect(board.options).toEqual({ n_rows: 20 });
  });

  test("drops a property that its class does not add, without naming it", async () => {
    const store = await opened((store) => store.register(dock));
    const dropped = await store.open(
      file({ class: ["dock_board", "board"], views: {}, extensions: {}, zoom: 2 }),
    );

    expect(dropped).toEqual([]);
    expect(Object.hasOwn(store.board(), "zoom")).toBe(false);
  });

  test("gives each property of its class that the board lacks its default", async () => {
    const store = await opened((store) => store.register(dock));
    await store.open(file({ class: ["dock_board", "board"], views: { main: { panels: [] } } }));

    expect(viewsOf(store.board())).toEqual({ main: { panels: [] } });
    expect((store.board() as DockBoard).extensions).toEqual({});
  });

  test("gives the board options that its class and its parents require their defaults where it lacks them, as no step", async () => {
    const read = vi.fn(() => Promise.resolve(25));
    const store = await opened((store) => {
      store.options.register({
        id: "view_tabs",
        label: "View tabs",
        category: "Dock options",
        schema: { type: "boolean" },
        default: true,
      });
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: read,
      });
      store.register({ ...dock, options: ["view_tabs", "n_rows"] });
      store.register({ ...noted, options: ["page_rows"] });
    });

    await store.open(file({ class: ["noted_board", "board"], options: { n_rows: 20 } }));

    expect(store.board().options).toEqual({ n_rows: 20, view_tabs: true, page_rows: 25 });
    expect(read).toHaveBeenCalledOnce();
    expect(store.canUndo()).toBe(false);
  });

  test("refuses a board whose class requires a board option that no package defines, naming both, before it calls a default", async () => {
    const read = vi.fn(() => Promise.resolve(25));
    const store = await opened((store) => {
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: read,
      });
      store.register({ ...dock, options: ["view_tabs", "page_rows"] });
    });
    const board = store.board();

    await expect(store.open(file({ class: ["dock_board", "board"] }))).rejects.toThrow(
      'The store refused to open the board\n- The board class "dock_board" requires the board option "view_tabs", which no loaded package defines.',
    );
    expect(read).not.toHaveBeenCalled();
    expect(store.board()).toBe(board);
  });

  test("refuses a board where the default of a board option that its class requires fails or does not fit its schema", async () => {
    const store = await opened((store) => {
      store.options.register({
        id: "page_rows",
        label: "Rows a page shows",
        category: "Table options",
        schema: { type: "integer" },
        default: () => Promise.reject(new Error("R has no page_rows.")),
      });
      store.options.register({
        id: "view_tabs",
        label: "View tabs",
        category: "Dock options",
        schema: { type: "boolean" },
        default: () => Promise.resolve("yes"),
      });
      store.register({ ...dock, options: ["page_rows", "view_tabs"] });
    });
    const board = store.board();

    const refused = store.open(file({ class: ["dock_board", "board"] }));
    await expect(refused).rejects.toThrow(
      'The default of the board option "page_rows", which the board class "dock_board" requires, failed: R has no page_rows.',
    );
    await expect(refused).rejects.toThrow(
      'The default of the board option "view_tabs", which the board class "dock_board" requires, fails its schema: Instance type "string" is invalid. Expected "boolean".',
    );
    expect(store.board()).toBe(board);
  });

  test("refuses a board whose class is no class vector, before R loads anything", async () => {
    const client = fakeR();
    const store = await opened(undefined, client);

    await expect(store.open(file({ class: [] }))).rejects.toThrow(
      /The board's class is no array of class names/,
    );
    await expect(store.open(file({ class: "board" } as unknown as Partial<Board>))).rejects.toThrow(
      StoreError,
    );
    expect(client.sent).toEqual([]);
  });

  test("refuses a board whose property fails its schema", async () => {
    const store = await opened((store) => store.register(dock));
    await expect(
      store.open(file({ class: ["dock_board", "board"], views: { main: { panels: "a" } } })),
    ).rejects.toThrow(/The property "views" fails its schema at \/main\/panels: Instance type "string" is invalid\. Expected "array"\./);
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
      startup_modules: replied.startup_modules,
      modules: replied.modules,
      renderers: replied.renderers,
    };
    const fresh = () => newStore(fakeR(answer));

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

  test("drops a block whose class the block registry lacks as a delta removes it, with what its board class keeps about it", async () => {
    const store = await opened((store) => store.register(dock));
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
    expect(viewsOf(board)).toEqual({ main: { panels: ["a", "b"] } });
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

  test("holds no board until it opens one", async () => {
    const store = newStore();
    expect(store.hasBoard()).toBe(false);
    expect(() => store.board()).toThrow("The store holds no board until it opens one.");
    expect(() => {
      store.apply({ name: "Built" });
    }).toThrow("The store rejected the delta\n- The store holds no board until it opens one.");
    expect(store.canUndo()).toBe(false);

    await store.open(hello);
    expect(store.hasBoard()).toBe(true);
  });
});

describe("migrations", () => {
  const replied: OpenReply = {
    ...loaded,
    packages: { ...loaded.packages, "blockr.dag": "0.3.0" },
  };

  // A dock app that loaded blockr.dag and blockr.io beside core,
  // whose packages register their migrations.
  function migrating(migrations: Record<string, Migration>): Promise<Store> {
    return opened(
      (store) => {
        store.register(dock);
        for (const [pkg, fn] of Object.entries(migrations)) store.registerMigration(pkg, fn);
      },
      fakeR(replied),
      { ...hello, packages: replied.packages },
    );
  }

  const old = file(
    {
      class: ["dock_board", "board"],
      extensions: { "blockr.dag-dag": { layout: { a: [1, 2], b: [3, 4] } } },
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
    // The DAG renames its layout to positions, in the state that the dock board
    // keeps of it.
    const dag = vi.fn<Migration>((saved) => {
      const extensions = saved.properties.extensions as unknown as Record<
        string,
        { layout?: Record<string, number[]> }
      >;
      const layout = extensions["blockr.dag-dag"]?.layout ?? {};
      return {
        ...saved,
        properties: {
          ...saved.properties,
          extensions: {
            ...extensions,
            "blockr.dag-dag": {
              positions: Object.fromEntries(
                Object.entries(layout).map(([id, [x = 0, y = 0]]) => [id, { x, y }]),
              ),
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
    const store = await opened((store) => {
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
    const store = await opened((store) => {
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
      frontEnd: "blockr.test-count",
      properties: { count: { schema: { type: "integer" }, default: 0 } },
      update(delta) {
        const last = delta.patches?.filter(({ path }) => path === "/count").at(-1);
        return [{ op: "replace", path: "/count", value: Number(last?.value ?? 0) + 1 }];
      },
    };
    const store = await opened((store) => store.register(counting));
    await store.open(file({ class: ["counting_board", "board"] }));

    expect(() => {
      store.apply({ name: "Renamed" });
    }).toThrow(new RegExp(`kept expanding after ${String(ROUNDS)} rounds, through the board class "counting_board"`));
  });

  test("rejects a delta that an update follows with a patch outside the properties its class adds", async () => {
    const store = await opened((store) => {
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
    ["a stack holding a block the board lacks", { stacks: { add: { s2: { name: "More", blocks: ["z"] } } } }, /stack "s2" holds block "z", which the board lacks/],
    ["a block in two stacks", { stacks: { add: { s2: { name: "More", blocks: ["a"] } } } }, /block "a" sits in the stacks "s1" and "s2", where it sits in one/],
    ["a board option value its schema refuses", { options: { set: { n_rows: -1 } } }, /value of the board option "n_rows" fails its schema: -1 is less than 0\./],
    ["a value of a board option no package registered", { options: { set: { nope: 1 } } }, /board option "nope", which no package registered/],
    ["an attribute no package registered", { blocks: { mod: { a: { attrs: { set: { color: "red" } } } } } }, /block "a" has the attribute "color", which no package registered/],
    ["an attribute its schema refuses", { blocks: { mod: { a: { attrs: { set: { visible: ["header"] } } } } } }, /attribute "visible" of block "a" fails its schema at \/0: Instance does not match any of \["inputs","outputs"\]\./],
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
        { options: { set: { n_rows: 50 }, unset: ["dark_mode"] } },
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
    const store = await opened((store) => {
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
        default: 0,
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
  test("core registers its board class board and its attribute visible", () => {
    const store = newStore();
    expect(store.registered()).toEqual([
      { class: "board", frontEnd: "blockr.core-basic", properties: {} },
      expect.objectContaining({ name: "visible" }),
    ]);
  });

  test("a board class, an attribute or a board option id registered twice fails", () => {
    const store = newStore();
    store.register(dock);

    expect(() => store.register({ ...dock })).toThrow('The board class "dock_board" is registered already.');
    expect(() => store.register({ name: "visible", schema: {} })).toThrow(
      'The attribute "visible" of blocks is registered already.',
    );
    expect(() =>
      store.options.register({ id: "n_rows", label: "Rows", category: "Table options", schema: {}, default: 50 }),
    ).toThrow(
      'The board option "n_rows" is registered already.',
    );
    expect(() => {
      store.registerMigration("blockr.foo", (saved) => saved);
      store.registerMigration("blockr.foo", (saved) => saved);
    }).toThrow("blockr.foo registers a migration, which it registered already.");
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

  test("a default or an initial value that its schema refuses fails", () => {
    const store = newStore();
    expect(() =>
      store.options.register({
        id: "seed",
        label: "Seed",
        category: "Board options",
        schema: { type: "integer" },
        default: "one",
      }),
    ).toThrow(
      'The default of the board option "seed" fails its schema: Instance type "string" is invalid. Expected "integer".',
    );
    expect(() =>
      store.register({ ...dock, properties: { views: { schema: { type: "object" }, default: [] } } }),
    ).toThrow(
      'The default of the property "views" of the board class "dock_board" fails its schema: Instance type "array" is invalid. Expected "object".',
    );
    expect(() =>
      store.register({ name: "pinned", schema: { type: "boolean" }, initial: "yes" }),
    ).toThrow(/The initial value of the attribute "pinned" of blocks fails its schema/);
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
