// @vitest-environment node

// The store, with R's replies taken from the fixtures of the wire protocol.

import { describe, expect, test, vi } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import { coreOptions, hello, noUi, reply, withPlugins } from "../testing/store.ts";
import type { Block, Board, Plugin } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { ROUNDS } from "./expand.ts";
import { createStore, StoreError } from "./index.ts";
import type {
  Change,
  Delta,
  Migration,
  Option,
  Patch,
  PluginClass,
  Store,
} from "./index.ts";

// The state of the dock's plugin, which lists the panels of each view, and
// that of the DAG's, which keeps the position of each block by its id, as
// their packages type them.
interface DockState {
  readonly views: Readonly<Record<string, { readonly panels: readonly string[] }>>;
}

interface DagState {
  readonly positions: Readonly<Record<string, Position>>;
}

interface NotesState {
  readonly notes: Readonly<Record<string, string>>;
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

// The state of the plugin `plugin` of the board, which a caller reads as its
// class types it.
function stateOf(board: Board, plugin: string): unknown {
  return board.plugins[plugin]?.state;
}

// The dock's plugin class, a frontend.
// Its update drops a removed block's panels from the views of the plugin it
// is called for, the last first, so that each index still names the panel it
// names on the board.
const dock: PluginClass = {
  id: "blockr.dock-dock_plugin",
  frontend: "blockr.dock-dock",
  state: {
    schema: {
      type: "object",
      properties: {
        views: {
          type: "object",
          additionalProperties: {
            type: "object",
            properties: { panels: { type: "array", items: { type: "string" } } },
            required: ["panels"],
            additionalProperties: false,
          },
        },
      },
      required: ["views"],
      additionalProperties: false,
    },
    default: { views: {} },
  },
  update(delta, board, plugin) {
    const gone = new Set(delta.blocks?.rm ?? []);
    const patches: Patch[] = [];
    for (const [view, { panels }] of Object.entries((stateOf(board, plugin) as DockState).views)) {
      for (let i = panels.length - 1; i >= 0; i--) {
        if (gone.has(panels[i] ?? "")) {
          patches.push({ op: "remove", path: `/plugins/${plugin}/state/views/${view}/panels/${String(i)}` });
        }
      }
    }
    return patches;
  },
};

// The DAG's plugin class, whose update drops a removed block's position.
const dag: PluginClass = {
  id: "blockr.dag-dag_plugin",
  ui: "blockr.dag-dag",
  state: {
    schema: {
      type: "object",
      properties: { positions: { type: "object", additionalProperties: position } },
      required: ["positions"],
    },
    default: { positions: {} },
  },
  update(delta, board, plugin) {
    const gone = new Set(delta.blocks?.rm ?? []);
    return Object.keys((stateOf(board, plugin) as DagState).positions)
      .filter((id) => gone.has(id))
      .map((id) => ({ op: "remove", path: `/plugins/${plugin}/state/positions/${id}` }));
  },
};

// A class of notes on blocks, by block id, of which a board may hold several
// plugins, a removed block's note going with it.
const noted: PluginClass = {
  id: "blockr.notes-notes_plugin",
  ui: "blockr.notes-notes",
  state: {
    schema: {
      type: "object",
      properties: { notes: { type: "object", additionalProperties: { type: "string" } } },
    },
    default: { notes: {} },
  },
  update(delta, board, plugin) {
    const { notes } = stateOf(board, plugin) as NotesState;
    return (delta.blocks?.rm ?? [])
      .filter((id) => Object.hasOwn(notes, id))
      .map((id) => ({ op: "remove", path: `/plugins/${plugin}/state/notes/${id}` }));
  },
};

// A storage plugin's class, which saves boards to a store of its own.
function storage(id: string): PluginClass {
  return {
    id,
    storage: { save: () => Promise.resolve(), open: () => Promise.resolve(null) },
  };
}

function plugin(def: PluginClass, name: string, state: Json): Plugin {
  return { class: def.id, name, state };
}

function viewsOf(board: Board, id = "dock"): DockState["views"] {
  return (stateOf(board, id) as DockState).views;
}

function positionsOf(board: Board, id = "dag"): DagState["positions"] {
  return (stateOf(board, id) as DagState).positions;
}

function notesOf(board: Board, id: string): NotesState["notes"] {
  return (stateOf(board, id) as NotesState).notes;
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

// The reply to `hello` of a dock app, which loaded blockr.dock and blockr.dag,
// and whose opening board holds the dock, with a view of the panels of a and
// b, and the DAG, with their positions.
const docking: HelloReply = withPlugins(
  {
    dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "b"] } } }),
    dag: plugin(dag, "DAG", { positions: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } }),
  },
  { ...hello, packages: { ...hello.packages, "blockr.dag": "0.2.0", "blockr.dock": "0.2.1" } },
);

// The plugin classes of the dock app.
function dockClasses(store: Store): void {
  store.plugins.register(dock);
  store.plugins.register(dag);
}

// A store that holds the board of a dock app, once the classes of the dock
// app and what `setup` registers are registered.
function docked(setup?: (store: Store) => void): Promise<Store> {
  return opened(
    (store) => {
      dockClasses(store);
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
    locked: null,
  };
}

// The text of a JSON board of a core app, with what `board` gives in place of
// its parts, its plugins among them.
// Each block records version 1 of its class's state unless it gives another,
// the board the versions of plugin classes that `board` gives, none unless it
// gives some, and each board option version 1 of its value unless
// `optionVersions` gives another.
function file(
  board: Partial<Board> & Readonly<Record<string, unknown>>,
  packages: Readonly<Record<string, string>> = { "blockr.core": "0.2.0" },
  optionVersions: Readonly<Record<string, number>> = {},
): string {
  const given = {
    id: "amber_tapir",
    name: "Cars",
    blocks: {
      a: {
        registry_id: "blockr.core-dataset_block",
        state: { dataset: "mtcars", package: "datasets" },
        name: "Cars",
        locked: null,
      },
      b: head(),
    } as Readonly<Record<string, Block & { version?: number }>>,
    links: { ab: { from: "a", to: "b", input: "data" } },
    stacks: { s1: { name: "Input", blocks: ["a", "b"], color: null } },
    options: { n_rows: 50, show_conditions: ["warning", "error"] },
    plugins: {},
    ...board,
  };
  const versions = (board.versions as Readonly<Record<string, number>> | undefined) ?? {};
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
    expect(board.name).toBe("Iris review");
    expect(Object.keys(board.blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
    expect(board.blocks.a).toEqual({
      registry_id: "blockr.core-dataset_block",
      state: { dataset: "iris", package: "datasets" },
      name: "Iris",
      locked: null,
    });

    expect(Object.keys(board.links)).toEqual(["ab", "ac", "bc", "ad", "be", "de", "keen_finch"]);
    expect(board.links.keen_finch).toEqual({ from: "a", to: "f", input: "data" });
    expect(board.stacks).toEqual({ s1: { name: "Input", blocks: ["a", "b"], color: null } });
    expect(board.plugins).toEqual({});

    expect(store.blockRegistry()).toBe(hello.block_registry);
    expect(store.packages()).toEqual({ "blockr.core": "0.2.0" });
    expect(seen).toEqual([{ opened: board, dropped: [] }]);
    expect(store.canUndo()).toBe(false);
  });

  test("writes in no default of a board option, which core adds for what requires the board option", async () => {
    const store = newStore();
    await store.open(reply("hello-block") as HelloReply);
    const board = store.board();

    expect(board.name).toBe("Quick wren");
    expect(Object.keys(board.blocks)).toEqual(["tidy_crab"]);
    expect(board.options).toEqual({ n_rows: 50 });
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
              locked: null,
            },
            x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter", locked: null },
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

  test("refuses a block whose lock is neither a boolean nor null", async () => {
    const json = JSON.parse(file({})) as { board: { blocks: Record<string, Record<string, unknown>> } };
    delete json.board.blocks.a?.locked;
    if (json.board.blocks.b !== undefined) json.board.blocks.b.locked = "yes";

    const refused = unopened().open(JSON.stringify(json));
    await expect(refused).rejects.toThrow('The block "a" has no boolean or null as its locked.');
    await expect(unopened().open(JSON.stringify(json))).rejects.toThrow(
      'The block "b" has no boolean or null as its locked.',
    );
  });

  test("refuses a JSON board's text under the load lock, and before R has answered hello", async () => {
    const store = unopened(undefined, reply("hello-locked") as HelloReply);

    const refused = /The load lock fixes the app to its opening board, so the store opens no other board/;
    await expect(store.open(file({}))).rejects.toThrow(refused);
    await expect(store.check(file({}))).rejects.toThrow(refused);
    expect(store.hasBoard()).toBe(false);

    // It opens the opening board in R's reply to hello all the same.
    await store.open(reply("hello-locked") as HelloReply);
    expect(store.board().id).toBe("steady_lynx");

    await expect(newStore().open(file({}))).rejects.toThrow(
      /The store opens a JSON board only once R has answered `hello`/,
    );
  });

  test("drops and names each plugin that the app does not run, with the package of its class", async () => {
    const store = unopened(
      dockClasses,
      withPlugins({
        dock: plugin(dock, "Dock", { views: {} }),
        notes: { class: "blockr.md-md_plugin", name: "Notes", state: { content: "" } },
      }),
    );
    const dropped = await store.open(
      file(
        {
          plugins: {
            dock: plugin(dock, "Dock", { views: { main: { panels: ["a"] } } }),
            notes: { class: "blockr.md-md_plugin", name: "Notes", state: { content: "# Notes" } },
            dag: plugin(dag, "DAG", { positions: {} }),
            mine: { class: "mine_plugin", name: "Mine", state: {} },
          },
          versions: { "blockr.md-md_plugin": 9, "blockr.dag-dag_plugin": 9 },
        },
        { "blockr.core": "0.2.0", "blockr.md": "0.2.0" },
      ),
    );

    // It keeps the dock, which the app's opening board holds, and drops the
    // plugin of a class that no package registers and those that the app's
    // opening board does not hold, whose versions then refuse nothing.
    expect(dropped).toEqual([
      { package: "blockr.md", version: "0.2.0" },
      { plugin: "notes", class: "blockr.md-md_plugin", package: "blockr.md" },
      { plugin: "dag", class: "blockr.dag-dag_plugin", package: "blockr.dag" },
      { plugin: "mine", class: "mine_plugin", package: null },
    ]);
    expect(Object.keys(store.board().plugins)).toEqual(["dock"]);
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
  });

  test("opens a board of a dock app in a core app without its plugins, naming the packages, and dropping and naming what no package registered", async () => {
    const store = unopened();
    const dropped = await store.open(
      JSON.stringify({
        ...docking.board,
        packages: { ...docking.board.packages, "blockr.dag": "0.2.0", "blockr.dock": "0.2.1" },
        board: {
          ...docking.board.board,
          options: { n_rows: { value: 20, version: 1 }, view_tabs: { value: true, version: 1 } },
        },
      }),
    );

    expect(dropped).toEqual([
      { package: "blockr.dag", version: "0.2.0" },
      { package: "blockr.dock", version: "0.2.1" },
      { plugin: "dock", class: "blockr.dock-dock_plugin", package: "blockr.dock" },
      { plugin: "dag", class: "blockr.dag-dag_plugin", package: "blockr.dag" },
      { option: "view_tabs" },
    ]);

    const board = store.board();
    expect(Object.keys(board)).toEqual(["id", "name", "blocks", "links", "stacks", "options", "plugins"]);
    expect(board.plugins).toEqual({});
    expect(board.options).toEqual({ n_rows: 20 });
  });

  test("drops what core's board does not hold, without naming it", async () => {
    const store = unopened();
    const dropped = await store.open(file({ class: ["dock_board", "board"], zoom: 2 }));

    expect(dropped).toEqual([]);
    expect(Object.hasOwn(store.board(), "zoom")).toBe(false);
    expect(Object.hasOwn(store.board(), "class")).toBe(false);
  });

  test("refuses an opening board that holds a plugin that is no plugin, two frontends or two storage plugins", async () => {
    const grid: PluginClass = { id: "blockr.grid-grid_plugin", frontend: "blockr.grid-grid" };
    const session = storage("blockr.session-session_plugin");
    const drive = storage("blockr.drive-drive_plugin");
    const open = (plugins: Readonly<Record<string, unknown>>) =>
      opened(
        (store) => {
          for (const def of [dock, dag, grid, session, drive]) store.plugins.register(def);
        },
        withPlugins(plugins as Readonly<Record<string, Plugin>>),
      );

    await expect(open({ dag: { class: dag.id, name: 3 } })).rejects.toThrow(
      'The plugin "dag" has no string as its name, has no value as its state.',
    );
    await expect(open({ dag: { ...plugin(dag, "DAG", { positions: {} }), zoom: 2 } })).rejects.toThrow(
      'The plugin "dag" holds "zoom", which core does not keep.',
    );
    await expect(open({ dock: plugin(dock, "Dock", { views: {} }), grid: plugin(grid, "Grid", {}) })).rejects.toThrow(
      'The board holds the frontends "dock" and "grid", where it holds one at most.',
    );
    await expect(
      open({ session: plugin(session, "Session", {}), drive: plugin(drive, "Drive", {}) }),
    ).rejects.toThrow('The board holds the storage plugins "session" and "drive", where it holds one at most.');
    await expect(open({ 7: plugin(dag, "DAG", { positions: {} }) })).rejects.toThrow(
      'The plugin id "7" is empty or made of digits alone.',
    );
  });

  test("refuses a board whose plugin's state fails the schema of its class", async () => {
    const store = unopened(
      (store) => store.plugins.register(dock),
      withPlugins({ dock: plugin(dock, "Dock", { views: {} }) }),
    );
    await expect(
      store.open(file({ plugins: { dock: plugin(dock, "Dock", { views: { main: { panels: "a" } } }) } })),
    ).rejects.toThrow(
      /The state of the plugin "dock" fails its schema at \/views\/main\/panels: Instance type "string" is invalid\. Expected "array"\./,
    );
  });

  test("gives the board each plugin of the opening board that it lacks, under its id and name, with its class's default state", async () => {
    const store = unopened(dockClasses, docking);
    await store.open(file({}));

    expect(store.board().plugins).toEqual({
      dock: plugin(dock, "Dock", { views: {} }),
      dag: plugin(dag, "DAG", { positions: {} }),
    });
    // R's reply holds the opening board as R wrote it.
    expect(viewsOf(store.board())).toEqual({});
  });

  test("keeps the board's plugins that the opening board holds under the same id and of the same class, with their names and states", async () => {
    const store = unopened(dockClasses, docking);
    const dropped = await store.open(
      file({
        plugins: {
          dag: plugin(dag, "Graph", { positions: { a: { x: 3, y: 3 } } }),
          graph: plugin(dag, "Another graph", { positions: {} }),
        },
      }),
    );

    // In the order of the opening board, whose dock the board lacks.
    expect(store.board().plugins).toEqual({
      dock: plugin(dock, "Dock", { views: {} }),
      dag: plugin(dag, "Graph", { positions: { a: { x: 3, y: 3 } } }),
    });
    expect(dropped).toEqual([{ plugin: "graph", class: dag.id, package: "blockr.dag" }]);
  });

  test("holds every plugin of the opening board, several of one class among them, each with the board's state under its id or its class's default", async () => {
    const opening = withPlugins({
      todo: plugin(noted, "To do", { notes: { a: "Check" } }),
      done: plugin(noted, "Done", { notes: {} }),
    });
    const store = unopened((store) => store.plugins.register(noted), opening);
    const dropped = await store.open(
      file({
        plugins: {
          done: plugin(noted, "Finished", { notes: { b: "Shipped" } }),
          later: plugin(noted, "Later", { notes: {} }),
        },
      }),
    );

    expect(store.board().plugins).toEqual({
      todo: plugin(noted, "To do", { notes: {} }),
      done: plugin(noted, "Finished", { notes: { b: "Shipped" } }),
    });
    expect(dropped).toEqual([{ plugin: "later", class: noted.id, package: "blockr.notes" }]);
  });

  test("holds the opening board's frontend and storage plugin, whichever the board holds", async () => {
    const grid: PluginClass = { id: "blockr.grid-grid_plugin", frontend: "blockr.grid-grid" };
    const session = storage("blockr.session-session_plugin");
    const drive = storage("blockr.drive-drive_plugin");
    const store = unopened(
      (store) => {
        for (const def of [dock, grid, session, drive]) store.plugins.register(def);
      },
      withPlugins({
        dock: plugin(dock, "Dock", { views: { main: { panels: ["a"] } } }),
        session: plugin(session, "Session", {}),
      }),
    );

    const dropped = await store.open(
      file({ plugins: { grid: plugin(grid, "Grid", {}), drive: plugin(drive, "Drive", {}) } }),
    );

    expect(store.board().plugins).toEqual({
      dock: plugin(dock, "Dock", { views: {} }),
      session: plugin(session, "Session", {}),
    });
    expect(dropped).toEqual([
      { plugin: "grid", class: grid.id, package: "blockr.grid" },
      { plugin: "drive", class: drive.id, package: "blockr.drive" },
    ]);
  });

  test("drops the board's plugin of another class under the id of one of the opening board's, which takes its class's default state", async () => {
    const store = unopened(
      (store) => {
        dockClasses(store);
        store.plugins.register(noted);
      },
      docking,
    );
    const dropped = await store.open(file({ plugins: { dag: plugin(noted, "Notes", { notes: {} }) } }));

    expect(store.board().plugins).toEqual({
      dock: plugin(dock, "Dock", { views: {} }),
      dag: plugin(dag, "DAG", { positions: {} }),
    });
    expect(dropped).toEqual([{ plugin: "dag", class: noted.id, package: "blockr.notes" }]);
  });

  test("holds no plugin of a class that no package registers, and opens R's opening board with the states that R wrote", async () => {
    const store = unopened((store) => store.plugins.register(dag), docking);
    await store.open(file({}));
    expect(store.board().plugins).toEqual({ dag: plugin(dag, "DAG", { positions: {} }) });

    // The opening board itself keeps the states of its plugins.
    const again = unopened(dockClasses, docking);
    await again.open(docking);
    expect(again.board().plugins).toEqual({
      dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "b"] } } }),
      dag: plugin(dag, "DAG", { positions: { a: { x: 0, y: 0 }, b: { x: 1, y: 1 } } }),
    });
  });

  test("gives no board option its default, those that a plugin's class requires included", async () => {
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
      store.plugins.register({ ...dock, options: ["page_rows"] });
    }, withPlugins({ dock: plugin(dock, "Dock", { views: {} }) }));

    await store.open(file({ options: { n_rows: 20 }, plugins: { dock: plugin(dock, "Dock", { views: {} }) } }));

    // The board options that a plugin's class requires, and those of core's
    // basic frontend, are core's to add as the board opens, rather than the
    // store's.
    expect(store.board().options).toEqual({ n_rows: 20 });
    expect(read).not.toHaveBeenCalled();
    expect(store.canUndo()).toBe(false);
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
    await expect(store.open(file({ blocks: { 12: head() } }))).rejects.toThrow(StoreError);

    await store.open(file({}));
    expect(store.board().name).toBe("Cars");
  });

  test("ends at the same board whichever way the board comes in, and saves the JSON board R sent as it is", async () => {
    // Without the load lock, so that the store opens a JSON board's text on it.
    const replied: HelloReply = { ...(reply("hello-locked") as HelloReply), lock: { board: false, load: false } };

    // As R sent it.
    const viaR = newStore();
    await viaR.open(replied);
    const board = viaR.board();
    expect(viaR.jsonBoard()).toEqual(replied.board);

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

  test("drops a block whose class the block registry lacks as a delta removes it, with what its plugins keep about it", async () => {
    const store = unopened(dockClasses, docking);
    const dropped = await store.open(
      file({
        plugins: {
          dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "x", "b"] } } }),
          dag: plugin(dag, "DAG", {
            positions: { a: { x: 0, y: 0 }, x: { x: 1, y: 1 }, b: { x: 2, y: 2 } },
          }),
        },
        blocks: {
          a: {
            registry_id: "blockr.core-dataset_block",
            state: { dataset: "mtcars", package: "datasets" },
            name: "Cars",
            locked: null,
          },
          x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter", locked: null },
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

  test("refuses a board that holds a value its schema refuses, a block's state, a plugin's state or a board option's value, naming every such value", async () => {
    const store = unopened(
      (store) => store.plugins.register(dock),
      withPlugins({ dock: plugin(dock, "Dock", { views: {} }) }),
    );
    const seen = changes(store);

    await expect(
      store.open(
        file({
          blocks: {
            a: { registry_id: "blockr.core-dataset_block", state: { dataset: "mtcars" }, name: "Cars", locked: null },
            b: head(-1),
          },
          options: { n_rows: "fifty", show_conditions: ["warning", "error"] },
          plugins: { dock: plugin(dock, "Dock", { views: [] }) },
        }),
      ),
    ).rejects.toThrow(
      [
        "The store refused to open the board",
        'The state of the block "a" fails its schema: Instance does not have required property "package".',
        'The state of the block "b" fails its schema at /n: -1 is less than 0.',
        'The value of the board option "n_rows" fails its schema: Instance type "string" is invalid. Expected "integer".',
        'The state of the plugin "dock" fails its schema at /views: Instance type "array" is invalid. Expected "object".',
      ].join("\n- "),
    );
    expect(store.hasBoard()).toBe(false);
    expect(seen).toEqual([]);
  });

  test("refuses an opening board that holds a value its schema refuses, as an app script gives one", async () => {
    const store = newStore();
    const options = { ...hello.board.board.options, n_rows: { value: 0, version: 1 } };
    const given: HelloReply = { ...hello, board: { ...hello.board, board: { ...hello.board.board, options } } };

    await expect(store.open(given)).rejects.toThrow(
      'The value of the board option "n_rows" fails its schema: 0 is less than 1.',
    );
    expect(store.hasBoard()).toBe(false);
  });

  test("drops the value of each board option that no package registers, naming the board option, and saves the board without it", async () => {
    const store = unopened();
    const dropped = await store.open(
      file({ options: { grid_gap: 4, n_rows: 20, view_tabs: true } }, undefined, { grid_gap: 3 }),
    );

    // The version that the board records of a value that the store drops
    // refuses nothing.
    expect(dropped).toEqual([{ option: "grid_gap" }, { option: "view_tabs" }]);
    expect(store.board().options).toEqual({ n_rows: 20 });
    expect(Object.keys(store.jsonBoard().board.options)).toEqual(["n_rows"]);
  });

  test("checks a JSON board's text as it would open it, holding nothing", async () => {
    const store = createStore();
    for (const option of coreOptions) store.options.register(option);
    dockClasses(store);
    await store.open(docking);
    const board = store.board();
    const seen = changes(store);

    const dropped = await store.check(
      file(
        {
          plugins: { dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "x"] } } }) },
          blocks: { a: head(), x: { registry_id: "blockr.dplyr-filter_block", state: {}, name: "Filter", locked: null } },
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
  });

  test("checks a JSON board's text with what is registered, rejecting with what would refuse it", async () => {
    const store = await docked();
    const board = store.board();

    await expect(store.check("{")).rejects.toThrow(StoreError);
    await expect(
      store.check(file({ plugins: { dock: plugin(dock, "Dock", { views: [] }) } })),
    ).rejects.toThrow('The state of the plugin "dock" fails its schema');
    await expect(
      store.check(
        file({
          blocks: {
            b: { registry_id: "blockr.core-head_block", state: { n: -1, direction: "head" }, name: "Head", locked: null },
          },
          links: {},
          stacks: {},
          options: { n_rows: 0 },
        }),
      ),
    ).rejects.toThrow(
      [
        'The state of the block "b" fails its schema at /n: -1 is less than 0.',
        'The value of the board option "n_rows" fails its schema: 0 is less than 1.',
      ].join("\n- "),
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
});

describe("saving a board", () => {
  test("writes each block's lock beside its name, null among its values", async () => {
    const store = await opened();
    store.apply({ blocks: { mod: { b: { locked: false } } } });

    const { blocks } = store.jsonBoard().board;
    expect(Object.keys(blocks.a ?? {})).toEqual(["registry_id", "version", "state", "name", "locked"]);
    expect([blocks.a?.locked, blocks.b?.locked]).toEqual([null, false]);
  });

  test("writes the board it holds as the text of its JSON board, which another store opens to the same board", async () => {
    const store = await docked();
    store.apply([
      { name: "Renamed" },
      { blocks: { mod: { b: { state: { n: 3 } } } } },
      { patches: [{ op: "replace", path: "/plugins/dock/state/views/main/panels", value: ["b", "a"] }] },
    ]);

    const text = store.save();
    expect(text).toBe(`${JSON.stringify(store.jsonBoard(), null, 2)}\n`);

    const other = unopened(dockClasses, docking);
    await other.open(text);
    expect(other.board()).toEqual(store.board());
    expect(other.board().name).toBe("Renamed");
    expect(other.board().blocks.b?.state).toEqual({ n: 3, direction: "head" });
    expect(viewsOf(other.board())).toEqual({ main: { panels: ["b", "a"] } });
  });

  test("records the package of each plugin's class and of each block's class, and the version that R gives each plugin class", async () => {
    const replied: HelloReply = { ...docking, plugin_versions: { "blockr.dock-dock_plugin": 3 } };
    const store = await opened(dockClasses, {
      ...replied,
      board: { ...replied.board, board: { ...replied.board.board, versions: { "blockr.dock-dock_plugin": 3 } } },
    });
    const saved = store.jsonBoard();

    expect(saved.packages).toEqual({ "blockr.core": "0.2.0", "blockr.dag": "0.2.0", "blockr.dock": "0.2.1" });
    // A class without a version from R is at version 1.
    expect(saved.board.versions).toEqual({ "blockr.dock-dock_plugin": 3, "blockr.dag-dag_plugin": 1 });
    expect(Object.keys(saved.board)).toEqual(["id", "versions", "name", "blocks", "links", "stacks", "options", "plugins"]);

    // A package that R did not load is recorded with no version.
    const store2 = await opened(
      (store) => store.plugins.register(noted),
      withPlugins({ notes: plugin(noted, "Notes", { notes: {} }) }),
    );
    expect(store2.jsonBoard().packages).toEqual({ "blockr.core": "0.2.0" });
    expect(store2.jsonBoard().board.versions).toEqual({ "blockr.notes-notes_plugin": 1 });
  });
});

describe("migrations", () => {
  // A dock app whose dock plugin class is at version 3 of its state and whose
  // head block class is at version 2 of its state, and whose opening board R
  // wrote at those versions.
  const replied: HelloReply = {
    ...docking,
    plugin_versions: { "blockr.dock-dock_plugin": 3 },
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
        versions: { "blockr.dock-dock_plugin": 3, "blockr.dag-dag_plugin": 1 },
        blocks: Object.fromEntries(
          Object.entries(docking.board.board.blocks).map(([id, block]) => [
            id,
            block.registry_id === "blockr.core-head_block" ? { ...block, version: 2 } : block,
          ]),
        ),
      },
    },
  };

  // The dock's plugin class, whose versions before 3 kept the panels of a view
  // under `layout`.
  const migrating: PluginClass = {
    ...dock,
    migrate(state, version) {
      const { views } = state as { readonly views: Readonly<Record<string, { layout?: string[] }>> };
      if (version >= 3) return state;
      return {
        views: Object.fromEntries(
          Object.entries(views).map(([id, view]) => [id, { panels: view.layout ?? [] }]),
        ),
      };
    },
  };

  // A board of the dock app that the dock's version 2 and the head block's
  // version 1 saved, when the head block kept its n as a string.
  const old = file({
    versions: { "blockr.dock-dock_plugin": 2 },
    plugins: { dock: plugin(dock, "Dock", { views: { main: { layout: ["a", "b"] } } }) },
    blocks: {
      a: {
        registry_id: "blockr.core-dataset_block",
        state: { dataset: "mtcars", package: "datasets" },
        name: "Cars",
        locked: null,
      },
      b: { ...head(), state: { n: "6", direction: "head" } },
    },
  });

  function store(head?: Migration, def: PluginClass = migrating): Store {
    return unopened((store) => {
      store.plugins.register(def);
      store.plugins.register(dag);
      store.plugins.register(noted);
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

    // The board saves at today's versions, with the DAG of the opening board.
    const saved = opening.jsonBoard();
    expect(saved.board.versions).toEqual({ "blockr.dock-dock_plugin": 3, "blockr.dag-dag_plugin": 1 });
    expect(saved.board.blocks.b?.version).toBe(2);
    expect(saved.board.blocks.a?.version).toBe(1);
  });

  test("migrates the state of each plugin of a class, each given frozen, and leaves alone a class that the board records no version of", async () => {
    const migrate = vi.fn((state: Json, version: number): Json => {
      const { notes } = state as unknown as NotesState;
      if (version >= 2) return state;
      return { notes: Object.fromEntries(Object.entries(notes).map(([id, note]) => [id, note.toUpperCase()])) };
    });
    const opening = unopened(
      (store) => {
        store.plugins.register({ ...noted, migrate });
        dockClasses(store);
      },
      withPlugins(
        {
          todo: plugin(noted, "To do", { notes: {} }),
          done: plugin(noted, "Done", { notes: {} }),
        },
        replied,
        { "blockr.notes-notes_plugin": 2 },
      ),
    );
    await opening.open(
      file({
        versions: { "blockr.notes-notes_plugin": 1 },
        plugins: {
          todo: plugin(noted, "To do", { notes: { a: "check" } }),
          done: plugin(noted, "Done", { notes: { b: "done" } }),
          dock: plugin(dock, "Dock", { views: { main: { panels: [] } } }),
        },
      }),
    );

    expect(migrate).toHaveBeenCalledTimes(2);
    expect(migrate.mock.calls.map(([, version]) => version)).toEqual([1, 1]);
    expect(Object.isFrozen(migrate.mock.calls[0]?.[0])).toBe(true);
    expect(notesOf(opening.board(), "todo")).toEqual({ a: "CHECK" });
    expect(notesOf(opening.board(), "done")).toEqual({ b: "DONE" });
    // The board records no version of the dock's class.
    expect(viewsOf(opening.board())).toEqual({ main: { panels: [] } });
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

    const failing = store(undefined, {
      ...dock,
      migrate: () => {
        throw new Error("The layout is unreadable.");
      },
    });
    await expect(failing.open(old)).rejects.toThrow(
      'The migration of the plugin class "blockr.dock-dock_plugin" failed: The layout is unreadable.',
    );

    const newer = file({
      versions: { "blockr.dock-dock_plugin": 4 },
      plugins: { dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "b"] } } }) },
    });
    await expect(store().open(newer)).rejects.toThrow(
      'The board records version 4 of the plugin class "blockr.dock-dock_plugin", while R gives version 3.',
    );
  });

  test("refuses a board whose block's state fails the schema of its class once migrated, or that no migration converts", async () => {
    const failure = 'The state of the block "b" fails its schema at /n: Instance type "string" is invalid. Expected "integer".';
    await expect(store().open(old)).rejects.toThrow(failure);
    await expect(store((state) => ({ ...state, direction: "tail" })).open(old)).rejects.toThrow(failure);
  });

  test("refuses a JSON board without the versions it records", async () => {
    const opening = store();
    const json = JSON.parse(old) as { board: Record<string, unknown> };
    await expect(
      opening.open(JSON.stringify({ ...json, board: { ...json.board, versions: { "blockr.dock-dock_plugin": 0 } } })),
    ).rejects.toThrow("The board records the versions of its plugin classes as no whole numbers from 1 by class.");
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
      opening.open(JSON.stringify({ ...json, board: { ...json.board, links: [], plugins: null } })),
    ).rejects.toThrow("The board holds its links and plugins in no object by id.");
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
    expect(opening.board().options).toEqual({ seed: 42, n_rows: 50 });

    const saved = opening.jsonBoard();
    expect(saved.board.options).toEqual({
      seed: { value: 42, version: 2 },
      n_rows: { value: 50, version: 1 },
    });
  });

  test("keeps a value without a migration, and refuses a board whose value then fails its schema, as one that a migration returns", async () => {
    const opening = store({ ...seed, migrate: undefined });
    await opening.open(file({ options: { seed: 7 } }, undefined, { seed: 1 }));
    expect(opening.board().options).toEqual({ seed: 7 });

    const unfit = store({ ...seed, migrate: undefined });
    await expect(unfit.open(file({ options: { seed: "seven" } }, undefined, { seed: 1 }))).rejects.toThrow(
      'The value of the board option "seed" fails its schema: Instance type "string" is invalid. Expected "integer".',
    );
    expect(unfit.hasBoard()).toBe(false);

    const halved = store({ ...seed, migrate: (value) => Number(value) / 2 });
    await expect(halved.open(file({ options: { seed: "7" } }, undefined, { seed: 1 }))).rejects.toThrow(
      'The value of the board option "seed" fails its schema: Instance type "number" is invalid. Expected "integer".',
    );
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
    expect(after.plugins).toBe(before.plugins);
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

  test("expands a delta through the update of each plugin's class, and one undo takes all of it back", async () => {
    const store = await docked();
    const before = store.board();
    const seen = changes(store);

    store.apply({ blocks: { rm: ["b"] } });
    expect(seen[0]).toMatchObject({
      delta: {
        patches: [
          { op: "remove", path: "/plugins/dock/state/views/main/panels/1" },
          { op: "remove", path: "/plugins/dag/state/positions/b" },
        ],
      },
    });
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
    expect(positionsOf(store.board())).toEqual({ a: { x: 0, y: 0 } });

    store.undo();
    expect(store.board()).toEqual(before);
    expect(Object.keys(store.board().links)).toEqual(Object.keys(before.links));
  });

  test("runs the update of a class once for each of its plugins, each for its own state", async () => {
    const update = vi.fn<NonNullable<PluginClass["update"]>>((delta, board, id) => noted.update?.(delta, board, id) ?? []);
    const store = await opened(
      (store) => {
        store.plugins.register(dock);
        store.plugins.register({ ...noted, update });
      },
      withPlugins({
        dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "b"] } } }),
        todo: plugin(noted, "To do", { notes: { a: "Raw", b: "First rows" } }),
        done: plugin(noted, "Done", { notes: { b: "Checked" } }),
      }),
    );

    store.apply({ blocks: { rm: ["b"] } });
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a"] } });
    expect(notesOf(store.board(), "todo")).toEqual({ a: "Raw" });
    expect(notesOf(store.board(), "done")).toEqual({});
    // Each round calls it for each plugin of the class, with the plugin's id.
    expect(new Set(update.mock.calls.map(([, , id]) => id))).toEqual(new Set(["todo", "done"]));
  });

  test("leaves out what an update returns at a path that the delta's own patches change", async () => {
    // A class that notes every block added, in place of a note the delta gives.
    const store = await opened(
      (store) => {
        store.plugins.register({
          ...noted,
          update: (delta, _board, id) =>
            Object.keys(delta.blocks?.add ?? {}).map((block) => ({
              op: "add",
              path: `/plugins/${id}/state/notes/${block}`,
              value: "New",
            })),
        });
      },
      withPlugins({ notes: plugin(noted, "Notes", { notes: {} }) }),
    );

    store.apply({
      blocks: { add: { g: head(3), h: head(4) } },
      patches: [{ op: "add", path: "/plugins/notes/state/notes/g", value: "Pasted" }],
    });
    expect(notesOf(store.board(), "notes")).toEqual({ g: "Pasted", h: "New" });
  });

  // The dock's class, with an update that puts a panel for each block that a
  // delta adds into the main view, at the place given.
  // It skips a block whose panel the delta's own patches add already.
  function placing(at: string): (store: Store) => void {
    return (store) => {
      store.plugins.register({
        ...dock,
        update(delta, _board, id) {
          const placed = new Set((delta.patches ?? []).map(({ value }) => value));
          return Object.keys(delta.blocks?.add ?? {})
            .filter((block) => !placed.has(block))
            .map((block) => ({ op: "add", path: `/plugins/${id}/state/views/main/panels/${at}`, value: block }));
        },
      });
    };
  }

  // The reply to `hello` of an app whose opening board holds the dock alone,
  // with a view of the panels of a and b.
  const dockOnly = withPlugins({ dock: plugin(dock, "Dock", { views: { main: { panels: ["a", "b"] } } }) });

  test.each([
    ["at its end", "-", ["a", "b", "g", "h"]],
    ["at an index", "0", ["h", "g", "a", "b"]],
  ])("keeps what an update adds to an array %s, which names no element, where the delta's own patches add there too", async (_, at, expected) => {
    const store = await opened(placing(at), dockOnly);

    store.apply({
      blocks: { add: { g: head(3), h: head(4) } },
      patches: [{ op: "add", path: `/plugins/dock/state/views/main/panels/${at}`, value: "g" }],
    });
    expect(viewsOf(store.board())).toEqual({ main: { panels: expected } });

    store.undo();
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["a", "b"] } });
  });

  test("leaves out what an update adds to an array that the delta's own patches set as a whole", async () => {
    const store = await opened(placing("-"), dockOnly);

    store.apply({
      blocks: { add: { g: head(3), h: head(4) } },
      patches: [{ op: "replace", path: "/plugins/dock/state/views/main/panels", value: ["g"] }],
    });
    expect(viewsOf(store.board())).toEqual({ main: { panels: ["g"] } });
  });

  test("rejects a delta that keeps expanding, naming the plugins whose updates kept adding", async () => {
    const counting: PluginClass = {
      id: "test.pkg-count_plugin",
      state: { schema: { type: "object", properties: { count: { type: "integer" } } }, default: { count: 0 } },
      update(delta, _board, id) {
        const path = `/plugins/${id}/state/count`;
        const last = delta.patches?.filter((patch) => patch.path === path).at(-1);
        return [{ op: "replace", path, value: Number(last?.value ?? 0) + 1 }];
      },
    };
    const store = await opened(
      (store) => store.plugins.register(counting),
      withPlugins({ count: plugin(counting, "Count", { count: 0 }) }),
    );

    expect(() => {
      store.apply({ name: "Renamed" });
    }).toThrow(new RegExp(`kept expanding after ${String(ROUNDS)} rounds, through the update of the plugin "count"`));
  });

  test("rejects a delta that an update follows with a patch outside the state of the plugin it was called for", async () => {
    const outside = (path: string) =>
      opened(
        (store) => {
          store.plugins.register(dock);
          store.plugins.register({ ...noted, update: () => [{ op: "replace", path, value: {} }] });
        },
        withPlugins({ dock: plugin(dock, "Dock", { views: {} }), notes: plugin(noted, "Notes", { notes: {} }) }),
      );

    for (const [path, called] of [
      ["/plugins/dock/state/views", "notes"],
      ["/plugins/notes", "notes"],
      ["/plugins/notes/name", "notes"],
      ["/name", "notes"],
    ] as const) {
      const store = await outside(path);
      expect(() => {
        store.apply({ name: "Renamed" });
      }).toThrow(
        `The plugin class "blockr.notes-notes_plugin" updated the delta for the plugin "${called}" with a patch at "${path}", outside that plugin's state.`,
      );
    }
  });

  const refusals: [string, Delta, RegExp][] = [
    ["a link from a block the board lacks", { links: { add: { zb: { from: "z", to: "e", input: "" } } } }, /comes from block "z", which the board lacks/],
    ["a link into an input the class lacks", { links: { add: { ab2: { from: "a", to: "b", input: "x" } } } }, /feeds the input "x" of block "b", whose class has no such input/],
    ["a second link into a fixed input", { links: { add: { db: { from: "d", to: "b", input: "data" } } } }, /input "data" of block "b" takes the links "ab" and "db", where it takes one/],
    ["a cycle", { links: { rm: ["ad"], add: { ed: { from: "e", to: "d", input: "data" } } } }, /links form a cycle through the blocks/],
    ["a stack holding a block the board lacks", { stacks: { add: { s2: { name: "More", blocks: ["z"], color: null } } } }, /stack "s2" holds block "z", which the board lacks/],
    ["a block in two stacks", { stacks: { add: { s2: { name: "More", blocks: ["a"], color: null } } } }, /block "a" sits in the stacks "s1" and "s2", where it sits in one/],
    ["a board option value its schema refuses", { options: { set: { n_rows: -1 } } }, /value of the board option "n_rows" fails its schema: -1 is less than 1\./],
    ["a block's state that its class's schema refuses", { blocks: { mod: { b: { state: { n: -1 } } } } }, /state of the block "b" fails its schema at \/n: -1 is less than 0\./],
    ["unsetting a state arg that its class's schema requires", { blocks: { mod: { b: { unset: ["direction"] } } } }, /state of the block "b" fails its schema: Instance does not have required property "direction"\./],
    ["a block whose state its class's schema refuses", { blocks: { add: { g: { ...head(), state: { n: 1, size: 2 } } } } }, /state of the block "g" fails its schema/],
    ["a value of a board option no package registered", { options: { set: { nope: 1 } } }, /board option "nope", which no package registered/],
    ["a block of a class the block registry lacks", { blocks: { add: { g: { ...head(), registry_id: "blockr.foo-bar" } } } }, /class "blockr.foo-bar", which the block registry lacks/],
    ["an id made of digits alone", { blocks: { add: { 7: head() } } }, /block id "7" is empty or made of digits alone/],
    ["removing a block the board lacks", { blocks: { rm: ["z"] } }, /board holds no block "z" to remove/],
    ["adding a block the board holds", { blocks: { add: { a: head() } } }, /board holds block "a" already/],
    ["changing a block the board lacks", { blocks: { mod: { z: { name: "Z" } } } }, /board holds no block "z" to change/],
    ["setting and unsetting one state arg", { blocks: { mod: { b: { state: { n: 1 }, unset: ["n"] } } } }, /both sets and unsets "n"/],
    ["setting and unsetting one board option", { options: { set: { n_rows: 5 }, unset: ["n_rows"] } }, /both sets and unsets the board option "n_rows"/],
    ["a patch of a plugin that the board lacks", { patches: [{ op: "add", path: "/plugins/dag/state/positions/a", value: {} }] }, /The patch 1, add at "\/plugins\/dag\/state\/positions\/a", finds no "dag" in "\/plugins"\./],
    ["a patch of core's properties", { patches: [{ op: "replace", path: "/name", value: "Renamed" }] }, /The patch 1, replace at "\/name", changes "name", where a patch changes the board's plugins alone\./],
    ["a patch of the board's plugins as a whole", { patches: [{ op: "replace", path: "/plugins", value: {} }] }, /changes the board's plugins as a whole, where a patch changes one plugin/],
    ["a patch of a plugin's class", { patches: [{ op: "replace", path: "/plugins/dag/class", value: "x" }] }, /changes "class" of the plugin "dag", where a patch below a plugin changes its name or its state/],
    ["adding a plugin", { patches: [{ op: "add", path: "/plugins/dag", value: { class: "blockr.dag-dag_plugin", name: "DAG", state: {} } }] }, /The patch 1, add at "\/plugins\/dag", changes the plugin "dag" as a whole, where a patch changes a plugin's name or its state, since the app's opening board sets the plugins that a board holds\./],
    ["removing a plugin", { patches: [{ op: "remove", path: "/plugins/dag" }] }, /The patch 1, remove at "\/plugins\/dag", changes the plugin "dag" as a whole/],
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

  test("checks the state of a plugin that a patch changes against its class's schema, and applies no patch that does not find its path", async () => {
    const store = await docked();
    const board = store.board();

    expect(() => {
      store.apply({ patches: [{ op: "add", path: "/plugins/dag/state/positions/c", value: { x: "left", y: 0 } }] });
    }).toThrow(/The state of the plugin "dag" fails its schema at \/positions\/c\/x: Instance type "string" is invalid\. Expected "number"\./);
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/plugins/dock/state/views/side/panels", value: [] }] });
    }).toThrow('The patch 1, replace at "/plugins/dock/state/views/side/panels", finds no "side" in "/plugins/dock/state/views".');
    expect(() => {
      store.apply({ patches: [{ op: "remove", path: "/plugins/dock/state" }] });
    }).toThrow('The patch 1, remove at "/plugins/dock/state", removes the state of the plugin "dock", which a plugin always holds.');
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/plugins/dock/name", value: 7 }] });
    }).toThrow('The plugin "dock" has no string as its name.');
    expect(store.board()).toBe(board);
  });

  test("applies patches in their order, after core's parts", async () => {
    const store = await docked();

    store.apply({
      blocks: { add: { g: head(3) } },
      patches: [
        { op: "add", path: "/plugins/dock/state/views/side", value: { panels: [] } },
        { op: "add", path: "/plugins/dock/state/views/side/panels/-", value: "g" },
        { op: "add", path: "/plugins/dock/state/views/side/panels/0", value: "a" },
        { op: "replace", path: "/plugins/dock/state/views/main/panels/1", value: "g" },
      ],
    });
    expect(viewsOf(store.board())).toEqual({
      main: { panels: ["a", "g"] },
      side: { panels: ["a", "g"] },
    });
  });

  test("refuses a patch that adds, removes or replaces a plugin, and renames a plugin by a patch at its name, which undo takes back", async () => {
    const store = await docked((store) => store.plugins.register(noted));
    const board = store.board();

    const patches: Patch[] = [
      { op: "add", path: "/plugins/notes", value: { class: noted.id, name: "Notes", state: { notes: {} } } },
      { op: "remove", path: "/plugins/dock" },
      { op: "replace", path: "/plugins/dag", value: { class: dag.id, name: "Graph", state: { positions: {} } } },
    ];
    for (const patch of patches) {
      expect(() => {
        store.apply({ patches: [patch] });
      }).toThrow(
        `The patch 1, ${patch.op} at "${patch.path}", changes the plugin "${patch.path.slice("/plugins/".length)}" as a whole, where a patch changes a plugin's name or its state, since the app's opening board sets the plugins that a board holds.`,
      );
    }
    expect(store.board()).toBe(board);

    store.apply({ patches: [{ op: "replace", path: "/plugins/dag/name", value: "Graph" }] });
    expect(store.board().plugins.dag?.name).toBe("Graph");
    store.undo();
    expect(store.board()).toEqual(board);
  });


  test("under the board lock takes a patch inside a plugin's state, which the plugin locks itself, but renames no plugin", async () => {
    const locked: HelloReply = { ...docking, lock: { board: true, load: false } };
    const store = await opened(dockClasses, locked);

    store.apply({ patches: [{ op: "replace", path: "/plugins/dag/state/positions/a", value: { x: 9, y: 9 } }] });
    expect(positionsOf(store.board()).a).toEqual({ x: 9, y: 9 });
    store.apply({ patches: [{ op: "replace", path: "/plugins/dag/state", value: { positions: {} } }] });
    expect(positionsOf(store.board())).toEqual({});
    store.undo();
    expect(positionsOf(store.board()).a).toEqual({ x: 9, y: 9 });

    const board = store.board();
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/plugins/dag/name", value: "Graph" }] });
    }).toThrow('The board lock refuses to rename plugin "dag".');
    expect(store.board()).toBe(board);
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
    const edits = [store.editId({ block: "b" }), store.editId({ plugin: "dock" })];
    const seen = changes(store);

    store.apply(
      [
        { name: "Iris review" },
        { blocks: { mod: { b: { state: { n: 6 }, unset: ["nothing"], name: "Head" } } } },
        { links: { mod: { ab: { input: "data" } } }, stacks: { mod: { s1: { blocks: ["a", "b"] } } } },
        { options: { set: { n_rows: 50 }, unset: ["zoom"] } },
        {
          patches: [
            { op: "replace", path: "/plugins/dock/state/views/main/panels/0", value: "a" },
            { op: "add", path: "/plugins/dock/state/views/main", value: { panels: ["a", "b"] } },
          ],
        },
      ],
      "assistant",
    );

    expect(store.board()).toBe(board);
    expect(seen).toEqual([]);
    expect(store.canUndo("assistant")).toBe(false);
    expect([store.editId({ block: "b" }), store.editId({ plugin: "dock" })]).toEqual(edits);
  });

  test("keeps in a step only what it changes, so that its undo sets back nothing else", async () => {
    const store = await opened();

    store.apply({ blocks: { mod: { b: { state: { n: 6, direction: "tail" } } } } });
    store.apply({ blocks: { mod: { b: { state: { n: 10 } } } } }, "assistant");
    store.undo();

    expect(store.board().blocks.b?.state).toEqual({ n: 10, direction: "head" });
  });
});

describe("the locks", () => {
  // R's reply to hello with the locks given, whose opening board, the sample
  // board, gives the head block b a lock of false, which the board lock leaves
  // open, and the merge block c a lock of true, while every other block's lock
  // is null, which follows the board lock.
  function locking(lock: HelloReply["lock"]): HelloReply {
    const { blocks } = hello.board.board;
    const lockOf = (id: string, locked: boolean | null): Block & { version: number } => {
      const block = blocks[id];
      if (block === undefined) throw new Error(`The fixture's board has no block ${id}.`);
      return { ...block, locked };
    };
    return {
      ...hello,
      lock,
      board: {
        ...hello.board,
        board: { ...hello.board.board, blocks: { ...blocks, b: lockOf("b", false), c: lockOf("c", true) } },
      },
    };
  }

  test("says which blocks a lock fixes, by the board lock and each block's lock", async () => {
    const loading = await opened(undefined, locking({ board: false, load: true }));
    expect(["a", "b", "c"].map((id) => loading.locked(id))).toEqual([false, false, false]);

    const store = await opened(undefined, locking({ board: true, load: false }));
    expect(store.board().blocks.a?.locked).toBeNull();
    // A block that the board lacks reads as fixed, since no delta adds one.
    expect(["a", "b", "c", "z"].map((id) => store.locked(id))).toEqual([true, false, true, true]);
  });

  test.each<[string, Delta, string]>([
    ["adding a block", { blocks: { add: { g: head(3) } } }, 'The board lock refuses to add block "g".'],
    ["removing a block", { blocks: { rm: ["f"] } }, 'The board lock refuses to remove block "f".'],
    ["renaming a block", { blocks: { mod: { b: { name: "Top" } } } }, 'The board lock refuses to rename block "b".'],
    [
      "changing a block's lock",
      { blocks: { mod: { b: { locked: true } } } },
      'The board lock refuses to change the lock of block "b".',
    ],
    [
      "changing the state of a block whose lock is true",
      { blocks: { mod: { c: { state: { all_x: true } } } } },
      'The lock of block "c" fixes its state under the board lock.',
    ],
    [
      "unsetting a state arg of a block whose lock follows the board lock",
      { blocks: { mod: { a: { unset: ["package"] } } } },
      'The board lock fixes the state of block "a", whose lock follows the board lock.',
    ],
    ["adding a link", { links: { add: { fe: { from: "f", to: "e", input: "" } } } }, 'The board lock refuses to add link "fe".'],
    ["changing a link", { links: { mod: { ad: { from: "b" } } } }, 'The board lock refuses to change link "ad".'],
    ["removing a link", { links: { rm: ["ab"] } }, 'The board lock refuses to remove link "ab".'],
    [
      "adding a stack",
      { stacks: { add: { s2: { name: "More", blocks: ["c"], color: null } } } },
      'The board lock refuses to add stack "s2".',
    ],
    ["changing a stack", { stacks: { mod: { s1: { color: "#66c2a5" } } } }, 'The board lock refuses to change stack "s1".'],
    ["removing a stack", { stacks: { rm: ["s1"] } }, 'The board lock refuses to remove stack "s1".'],
    ["renaming the board", { name: "Sepals" }, "The board lock refuses to rename the board."],
  ])("under the board lock, refuses %s, naming the lock, and changes nothing", async (_, delta, message) => {
    const store = await opened(undefined, locking({ board: true, load: false }));
    const board = store.board();

    expect(() => {
      store.apply(delta);
    }).toThrow(message);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });

  test("under the board lock, applies a change to the state of a block that it leaves open and to a board option's value", async () => {
    const store = await opened(undefined, locking({ board: true, load: true }));

    store.apply({ blocks: { mod: { b: { state: { n: 3 } } } } });
    expect(store.board().blocks.b?.state).toEqual({ n: 3, direction: "head" });
    store.undo();
    expect(store.board().blocks.b?.state).toEqual({ n: 6, direction: "head" });
    store.redo();
    expect(store.board().blocks.b?.state).toEqual({ n: 3, direction: "head" });

    store.apply({ options: { set: { n_rows: 25 } } });
    expect(store.board().options.n_rows).toBe(25);

    // Setting what a locked block holds already changes nothing, so the lock
    // refuses nothing.
    const board = store.board();
    store.apply({ blocks: { mod: { c: { state: { all_x: false } } } } });
    expect(store.board()).toBe(board);
  });

  test("applies under the load lock alone what the board lock refuses, and opens a JSON board's text under the board lock alone", async () => {
    const loading = await opened(undefined, locking({ board: false, load: true }));
    loading.apply([{ blocks: { mod: { c: { state: { all_x: true }, locked: false } } } }, { name: "Sepals" }]);
    expect(loading.board().blocks.c?.locked).toBe(false);

    const store = unopened(undefined, locking({ board: true, load: false }));
    await store.open(file({}));
    expect(store.board().id).toBe("amber_tapir");
    expect(store.locked("a")).toBe(true);
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

  test("are kept for each plugin, which a patch names", async () => {
    const store = await docked();
    const first = store.editId({ plugin: "dock" });
    const placed = store.editId({ plugin: "dag" });
    expect(first).toBeTypeOf("number");

    store.apply({ patches: [{ op: "add", path: "/plugins/dock/state/views/side", value: { panels: [] } }] });
    expect(store.editId({ plugin: "dag" })).toBe(placed);
    const edit = store.editId({ plugin: "dock" });
    expect(edit).toBeGreaterThan(first ?? Infinity);

    store.apply({ patches: [{ op: "replace", path: "/plugins/dock/state/views/side/panels", value: ["a"], edit }] });
    expect(() => {
      store.apply({ patches: [{ op: "replace", path: "/plugins/dock/state/views/side/panels", value: ["b"], edit }] });
    }).toThrow(/built on edit \d+ of the plugin "dock", which another edit has replaced with edit \d+\.$/);
    expect(viewsOf(store.board()).side).toEqual({ panels: ["a"] });
    expect(store.editId({ plugin: "notes" })).toBeUndefined();
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
    // The scatter block's class requires neither of its state args.
    store.apply({ blocks: { mod: { f: { unset: ["y"] } } } });
    const before = store.board();

    store.apply({ blocks: { mod: { f: { state: { y: "Petal.Width" } }, a: { name: "Renamed" } } } });
    store.undo();

    expect(store.board()).toEqual(before);
    expect(Object.hasOwn(store.board().blocks.f?.state ?? {}, "y")).toBe(false);
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
    expect(options).toEqual({ n_rows: 20, seed: 42 });

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
    expect(store.board().options).toEqual({ n_rows: 20 });
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
        { op: "add", path: "/plugins/dock/state/views/side", value: { panels: ["a"] } },
        { op: "add", path: "/plugins/dock/state/views/main/panels/0", value: "c" },
        { op: "replace", path: "/plugins/dag/state/positions/a", value: { x: 5, y: 5 } },
      ],
    });
    store.undo();

    expect(store.board()).toEqual(before);
    const undo = seen[1];
    expect(undo && "delta" in undo ? JSON.parse(JSON.stringify(undo.delta)) : undefined).toEqual({
      patches: [
        { op: "replace", path: "/plugins/dag/state/positions/a", value: { x: 0, y: 0 } },
        { op: "remove", path: "/plugins/dock/state/views/main/panels/0" },
        { op: "remove", path: "/plugins/dock/state/views/side" },
      ],
    });
  });

  test("puts a member that a patch removed back in its place, before the member that followed it", async () => {
    const store = await docked();
    const order = () => Object.keys(viewsOf(store.board()));
    store.apply({
      patches: [
        { op: "add", path: "/plugins/dock/state/views/side", value: { panels: [] } },
        { op: "add", path: "/plugins/dock/state/views/data", value: { panels: [] } },
      ],
    });
    expect(order()).toEqual(["main", "side", "data"]);

    store.apply({
      patches: [
        { op: "remove", path: "/plugins/dock/state/views/side" },
        { op: "remove", path: "/plugins/dock/state/views/main" },
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
          { op: "add", path: "/plugins/dock/state/views/side", value: { panels: [] } },
          { op: "add", path: "/plugins/dock/state/views/data", value: { panels: [] } },
        ],
      },
      "setup",
    );

    store.apply({ patches: [{ op: "remove", path: "/plugins/dock/state/views/side" }] });
    store.apply({ patches: [{ op: "add", path: "/plugins/dock/state/views/extra", value: { panels: [] } }] }, "assistant");
    store.undo();
    expect(order()).toEqual(["main", "side", "data", "extra"]);

    store.apply({ patches: [{ op: "remove", path: "/plugins/dock/state/views/side" }] });
    store.apply({ patches: [{ op: "remove", path: "/plugins/dock/state/views/data" }] }, "assistant");
    store.undo();
    expect(order()).toEqual(["main", "extra", "side"]);
  });

  test("sets back a plugin's state over what another origin changed since, and leaves out the patches whose path is gone", async () => {
    const store = await docked();
    store.apply({
      patches: [
        { op: "add", path: "/plugins/dock/state/views/side", value: { panels: ["a"] } },
        { op: "replace", path: "/plugins/dock/state/views/main/panels", value: ["b", "a"] },
      ],
    });
    store.apply(
      {
        patches: [
          { op: "remove", path: "/plugins/dock/state/views/side" },
          { op: "replace", path: "/plugins/dock/state/views/main/panels", value: ["a"] },
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

  test("leaves out the patches of a plugin's state that no longer find their path, and puts back what another origin removed", async () => {
    const store = await docked();
    store.apply({ patches: [{ op: "replace", path: "/plugins/dag/state/positions/a", value: { x: 5, y: 5 } }] });
    store.apply({ patches: [{ op: "remove", path: "/plugins/dag/state/positions/a" }] }, "assistant");

    // The user's step changed a position that the assistant has removed since.
    store.undo();
    expect(Object.hasOwn(positionsOf(store.board()), "a")).toBe(false);

    store.undo("assistant");
    expect(positionsOf(store.board()).a).toEqual({ x: 5, y: 5 });
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
  test("holds no plugin class until a package registers one", () => {
    expect(newStore().plugins.registered()).toEqual({});
  });

  test("a plugin class, a board option id or a block class's migration registered twice fails", () => {
    const store = newStore();
    store.plugins.register(dock);

    expect(() => store.plugins.register({ ...dock })).toThrow('The plugin class "blockr.dock-dock_plugin" is registered already.');
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

  test("a plugin class is named as R names one, {pkg}-{name}_plugin", () => {
    const store = newStore();

    for (const id of ["dock", "dock_plugin", "blockr.dock-dock", "x-dock_plugin", "blockr.dock-_plugin", 7]) {
      expect(() => store.plugins.register({ id } as unknown as PluginClass)).toThrow(
        `The plugin class id ${JSON.stringify(id)} is not named as R names a plugin class, "{pkg}-{name}_plugin".`,
      );
    }
    expect(() => store.plugins.register({ id: "blockr.dock2-my_dock_plugin" })).not.toThrow();
  });

  test("a plugin class's members are of their kinds", () => {
    const store = newStore();
    // As a package that writes its JS without types may register it.
    const register = (def: unknown) => store.plugins.register(def as PluginClass);
    const id = "blockr.dock-dock_plugin";

    expect(() => register({ id, update: "views" })).toThrow(`The plugin class "${id}" has an update that is no function.`);
    expect(() => register({ id, migrate: 3 })).toThrow(`The plugin class "${id}" has a migrate that is no function.`);
    expect(() => register({ id, start: {} })).toThrow(`The plugin class "${id}" has a start that is no function.`);
    expect(() => register({ id, ui: "" })).toThrow(`The plugin class "${id}" has a ui that is no module's id.`);
    expect(() => register({ id, frontend: 1 })).toThrow(`The plugin class "${id}" has a frontend that is no module's id.`);
    expect(() => register({ id, storage: { save: () => undefined } })).toThrow(
      `The plugin class "${id}" has a storage without save() and open() functions.`,
    );
    expect(() => register({ id, options: "n_rows" })).toThrow(
      `The plugin class "${id}" requires board options that are no array of their ids.`,
    );
    expect(() => register({ id, state: { schema: {} } })).toThrow(
      `The plugin class "${id}" gives a state without a schema and a default.`,
    );
    expect(store.plugins.registered()).toEqual({});
  });

  test("registered() returns the same record until a registration changes it", () => {
    const store = newStore();
    const before = store.plugins.registered();
    expect(store.plugins.registered()).toBe(before);

    const unregister = store.plugins.register(dock);
    const during = store.plugins.registered();
    expect(during).not.toBe(before);
    expect(during).toEqual({ "blockr.dock-dock_plugin": dock });

    unregister();
    expect(store.plugins.registered()).toEqual(before);
    expect(() => store.plugins.register(dock)).not.toThrow();
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
      store.plugins.register({ ...dock, state: { schema: { type: "object" }, default: [] } }),
    ).toThrow(
      'The default of the state of the plugin class "blockr.dock-dock_plugin" fails its schema: Instance type "array" is invalid. Expected "object".',
    );
  });

  test("a board option's migrate that is no function fails", () => {
    const store = newStore();
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
