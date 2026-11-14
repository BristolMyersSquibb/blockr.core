// @vitest-environment node

// The command registry, on the board of a core app from the wire protocol's
// fixtures.

import { describe, expect, test, vi } from "vitest";
import type { Lock } from "../protocol/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { PluginClass, Store } from "../store/index.ts";
import { openedStore, withPlugins } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { CommandError, createCommands } from "./index.ts";
import type { Command, Commands } from "./index.ts";

async function setup(
  register?: (store: Store) => void,
): Promise<{ store: Store; commands: Commands }> {
  const store = await openedStore(register);
  return { store, commands: createCommands(store) };
}

// A command of another package, which renames block "b".
function renameB(
  more: Partial<Command<{ readonly name: string }>> = {},
): Command<{ readonly name: string }> {
  return {
    id: "test.pkg-rename_b",
    label: "Rename b",
    args: {
      type: "object",
      properties: { name: { type: "string" } },
      required: ["name"],
      additionalProperties: false,
    },
    run: (_board, { name }) => ({ blocks: { mod: { b: { name } } } }),
    ...more,
  };
}

// A plugin class that keeps a position for each block, as the DAG keeps one,
// and the command of its package that changes one, for the plugin of the
// class, which the id of the class names.
const placed: PluginClass = {
  id: "test.pkg-placed_plugin",
  state: {
    schema: {
      type: "object",
      properties: {
        positions: {
          type: "object",
          additionalProperties: { type: "object", properties: { x: { type: "number" } } },
        },
      },
    },
    default: { positions: {} },
  },
};

// A command of another package that names block "b" after the board, which
// reads the board that it is planned on.
function nameB(run = vi.fn((board: Board) => ({ blocks: { mod: { b: { name: board.name } } } }))): Command {
  return { id: "test.pkg-name_b", label: "Name b after the board", run };
}

// A command of another package that renames the board once its prepare()
// resolves with what to add to the board's name.
function later(prepare: () => Promise<string>): Command<Json, string> {
  return {
    id: "test.pkg-later",
    label: "Later",
    prepare,
    run: (board, suffix) => ({ name: `${board.name}${suffix}` }),
  };
}

// What a promise rejects with, which has to be a CommandError.
async function rejection(promise: Promise<unknown>): Promise<CommandError> {
  const error: unknown = await promise.then(
    () => undefined,
    (reason: unknown) => reason,
  );
  if (!(error instanceof CommandError)) {
    throw new Error(`Expected a CommandError, but got ${String(error)}.`);
  }
  return error;
}

// The origins of the changes that the store announces from now on.
function origins(store: Store): string[] {
  const seen: string[] = [];
  store.subscribe((change) => {
    if ("delta" in change) seen.push(change.origin);
  });
  return seen;
}

const move: Command<{ readonly id: string; readonly x: number }> = {
  id: "test.pkg-move",
  label: "Move block",
  args: {
    type: "object",
    properties: { id: { type: "string" }, x: { type: "number" } },
    required: ["id", "x"],
    additionalProperties: false,
  },
  run: (_board, { id, x }) => ({
    patches: [{ op: "add", path: `/plugins/test.pkg-placed_plugin/positions/${id}`, value: { x } }],
  }),
};

describe("registering", () => {
  test("holds core's commands", async () => {
    const { commands } = await setup();

    expect(Object.keys(commands.registered())).toEqual([
      "blockr.core-add_block",
      "blockr.core-remove_block",
      "blockr.core-rename_block",
      "blockr.core-set_block_lock",
      "blockr.core-set_block",
      "blockr.core-append_block",
      "blockr.core-prepend_block",
      "blockr.core-insert_block",
      "blockr.core-add_link",
      "blockr.core-change_link",
      "blockr.core-move_link",
      "blockr.core-remove_link",
      "blockr.core-add_stack",
      "blockr.core-change_stack",
      "blockr.core-rename_stack",
      "blockr.core-remove_stack",
      "blockr.core-rename_board",
      "blockr.core-set_option",
      "blockr.core-set_plugin_state",
    ]);
  });

  test("registered() returns the same record until a registration changes it", async () => {
    const { commands } = await setup();
    const before = commands.registered();
    expect(commands.registered()).toBe(before);

    const unregister = commands.register(renameB());
    const after = commands.registered();
    expect(after).not.toBe(before);
    expect(after["test.pkg-rename_b"]?.label).toBe("Rename b");

    unregister();
    expect(Object.keys(commands.registered())).toEqual(Object.keys(before));
  });

  test("a command id registered twice fails, and so does one not named as a registry id is", async () => {
    const { commands } = await setup();
    commands.register(renameB());

    expect(() => commands.register(renameB())).toThrow(
      'The command "test.pkg-rename_b" is registered already.',
    );
    expect(() => commands.register(renameB({ id: "rename_b" }))).toThrow(
      'The command id "rename_b" is not named as a registry id is, "{pkg}-{name}".',
    );
    expect(() => commands.register(renameB({ id: "test.pkg-" }))).toThrow("not named as a registry id is");
    expect(() => commands.register(renameB({ id: "test.pkg-x", label: "" }))).toThrow(
      'The command "test.pkg-x" has no label.',
    );
  });
});

describe("running a command", () => {
  test("applies its deltas as one step of the origin, which one undo takes back", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-insert_block", {
      link: "ab",
      registry_id: "blockr.core-head_block",
    }, "assistant");

    expect(Object.keys(store.board().blocks)).toHaveLength(7);
    expect(store.canUndo("user")).toBe(false);
    expect(store.canUndo("assistant")).toBe(true);

    store.undo("assistant");
    expect(store.board()).toEqual(board);
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
  });

  test("applies the deltas of a command with undo: false as no undo step, which leaves the undo and redo lists as they are", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));
    store.apply({ name: "Sepals" });
    store.apply({ name: "Petals" });
    store.undo();

    await commands.run("test.pkg-rename_b", { name: "Renamed" });

    expect(store.board().blocks.b?.name).toBe("Renamed");
    expect([store.canUndo(), store.canRedo()]).toEqual([true, true]);
    store.redo();
    expect(store.board().name).toBe("Petals");
    store.undo();
    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.canUndo()).toBe(false);
    expect(store.board().blocks.b?.name).toBe("Renamed");
  });

  test("applies a command that does not prepare before run() returns", async () => {
    const { store, commands } = await setup();

    const done = commands.run("blockr.core-rename_board", { name: "Sepals" });
    expect(store.board().name).toBe("Sepals");
    await done;
  });

  test("refuses args that fail the command's schema, naming each failure, and changes nothing", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const refused = commands.run("blockr.core-add_link", { from: 1, to: "12", extra: true });
    await expect(refused).rejects.toThrow(CommandError);
    await expect(refused).rejects.toThrow('The command "blockr.core-add_link" refused to run');
    await expect(refused).rejects.toThrow(
      'The args fail their schema at /from: Instance type "number" is invalid. Expected "string".',
    );
    await expect(refused).rejects.toThrow("The args fail their schema at /to:");
    await expect(refused).rejects.toThrow('"extra"');
    expect(store.board()).toBe(board);
  });

  test("takes args left out as no args", async () => {
    const { commands } = await setup();
    await expect(commands.run("blockr.core-rename_board")).rejects.toThrow(
      'Instance does not have required property "name".',
    );
    await expect(commands.run("blockr.core-add_stack")).resolves.toBeUndefined();
  });

  test("refuses a command that no package registered", async () => {
    const { commands } = await setup();
    await expect(commands.run("test.pkg-nothing")).rejects.toThrow(
      'No command "test.pkg-nothing" is registered.',
    );
  });

  test("refuses a command that is not enabled with its args, naming them", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(commands.run("blockr.core-remove_block", { id: "zz" })).rejects.toThrow(
      'It is not enabled on the board as it is, with the args {"id":"zz"}.',
    );
    expect(store.board()).toBe(board);

    // Its CommandError says that its enabled() refused it, and has no index,
    // since the command ran alone.
    const error = await rejection(commands.run("blockr.core-remove_block", { id: "zz" }));
    expect([error.enabled, error.index]).toEqual([false, undefined]);
    const failed = await rejection(commands.run("blockr.core-rename_board", { name: 1 }));
    expect(failed.enabled).toBeUndefined();
  });

  test("rejects with what the store refused, having changed nothing", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const refused = commands.run("blockr.core-add_link", { from: "e", to: "e" });
    await expect(refused).rejects.toThrow(StoreError);
    await expect(refused).rejects.toThrow('The links form a cycle through the blocks "e".');
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });

  test("names the command in what its own code threw", async () => {
    const { commands } = await setup();
    const error = new Error("No layout.");
    commands.register(
      renameB({
        run: () => {
          throw error;
        },
      }),
    );

    const failed = commands.run("test.pkg-rename_b", { name: "Top" });
    await expect(failed).rejects.toThrow('The command "test.pkg-rename_b" failed\n- No layout.');
    await expect(failed).rejects.toHaveProperty("cause", error);
  });

  test("refuses to run before the store opens a board", async () => {
    const store = createStore();
    const commands = createCommands(store);
    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).rejects.toThrow(
      'The command "blockr.core-rename_board" refused to run\n- The store holds no board until it opens one.',
    );
    expect(commands.enabled("blockr.core-rename_board", { name: "Sepals" })).toBe(false);
  });

  test("applies no step where the command changes nothing", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-rename_board", { name: "Iris review" });
    expect(store.canUndo()).toBe(false);
  });
});

describe("enabled()", () => {
  test("says whether the command applies to the current board with these args", async () => {
    const { commands } = await setup();

    expect(commands.enabled("blockr.core-remove_block", { id: "b" })).toBe(true);
    expect(commands.enabled("blockr.core-remove_block", { id: "zz" })).toBe(false);
    expect(commands.enabled("blockr.core-remove_block", { id: 1 })).toBe(false);
    expect(commands.enabled("test.pkg-nothing")).toBe(false);
  });

  test("is false where the command's enabled() throws", async () => {
    const { commands } = await setup();
    commands.register(
      renameB({
        enabled: () => {
          throw new Error("No idea.");
        },
      }),
    );
    expect(commands.enabled("test.pkg-rename_b", { name: "Top" })).toBe(false);
  });
});

describe("preparing", () => {
  test("waits for prepare(), then runs on the board as it is once prepare() resolves", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<string>();
    const read: string[] = [];
    commands.register<Json, string>({
      id: "test.pkg-later",
      label: "Later",
      prepare: () => ready.promise,
      run(board, suffix) {
        read.push(board.name);
        return { name: `${board.name}${suffix}` };
      },
    });

    const done = commands.run("test.pkg-later");
    store.apply({ name: "Sepals" });
    ready.resolve("!");
    await done;

    expect(read).toEqual(["Sepals"]);
    expect(store.board().name).toBe("Sepals!");
  });

  test("checks enabled() again once prepare() resolves", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<null>();
    const run = vi.fn(() => ({ name: "Never" }));
    commands.register<{ readonly id: string }, null>({
      id: "test.pkg-later",
      label: "Later",
      enabled: (board, { id }) => Object.hasOwn(board.blocks, id),
      prepare: () => ready.promise,
      run,
    });

    const done = commands.run("test.pkg-later", { id: "b" });
    store.apply({ blocks: { rm: ["b"] } });
    ready.resolve(null);

    await expect(done).rejects.toThrow('It is not enabled on the board as it is, with the args {"id":"b"}.');
    expect(run).not.toHaveBeenCalled();
  });

  test("drops a command still in its prepare() as another board opens, and runs none after", async () => {
    const store = await openedStore();
    const controller = new AbortController();
    const commands = createCommands(store, { signal: controller.signal });
    const ready = Promise.withResolvers<null>();
    const run = vi.fn(() => ({ name: "Never" }));
    commands.register<Json, null>({
      id: "test.pkg-later",
      label: "Later",
      prepare: () => ready.promise,
      run,
    });

    const done = commands.run("test.pkg-later");
    controller.abort();
    ready.resolve(null);

    await expect(done).rejects.toThrow("A board opened while it prepared, which drops it.");
    expect(run).not.toHaveBeenCalled();
    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).rejects.toThrow(
      'The command "blockr.core-rename_board" refused to run\n- Another board opened in a new store, so this command registry runs no command.',
    );
    await expect(commands.plan("blockr.core-rename_board", { name: "Sepals" })).rejects.toThrow(CommandError);
    expect(commands.enabled("blockr.core-rename_board", { name: "Sepals" })).toBe(false);
    expect(store.board().name).toBe("Iris review");
  });

  test("names the command where prepare() rejects", async () => {
    const { commands } = await setup();
    commands.register<Json, null>({
      id: "test.pkg-later",
      label: "Later",
      prepare: () => Promise.reject(new Error("No layout.")),
      run: () => [],
    });
    await expect(commands.run("test.pkg-later")).rejects.toThrow(
      'The command "test.pkg-later" failed\n- No layout.',
    );
  });
});

describe("the context", () => {
  test("hands enabled(), prepare() and run() the locks, the blockr options and ids in the shape that rand_id names", async () => {
    const store = await openedStore();
    const lock: Lock = { board: false, load: true };
    const asked: string[] = [];
    const commands = createCommands(store, {
      lock,
      randId: "letters_6",
      blockrOption(name) {
        asked.push(name);
        return Promise.resolve(name === "stack_name" ? "Petals" : undefined);
      },
    });
    const locks: Lock[] = [];
    commands.register<Json, Json | undefined>({
      id: "test.pkg-add_stack",
      label: "Add stack",
      enabled(_board, _args, context) {
        locks.push(context.lock);
        return true;
      },
      prepare: (_board, _args, context) => context.blockrOption("stack_name"),
      run(board, name, context) {
        const id = context.mintId((id) => Object.hasOwn(board.stacks, id));
        // The command converts the blockr option's value and takes a fallback
        // of its own where nothing sets it.
        return { stacks: { add: { [id]: { name: typeof name === "string" ? name : "Stack", blocks: [], color: null } } } };
      },
    });
    const before = Object.keys(store.board().stacks);

    await commands.run("test.pkg-add_stack");

    // The registry checks enabled() before prepare() and again once it
    // resolves.
    expect(locks).toEqual([lock, lock]);
    expect(asked).toEqual(["stack_name"]);
    const minted = Object.keys(store.board().stacks).filter((id) => !before.includes(id));
    expect(minted).toEqual([expect.stringMatching(/^[a-z]{6}$/)]);
    expect(store.board().stacks[minted[0] ?? ""]?.name).toBe("Petals");
  });
});

describe("planning", () => {
  test("returns the deltas without applying them", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const planned = await commands.plan("blockr.core-rename_board", { name: "Sepals" });
    expect(planned).toEqual([{ name: "Sepals" }]);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);

    store.apply(planned, "assistant");
    expect(store.board().name).toBe("Sepals");
    expect(store.canUndo("assistant")).toBe(true);
  });

  test("fills in the edit ids of the board it read, so that the store refuses a plan once another edit came first", async () => {
    const { store, commands } = await setup();

    const planned = await commands.plan("blockr.core-set_block", { id: "b", state: { n: 10 } });
    expect(planned).toEqual([
      { blocks: { mod: { b: { edit: store.editId({ block: "b" }), state: { n: 10 } } } } },
    ]);

    await commands.run("blockr.core-set_block", { id: "b", state: { n: 20 } });
    expect(() => {
      store.apply(planned, "assistant");
    }).toThrow(/The delta was built on edit \d+ of block "b", which another edit has replaced/);
  });

  test("fills in the edit id of the plugin that each patch changes", async () => {
    const store = await openedStore(
      (store) => store.plugins.register(placed),
      withPlugins({ [placed.id]: { name: "Placed", state: { positions: {} } } }),
    );
    const commands = createCommands(store);
    commands.register(move);

    await commands.run("test.pkg-move", { id: "a", x: 1 });
    const planned = await commands.plan("test.pkg-move", { id: "a", x: 2 });
    expect(planned).toEqual([
      {
        patches: [
          {
            op: "add",
            path: "/plugins/test.pkg-placed_plugin/positions/a",
            value: { x: 2 },
            edit: store.editId({ plugin: placed.id }),
          },
        ],
      },
    ]);

    await commands.run("test.pkg-move", { id: "a", x: 3 });
    expect(() => {
      store.apply(planned, "assistant");
    }).toThrow(/The delta was built on edit \d+ of the plugin "test\.pkg-placed_plugin", which another edit has replaced/);
  });

  test("keeps the edit ids that a command names itself", async () => {
    const { commands } = await setup();
    commands.register(renameB({ run: (_board, { name }) => ({ blocks: { mod: { b: { edit: 99, name } } } }) }));

    expect(await commands.plan("test.pkg-rename_b", { name: "Top" })).toEqual([
      { blocks: { mod: { b: { edit: 99, name: "Top" } } } },
    ]);
  });
});

describe("running a list", () => {
  test("applies the commands in order as one step of the origin, each planned on the board that those before it leave, which one undo takes back", async () => {
    const { store, commands } = await setup();
    const board = store.board();
    const seen = origins(store);
    // Alone, the link is not enabled, since the board lacks block "g".
    expect(commands.enabled("blockr.core-add_link", { from: "d", to: "g" })).toBe(false);

    await commands.run(
      [
        { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
        { id: "blockr.core-add_link", args: { from: "d", to: "g", id: "dg" } },
        { id: "blockr.core-rename_block", args: { id: "g", name: "Top" } },
      ],
      "assistant",
    );

    expect(store.board().blocks.g?.name).toBe("Top");
    expect(store.board().links.dg).toEqual({ from: "d", to: "g", input: "data" });
    expect(seen).toEqual(["assistant", "assistant", "assistant"]);
    expect(store.canUndo("user")).toBe(false);
    store.undo("assistant");
    expect(store.board()).toEqual(board);
    expect(store.canUndo("assistant")).toBe(false);
  });

  test("applies a list whose commands do not prepare before run() returns", async () => {
    const { store, commands } = await setup();

    const done = commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }]);
    expect(store.board().name).toBe("Sepals");
    await done;
  });

  test("takes no undo step where every command of the list has undo: false, and one where any lacks it", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));

    await commands.run([
      { id: "test.pkg-rename_b", args: { name: "One" } },
      { id: "test.pkg-rename_b", args: { name: "Two" } },
    ]);
    expect(store.board().blocks.b?.name).toBe("Two");
    expect(store.canUndo()).toBe(false);

    await commands.run([
      { id: "test.pkg-rename_b", args: { name: "Three" } },
      { id: "blockr.core-rename_board", args: { name: "Sepals" } },
    ]);
    expect(store.canUndo()).toBe(true);
    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.board().blocks.b?.name).toBe("Two");
  });

  test("applies nothing for no commands", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run([], "assistant");
    expect(store.board()).toBe(board);
    expect(store.canUndo("assistant")).toBe(false);
  });
});

describe("refusing a list", () => {
  test("applies none of it where a command is not enabled, and rejects with the command's index and that its enabled() refused it", async () => {
    const { store, commands } = await setup();
    const board = store.board();
    const seen = origins(store);

    const error = await rejection(
      commands.run(
        [
          { id: "blockr.core-rename_board", args: { name: "Sepals" } },
          { id: "blockr.core-remove_block", args: { id: "zz" } },
        ],
        "assistant",
      ),
    );

    expect(error.message).toBe(
      'The list of commands was refused at command 2, "blockr.core-remove_block"\n- It is not enabled on the board as it is, with the args {"id":"zz"}.',
    );
    expect([error.index, error.enabled]).toEqual([1, false]);
    expect(error.failures).toEqual(['It is not enabled on the board as it is, with the args {"id":"zz"}.']);
    expect(error.cause).toBeInstanceOf(CommandError);
    expect(store.board()).toBe(board);
    expect(store.canUndo("assistant")).toBe(false);
    expect(seen).toEqual([]);
  });

  test("checks the args of every command against its schema before it plans any", async () => {
    const { commands } = await setup();
    const prepare = vi.fn(() => Promise.resolve("!"));
    commands.register(later(prepare));

    const error = await rejection(
      commands.run([{ id: "test.pkg-later" }, { id: "blockr.core-rename_board", args: { name: 1 } }]),
    );
    expect(error.index).toBe(1);
    expect(error.enabled).toBeUndefined();
    expect(error.failures).toEqual([expect.stringMatching(/^The args fail their schema at \/name:/)]);
    expect(prepare).not.toHaveBeenCalled();

    const missing = await rejection(commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }, { id: "test.pkg-nothing" }]));
    expect(missing.index).toBe(1);
    expect(missing.failures).toEqual(['No command "test.pkg-nothing" is registered.']);
  });

  test("rejects with the failures with which the store refused a command's deltas, and with its StoreError as the cause", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const error = await rejection(
      commands.run([
        { id: "blockr.core-rename_board", args: { name: "Sepals" } },
        { id: "blockr.core-add_link", args: { from: "e", to: "e" } },
        { id: "blockr.core-rename_block", args: { id: "b", name: "Top" } },
      ]),
    );

    expect(error.index).toBe(1);
    expect(error.enabled).toBeUndefined();
    expect(error.cause).toBeInstanceOf(StoreError);
    expect(error.failures).toEqual([expect.stringContaining('The links form a cycle through the blocks "e".')]);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });

  test("rejects with what a command's own code threw, naming the command", async () => {
    const { commands } = await setup();
    commands.register(
      renameB({
        run: () => {
          throw new Error("No layout.");
        },
      }),
    );

    const error = await rejection(commands.run([{ id: "test.pkg-rename_b", args: { name: "Top" } }]));
    expect(error.index).toBe(0);
    expect(error.failures).toEqual(["No layout."]);
    expect(String(error.cause)).toContain('The command "test.pkg-rename_b" failed');
  });

  test("refuses a list at its first command before the store opens a board", async () => {
    const commands = createCommands(createStore());

    const error = await rejection(commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }]));
    expect(error.index).toBe(0);
    expect(error.failures).toEqual(["The store holds no board until it opens one."]);
  });
});

describe("a list with a command that prepares", () => {
  test("prepares the command on the board that the commands before it leave, and plans those after it on the board that it leaves", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<string>();
    const read: string[][] = [];
    commands.register<Json, string>({
      id: "test.pkg-later",
      label: "Later",
      prepare(board) {
        read.push(Object.keys(board.blocks));
        return ready.promise;
      },
      run: (_board, name) => ({ blocks: { mod: { g: { name } } } }),
    });
    const run = vi.fn((board: Board) => ({ blocks: { mod: { b: { name: board.blocks.g?.name ?? "" } } } }));
    commands.register(nameB(run));

    const done = commands.run([
      { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
      { id: "test.pkg-later" },
      { id: "test.pkg-name_b" },
    ]);
    expect(store.board().blocks.g).toBeUndefined();
    ready.resolve("Later");
    await done;

    expect(read).toEqual([["a", "b", "c", "d", "e", "f", "g"]]);
    expect(store.board().blocks.g?.name).toBe("Later");
    expect(store.board().blocks.b?.name).toBe("Later");
    // The board did not change while the command prepared, so the registry
    // planned the list once.
    expect(run).toHaveBeenCalledTimes(1);
  });

  test("plans the list again from its first command where the board changes while a command prepares, with what each prepare() gave", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<string>();
    const prepare = vi.fn(() => ready.promise);
    commands.register(nameB());
    commands.register(later(prepare));

    const done = commands.run([{ id: "test.pkg-name_b" }, { id: "test.pkg-later" }], "assistant");
    store.apply({ name: "Sepals" });
    ready.resolve("!");
    await done;

    expect(prepare).toHaveBeenCalledTimes(1);
    expect(store.board().blocks.b?.name).toBe("Sepals");
    expect(store.board().name).toBe("Sepals!");
    store.undo("assistant");
    expect(store.board().name).toBe("Sepals");
    expect(store.board().blocks.b?.name).toBe("Head");
  });

  test("checks each command again with its enabled() as it plans the list again", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<string>();
    commands.register(later(() => ready.promise));

    const done = commands.run([
      { id: "blockr.core-rename_block", args: { id: "c", name: "Joined" } },
      { id: "test.pkg-later" },
    ]);
    store.apply({ blocks: { rm: ["c"] } });
    const board = store.board();
    ready.resolve("!");

    const error = await rejection(done);
    expect([error.index, error.enabled]).toEqual([0, false]);
    expect(store.board()).toBe(board);
  });

  test("drops the list where another board opens while a command prepares, applying none of it", async () => {
    const store = await openedStore();
    const controller = new AbortController();
    const commands = createCommands(store, { signal: controller.signal });
    const ready = Promise.withResolvers<string>();
    commands.register(later(() => ready.promise));

    const done = commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }, { id: "test.pkg-later" }]);
    controller.abort();
    ready.resolve("!");

    const error = await rejection(done);
    expect(error.index).toBe(1);
    expect(error.failures).toEqual(["A board opened while it prepared, which drops it."]);
    expect(store.board().name).toBe("Iris review");
  });

  test("names the command where its prepare() rejects", async () => {
    const { commands } = await setup();
    commands.register(later(() => Promise.reject(new Error("No layout."))));

    const error = await rejection(commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }, { id: "test.pkg-later" }]));
    expect(error.index).toBe(1);
    expect(error.failures).toEqual(["No layout."]);
    expect(String(error.cause)).toContain('The command "test.pkg-later" failed');
  });
});

describe("planning a list", () => {
  test("returns the deltas of the commands in order without applying them, with the edit ids of the board it was planned on, and none for a block that an earlier command adds", async () => {
    const { store, commands } = await setup();
    const board = store.board();
    const b = store.editId({ block: "b" });

    const planned = await commands.plan([
      { id: "blockr.core-set_block", args: { id: "b", state: { n: 7 } } },
      { id: "blockr.core-rename_block", args: { id: "b", name: "Top" } },
      { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
      { id: "blockr.core-set_block", args: { id: "g", state: { n: 3 } } },
    ]);

    expect(planned).toEqual([
      { blocks: { mod: { b: { edit: b, state: { n: 7 } } } } },
      { blocks: { mod: { b: { edit: b, name: "Top" } } } },
      { blocks: { add: { g: expect.objectContaining({ registry_id: "blockr.core-head_block" }) as unknown } } },
      { blocks: { mod: { g: { state: { n: 3 } } } } },
    ]);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);

    store.apply(planned, "assistant");
    expect(store.board().blocks.b).toMatchObject({ state: { n: 7 }, name: "Top" });
    expect(store.board().blocks.g?.state).toMatchObject({ n: 3 });
  });

  test("leaves the store to refuse a planned list once another edit came first", async () => {
    const { store, commands } = await setup();

    const planned = await commands.plan([
      { id: "blockr.core-rename_board", args: { name: "Sepals" } },
      { id: "blockr.core-set_block", args: { id: "b", state: { n: 10 } } },
    ]);
    await commands.run("blockr.core-set_block", { id: "b", state: { n: 20 } });

    expect(() => {
      store.apply(planned, "assistant");
    }).toThrow(/The delta was built on edit \d+ of block "b", which another edit has replaced/);
  });

  test("refuses as running the list would, and plans no deltas for no commands", async () => {
    const { commands } = await setup();

    const error = await rejection(
      commands.plan([
        { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
        { id: "blockr.core-add_link", args: { from: "zz", to: "g" } },
      ]),
    );
    expect([error.index, error.enabled]).toEqual([1, false]);
    expect(await commands.plan([])).toEqual([]);
  });
});
