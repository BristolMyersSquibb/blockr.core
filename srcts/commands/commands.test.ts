// @vitest-environment node

// The command registry, on the board of a core app from the wire protocol's
// fixtures.

import { describe, expect, test, vi } from "vitest";
import { createStore, StoreError } from "../store/index.ts";
import type { StateKey, Store } from "../store/index.ts";
import { hello, openedStore } from "../testing/store.ts";
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

const positions: StateKey = {
  path: ["extensions", "blockr.dag-dag", "positions"],
  kind: "collection",
  schema: { type: "object", properties: { x: { type: "number" } } },
};

describe("registering", () => {
  test("holds core's commands", async () => {
    const { commands } = await setup();

    expect(Object.keys(commands.registered())).toEqual([
      "blockr.core-add_block",
      "blockr.core-remove_block",
      "blockr.core-rename_block",
      "blockr.core-set_block",
      "blockr.core-append_block",
      "blockr.core-prepend_block",
      "blockr.core-insert_block",
      "blockr.core-add_link",
      "blockr.core-change_link",
      "blockr.core-remove_link",
      "blockr.core-add_stack",
      "blockr.core-change_stack",
      "blockr.core-rename_stack",
      "blockr.core-remove_stack",
      "blockr.core-rename_board",
      "blockr.core-set_option",
      "blockr.core-add_entry",
      "blockr.core-change_entry",
      "blockr.core-remove_entry",
      "blockr.core-set_value",
      "blockr.core-set_block_attribute",
      "blockr.core-set_front_end_attribute",
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
    expect(store.canUndo()).toBe(false);
    expect(store.canUndo("assistant")).toBe(true);

    store.undo("assistant");
    expect(store.board()).toEqual(board);
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
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
    const store = createStore({ client: { request: () => Promise.reject(new Error("No R.")) } });
    const commands = createCommands(store);
    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).rejects.toThrow(
      "The store holds no board until it opens one.",
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

  test("drops a command still in its prepare() when a board opens", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<null>();
    const run = vi.fn(() => ({ name: "Never" }));
    commands.register<Json, null>({
      id: "test.pkg-later",
      label: "Later",
      prepare: () => ready.promise,
      run,
    });

    const done = commands.run("test.pkg-later");
    await store.open(hello);
    ready.resolve(null);

    await expect(done).rejects.toThrow("A board opened while it prepared, which drops it.");
    expect(run).not.toHaveBeenCalled();
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

  test("fills in the edit ids of entries and values of the front-end's state", async () => {
    const layout: StateKey = {
      path: ["extensions", "blockr.dag-dag", "layout"],
      kind: "value",
      schema: { enum: ["LR", "TB"] },
      initial: "LR",
    };
    const { store, commands } = await setup((store) => {
      store.register(positions);
      store.register(layout);
    });
    await commands.run("blockr.core-add_entry", { path: positions.path, id: "a", entry: { x: 1 } });

    const entry = await commands.plan("blockr.core-change_entry", {
      path: positions.path,
      id: "a",
      set: { x: 2 },
    });
    expect(entry).toEqual([
      {
        front_end: {
          state: [
            {
              path: positions.path,
              change: { mod: { a: { edit: store.editId({ path: positions.path, entry: "a" }), set: { x: 2 } } } },
            },
          ],
        },
      },
    ]);

    const value = await commands.plan("blockr.core-set_value", { path: layout.path, value: "TB" });
    expect(value).toEqual([
      {
        front_end: {
          state: [{ path: layout.path, change: { edit: store.editId({ path: layout.path }), set: "TB" } }],
        },
      },
    ]);
  });

  test("keeps the edit ids that a command names itself", async () => {
    const { commands } = await setup();
    commands.register(renameB({ run: (_board, { name }) => ({ blocks: { mod: { b: { edit: 99, name } } } }) }));

    expect(await commands.plan("test.pkg-rename_b", { name: "Top" })).toEqual([
      { blocks: { mod: { b: { edit: 99, name: "Top" } } } },
    ]);
  });
});
