// @vitest-environment node

// The command registry, on the board of a core app from the wire protocol's
// fixtures.

import { context, SpanStatusCode, trace } from "@opentelemetry/api";
import { describe, expect, test, vi } from "vitest";
import type { Lock } from "../protocol/index.ts";
import { createStore, StoreError } from "../store/index.ts";
import type { PluginClass, Store } from "../store/index.ts";
import { logged } from "../testing/logged.ts";
import type { Logged } from "../testing/logged.ts";
import { recordSpans } from "../testing/spans.ts";
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

// The one line that the test logged, which the command registry logged at
// error with the CommandError that the call rejected with or that
// preflight() swallowed, as the message of that error and its cause.
function loggedFailure(reported: Logged): { message: string; cause: unknown } {
  const [line, ...rest] = reported.lines();
  expect(rest).toEqual([]);
  expect(line?.level).toBe("error");
  expect(line?.package).toBe("blockr.core");
  expect(line?.error).toBeInstanceOf(CommandError);
  expect(line?.message).toBe(line?.error?.message);
  return { message: line?.message ?? "", cause: line?.error?.cause };
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

// An afterStep for the class of `placed`, which places each block that a
// step adds one further along than the block that it reads from, by the links
// of the board that the step leaves, as the DAG places what Append block adds.
const placeAfterSource: NonNullable<PluginClass["afterStep"]> = (board, before, plugin) => {
  const { positions } = board.plugins[plugin] as { readonly positions: Readonly<Record<string, { readonly x: number }>> };
  return Object.keys(board.blocks)
    .filter((id) => !Object.hasOwn(before.blocks, id))
    .map((id) => {
      const from = Object.values(board.links).find((link) => link.to === id)?.from;
      const x = from === undefined ? 0 : (positions[from]?.x ?? 0) + 1;
      return { op: "add", path: `/plugins/${plugin}/positions/${id}`, value: { x } };
    });
};

// A store and its command registry on the board of a core app that runs the
// plugin of `placed`, which holds a position for block d, with the afterStep
// given.
async function setupPlacing(
  afterStep: NonNullable<PluginClass["afterStep"]> = placeAfterSource,
): Promise<{ store: Store; commands: Commands }> {
  const store = await openedStore(
    (store) => store.plugins.register({ ...placed, afterStep }),
    withPlugins({ [placed.id]: { name: "Placed", state: { positions: { d: { x: 4 } } } } }),
  );
  return { store, commands: createCommands(store) };
}

// The commands of a batch that adds block g and then links block d to it, as
// Append block does.
const appendG: readonly { readonly id: string; readonly args?: Json }[] = [
  { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
  { id: "blockr.core-add_link", args: { from: "d", to: "g", id: "dg" } },
];

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

    // Removing a block removes its links and its place in its stack as well.
    await commands.run("blockr.core-remove_block", { id: "b" }, "assistant");

    expect(Object.keys(store.board().blocks)).toHaveLength(5);
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
    await expect(commands.run("blockr.core-add_stack")).resolves.toBe(true);
  });

  test("refuses a command that no package registered", async () => {
    const { commands } = await setup();
    await expect(commands.run("test.pkg-nothing")).rejects.toThrow(
      'No command "test.pkg-nothing" is registered.',
    );
  });

  test("refuses a command that fails its preflight, naming its args", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(commands.run("blockr.core-remove_block", { id: "zz" })).rejects.toThrow(
      'It fails its preflight on the board as it is, with the args {"id":"zz"}.',
    );
    expect(store.board()).toBe(board);

    // Its CommandError says that it failed its preflight, and has no index,
    // since the command ran alone.
    const error = await rejection(commands.run("blockr.core-remove_block", { id: "zz" }));
    expect([error.preflight, error.index]).toEqual([false, undefined]);
    const failed = await rejection(commands.run("blockr.core-rename_board", { name: 1 }));
    expect(failed.preflight).toBeUndefined();
  });

  test("rejects with what the store refused, having changed nothing", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const refused = commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block", id: "a" });
    await expect(refused).rejects.toThrow(StoreError);
    await expect(refused).rejects.toThrow('The board holds block "a" already.');
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });

  test("names the command in what its own code threw, which it logs at error with what was thrown as the cause", async () => {
    const { commands } = await setup();
    const error = new Error("No layout.");
    commands.register(
      renameB({
        run: () => {
          throw error;
        },
      }),
    );
    const reported = logged();

    const failed = commands.run("test.pkg-rename_b", { name: "Top" });
    await expect(failed).rejects.toThrow('The command "test.pkg-rename_b" failed\n- No layout.');
    await expect(failed).rejects.toHaveProperty("cause", error);
    reported.restore();

    // The line holds the error that run() rejected with, and so the stack of
    // what the command threw, as its cause.
    const rejection: unknown = await failed.catch((reason: unknown) => reason);
    expect(reported.lines()[0]?.error).toBe(rejection);
    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-rename_b" failed\n- No layout.', cause: error });
  });

  test("logs no refusal, its own or the command's, and nothing that the store refuses", async () => {
    const { commands } = await setup();
    const reported = logged();

    await expect(commands.run("test.pkg-nothing")).rejects.toThrow("is registered");
    await expect(commands.run("blockr.core-rename_board", { name: 1 })).rejects.toThrow("The args fail their schema");
    await expect(commands.run("blockr.core-remove_block", { id: "zz" })).rejects.toThrow("It fails its preflight");
    // Core's command refuses in its own run(), with a CommandError.
    await expect(commands.run("blockr.core-add_block", { registry_id: "test.pkg-nowhere" })).rejects.toThrow(
      'The block registry has no block class "test.pkg-nowhere".',
    );
    // Core's command refuses in its preflight() too, with the refusal() of its
    // context.
    await expect(commands.run("blockr.core-add_link", { from: "e", to: "e" })).rejects.toThrow(
      'The link would join block "e" to itself.',
    );
    // The store refuses a block whose id the board holds already.
    await expect(commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block", id: "a" })).rejects.toThrow(
      StoreError,
    );
    expect(commands.preflight("blockr.core-remove_block", { id: "zz" })).toBe(false);
    reported.restore();

    expect(reported.lines()).toEqual([]);
  });

  test("refuses to run before the store opens a board", async () => {
    const store = createStore();
    const commands = createCommands(store);
    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).rejects.toThrow(
      'The command "blockr.core-rename_board" refused to run\n- The store holds no board until it opens one.',
    );
    expect(commands.preflight("blockr.core-rename_board", { name: "Sepals" })).toBe(false);
  });

  test("applies no step where the command changes nothing", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-rename_board", { name: "Iris review" });
    expect(store.canUndo()).toBe(false);
  });

  test("resolves with whether its step took an undo step, as the store's apply() returns it", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));
    commands.register(later(() => Promise.resolve(" and petals")));
    // A plugin's command whose delta sets a board option's value and gives
    // block b the state it holds, { n: 6 }.
    commands.register({
      id: "test.pkg-set_rows",
      label: "Set the rows",
      run: () => ({ blocks: { mod: { b: { state: { n: 6 } } } }, options: { set: { n_rows: 10 } } }),
    });

    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).resolves.toBe(true);
    await expect(commands.run("blockr.core-rename_board", { name: "Sepals" })).resolves.toBe(false);
    await expect(commands.run("test.pkg-later")).resolves.toBe(true);
    await expect(commands.run("test.pkg-rename_b", { name: "Renamed" })).resolves.toBe(false);
    await expect(commands.run("blockr.core-set_option", { id: "n_rows", value: 30 })).resolves.toBe(false);
    await expect(commands.run("test.pkg-set_rows", {}, "assistant")).resolves.toBe(false);

    expect(store.board().name).toBe("Sepals and petals");
    expect(store.board().options.n_rows).toBe(10);
    expect(store.canUndo("assistant")).toBe(false);
  });
});

describe("preflight()", () => {
  test("says whether the command passes its preflight on the current board with these args", async () => {
    const { commands } = await setup();

    expect(commands.preflight("blockr.core-remove_block", { id: "b" })).toBe(true);
    expect(commands.preflight("blockr.core-remove_block", { id: "zz" })).toBe(false);
    expect(commands.preflight("blockr.core-remove_block", { id: 1 })).toBe(false);
    expect(commands.preflight("test.pkg-nothing")).toBe(false);
  });

  test("is false where the command's preflight() throws, which the command registry logs at error", async () => {
    const { commands } = await setup();
    const error = new Error("No idea.");
    commands.register(
      renameB({
        preflight: () => {
          throw error;
        },
      }),
    );
    const reported = logged();
    expect(commands.preflight("test.pkg-rename_b", { name: "Top" })).toBe(false);
    reported.restore();

    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-rename_b" failed\n- No idea.', cause: error });
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

  test("runs preflight() again once prepare() resolves", async () => {
    const { store, commands } = await setup();
    const ready = Promise.withResolvers<null>();
    const run = vi.fn(() => ({ name: "Never" }));
    commands.register<{ readonly id: string }, null>({
      id: "test.pkg-later",
      label: "Later",
      preflight: (board, { id }) => Object.hasOwn(board.blocks, id),
      prepare: () => ready.promise,
      run,
    });

    const done = commands.run("test.pkg-later", { id: "b" });
    store.apply({ blocks: { rm: ["b"] } });
    ready.resolve(null);

    await expect(done).rejects.toThrow('It fails its preflight on the board as it is, with the args {"id":"b"}.');
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
    expect(commands.preflight("blockr.core-rename_board", { name: "Sepals" })).toBe(false);
    expect(store.board().name).toBe("Iris review");
  });

  test("names the command where prepare() rejects, which it logs at error", async () => {
    const { commands } = await setup();
    const error = new Error("No layout.");
    commands.register<Json, null>({
      id: "test.pkg-later",
      label: "Later",
      prepare: () => Promise.reject(error),
      run: () => [],
    });
    const reported = logged();
    await expect(commands.run("test.pkg-later")).rejects.toThrow(
      'The command "test.pkg-later" failed\n- No layout.',
    );
    reported.restore();

    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-later" failed\n- No layout.', cause: error });
  });
});

describe("the context", () => {
  test("hands preflight(), prepare() and run() the locks, the blockr options and ids in the shape that rand_id names", async () => {
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
      preflight(_board, _args, context) {
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

    // The registry runs preflight() before prepare() and again once it
    // resolves.
    expect(locks).toEqual([lock, lock]);
    expect(asked).toEqual(["stack_name"]);
    const minted = Object.keys(store.board().stacks).filter((id) => !before.includes(id));
    expect(minted).toEqual([expect.stringMatching(/^[a-z]{6}$/)]);
    expect(store.board().stacks[minted[0] ?? ""]?.name).toBe("Petals");
  });

  test("hands each command a refusal() that names it, which the command throws to refuse from its preflight(), prepare() or run(), and which the command registry rejects with as it is and logs no line of", async () => {
    const { commands } = await setup();
    const failures = ['The grid places the panel of the block "z", which the board does not hold.'];
    commands.register(
      renameB({
        id: "test.pkg-in_preflight",
        preflight: (_board, _args, { refusal }) => {
          throw refusal(failures);
        },
      }),
    );
    commands.register<Json, null>({
      id: "test.pkg-in_prepare",
      label: "In prepare",
      prepare: (_board, _args, { refusal }) => Promise.reject(refusal(failures)),
      run: () => [],
    });
    commands.register(
      renameB({
        id: "test.pkg-in_run",
        run: (_board, _args, { refusal }) => {
          throw refusal(failures);
        },
      }),
    );
    const reported = logged();

    for (const id of ["test.pkg-in_preflight", "test.pkg-in_prepare", "test.pkg-in_run"]) {
      const refused = commands.run(id, { name: "Top" });
      await expect(refused).rejects.toThrow(CommandError);
      await expect(refused).rejects.toThrow(`The command "${id}" refused to run\n- ${failures.join("")}`);
      await expect(refused).rejects.toHaveProperty("failures", failures);
    }
    expect(commands.preflight("test.pkg-in_preflight", { name: "Top" })).toBe(false);
    reported.restore();

    expect(reported.lines()).toEqual([]);
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

  test("rejects with what the command's own code threw, which the command registry logs at error, as it does for run()", async () => {
    const { commands } = await setup();
    const error = new Error("No layout.");
    commands.register(
      renameB({
        run: () => {
          throw error;
        },
      }),
    );
    const reported = logged();
    await expect(commands.plan("test.pkg-rename_b", { name: "Top" })).rejects.toThrow('The command "test.pkg-rename_b" failed\n- No layout.');
    reported.restore();

    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-rename_b" failed\n- No layout.', cause: error });
  });
});

describe("running a batch", () => {
  test("applies the commands in order as one step of the origin, each planned on the board that those before it leave, which one undo takes back", async () => {
    const { store, commands } = await setup();
    const board = store.board();
    const seen = origins(store);
    // Alone, the link fails its preflight, since the board lacks block "g".
    expect(commands.preflight("blockr.core-add_link", { from: "d", to: "g" })).toBe(false);

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

  test("has the afterStep of a plugin's class place what the batch adds by the links of its later commands, in the batch's one step", async () => {
    const { store, commands } = await setupPlacing();
    const seen = origins(store);

    await commands.run(appendG, "assistant");

    expect(store.board().plugins[placed.id]).toEqual({ positions: { d: { x: 4 }, g: { x: 5 } } });
    expect(seen).toEqual(["assistant", "assistant", "assistant"]);
    store.undo("assistant");
    expect(store.board().blocks.g).toBeUndefined();
    expect(store.board().plugins[placed.id]).toEqual({ positions: { d: { x: 4 } } });
  });

  test("applies a batch whose commands do not prepare before run() returns", async () => {
    const { store, commands } = await setup();

    const done = commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }]);
    expect(store.board().name).toBe("Sepals");
    await done;
  });

  test("takes no undo step for a batch of commands with undo: false, and leaves their deltas out of the undo step of a batch that takes one", async () => {
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
    expect(store.board().blocks.b?.name).toBe("Three");
    store.redo();
    expect(store.board().name).toBe("Sepals");
    expect(store.board().blocks.b?.name).toBe("Three");
  });

  test("takes no undo step for a batch that sets only board options beside commands with undo: false, and leaves the undo and redo lists as they are", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));
    store.apply({ name: "Sepals" });
    store.undo();

    await commands.run([
      { id: "blockr.core-set_option", args: { id: "n_rows", value: 30 } },
      { id: "test.pkg-rename_b", args: { name: "Viewed" } },
    ]);
    expect([store.canUndo(), store.canRedo()]).toEqual([false, true]);

    store.redo();
    expect(store.board().name).toBe("Sepals");
    expect(store.board().blocks.b?.name).toBe("Viewed");
    expect(store.board().options.n_rows).toBe(30);
  });

  test("applies nothing for no commands", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run([], "assistant");
    expect(store.board()).toBe(board);
    expect(store.canUndo("assistant")).toBe(false);
  });

  test("resolves with whether its step took an undo step, as for a command alone", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));
    commands.register(later(() => Promise.resolve(" and petals")));

    await expect(
      commands.run(
        [
          { id: "test.pkg-rename_b", args: { name: "Viewed" } },
          { id: "blockr.core-rename_board", args: { name: "Sepals" } },
        ],
        "assistant",
      ),
    ).resolves.toBe(true);
    // As for a batch of the dock's activate_view, which has undo: false, and
    // core's set_option.
    await expect(
      commands.run(
        [
          { id: "test.pkg-rename_b", args: { name: "Looked" } },
          { id: "blockr.core-set_option", args: { id: "n_rows", value: 30 } },
        ],
        "assistant",
      ),
    ).resolves.toBe(false);
    await expect(commands.run([{ id: "test.pkg-later" }])).resolves.toBe(true);
    await expect(commands.run([], "assistant")).resolves.toBe(false);

    expect(store.board().name).toBe("Sepals and petals");
    expect(store.board().blocks.b?.name).toBe("Looked");
  });
});

describe("refusing a batch", () => {
  test("applies none of it where a command fails its preflight, and rejects with the command's index and that it failed its preflight", async () => {
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
      'The batch was refused at command 2, "blockr.core-remove_block"\n- It fails its preflight on the board as it is, with the args {"id":"zz"}.',
    );
    expect([error.index, error.preflight]).toEqual([1, false]);
    expect(error.failures).toEqual(['It fails its preflight on the board as it is, with the args {"id":"zz"}.']);
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
    expect(error.preflight).toBeUndefined();
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
        { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "a" } },
        { id: "blockr.core-rename_block", args: { id: "b", name: "Top" } },
      ]),
    );

    expect(error.index).toBe(1);
    expect(error.preflight).toBeUndefined();
    expect(error.cause).toBeInstanceOf(StoreError);
    expect(error.failures).toEqual([expect.stringContaining('The board holds block "a" already.')]);
    expect(store.board()).toBe(board);
    expect(store.canUndo()).toBe(false);
  });

  test("rejects with what a command's own code threw, naming the command, which it logs at error once", async () => {
    const { commands } = await setup();
    const thrown = new Error("No layout.");
    commands.register(
      renameB({
        run: () => {
          throw thrown;
        },
      }),
    );
    const reported = logged();

    const error = await rejection(commands.run([{ id: "test.pkg-rename_b", args: { name: "Top" } }]));
    reported.restore();
    expect(error.index).toBe(0);
    expect(error.failures).toEqual(["No layout."]);
    expect(String(error.cause)).toContain('The command "test.pkg-rename_b" failed');
    // The line holds the command's own CommandError, the batch's cause.
    expect(reported.lines()[0]?.error).toBe(error.cause);
    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-rename_b" failed\n- No layout.', cause: thrown });
  });

  test("refuses a batch at its first command before the store opens a board", async () => {
    const commands = createCommands(createStore());

    const error = await rejection(commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }]));
    expect(error.index).toBe(0);
    expect(error.failures).toEqual(["The store holds no board until it opens one."]);
  });
});

describe("a batch with a command that prepares", () => {
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
    // planned the batch once.
    expect(run).toHaveBeenCalledTimes(1);
  });

  test("plans the batch again from its first command where the board changes while a command prepares, with what each prepare() gave", async () => {
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

  test("runs the preflight of each command again as it plans the batch again", async () => {
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
    expect([error.index, error.preflight]).toEqual([0, false]);
    expect(store.board()).toBe(board);
  });

  test("drops the batch where another board opens while a command prepares, applying none of it", async () => {
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

  test("names the command where its prepare() rejects, which it logs at error once", async () => {
    const { commands } = await setup();
    const thrown = new Error("No layout.");
    commands.register(later(() => Promise.reject(thrown)));
    const reported = logged();

    const error = await rejection(commands.run([{ id: "blockr.core-rename_board", args: { name: "Sepals" } }, { id: "test.pkg-later" }]));
    reported.restore();
    expect(error.index).toBe(1);
    expect(error.failures).toEqual(["No layout."]);
    expect(String(error.cause)).toContain('The command "test.pkg-later" failed');
    expect(loggedFailure(reported)).toEqual({ message: 'The command "test.pkg-later" failed\n- No layout.', cause: thrown });
  });
});

describe("planning a batch", () => {
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

  test("plans each command on the board between the deltas of the step, where no afterStep has run, and returns none of what afterStep returns, which the store adds as the plan applies", async () => {
    const afterStep = vi.fn(placeAfterSource);
    const { store, commands } = await setupPlacing(afterStep);

    const planned = await commands.plan(appendG);

    expect(afterStep).not.toHaveBeenCalled();
    expect(planned).toHaveLength(2);
    expect(planned.some((delta) => delta.patches !== undefined)).toBe(false);
    store.apply(planned);
    expect(afterStep).toHaveBeenCalledTimes(1);
    expect(store.board().plugins[placed.id]).toEqual({ positions: { d: { x: 4 }, g: { x: 5 } } });
  });

  test("gives the deltas of a command with undo: false undo: false, so that a plan applied later keeps them out of its undo step", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ undo: false }));

    const planned = await commands.plan([
      { id: "blockr.core-rename_board", args: { name: "Sepals" } },
      { id: "test.pkg-rename_b", args: { name: "Planned" } },
    ]);
    expect(planned).toEqual([
      { name: "Sepals" },
      { blocks: { mod: { b: { edit: store.editId({ block: "b" }), name: "Planned" } } }, undo: false },
    ]);

    store.apply(planned, "assistant");
    store.undo();
    expect(store.board().name).toBe("Iris review");
    expect(store.board().blocks.b?.name).toBe("Planned");
  });

  test("leaves the store to refuse a planned batch once another edit came first", async () => {
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

  test("refuses as running the batch would, and plans no deltas for no commands", async () => {
    const { commands } = await setup();

    const error = await rejection(
      commands.plan([
        { id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "g" } },
        { id: "blockr.core-add_link", args: { from: "zz", to: "g" } },
      ]),
    );
    expect([error.index, error.preflight]).toEqual([1, false]);
    expect(await commands.plan([])).toEqual([]);
  });
});

describe("spans", () => {
  test("record each command from run() until the store announces its deltas, under the span active at the call, and active while the store announces them", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ prepare: (_board, args) => Promise.resolve(args) }));
    const recorded = recordSpans();
    const tracer = trace.getTracer("blockr.test");
    const announced: (string | undefined)[] = [];
    store.subscribe(() => {
      announced.push(trace.getActiveSpan()?.spanContext().spanId);
    });

    const tool = tracer.startSpan("execute_tool edit");
    await context.with(trace.setSpan(context.active(), tool), () =>
      commands.run("test.pkg-rename_b", { name: "Bee" }, "assistant"),
    );
    tool.end();

    const span = recorded.span("command test.pkg-rename_b");
    expect(span.parentSpanContext?.spanId).toBe(tool.spanContext().spanId);
    expect(span.attributes).toEqual({ "blockr.command.id": "test.pkg-rename_b", "blockr.delta.origin": "assistant" });
    expect(span.status.code).toBe(SpanStatusCode.UNSET);
    // The command prepared before the store applied its delta, after an
    // await, and the span was active as the store announced it all the same.
    expect(announced).toEqual([span.spanContext().spanId]);
    recorded.restore();
  });

  test("mark a command that is refused, or that fails, as failed", async () => {
    const { commands } = await setup();
    const recorded = recordSpans();

    await expect(commands.run("test.pkg-nothing")).rejects.toBeInstanceOf(CommandError);

    const span = recorded.span("command test.pkg-nothing");
    expect(span.status).toEqual({
      code: SpanStatusCode.ERROR,
      message: 'The command "test.pkg-nothing" refused to run\n- No command "test.pkg-nothing" is registered.',
    });
    expect(span.attributes["blockr.delta.origin"]).toBe("user");
    recorded.restore();
  });

  test("record a batch as one span, which names the ids of its commands in order, and is active while the store announces the batch's deltas", async () => {
    const { store, commands } = await setup();
    commands.register(renameB({ prepare: (_board, args) => Promise.resolve(args) }));
    const recorded = recordSpans();
    const tracer = trace.getTracer("blockr.test");
    const announced: (string | undefined)[] = [];
    store.subscribe(() => {
      announced.push(trace.getActiveSpan()?.spanContext().spanId);
    });

    const tool = tracer.startSpan("execute_tool edit");
    await context.with(trace.setSpan(context.active(), tool), () =>
      commands.run(
        [
          { id: "test.pkg-rename_b", args: { name: "Bee" } },
          { id: "blockr.core-rename_block", args: { id: "b", name: "Bea" } },
        ],
        "assistant",
      ),
    );
    tool.end();

    const span = recorded.span("command batch");
    expect(span.parentSpanContext?.spanId).toBe(tool.spanContext().spanId);
    expect(span.attributes).toEqual({
      "blockr.command.ids": ["test.pkg-rename_b", "blockr.core-rename_block"],
      "blockr.delta.origin": "assistant",
    });
    expect(span.status.code).toBe(SpanStatusCode.UNSET);
    // The first command prepared, after which the registry planned the
    // second and the store applied both, and the span was active all the
    // same as the store announced them.
    expect(new Set(announced)).toEqual(new Set([span.spanContext().spanId]));
    expect(recorded.spans().map(({ name }) => name)).toEqual(["command batch", "execute_tool edit"]);
    recorded.restore();
  });

  test("mark a batch that is refused as failed", async () => {
    const { commands } = await setup();
    const recorded = recordSpans();

    await expect(commands.run([{ id: "blockr.core-remove_block", args: { id: "zz" } }])).rejects.toBeInstanceOf(
      CommandError,
    );

    const span = recorded.span("command batch");
    expect(span.status.code).toBe(SpanStatusCode.ERROR);
    expect(span.status.message).toMatch(/^The batch was refused at command 1, "blockr.core-remove_block"\n/);
    expect(span.attributes["blockr.delta.origin"]).toBe("user");
    recorded.restore();
  });
});
