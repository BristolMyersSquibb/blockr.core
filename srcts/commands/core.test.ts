// @vitest-environment node

// Core's commands, on the board of a core app from the wire protocol's
// fixtures: blocks a (dataset), b (head), c (merge), d (subset), e (rbind,
// which is variadic) and f (scatter), the links ab, ac, bc, ad, be, de and one
// from a to f, and the stack s1 of a and b.

import { describe, expect, test } from "vitest";
import { blockRegistry } from "../protocol/index.ts";
import type { HelloReply } from "../protocol/index.ts";
import { isId } from "../store/index.ts";
import type { PluginClass, Store } from "../store/index.ts";
import { hello, openedStore, withPlugins } from "../testing/store.ts";
import type { GivenPlugin } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { interned } from "../util/index.ts";
import { CommandError, createCommands, freeInputs, linkFeeding, schemas } from "./index.ts";
import type { CommandContext, Commands } from "./index.ts";

async function setup(
  register?: (store: Store) => void,
): Promise<{ store: Store; commands: Commands }> {
  const store = await openedStore(register);
  return { store, commands: createCommands(store) };
}

// The blocks that feed a block, in link order, with the input each feeds.
function inputs(board: Board, to: string): [string, string][] {
  return Object.values(board.links)
    .filter((link) => link.to === to)
    .map((link) => [link.from, link.input]);
}

// What a step added to a collection.
function added<T>(before: Readonly<Record<string, T>>, after: Readonly<Record<string, T>>): string[] {
  return Object.keys(after).filter((id) => !Object.hasOwn(before, id));
}

// The registry id of core's head block, as args give it.
const head = { package: "blockr.core", id: "head_block" };

describe("blocks", () => {
  test("add_block adds a block with the state its class starts one with, its name and a minted id", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: head });

    const [id = ""] = added(board.blocks, store.board().blocks);
    expect(isId(id)).toBe(true);
    expect(store.board().blocks[id]).toEqual({
      registry_id: head,
      state: { n: 6, direction: "head" },
      name: "Head",
      locked: null,
    });
  });

  test("add_block takes an unnamed block's name from R's registry entry", async () => {
    const base = blockRegistry(hello).get(interned("blockr.core", "head_block"));
    if (base === undefined) throw new Error("The fixture has no head block class.");
    // A class that an app script defines, which no package names.
    const entry = { ...base, package: null, id: "PCA_block", block_name: "PCA" };
    const store = await openedStore(undefined, {
      ...hello,
      block_registry: [...hello.block_registry, entry],
    });
    const commands = createCommands(store);

    await commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: { package: entry.package, id: entry.id }, id: "pca" });

    expect(store.board().blocks.pca?.name).toBe("PCA");
  });

  test("add_block changes the state its class starts a block with by the state args given", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "add_block" }, {
      registry_id: head,
      id: "top",
      name: "Top rows",
      state: { n: 3 },
    });

    expect(store.board().blocks.top).toEqual({
      registry_id: head,
      state: { n: 3, direction: "head" },
      name: "Top rows",
      locked: null,
    });
  });

  test("add_block refuses a class that the block registry lacks and a state that the schema of the class refuses", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(
      commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: { package: "blockr.dplyr", id: "filter_block" } }),
    ).rejects.toThrow('The block registry has no block class "filter_block of blockr.dplyr".');

    const refused = commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: head, state: { n: "ten" } });
    await expect(refused).rejects.toThrow(CommandError);
    await expect(refused).rejects.toThrow("The state of the new block fails its schema at /n:");
    expect(store.board()).toBe(board);
  });

  test("add_block leaves an id that the board holds already to the store, which refuses it", async () => {
    const { commands } = await setup();
    await expect(commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: head, id: "a" })).rejects.toThrow(
      'The board holds block "a" already.',
    );
  });

  test("remove_block removes a block with its links and its place in its stack", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "remove_block" }, { id: "b" });

    const board = store.board();
    expect(Object.keys(board.blocks)).toEqual(["a", "c", "d", "e", "f"]);
    expect(Object.values(board.links).some((link) => link.from === "b" || link.to === "b")).toBe(false);
    expect(board.stacks.s1?.blocks).toEqual(["a"]);
  });

  test("change_block renames a block, and makes no step for the name it has", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", name: "Head" });
    expect(store.canUndo()).toBe(false);

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", name: "Top rows" });
    expect(store.board().blocks.b?.name).toBe("Top rows");
  });

  test("change_block sets a block's lock, null among its values, and makes no step for the lock it has", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", locked: null });
    expect(store.canUndo()).toBe(false);

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", locked: false });
    expect(store.board().blocks.b?.locked).toBe(false);
    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", locked: true });
    expect(store.board().blocks.b?.locked).toBe(true);
    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", locked: null });
    expect(store.board().blocks.b?.locked).toBeNull();

    store.undo();
    expect(store.board().blocks.b?.locked).toBe(true);
    store.undo();
    store.undo();
    expect(store.board().blocks.b?.locked).toBeNull();

    await expect(commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", locked: "yes" })).rejects.toThrow(
      "The args fail their schema at /locked:",
    );
    expect(commands.preflight({ package: "blockr.core", id: "change_block" }, { id: "z", locked: true })).toBe(false);
  });

  test("change_block sets and unsets state args, as one step", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "a", state: { package: "ggplot2" }, unset: ["dataset"] });

    expect(store.board().blocks.a?.state).toEqual({ package: "ggplot2" });
    store.undo();
    expect(store.board().blocks.a?.state).toEqual({ dataset: "iris", package: "datasets" });
  });

  test("change_block sets a block's name, its lock and its state in one step, and keeps what the args leave out", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    const state = { ...board.blocks.b?.state, n: 3 };
    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", name: "Top rows", locked: true, state: { n: 3 } });
    expect(store.board().blocks.b).toEqual({ ...board.blocks.b, name: "Top rows", locked: true, state });

    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", name: "First rows" });
    expect(store.board().blocks.b).toEqual({ ...board.blocks.b, name: "First rows", locked: true, state });

    store.undo();
    store.undo();
    expect(store.board().blocks.b).toEqual(board.blocks.b);
    expect(store.canUndo()).toBe(false);
  });

  test("runs where its run() is called on its own, off the command", async () => {
    const { store, commands } = await setup();
    // eslint-disable-next-line @typescript-eslint/unbound-method -- takes run() off the command on purpose
    const { run } = commands.registered().get(interned("blockr.core", "add_block")) ?? {};
    const context: CommandContext = {
      lock: { board: false, load: false },
      mintId: () => "calm_heron",
      blockrOption: () => Promise.resolve(undefined),
      refusal: (failures) => new CommandError({ command: interned("blockr.core", "add_block"), by: "command", failures }),
    };
    expect(() => run?.(store.board(), { registry_id: { package: "blockr.dplyr", id: "filter_block" } }, context)).toThrow(
      'The command "add_block of blockr.core" refused to run',
    );
  });

  test("change_block makes no step where nothing changes", async () => {
    const { store, commands } = await setup();
    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", state: { n: 6 }, unset: ["x"] });
    await commands.run({ package: "blockr.core", id: "change_block" }, { id: "b" });
    expect(store.canUndo()).toBe(false);
  });

  test("change_block refuses a state that the schema of the block's class refuses, with the rest of its change", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", state: { n: -1 } })).rejects.toThrow(
      'The state of block "b" fails its schema at /n:',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", unset: ["n"] })).rejects.toThrow(
      'The state of block "b" fails its schema: Instance does not have required property "n".',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", state: { m: 1 } })).rejects.toThrow(
      'The state of block "b" fails its schema',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_block" }, { id: "b", name: "Top rows", state: { n: -1 } })).rejects.toThrow(
      'The state of block "b" fails its schema at /n:',
    );
    expect(store.board()).toBe(board);
  });
});

describe("appending, prepending and inserting, as batches", () => {
  test("add_block and add_link append a block in one step, which takes a block's result into its only input", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run([
      { package: "blockr.core", id: "add_block", args: { registry_id: head, id: "top" } },
      { package: "blockr.core", id: "add_link", args: { from: "d", to: "top" } },
    ]);

    expect(store.board().blocks.top?.registry_id).toBe(interned("blockr.core", "head_block"));
    expect(inputs(store.board(), "top")).toEqual([["d", "data"]]);
    expect(added(board.links, store.board().links)).toHaveLength(1);

    store.undo();
    expect(store.board()).toEqual(board);
  });

  test("the batch that appends a block of a class that takes no input adds nothing", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(
      commands.run([
        { package: "blockr.core", id: "add_block", args: { registry_id: { package: "blockr.core", id: "dataset_block" }, id: "cars" } },
        { package: "blockr.core", id: "add_link", args: { from: "d", to: "cars" } },
      ]),
    ).rejects.toThrow(
      'The batch was refused at command 2, "add_link of blockr.core"\n- The block "cars" takes no input, since its class "dataset_block of blockr.core" has none.',
    );
    expect(store.board()).toBe(board);
  });

  test("add_block and add_link prepend a block, whose result goes to a block's free input, in link order where it is variadic", async () => {
    const { store, commands } = await setup();

    await commands.run([
      {
        package: "blockr.core",
        id: "add_block",
        args: { registry_id: { package: "blockr.core", id: "dataset_block" }, id: "cars", state: { dataset: "mtcars" } },
      },
      { package: "blockr.core", id: "add_link", args: { from: "cars", to: "e" } },
    ]);

    expect(store.board().blocks.cars?.state).toEqual({ dataset: "mtcars", package: "datasets" });
    expect(inputs(store.board(), "e")).toEqual([
      ["b", ""],
      ["d", ""],
      ["cars", ""],
    ]);
  });

  test("add_block, add_link and change_link insert a block on a link, which keeps its id, its input and its place among the links", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run([
      { package: "blockr.core", id: "add_block", args: { registry_id: head, id: "top" } },
      { package: "blockr.core", id: "add_link", args: { from: "b", to: "top" } },
      { package: "blockr.core", id: "change_link", args: { id: "be", from: "top" } },
    ]);

    const after = store.board();
    expect(after.links.be).toEqual({ from: "top", to: "e", input: "" });
    expect(inputs(after, "top")).toEqual([["b", "data"]]);
    expect(inputs(after, "e")).toEqual([
      ["top", ""],
      ["d", ""],
    ]);
    expect(Object.keys(after.links).filter((id) => Object.hasOwn(board.links, id))).toEqual(Object.keys(board.links));

    store.undo();
    expect(store.board()).toEqual(board);
  });

  test("the batch that inserts a block keeps the input that the link fed", async () => {
    const { store, commands } = await setup();

    await commands.run([
      { package: "blockr.core", id: "add_block", args: { registry_id: head, id: "top" } },
      { package: "blockr.core", id: "add_link", args: { from: "a", to: "top" } },
      { package: "blockr.core", id: "change_link", args: { id: "ac", from: "top" } },
    ]);

    expect(inputs(store.board(), "top")).toEqual([["a", "data"]]);
    expect(inputs(store.board(), "c")).toEqual([
      ["top", "x"],
      ["b", "y"],
    ]);
  });
});

describe("links", () => {
  test("add_link goes to the only free input of a block of fixed arity, and in link order into a variadic block", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "ab" });
    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "b", id: "db" });
    expect(store.board().links.db).toEqual({ from: "d", to: "b", input: "data" });

    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "ac" });
    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c", id: "dc" });
    expect(store.board().links.dc).toEqual({ from: "d", to: "c", input: "x" });

    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "c", to: "e", id: "ce" });
    expect(store.board().links.ce).toEqual({ from: "c", to: "e", input: "" });
  });

  test("add_link refuses to pick one of several free inputs, naming them, and takes the one given", async () => {
    const { store, commands } = await setup();
    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "ac" });
    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "bc" });

    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c" })).toBe(true);
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c" })).rejects.toThrow(
      'The block "c" has the free inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c", input: "y", id: "dc" });
    expect(store.board().links.dc).toEqual({ from: "d", to: "c", input: "y" });
  });

  test("add_link fails its preflight for a block without a free input, saying why, and for a block that the board lacks", async () => {
    const { commands } = await setup();
    await commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: { package: "blockr.core", id: "dataset_block" }, id: "cars" });

    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c" })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "cars" })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "zz" })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "zz", to: "e" })).toBe(false);

    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c" })).rejects.toThrow(
      'The command "add_link of blockr.core" refused to run\n- Every input of the block "c" takes a link already.',
    );
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "cars" })).rejects.toThrow(
      'The block "cars" takes no input, since its class "dataset_block of blockr.core" has none.',
    );
    // A block that the board lacks fails the preflight without a sentence.
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "zz" })).rejects.toMatchObject({
      by: "preflight",
    });
  });

  test("add_link fails its preflight for an input that another link feeds, naming that link, and leaves an input that the class lacks to the store", async () => {
    const { commands } = await setup();
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c", input: "x" })).toBe(false);
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c", input: "x" })).rejects.toThrow(
      'The link "ac" feeds the input "x" of block "c" already.',
    );
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: "c", input: "z", id: "dc" })).rejects.toThrow(
      'The link "dc" feeds the input "z" of block "c", whose class has no such input.',
    );
    // The rbind block e takes any number of links, so a third link without a
    // name passes, beside be and de.
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "c", to: "e", input: "" })).toBe(true);
  });

  test("add_link fails its preflight for a link that would join a block to itself or close a cycle, naming the blocks of the cycle in their order", async () => {
    const { commands } = await setup();
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "e", to: "e" })).toBe(false);
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "e", to: "e" })).rejects.toThrow(
      'The link would join block "e" to itself.',
    );

    // A head block g takes the result of the rbind block e, and e takes that
    // of the head block b. The one input of b is free.
    await commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: head, id: "g" });
    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "e", to: "g" });
    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "ab" });
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "g", to: "b" })).toBe(false);
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "g", to: "b" })).rejects.toThrow(
      'The link would close a cycle through the blocks "b", "e" and "g".',
    );
    expect(commands.preflight({ package: "blockr.core", id: "add_link" }, { from: "d", to: "b" })).toBe(true);
  });

  test("add_link and change_link give a sentence for each failure of the link", async () => {
    const { commands } = await setup();

    // The head block b takes the result of a into its one input, and feeds the
    // merge block c.
    const failures = [
      'The link would close a cycle through the blocks "b" and "c".',
      'The link "ab" feeds the input "data" of block "b" already.',
    ];
    await expect(commands.run({ package: "blockr.core", id: "add_link" }, { from: "c", to: "b", input: "data" })).rejects.toMatchObject({
      failures,
    });
    await expect(
      commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", from: "c", to: "b", input: "data" }),
    ).rejects.toMatchObject({ failures });
  });

  test("change_link keeps the input where the link goes to the same block, and takes the only free one where it goes to another", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "bc", from: "d" });
    expect(store.board().links.bc).toEqual({ from: "d", to: "c", input: "y" });

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "bc", to: "e" });
    expect(store.board().links.bc).toEqual({ from: "d", to: "e", input: "" });

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "ab", from: "a" });
    expect(store.canUndo()).toBe(true);
    store.undo();
    store.undo();
    expect(store.board().links.bc).toEqual({ from: "b", to: "c", input: "y" });
  });

  test("change_link fails its preflight for another block whose inputs other links feed, saying so, and for a block or a link that the board lacks", async () => {
    const { commands } = await setup();
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "ab", to: "d" })).toBe(false);
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "ab", to: "d" })).rejects.toThrow(
      'Every input of the block "d" takes a link already.',
    );

    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "ab", from: "zz" })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "ab", to: "zz" })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "zz", from: "d" })).toBe(false);
  });

  test("change_link fails its preflight for a source that would join the link's block to itself or close a cycle, so that a menu offers no such source", async () => {
    const { commands } = await setup();
    const sources = (id: string): string[] =>
      ["a", "b", "c", "d", "e", "f"].filter((from) => commands.preflight({ package: "blockr.core", id: "change_link" }, { id, from }));

    // The link ab feeds the head block b. The merge block c and the rbind
    // block e take the result of b.
    expect(sources("ab")).toEqual(["a", "d", "f"]);
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "ab", from: "b" })).rejects.toThrow(
      'The link would join block "b" to itself.',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "ab", from: "e" })).rejects.toThrow(
      'The link would close a cycle through the blocks "b" and "e".',
    );

    // The link bc feeds the merge block c, and no block takes the result of c.
    expect(sources("bc")).toEqual(["a", "b", "d", "e", "f"]);
  });

  test("change_link swaps the inputs of two links into a block that is not variadic, in one step that one undo takes back", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    // The merge block c takes ac as x and bc as y. The link ac passes the
    // preflight for y, which bc feeds, since the two links swap their inputs.
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "ac", input: "y" })).toBe(true);
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "ac", input: "y" });
    expect(inputs(store.board(), "c")).toEqual([
      ["a", "y"],
      ["b", "x"],
    ]);

    store.undo();
    expect(store.board().links).toEqual(board.links);
    expect(store.canUndo()).toBe(false);

    // A link that comes from another block now swaps too.
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "bc", from: "d", input: "x" });
    expect(inputs(store.board(), "c")).toEqual([
      ["a", "y"],
      ["d", "x"],
    ]);
  });

  test("change_link swaps no inputs where the link goes to another block, nor among the links into a variadic block", async () => {
    const { store, commands } = await setup();

    // The old input of ab, the data of block b, means nothing to block c, so
    // ac keeps x. The preflight refuses to have ab feed x as well.
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "ab", to: "c", input: "x" })).toBe(false);
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "ab", to: "c", input: "x" })).rejects.toThrow(
      'The link "ac" feeds the input "x" of block "c" already.',
    );

    // The rbind block e takes be and de by their place, as neither gives a
    // name, and de gives none still once be has had a name and lost it.
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", input: "top" });
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", input: "" });
    expect(inputs(store.board(), "e")).toEqual([
      ["b", ""],
      ["d", ""],
    ]);
  });

  test("remove_link removes a link", async () => {
    const { store, commands } = await setup();
    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "ab" });
    expect(Object.hasOwn(store.board().links, "ab")).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "remove_link" }, { id: "ab" })).toBe(false);
  });

  test("change_link moves a link before another into its block, keeping its id and its ends, as one step", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", before: "be" });
    expect(inputs(store.board(), "e")).toEqual([
      ["d", ""],
      ["b", ""],
    ]);
    expect(store.board().links.de).toBe(board.links.de);

    store.undo();
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
    expect(store.canUndo()).toBe(false);
  });

  test("change_link puts a link last among the links into its block for a before of null, right after the block's last other link", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", before: null });
    expect(Object.keys(store.board().links)).toEqual(["ab", "ac", "bc", "ad", "de", "be", "keen_finch"]);

    // The last link into its block, and the only one, stay where they are.
    const board = store.board();
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", before: null });
    expect(store.board()).toBe(board);
    await commands.run({ package: "blockr.core", id: "remove_link" }, { id: "de" });
    const alone = store.board();
    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "be", before: null });
    expect(store.board()).toBe(alone);
  });

  test("change_link moves a link that goes to another variadic block to the place given among the links into that block, in one step", async () => {
    const { store, commands } = await setup();
    await commands.run([
      { package: "blockr.core", id: "add_block", args: { registry_id: { package: "blockr.core", id: "rbind_block" }, id: "g" } },
      { package: "blockr.core", id: "add_link", args: { id: "cg", from: "c", to: "g" } },
      { package: "blockr.core", id: "add_link", args: { id: "ag", from: "a", to: "g" } },
    ]);
    const board = store.board();

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", to: "g", before: "ag" });
    expect(inputs(store.board(), "g")).toEqual([
      ["c", ""],
      ["d", ""],
      ["a", ""],
    ]);
    expect(inputs(store.board(), "e")).toEqual([["b", ""]]);

    store.undo();
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
    expect(store.board().links.de).toEqual(board.links.de);

    await commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", to: "g", before: null });
    expect(inputs(store.board(), "g")).toEqual([
      ["c", ""],
      ["a", ""],
      ["d", ""],
    ]);
  });

  test("change_link refuses a before for a link into a block that is not variadic, before a link into another block or before itself, and fails its preflight for a link that the board lacks", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    // The merge block c takes its links by their inputs, x and y.
    const variadic =
      'The block "c" is not variadic and takes each input by its name, so the order of its links changes nothing that it computes. The arg "input" changes the input that a link into it feeds, and "before" has to be left out.';
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "bc", before: "ac" })).rejects.toThrow(variadic);
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "bc", input: "x", before: null })).rejects.toThrow(variadic);

    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", before: "ad" })).rejects.toThrow(
      'The link "de" goes into block "e" and the link "ad" into block "d", so the args have to name a link into block "e".',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", before: "de" })).rejects.toThrow(
      'The link "de" cannot go before itself.',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", before: "zz" })).rejects.toThrow(
      'The board holds no link "zz" for link "de" to go before.',
    );
    await expect(commands.run({ package: "blockr.core", id: "change_link" }, { id: "de", before: 1 })).rejects.toThrow(
      "The args fail their schema at /before:",
    );
    expect(commands.preflight({ package: "blockr.core", id: "change_link" }, { id: "zz", before: null })).toBe(false);
    expect(store.board()).toBe(board);
  });

  test("free inputs are those of a block that no link feeds, an empty one for a variadic block, and none of a block the board lacks", async () => {
    const { store } = await setup();
    const board = store.board();
    const registry = store.blockRegistry();

    expect(freeInputs(board, registry, "c")).toEqual([]);
    expect(freeInputs(board, registry, "c", "bc")).toEqual(["y"]);
    expect(freeInputs(board, registry, "e")).toEqual([""]);
    expect(freeInputs(board, registry, "a")).toEqual([]);
    expect(freeInputs(board, registry, "z")).toBeUndefined();
    expect(freeInputs(board, new Map(), "c")).toBeUndefined();
  });

  test("the link that feeds an input is the one into the block, and none for a variadic block or a block the board lacks", async () => {
    const { store } = await setup();
    const board = store.board();
    const registry = store.blockRegistry();

    expect(linkFeeding(board, registry, "c", "y")).toBe("bc");
    expect(linkFeeding(board, registry, "c", "y", "bc")).toBeUndefined();
    expect(linkFeeding(board, registry, "e", "", "be")).toBeUndefined();
    expect(linkFeeding(board, registry, "z", "x")).toBeUndefined();
  });
});

describe("stacks", () => {
  test("add_stack adds a stack, taking its blocks out of the stack they are in", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "add_stack" }, { id: "s2", name: "Joined", blocks: ["b", "c"] });

    expect(store.board().stacks).toEqual({
      s1: { name: "Input", blocks: ["a"], color: null },
      s2: { name: "Joined", blocks: ["b", "c"], color: null },
    });
  });

  test("add_stack names a stack it is given no name for by the first free Stack n, and mints its id", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run({ package: "blockr.core", id: "add_stack" });
    await commands.run({ package: "blockr.core", id: "add_stack" });

    const ids = added(board.stacks, store.board().stacks);
    expect(ids).toHaveLength(2);
    for (const id of ids) expect(isId(id)).toBe(true);
    expect(store.board().stacks[ids[0] ?? ""]).toEqual({ name: "Stack 1", blocks: [], color: null });
    expect(store.board().stacks[ids[1] ?? ""]).toEqual({ name: "Stack 2", blocks: [], color: null });
  });

  test("change_stack sets a stack's blocks, taking them out of the stack they are in", async () => {
    const { store, commands } = await setup();
    await commands.run({ package: "blockr.core", id: "add_stack" }, { id: "s2", name: "Joined", blocks: ["c"] });

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s2", blocks: ["b", "c"] });
    expect(store.board().stacks.s1?.blocks).toEqual(["a"]);
    expect(store.board().stacks.s2?.blocks).toEqual(["b", "c"]);

    store.undo();
    expect(store.board().stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("add_stack gives a stack the colour given, and null for none where it is given none", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "add_stack" }, { id: "s2", blocks: ["c"], color: "#66c2a5" });
    await commands.run({ package: "blockr.core", id: "add_stack" }, { id: "s3", blocks: ["d"] });

    expect(store.board().stacks.s2).toStrictEqual({ name: "Stack 1", blocks: ["c"], color: "#66c2a5" });
    expect(store.board().stacks.s3).toStrictEqual({ name: "Stack 2", blocks: ["d"], color: null });
  });

  test("change_stack sets a stack's colour, or clears it with null, and keeps its blocks", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", color: "#fc8d62" });
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a", "b"], color: "#fc8d62" });

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", blocks: ["a"] });
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a"], color: "#fc8d62" });

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", color: null });
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a"], color: null });

    store.undo();
    expect(store.board().stacks.s1?.color).toBe("#fc8d62");
    store.undo();
    store.undo();
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a", "b"], color: null });
  });

  test("add_stack and change_stack refuse a colour that is no hex colour", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(commands.run({ package: "blockr.core", id: "add_stack" }, { color: "teal" })).rejects.toThrow(
      "The args fail their schema at /color:",
    );
    await expect(commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", color: "#66c2a" })).rejects.toThrow(
      "The args fail their schema at /color:",
    );
    expect(store.board()).toBe(board);
  });

  test("change_stack renames a stack, in one step with its other fields, and keeps what the args leave out", async () => {
    const { store, commands } = await setup();
    const stack = store.board().stacks.s1;

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", name: "Sources" });
    expect(store.board().stacks.s1).toEqual({ ...stack, name: "Sources" });

    await commands.run({ package: "blockr.core", id: "change_stack" }, { id: "s1", name: "Inputs", blocks: ["a"], color: "#66c2a5" });
    expect(store.board().stacks.s1).toEqual({ name: "Inputs", blocks: ["a"], color: "#66c2a5" });
    store.undo();
    expect(store.board().stacks.s1).toEqual({ ...stack, name: "Sources" });
  });

  test("remove_stack removes a stack, leaving its blocks", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "remove_stack" }, { id: "s1" });
    expect(store.board().stacks).toEqual({});
    expect(Object.keys(store.board().blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
  });
});

describe("the board", () => {
  test("change_board renames the board, and changes nothing where the args leave out the name", async () => {
    const { store, commands } = await setup();
    await commands.run({ package: "blockr.core", id: "change_board" }, { name: "Sepals" });
    expect(store.board().name).toBe("Sepals");

    const board = store.board();
    await expect(commands.run({ package: "blockr.core", id: "change_board" }, {})).resolves.toBe(false);
    expect(store.board()).toBe(board);
  });

  test("set_option sets a board option that a package registered, whose schema the store checks", async () => {
    const { store, commands } = await setup();

    await commands.run({ package: "blockr.core", id: "set_option" }, { id: "n_rows", value: 25 });
    expect(store.board().options.n_rows).toBe(25);

    expect(commands.preflight({ package: "blockr.core", id: "set_option" }, { id: "zoom", value: 2 })).toBe(false);
    const refused = commands.run({ package: "blockr.core", id: "set_option" }, { id: "n_rows", value: -1 });
    await expect(refused).rejects.toMatchObject({ by: "store" });
    await expect(refused).rejects.toThrow('The value of the board option "n_rows" fails its schema');
  });
});

// The DAG's plugin class, which keeps a position for each block.
const dag: PluginClass = {
  id: interned("blockr.dag", "dag_plugin"),
  state: {
    schema: {
      type: "object",
      properties: {
        positions: {
          type: "object",
          additionalProperties: {
            type: "object",
            properties: { x: { type: "number" }, y: { type: "number" } },
            required: ["x", "y"],
          },
        },
      },
      required: ["positions"],
    },
    default: { positions: {} },
  },
};

// The id of the DAG's plugin class, as args give it.
const dagId = { package: "blockr.dag", id: "dag_plugin" };

// The app's plugins, by the package and then by the id of each one's class.
type Plugins = Readonly<Record<string, Readonly<Record<string, GivenPlugin>>>>;

// The app's plugin of the class given, with the name given and the state that
// the opening board holds for it.
function plugin(def: PluginClass, name: string, state: Json = {}): Plugins {
  return { [def.id.package]: { [def.id.id]: { name, state } } };
}

// A store with the DAG's class, which has opened the board of R's reply to
// hello, whose app runs the plugins given, and the command registry of that
// store.
async function pluginStore(
  plugins: Plugins,
  lock: HelloReply["lock"] = { board: false, load: false },
): Promise<{ store: Store; commands: Commands }> {
  const store = await openedStore((store) => store.plugins.register(dag), { ...withPlugins(plugins), lock });
  return { store, commands: createCommands(store, { lock }) };
}

describe("plugins", () => {
  test("set_plugin_state patches within a plugin's state, by the id of the plugin's class, which the class's schema checks", async () => {
    const { store, commands } = await pluginStore(plugin(dag, "DAG", { positions: {} }));

    await commands.run(
      { package: "blockr.core", id: "set_plugin_state" },
      { plugin: dagId, patches: [{ op: "add", path: "/positions/a", value: { x: 40, y: 80 } }] },
      "assistant",
    );
    expect(store.board().plugins["blockr.dag"]?.dag_plugin).toEqual({ positions: { a: { x: 40, y: 80 } } });
    expect(store.canUndo("assistant")).toBe(true);

    // A patch of the whole state replaces what the board holds for the plugin.
    expect(await commands.plan({ package: "blockr.core", id: "set_plugin_state" }, { plugin: dagId, patches: [{ op: "replace", path: "", value: { positions: {} } }] })).toEqual([
      {
        patches: [
          {
            op: "replace",
            path: "/plugins/blockr.dag/dag_plugin",
            value: { positions: {} },
            edit: store.editId({ plugin: dag.id }),
          },
        ],
      },
    ]);
    await commands.run({ package: "blockr.core", id: "set_plugin_state" }, {
      plugin: dagId,
      patches: [{ op: "replace", path: "", value: { positions: {} } }],
    });
    expect(store.board().plugins["blockr.dag"]?.dag_plugin).toEqual({ positions: {} });

    await expect(
      commands.run({ package: "blockr.core", id: "set_plugin_state" }, {
        plugin: dagId,
        patches: [{ op: "add", path: "/positions/b", value: { x: "left", y: 0 } }],
      }),
    ).rejects.toMatchObject({ by: "store" });
    // Removing the whole state would remove what the board holds for the
    // plugin, which the app runs all the same.
    await expect(
      commands.run({ package: "blockr.core", id: "set_plugin_state" }, { plugin: dagId, patches: [{ op: "remove", path: "" }] }),
    ).rejects.toMatchObject({ by: "store" });
    await expect(
      commands.run({ package: "blockr.core", id: "set_plugin_state" }, { plugin: dagId, patches: [{ op: "move", path: "/positions" }] }),
    ).rejects.toThrow(/The args/);
    // It fails its preflight for a plugin whose state the board does not
    // hold, as one that the app does not run, or of a class that keeps no
    // state.
    expect(commands.preflight({ package: "blockr.core", id: "set_plugin_state" }, { plugin: { package: "blockr.dag", id: "nowhere_plugin" }, patches: [] })).toBe(false);
    expect(commands.preflight({ package: "blockr.core", id: "set_plugin_state" }, { plugin: { package: "blockr.core", id: "basic_frontend_plugin" }, patches: [] })).toBe(false);
  });

  test("under the board lock, set_plugin_state fails its preflight, and under the load lock alone it passes", async () => {
    const state = plugin(dag, "DAG", { positions: {} });
    const args = { plugin: dagId, patches: [] };
    const locked = await pluginStore(state, { board: true, load: false });
    expect(locked.commands.preflight({ package: "blockr.core", id: "set_plugin_state" }, args)).toBe(false);
    const loading = await pluginStore(state, { board: false, load: true });
    expect(loading.commands.preflight({ package: "blockr.core", id: "set_plugin_state" }, args)).toBe(true);
  });
});

describe("ids", () => {
  test("core's commands mint the ids of blocks, links and stacks in the shape that rand_id names", async () => {
    const store = await openedStore();
    const commands = createCommands(store, { randId: "letters_8" });
    const board = store.board();

    await commands.run({ package: "blockr.core", id: "add_block" }, { registry_id: head });
    const [block = ""] = added(board.blocks, store.board().blocks);
    await commands.run({ package: "blockr.core", id: "add_link" }, { from: "d", to: block });
    await commands.run({ package: "blockr.core", id: "add_stack" });

    const minted = [
      ...added(board.blocks, store.board().blocks),
      ...added(board.links, store.board().links),
      ...added(board.stacks, store.board().stacks),
    ];
    expect(minted).toHaveLength(3);
    for (const id of minted) expect(id).toMatch(/^[a-z]{8}$/);
  });

  test("the command registry refuses a rand_id that names no shape", async () => {
    const store = await openedStore();
    expect(() => createCommands(store, { randId: "counter" })).toThrow(
      'The blockr option rand_id is "counter", where it takes one of "adjective_animal", "adjective_adjective_animal", "letters_6", "letters_8", "letters_12", "bytes_4", "bytes_8" and "uuid".',
    );
    expect(() => createCommands(store, { randId: 3 })).toThrow("The blockr option rand_id is 3, where it takes one of");
    expect(() => createCommands(store, { randId: null })).toThrow("The blockr option rand_id is null, where it takes one of");
  });
});

describe("the schemas", () => {
  test("a block's id is neither empty nor made of digits alone", async () => {
    const { commands } = await setup();
    await expect(commands.run({ package: "blockr.core", id: "remove_block" }, { id: "12" })).rejects.toThrow(
      "The args fail their schema at /id:",
    );
    expect(schemas.blockId.pattern).toBe("\\D");
  });

});
