// @vitest-environment node

// Core's commands, on the board of a core app from the wire protocol's
// fixtures: blocks a (dataset), b (head), c (merge), d (subset), e (rbind,
// which is variadic) and f (scatter), the links ab, ac, bc, ad, be, de and one
// from a to f, and the stack s1 of a and b.

import { describe, expect, test } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import { isId, StoreError } from "../store/index.ts";
import type { PluginClass, Store } from "../store/index.ts";
import { openedStore, withPlugins } from "../testing/store.ts";
import type { GivenPlugin } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { CommandError, createCommands, defaultBlockName, freeInputs, linkFeeding, schemas } from "./index.ts";
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

const head = "blockr.core-head_block";

describe("blocks", () => {
  test("add_block adds a block with the state its class starts one with, its name and a minted id", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-add_block", { registry_id: head });

    const [id = ""] = added(board.blocks, store.board().blocks);
    expect(isId(id)).toBe(true);
    expect(store.board().blocks[id]).toEqual({
      registry_id: head,
      state: { n: 6, direction: "head" },
      name: "Head",
      locked: null,
    });
  });

  test("add_block changes the state its class starts a block with by the state args given", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-add_block", {
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
      commands.run("blockr.core-add_block", { registry_id: "blockr.dplyr-filter_block" }),
    ).rejects.toThrow('The block registry has no block class "blockr.dplyr-filter_block".');

    const refused = commands.run("blockr.core-add_block", { registry_id: head, state: { n: "ten" } });
    await expect(refused).rejects.toThrow(CommandError);
    await expect(refused).rejects.toThrow("The state of the new block fails its schema at /n:");
    expect(store.board()).toBe(board);
  });

  test("add_block leaves an id that the board holds already to the store, which refuses it", async () => {
    const { commands } = await setup();
    await expect(commands.run("blockr.core-add_block", { registry_id: head, id: "a" })).rejects.toThrow(
      'The board holds block "a" already.',
    );
  });

  test("remove_block removes a block with its links and its place in its stack", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-remove_block", { id: "b" });

    const board = store.board();
    expect(Object.keys(board.blocks)).toEqual(["a", "c", "d", "e", "f"]);
    expect(Object.values(board.links).some((link) => link.from === "b" || link.to === "b")).toBe(false);
    expect(board.stacks.s1?.blocks).toEqual(["a"]);
  });

  test("rename_block renames a block, and makes no step for the name it has", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-rename_block", { id: "b", name: "Head" });
    expect(store.canUndo()).toBe(false);

    await commands.run("blockr.core-rename_block", { id: "b", name: "Top rows" });
    expect(store.board().blocks.b?.name).toBe("Top rows");
  });

  test("set_block_lock sets a block's lock, null among its values, and makes no step for the lock it has", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-set_block_lock", { id: "b", locked: null });
    expect(store.canUndo()).toBe(false);

    await commands.run("blockr.core-set_block_lock", { id: "b", locked: false });
    expect(store.board().blocks.b?.locked).toBe(false);
    await commands.run("blockr.core-set_block_lock", { id: "b", locked: true });
    expect(store.board().blocks.b?.locked).toBe(true);
    await commands.run("blockr.core-set_block_lock", { id: "b", locked: null });
    expect(store.board().blocks.b?.locked).toBeNull();

    store.undo();
    expect(store.board().blocks.b?.locked).toBe(true);
    store.undo();
    store.undo();
    expect(store.board().blocks.b?.locked).toBeNull();

    await expect(commands.run("blockr.core-set_block_lock", { id: "b", locked: "yes" })).rejects.toThrow(
      "The args fail their schema at /locked:",
    );
    expect(commands.preflight("blockr.core-set_block_lock", { id: "z", locked: true })).toBe(false);
  });

  test("set_block sets and unsets state args, as one step", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-set_block", { id: "a", state: { package: "ggplot2" }, unset: ["dataset"] });

    expect(store.board().blocks.a?.state).toEqual({ package: "ggplot2" });
    store.undo();
    expect(store.board().blocks.a?.state).toEqual({ dataset: "iris", package: "datasets" });
  });

  test("runs where its run() is called on its own, off the command", async () => {
    const { store, commands } = await setup();
    // eslint-disable-next-line @typescript-eslint/unbound-method -- takes run() off the command on purpose
    const { run } = commands.registered()["blockr.core-add_block"] ?? {};
    const context: CommandContext = {
      lock: { board: false, load: false },
      mintId: () => "calm_heron",
      blockrOption: () => Promise.resolve(undefined),
      refusal: (failures) => new CommandError("The test refused", failures),
    };
    expect(() => run?.(store.board(), { registry_id: "blockr.dplyr-filter_block" }, context)).toThrow(
      'The command "blockr.core-add_block" refused to run',
    );
  });

  test("set_block makes no step where nothing changes", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-set_block", { id: "b", state: { n: 6 }, unset: ["x"] });
    expect(store.canUndo()).toBe(false);
  });

  test("set_block refuses a state that the schema of the block's class refuses", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await expect(commands.run("blockr.core-set_block", { id: "b", state: { n: -1 } })).rejects.toThrow(
      'The state of block "b" fails its schema at /n:',
    );
    await expect(commands.run("blockr.core-set_block", { id: "b", unset: ["n"] })).rejects.toThrow(
      'The state of block "b" fails its schema: Instance does not have required property "n".',
    );
    await expect(commands.run("blockr.core-set_block", { id: "b", state: { m: 1 } })).rejects.toThrow(
      'The state of block "b" fails its schema',
    );
    expect(store.board()).toBe(board);
  });
});

describe("appending, prepending and inserting, as batches", () => {
  test("add_block and add_link append a block in one step, which takes a block's result into its only input", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run([
      { id: "blockr.core-add_block", args: { registry_id: head, id: "top" } },
      { id: "blockr.core-add_link", args: { from: "d", to: "top" } },
    ]);

    expect(store.board().blocks.top?.registry_id).toBe(head);
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
        { id: "blockr.core-add_block", args: { registry_id: "blockr.core-dataset_block", id: "cars" } },
        { id: "blockr.core-add_link", args: { from: "d", to: "cars" } },
      ]),
    ).rejects.toThrow(
      'The list of commands was refused at command 2, "blockr.core-add_link"\n- The block "cars" takes no input, since its class "blockr.core-dataset_block" has none.',
    );
    expect(store.board()).toBe(board);
  });

  test("add_block and add_link prepend a block, whose result goes to a block's free input, in link order where it is variadic", async () => {
    const { store, commands } = await setup();

    await commands.run([
      {
        id: "blockr.core-add_block",
        args: { registry_id: "blockr.core-dataset_block", id: "cars", state: { dataset: "mtcars" } },
      },
      { id: "blockr.core-add_link", args: { from: "cars", to: "e" } },
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
      { id: "blockr.core-add_block", args: { registry_id: head, id: "top" } },
      { id: "blockr.core-add_link", args: { from: "b", to: "top" } },
      { id: "blockr.core-change_link", args: { id: "be", from: "top" } },
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
      { id: "blockr.core-add_block", args: { registry_id: head, id: "top" } },
      { id: "blockr.core-add_link", args: { from: "a", to: "top" } },
      { id: "blockr.core-change_link", args: { id: "ac", from: "top" } },
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

    await commands.run("blockr.core-remove_link", { id: "ab" });
    await commands.run("blockr.core-add_link", { from: "d", to: "b", id: "db" });
    expect(store.board().links.db).toEqual({ from: "d", to: "b", input: "data" });

    await commands.run("blockr.core-remove_link", { id: "ac" });
    await commands.run("blockr.core-add_link", { from: "d", to: "c", id: "dc" });
    expect(store.board().links.dc).toEqual({ from: "d", to: "c", input: "x" });

    await commands.run("blockr.core-add_link", { from: "c", to: "e", id: "ce" });
    expect(store.board().links.ce).toEqual({ from: "c", to: "e", input: "" });
  });

  test("add_link refuses to pick one of several free inputs, naming them, and takes the one given", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-remove_link", { id: "ac" });
    await commands.run("blockr.core-remove_link", { id: "bc" });

    expect(commands.preflight("blockr.core-add_link", { from: "d", to: "c" })).toBe(true);
    await expect(commands.run("blockr.core-add_link", { from: "d", to: "c" })).rejects.toThrow(
      'The block "c" has the free inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run("blockr.core-add_link", { from: "d", to: "c", input: "y", id: "dc" });
    expect(store.board().links.dc).toEqual({ from: "d", to: "c", input: "y" });
  });

  test("add_link fails its preflight for a block without a free input, and says why", async () => {
    const { commands } = await setup();
    await commands.run("blockr.core-add_block", { registry_id: "blockr.core-dataset_block", id: "cars" });

    expect(commands.preflight("blockr.core-add_link", { from: "d", to: "c" })).toBe(false);
    expect(commands.preflight("blockr.core-add_link", { from: "d", to: "cars" })).toBe(false);
    expect(commands.preflight("blockr.core-add_link", { from: "d", to: "zz" })).toBe(false);

    await expect(commands.run("blockr.core-add_link", { from: "d", to: "c" })).rejects.toThrow(
      'The command "blockr.core-add_link" refused to run\n- Every input of the block "c" takes a link already.',
    );
    await expect(commands.run("blockr.core-add_link", { from: "d", to: "cars" })).rejects.toThrow(
      'The block "cars" takes no input, since its class "blockr.core-dataset_block" has none.',
    );
  });

  test("add_link keeps the input given, leaving its checks to the store", async () => {
    const { commands } = await setup();
    await expect(commands.run("blockr.core-add_link", { from: "d", to: "c", input: "x", id: "dc" })).rejects.toThrow(
      'The input "x" of block "c" takes the links "ac" and "dc", where it takes one.',
    );
  });

  test("change_link keeps the input where the link goes to the same block, and takes the only free one where it goes to another", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-change_link", { id: "bc", from: "d" });
    expect(store.board().links.bc).toEqual({ from: "d", to: "c", input: "y" });

    await commands.run("blockr.core-change_link", { id: "bc", to: "e" });
    expect(store.board().links.bc).toEqual({ from: "d", to: "e", input: "" });

    await commands.run("blockr.core-change_link", { id: "ab", from: "a" });
    expect(store.canUndo()).toBe(true);
    store.undo();
    store.undo();
    expect(store.board().links.bc).toEqual({ from: "b", to: "c", input: "y" });
  });

  test("change_link refuses a block whose inputs other links feed", async () => {
    const { commands } = await setup();
    await expect(commands.run("blockr.core-change_link", { id: "ab", to: "d" })).rejects.toThrow(
      'Every input of the block "d" takes a link already.',
    );
  });

  test("change_link swaps the inputs of two links into a block that is not variadic, in one step that one undo takes back", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    // The merge block c takes ac as x and bc as y.
    await commands.run("blockr.core-change_link", { id: "ac", input: "y" });
    expect(inputs(store.board(), "c")).toEqual([
      ["a", "y"],
      ["b", "x"],
    ]);

    store.undo();
    expect(store.board().links).toEqual(board.links);
    expect(store.canUndo()).toBe(false);

    // A link that comes from another block now swaps too.
    await commands.run("blockr.core-change_link", { id: "bc", from: "d", input: "x" });
    expect(inputs(store.board(), "c")).toEqual([
      ["a", "y"],
      ["d", "x"],
    ]);
  });

  test("change_link swaps no inputs where the link goes to another block, nor among the links into a variadic block", async () => {
    const { store, commands } = await setup();

    // The old input of ab, the data of block b, means nothing to block c, so
    // ac keeps x.
    await expect(commands.run("blockr.core-change_link", { id: "ab", to: "c", input: "x" })).rejects.toThrow(
      'The input "x" of block "c" takes the links "ab" and "ac", where it takes one.',
    );

    // The rbind block e takes be and de by their place, as neither gives a
    // name, and de gives none still once be has had a name and lost it.
    await commands.run("blockr.core-change_link", { id: "be", input: "top" });
    await commands.run("blockr.core-change_link", { id: "be", input: "" });
    expect(inputs(store.board(), "e")).toEqual([
      ["b", ""],
      ["d", ""],
    ]);
  });

  test("remove_link removes a link", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-remove_link", { id: "ab" });
    expect(Object.hasOwn(store.board().links, "ab")).toBe(false);
    expect(commands.preflight("blockr.core-remove_link", { id: "ab" })).toBe(false);
  });

  test("move_link moves a link before another into its block, keeping its id and its ends, as one step", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-move_link", { id: "de", before: "be" });
    expect(inputs(store.board(), "e")).toEqual([
      ["d", ""],
      ["b", ""],
    ]);
    expect(store.board().links.de).toBe(board.links.de);

    store.undo();
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
    expect(store.canUndo()).toBe(false);
  });

  test("move_link puts a link last among the links into its block, right after the block's last other link", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-move_link", { id: "be" });
    expect(Object.keys(store.board().links)).toEqual(["ab", "ac", "bc", "ad", "de", "be", "keen_finch"]);

    // The last link into its block, and the only one, stay where they are.
    const board = store.board();
    await commands.run("blockr.core-move_link", { id: "be" });
    expect(store.board()).toBe(board);
    await commands.run("blockr.core-remove_link", { id: "de" });
    const alone = store.board();
    await commands.run("blockr.core-move_link", { id: "be" });
    expect(store.board()).toBe(alone);
  });

  test("move_link refuses a link into a block that is not variadic, a link into another block and the link itself, and fails its preflight for a link that the board lacks", async () => {
    const { commands } = await setup();

    // The merge block c takes its links by their inputs, x and y.
    await expect(commands.run("blockr.core-move_link", { id: "bc", before: "ac" })).rejects.toThrow(
      'The block "c" is not variadic and takes each input by its name, so the order of its links changes nothing that it computes. The command "blockr.core-change_link" changes the input that a link feeds.',
    );

    await expect(commands.run("blockr.core-move_link", { id: "de", before: "ad" })).rejects.toThrow(
      'The link "de" goes into block "e" and the link "ad" into block "d", so the args have to name a link into block "e".',
    );
    await expect(commands.run("blockr.core-move_link", { id: "de", before: "de" })).rejects.toThrow(
      'The link "de" cannot go before itself.',
    );
    await expect(commands.run("blockr.core-move_link", { id: "de", before: "zz" })).rejects.toThrow(
      'The board holds no link "zz" for link "de" to go before.',
    );
    expect(commands.preflight("blockr.core-move_link", { id: "zz" })).toBe(false);
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
    expect(freeInputs(board, {}, "c")).toBeUndefined();
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

    await commands.run("blockr.core-add_stack", { id: "s2", name: "Joined", blocks: ["b", "c"] });

    expect(store.board().stacks).toEqual({
      s1: { name: "Input", blocks: ["a"], color: null },
      s2: { name: "Joined", blocks: ["b", "c"], color: null },
    });
  });

  test("add_stack names a stack it is given no name for, and mints its id", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-add_stack");

    const [id = ""] = added(board.stacks, store.board().stacks);
    expect(isId(id)).toBe(true);
    expect(store.board().stacks[id]).toEqual({ name: "Stack", blocks: [], color: null });
  });

  test("change_stack sets a stack's blocks, taking them out of the stack they are in", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-add_stack", { id: "s2", name: "Joined", blocks: ["c"] });

    await commands.run("blockr.core-change_stack", { id: "s2", blocks: ["b", "c"] });
    expect(store.board().stacks.s1?.blocks).toEqual(["a"]);
    expect(store.board().stacks.s2?.blocks).toEqual(["b", "c"]);

    store.undo();
    expect(store.board().stacks.s1?.blocks).toEqual(["a", "b"]);
  });

  test("add_stack gives a stack the colour given, and null for none where it is given none", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-add_stack", { id: "s2", blocks: ["c"], color: "#66c2a5" });
    await commands.run("blockr.core-add_stack", { id: "s3", blocks: ["d"] });

    expect(store.board().stacks.s2).toStrictEqual({ name: "Stack", blocks: ["c"], color: "#66c2a5" });
    expect(store.board().stacks.s3).toStrictEqual({ name: "Stack", blocks: ["d"], color: null });
  });

  test("change_stack sets a stack's colour, or clears it with null, and keeps its blocks", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-change_stack", { id: "s1", color: "#fc8d62" });
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a", "b"], color: "#fc8d62" });

    await commands.run("blockr.core-change_stack", { id: "s1", blocks: ["a"] });
    expect(store.board().stacks.s1).toStrictEqual({ name: "Input", blocks: ["a"], color: "#fc8d62" });

    await commands.run("blockr.core-change_stack", { id: "s1", color: null });
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

    await expect(commands.run("blockr.core-add_stack", { color: "teal" })).rejects.toThrow(
      "The args fail their schema at /color:",
    );
    await expect(commands.run("blockr.core-change_stack", { id: "s1", color: "#66c2a" })).rejects.toThrow(
      "The args fail their schema at /color:",
    );
    expect(store.board()).toBe(board);
  });

  test("rename_stack and remove_stack, which leaves the stack's blocks", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-rename_stack", { id: "s1", name: "Sources" });
    expect(store.board().stacks.s1?.name).toBe("Sources");

    await commands.run("blockr.core-remove_stack", { id: "s1" });
    expect(store.board().stacks).toEqual({});
    expect(Object.keys(store.board().blocks)).toEqual(["a", "b", "c", "d", "e", "f"]);
  });
});

describe("the board", () => {
  test("rename_board renames the board", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-rename_board", { name: "Sepals" });
    expect(store.board().name).toBe("Sepals");
  });

  test("set_option sets a board option that a package registered, whose schema the store checks", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-set_option", { id: "n_rows", value: 25 });
    expect(store.board().options.n_rows).toBe(25);

    expect(commands.preflight("blockr.core-set_option", { id: "zoom", value: 2 })).toBe(false);
    const refused = commands.run("blockr.core-set_option", { id: "n_rows", value: -1 });
    await expect(refused).rejects.toThrow(StoreError);
    await expect(refused).rejects.toThrow('The value of the board option "n_rows" fails its schema');
  });
});

// The DAG's plugin class, which keeps a position for each block.
const dag: PluginClass = {
  id: "blockr.dag-dag_plugin",
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

// The app's plugin of the class given, with the name given and the state that
// the opening board holds for it.
function plugin(def: PluginClass, name: string, state: Json = {}): Readonly<Record<string, GivenPlugin>> {
  return { [def.id]: { name, state } };
}

// A store with the DAG's class, which has opened the board of R's reply to
// hello, whose app runs the plugins given, and the command registry of that
// store.
async function pluginStore(
  plugins: Readonly<Record<string, GivenPlugin>>,
  lock: HelloReply["lock"] = { board: false, load: false },
): Promise<{ store: Store; commands: Commands }> {
  const store = await openedStore((store) => store.plugins.register(dag), { ...withPlugins(plugins), lock });
  return { store, commands: createCommands(store, { lock }) };
}

describe("plugins", () => {
  test("set_plugin_state patches within a plugin's state, by the id of the plugin's class, which the class's schema checks", async () => {
    const { store, commands } = await pluginStore(plugin(dag, "DAG", { positions: {} }));

    await commands.run(
      "blockr.core-set_plugin_state",
      { plugin: dag.id, patches: [{ op: "add", path: "/positions/a", value: { x: 40, y: 80 } }] },
      "assistant",
    );
    expect(store.board().plugins[dag.id]).toEqual({ positions: { a: { x: 40, y: 80 } } });
    expect(store.canUndo("assistant")).toBe(true);

    // A patch of the whole state replaces what the board holds for the plugin.
    expect(await commands.plan("blockr.core-set_plugin_state", { plugin: dag.id, patches: [{ op: "replace", path: "", value: { positions: {} } }] })).toEqual([
      {
        patches: [
          {
            op: "replace",
            path: "/plugins/blockr.dag-dag_plugin",
            value: { positions: {} },
            edit: store.editId({ plugin: dag.id }),
          },
        ],
      },
    ]);
    await commands.run("blockr.core-set_plugin_state", {
      plugin: dag.id,
      patches: [{ op: "replace", path: "", value: { positions: {} } }],
    });
    expect(store.board().plugins[dag.id]).toEqual({ positions: {} });

    await expect(
      commands.run("blockr.core-set_plugin_state", {
        plugin: dag.id,
        patches: [{ op: "add", path: "/positions/b", value: { x: "left", y: 0 } }],
      }),
    ).rejects.toThrow(StoreError);
    // Removing the whole state would remove what the board holds for the
    // plugin, which the app runs all the same.
    await expect(
      commands.run("blockr.core-set_plugin_state", { plugin: dag.id, patches: [{ op: "remove", path: "" }] }),
    ).rejects.toThrow(StoreError);
    await expect(
      commands.run("blockr.core-set_plugin_state", { plugin: dag.id, patches: [{ op: "move", path: "/positions" }] }),
    ).rejects.toThrow(/The args/);
    // It fails its preflight for a plugin whose state the board does not
    // hold, as one that the app does not run, or of a class that keeps no
    // state.
    expect(commands.preflight("blockr.core-set_plugin_state", { plugin: "blockr.dag-nowhere_plugin", patches: [] })).toBe(false);
    expect(commands.preflight("blockr.core-set_plugin_state", { plugin: "blockr.core-basic_frontend_plugin", patches: [] })).toBe(false);
  });

  test("under the board lock, set_plugin_state fails its preflight, and under the load lock alone it passes", async () => {
    const state = plugin(dag, "DAG", { positions: {} });
    const args = { plugin: dag.id, patches: [] };
    const locked = await pluginStore(state, { board: true, load: false });
    expect(locked.commands.preflight("blockr.core-set_plugin_state", args)).toBe(false);
    const loading = await pluginStore(state, { board: false, load: true });
    expect(loading.commands.preflight("blockr.core-set_plugin_state", args)).toBe(true);
  });
});

describe("ids", () => {
  test("core's commands mint the ids of blocks, links and stacks in the shape that rand_id names", async () => {
    const store = await openedStore();
    const commands = createCommands(store, { randId: "letters_8" });
    const board = store.board();

    await commands.run("blockr.core-add_block", { registry_id: head });
    const [block = ""] = added(board.blocks, store.board().blocks);
    await commands.run("blockr.core-add_link", { from: "d", to: block });
    await commands.run("blockr.core-add_stack");

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
    await expect(commands.run("blockr.core-remove_block", { id: "12" })).rejects.toThrow(
      "The args fail their schema at /id:",
    );
    expect(schemas.blockId.pattern).toBe("\\D");
  });

  test("defaultBlockName() names a block as R names it by its class", () => {
    expect(defaultBlockName("blockr.core-head_block")).toBe("Head");
    expect(defaultBlockName("blockr.dplyr-filter_expr_block")).toBe("Filter expr");
    expect(defaultBlockName("my_block")).toBe("My");
  });
});
