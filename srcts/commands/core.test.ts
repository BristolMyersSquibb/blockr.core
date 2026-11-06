// @vitest-environment node

// Core's commands, on the board of a core app from the wire protocol's
// fixtures: blocks a (dataset), b (head), c (merge), d (subset), e (rbind,
// which is variadic) and f (scatter), the links ab, ac, bc, ad, be, de and one
// from a to f, and the stack s1 of a and b.

import { describe, expect, test } from "vitest";
import { isId, StoreError } from "../store/index.ts";
import type { Store } from "../store/index.ts";
import { hello, openedStore } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { blockName, CommandError, createCommands, schemas, stateSchema } from "./index.ts";
import type { Commands } from "./index.ts";

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

function headClass(): RegistryEntry {
  const entry = hello.block_registry[head];
  if (entry === undefined) throw new Error(`The fixture's block registry has no "${head}".`);
  return entry;
}

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
    expect(() => run?.(store.board(), { registry_id: "blockr.dplyr-filter_block" })).toThrow(
      'The command "blockr.core-add_block" refused to run',
    );
  });

  test("set_block refuses only what a change makes fail, so that a failing state can be repaired one arg at a time", async () => {
    const { store, commands } = await setup();
    // A state that fails the schema of its class in two args, as a file that
    // no migration converted may hold.
    store.apply({ blocks: { mod: { b: { state: { n: -1, direction: "sideways" } } } } });

    await expect(commands.run("blockr.core-set_block", { id: "b", state: { n: -2 } })).rejects.toThrow(
      'The state of block "b" fails its schema at /n: -2 is less than 0.',
    );

    await commands.run("blockr.core-set_block", { id: "b", state: { n: 5 } });
    await commands.run("blockr.core-set_block", { id: "b", state: { direction: "head" } });
    expect(store.board().blocks.b?.state).toEqual({ n: 5, direction: "head" });
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

describe("appending, prepending and inserting", () => {
  test("append_block adds a block that takes a block's result into its only input", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-append_block", { from: "d", registry_id: head, id: "top" });

    expect(store.board().blocks.top?.registry_id).toBe(head);
    expect(inputs(store.board(), "top")).toEqual([["d", "data"]]);
    expect(added(board.links, store.board().links)).toHaveLength(1);
  });

  test("append_block refuses a class that takes no input", async () => {
    const { commands } = await setup();
    await expect(
      commands.run("blockr.core-append_block", { from: "d", registry_id: "blockr.core-dataset_block" }),
    ).rejects.toThrow('The block class "blockr.core-dataset_block" has no input to take a link.');
  });

  test("append_block takes the input given where the new block has several, and refuses to pick one", async () => {
    const { store, commands } = await setup();
    const merge = { from: "d", registry_id: "blockr.core-merge_block", id: "joined" };

    await expect(commands.run("blockr.core-append_block", merge)).rejects.toThrow(
      'The block class "blockr.core-merge_block" has the inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run("blockr.core-append_block", { ...merge, input: "y" });
    expect(inputs(store.board(), "joined")).toEqual([["d", "y"]]);
  });

  test("prepend_block adds a block whose result goes to a block's free input, in link order where it is variadic", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-prepend_block", {
      to: "e",
      registry_id: "blockr.core-dataset_block",
      id: "cars",
      state: { dataset: "mtcars" },
    });

    expect(store.board().blocks.cars?.state).toEqual({ dataset: "mtcars", package: "datasets" });
    expect(inputs(store.board(), "e")).toEqual([
      ["b", ""],
      ["d", ""],
      ["cars", ""],
    ]);
  });

  test("prepend_block is not enabled for a block whose inputs links feed", async () => {
    const { commands } = await setup();
    expect(commands.enabled("blockr.core-prepend_block", { to: "b", registry_id: head })).toBe(false);
    expect(commands.enabled("blockr.core-prepend_block", { to: "e", registry_id: head })).toBe(true);
  });

  test("prepend_block refuses to pick one of several free inputs", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-remove_link", { id: "ac" });
    await commands.run("blockr.core-remove_link", { id: "bc" });
    const top = { to: "c", registry_id: head, id: "top" };

    expect(commands.enabled("blockr.core-prepend_block", top)).toBe(true);
    await expect(commands.run("blockr.core-prepend_block", top)).rejects.toThrow(
      'The block "c" has the free inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run("blockr.core-prepend_block", { ...top, input: "y" });
    expect(inputs(store.board(), "c")).toEqual([["top", "y"]]);
  });

  test("insert_block puts a block on a link, whose input the new link takes in its place among the links", async () => {
    const { store, commands } = await setup();
    const board = store.board();

    await commands.run("blockr.core-insert_block", { link: "be", registry_id: head, id: "top" });

    const after = store.board();
    expect(Object.hasOwn(after.links, "be")).toBe(false);
    expect(inputs(after, "top")).toEqual([["b", "data"]]);
    expect(inputs(after, "e")).toEqual([
      ["top", ""],
      ["d", ""],
    ]);

    store.undo();
    expect(store.board()).toEqual(board);
    expect(Object.keys(store.board().links)).toEqual(Object.keys(board.links));
  });

  test("insert_block keeps the input that the link fed", async () => {
    const { store, commands } = await setup();

    await commands.run("blockr.core-insert_block", { link: "ac", registry_id: head, id: "top" });

    expect(inputs(store.board(), "top")).toEqual([["a", "data"]]);
    expect(inputs(store.board(), "c")).toEqual([
      ["top", "x"],
      ["b", "y"],
    ]);
  });

  test("insert_block takes the input given where the new block has several, and refuses to pick one", async () => {
    const { store, commands } = await setup();
    const merge = { link: "ab", registry_id: "blockr.core-merge_block", id: "joined" };

    await expect(commands.run("blockr.core-insert_block", merge)).rejects.toThrow(
      'The block class "blockr.core-merge_block" has the inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run("blockr.core-insert_block", { ...merge, input: "x" });
    expect(inputs(store.board(), "joined")).toEqual([["a", "x"]]);
    expect(inputs(store.board(), "b")).toEqual([["joined", "data"]]);
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

    expect(commands.enabled("blockr.core-add_link", { from: "d", to: "c" })).toBe(true);
    await expect(commands.run("blockr.core-add_link", { from: "d", to: "c" })).rejects.toThrow(
      'The block "c" has the free inputs "x" and "y", so the args have to name the one to use.',
    );

    await commands.run("blockr.core-add_link", { from: "d", to: "c", input: "y", id: "dc" });
    expect(store.board().links.dc).toEqual({ from: "d", to: "c", input: "y" });
  });

  test("add_link is not enabled for a block whose inputs links feed", async () => {
    const { commands } = await setup();
    expect(commands.enabled("blockr.core-add_link", { from: "d", to: "c" })).toBe(false);
    expect(commands.enabled("blockr.core-add_link", { from: "d", to: "zz" })).toBe(false);
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
      'The block "d" has no input that no link feeds.',
    );
  });

  test("remove_link removes a link", async () => {
    const { store, commands } = await setup();
    await commands.run("blockr.core-remove_link", { id: "ab" });
    expect(Object.hasOwn(store.board().links, "ab")).toBe(false);
    expect(commands.enabled("blockr.core-remove_link", { id: "ab" })).toBe(false);
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

    expect(commands.enabled("blockr.core-set_option", { id: "zoom", value: 2 })).toBe(false);
    const refused = commands.run("blockr.core-set_option", { id: "n_rows", value: -1 });
    await expect(refused).rejects.toThrow(StoreError);
    await expect(refused).rejects.toThrow('The value of the board option "n_rows" fails its schema');
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

  test("stateSchema() describes each state arg by its arg spec, once per class", () => {
    const entry = headClass();

    const schema = stateSchema(entry);
    expect(schema.properties).toEqual({
      n: { description: "Number of rows to keep.", examples: [10], type: "integer", minimum: 0 },
      direction: {
        description: "Row end to keep, either head (first) or tail (last).",
        examples: ["tail"],
        enum: ["head", "tail"],
      },
    });
    expect(schema.required).toEqual(["n", "direction"]);
    expect(stateSchema(entry)).toBe(schema);
  });

  test("stateSchema() keeps what the schema says itself", () => {
    const entry: RegistryEntry = {
      ...headClass(),
      schema: { type: "object", properties: { n: { type: "integer", description: "Rows." } } },
    };
    expect(stateSchema(entry).properties).toEqual({
      n: { type: "integer", description: "Rows.", examples: [10] },
    });
  });

  test("blockName() names a block as R names it by its class", () => {
    expect(blockName("blockr.core-head_block")).toBe("Head");
    expect(blockName("blockr.dplyr-filter_expr_block")).toBe("Filter expr");
    expect(blockName("my_block")).toBe("My");
  });
});
