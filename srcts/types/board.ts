// The board the client holds: plain data, which serializes as it is and names
// each block's class by registry id and each plugin's class by the class's id,
// so that no object in it carries an R class.
// Its types are read-only, so that nothing changes a board in place:
// the store makes a new board for every change, sharing what did not change,
// as copy-on-modify did in R.

import type { Json } from "./json.ts";

export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;

declare const interned: unique symbol;

// What a package defines, such as a block class, a plugin class, a renderer,
// a module that it offers other packages, a command or a board action, named
// by the package and by the id within that package, as
// { package: "blockr.core", id: "head_block" }.
// The package is null only for what an app script defines: a block class, or
// a renderer whose module the script serves.
// Core makes one object for each pair (interned(), ../util/ids.ts), so two that
// name the same are the same object, which ===, a Set and a Map compare as they
// compare strings.
// A package gets that object from CoreApi's id().
// The brand, which only core gives, keeps a record written by hand, which
// would be another object, from passing for one.
// Every function of core's takes an id as such an object, and gives one.
// Only JSON holds an id as a plain record: a JSON board (JsonBlock, below),
// R's reply to `hello`, the args of a command and the state of a plugin.
// What reads JSON makes an Id of each record, as the store does as a board
// opens, and so does a package that reads a plugin's state.
// The brand exists only for the type checker, so that one serializes as
// { "package": "blockr.core", "id": "head_block" }.
// Its tag @hidden keeps it out of the JSON Schema that a build makes from a
// type, so that no schema asks JSON for the brand.
export interface Id<P extends string | null = string> {
  readonly package: P;
  readonly id: string;
  /** @hidden */
  readonly [interned]: true;
}

// A plugin class's id: the package that lists the class in R, with the module
// whose default export the class is, and the class's id within the package,
// as { package: "blockr.dag", id: "dag_plugin" }.
// An app runs at most one plugin of each class, so the id of its class
// identifies a plugin.
export type PluginClassId = Id;

// A block class's id in the block registry: the package that defines the
// class, or null for one that an app script defines, and the id that
// `new_block_class()` takes, as { package: "blockr.core", id: "head_block" }.
export type RegistryId = Id<string | null>;

// A renderer's id: the package of its module, or null for one whose module an
// app script serves, and the name that `new_renderer()` takes, as
// { package: "blockr.core", id: "table_output" }.
export type RendererId = Id<string | null>;

// The id of a module that a package offers other packages' UIs, as core's
// controls: the package and the module's id within it, as
// { package: "blockr.core", id: "controls" }.
export type ModuleId = Id;

// A command's id: the package that registered it and its id within the
// package, as { package: "blockr.core", id: "remove_block" }.
export type CommandId = Id;

// A board action's id: the package of the plugin class that lists it, or
// blockr.core for core's own, and its id within the package, as
// { package: "blockr.core", id: "rename_action" }.
export type BoardActionId = Id;

// A board has no class of its own. What a package adds to a board, such as the
// dock's views, is the state of one of its plugins, under the package and the
// id of the plugin's class in `plugins`, which core carries without reading
// it.
export interface Board {
  readonly id: string;
  readonly name: string;
  readonly blocks: Readonly<Record<BlockId, Block>>;
  readonly links: Readonly<Record<LinkId, Link>>;
  readonly stacks: Readonly<Record<StackId, Stack>>;
  // Each board option's value by the board option's id, as { n_rows: 50 }.
  readonly options: Readonly<Record<OptionId, Json>>;
  // The state of each plugin whose class has `state`, by the package of the
  // plugin's class and then by the class's id within it, in the shape that the
  // class's schema gives, as { "blockr.dag": { dag_plugin: { positions: {} } } }.
  // The app says which plugins run, so the board holds nothing for a plugin
  // whose class keeps no state.
  readonly plugins: Readonly<Record<string, Readonly<Record<string, Json>>>>;
}

export interface Block {
  readonly registry_id: RegistryId;
  // Its values for the formals of its class's `expr`, as { n: 6 }.
  readonly state: Readonly<Record<string, Json>>;
  readonly name: string;
  // Its lock, which fixes its state where the board lock is on: true locks the
  // block, false leaves it open, and null follows the board lock, so that the
  // board lock locks the block. The store's locked() says which blocks a lock
  // fixes, so that no frontend reads null as open.
  readonly locked: boolean | null;
}

export interface Link {
  readonly from: BlockId;
  readonly to: BlockId;
  // The input of the target it feeds, empty for a positional link into a
  // variadic block.
  readonly input: string;
}

export interface Stack {
  readonly name: string;
  readonly blocks: readonly BlockId[];
  // The colour that frontends draw the stack in, a hex colour of six digits
  // such as "#66c2a5", or null for none.
  // A frontend draws a stack without a colour in a default of its own.
  // The pattern is the store's HEX_COLOR, by which the store checks a stack,
  // and a test holds the two to each other.
  /**
   * @pattern ^#[0-9a-fA-F]{6}$
   */
  readonly color: string | null;
}

// A JSON board, the one format of a board outside the store: the board, the
// version of the format, and the version of every package that the board's
// block classes, plugin classes and board options come from, which names what
// to install.
// The client saves a board as one, and R writes the opening board that an app
// script built as one, with every id.
// The board records the version of each shape that a definition outside core
// gives it, beside what it holds in that shape, and migrations key on those
// versions, while the format versions core's own.
export interface JsonBoard {
  readonly format: number;
  readonly packages: Readonly<Record<string, string>>;
  readonly board: Omit<Board, "blocks" | "options" | "plugins"> & {
    readonly blocks: Readonly<Record<BlockId, JsonBlock>>;
    readonly options: Readonly<Record<OptionId, JsonOption>>;
    // The state of each plugin by the package of its class, then by the id of
    // its class within that package, as
    // { "blockr.dag": { "dag_plugin": { state: { positions: {} }, version: 1 } } }.
    readonly plugins: Readonly<Record<string, Readonly<Record<string, JsonPlugin>>>>;
  };
}

// A block as a JSON board holds it, with the version of its class's state,
// which the class's entry in the block registry gives. It names its class by
// the class's package and the class's id within that package, as
// { package: "blockr.core", id: "head_block" }, with a package of null for a
// class that an app script defines.
export interface JsonBlock extends Omit<Block, "registry_id"> {
  readonly registry_id: { readonly package: string | null; readonly id: string };
  readonly version: number;
}

// A board option's value as a JSON board holds it, with the version of the
// value's shape, which R's `board_option_version()` gives, and the package
// that lists the board option, or null where no package lists it.
// A board option's id holds no package, unlike the id of a block's class or of
// a plugin's class, which holds the package beside the id within it.
// So the JSON board records the package here, and a board that opens without
// the package names it.
export interface JsonOption {
  readonly value: Json;
  readonly version: number;
  readonly package: string | null;
}

// A plugin's state as a JSON board holds it, under the package and the id of
// the plugin's class, with the version of the state's shape, which R's
// `plugin_version()` gives for the class.
export interface JsonPlugin {
  readonly state: Json;
  readonly version: number;
}
