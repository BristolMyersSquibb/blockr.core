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
export type PluginId = string;

// A plugin class's id, `{pkg}-{name}_plugin`, such as "blockr.dag-dag_plugin",
// which names the package that registers the class from its startup module.
export type PluginClassId = string;

// A block class's id in the block registry, `{pkg}-{name}`, such as
// "blockr.core-head_block".
export type RegistryId = string;

// A board has no class of its own. What a package adds to a board, such as the
// dock's views, is the state of one of its plugins, under the plugin's id in
// `plugins`, which core carries without reading it.
export interface Board {
  readonly id: string;
  readonly name: string;
  readonly blocks: Readonly<Record<BlockId, Block>>;
  readonly links: Readonly<Record<LinkId, Link>>;
  readonly stacks: Readonly<Record<StackId, Stack>>;
  // Each board option's value by the board option's id, as { n_rows: 50 }.
  readonly options: Readonly<Record<OptionId, Json>>;
  // Each plugin by its id, as { dag: { class: "blockr.dag-dag_plugin", ... } }.
  readonly plugins: Readonly<Record<PluginId, Plugin>>;
}

export interface Block {
  readonly registry_id: RegistryId;
  // Its values for the formals of its class's `expr`, as { n: 6 }.
  readonly state: Readonly<Record<string, Json>>;
  readonly name: string;
}

// A plugin, which names its class by the class's id and keeps its state in the
// shape that its class's schema gives, such as the DAG's positions.
// It is declared with `type`, so that it is assignable to Json, as the value
// of a patch that adds a plugin is.
// eslint-disable-next-line @typescript-eslint/consistent-type-definitions -- an interface would not be assignable to Json
export type Plugin = {
  readonly class: PluginClassId;
  // What the user calls it, which titles its panel, as "Notes".
  readonly name: string;
  readonly state: Json;
};

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
  // The colour that front-ends draw the stack in, a hex colour of six digits
  // such as "#66c2a5", or null for none.
  // A front-end draws a stack without a colour in a default of its own.
  // The pattern is the store's HEX_COLOR, by which the store checks a stack,
  // and a test holds the two to each other.
  /**
   * @pattern ^#[0-9a-fA-F]{6}$
   */
  readonly color: string | null;
}

// A JSON board, the one format of a board outside the store: the board, the
// version of the format, and the version of every package that the board's
// block classes and plugin classes come from, which names what to install.
// The client saves a board as one, and R writes the opening board that an app
// script built as one, with every id.
// The board records the version of each shape that a definition outside core
// gives it, which migrations key on, while the format versions core's own.
export interface JsonBoard {
  readonly format: number;
  readonly packages: Readonly<Record<string, string>>;
  readonly board: Omit<Board, "blocks" | "options"> & {
    // The version of the state of each plugin class whose plugins the board
    // holds, by the class's id, as R's `plugin_version()` gives it.
    readonly versions: Readonly<Record<PluginClassId, number>>;
    readonly blocks: Readonly<Record<BlockId, JsonBlock>>;
    readonly options: Readonly<Record<OptionId, JsonOption>>;
  };
}

// A block as a JSON board holds it, with the version of its class's state,
// which the class's entry in the block registry gives.
export interface JsonBlock extends Block {
  readonly version: number;
}

// A board option's value as a JSON board holds it, with the version of the
// value's shape, which R's `board_option_version()` gives.
export interface JsonOption {
  readonly value: Json;
  readonly version: number;
}
