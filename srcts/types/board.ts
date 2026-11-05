// The board the client holds: plain data, which serializes as it is and names
// its own class by the R board's class vector and each block's class by id,
// so that no other object in it carries an R class.
// Its types are read-only, so that nothing changes a board in place:
// the store makes a new board for every change, sharing what did not change,
// as copy-on-modify did in R.

import type { Json } from "./json.ts";

export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;

// A block class's id in the block registry, `{pkg}-{name}`, such as
// "blockr.core-head_block".
export type RegistryId = string;

// Besides core's properties, below, a board holds those that its board class
// adds, such as a dock board's views, which core carries without reading them.
// A board class types its properties by extending this interface,
// as `interface DockBoard extends Board { views: ... }`.
// The board declares no index signature, so neither its members nor those types
// need be assignable to Json.
// Code that does not import a class's types reads a property as unknown,
// from the board taken as a record.
export interface Board {
  readonly id: string;
  // The class vector of the R board it comes from, most specific first,
  // as ["dock_board", "board"], which the client keeps and saves unchanged.
  // The client builds the board as the first class in it that a loaded
  // package defines.
  readonly class: readonly string[];
  readonly name: string;
  readonly blocks: Readonly<Record<BlockId, Block>>;
  readonly links: Readonly<Record<LinkId, Link>>;
  readonly stacks: Readonly<Record<StackId, Stack>>;
  // Each board option's value by the board option's id, as { n_rows: 50 }.
  readonly options: Readonly<Record<OptionId, Json>>;
}

export interface Block {
  readonly registry_id: RegistryId;
  // Its values for the formals of its class's `expr`, as { n: 6 }.
  readonly state: Readonly<Record<string, Json>>;
  readonly name: string;
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
}

// A JSON board, the one format of a board outside the store: the board, the
// version of the format, and the version of every package that the board's
// block classes and board classes come from, which names what to install.
// The client saves a board as one, and R writes the opening board that an app
// script built as one, with every id.
// The board records the version of each shape that a definition outside core
// gives it, which migrations key on, while the format versions core's own.
export interface JsonBoard {
  readonly format: number;
  readonly packages: Readonly<Record<string, string>>;
  readonly board: Omit<Board, "blocks" | "options"> & {
    // The version of each class in the board's class vector that R names one
    // for, by class, as R's `board_version()` gives it: the version of the
    // properties that the class adds.
    readonly versions: Readonly<Record<string, number>>;
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
