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
  // Its attributes, values that are not its state, as { visible: ["outputs"] }.
  readonly attrs: Readonly<Record<string, Json>>;
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

// A saved board, the file that only the client writes: the board,
// the version of the file format, and the version of every package that the
// board's block classes and the properties of its class come from.
export interface SavedBoard {
  readonly format: number;
  readonly packages: Readonly<Record<string, string>>;
  readonly board: Board;
}

// The opening board that an app script built, as R sends it.
// R mints no ids, so it holds the blocks, links and stacks as arrays,
// in their order, each with its id where the script gave one,
// and the board's own id and name where it has them.
// The client keys the collections and mints each missing id as it opens
// the board.
// Beside core's properties, it holds those of the board's class that the
// script gives.
export interface OpeningBoard {
  readonly id?: string;
  readonly class: readonly string[];
  readonly name?: string;
  readonly blocks: readonly (Block & { readonly id?: BlockId })[];
  readonly links: readonly (Link & { readonly id?: LinkId })[];
  readonly stacks: readonly (Stack & { readonly id?: StackId })[];
  readonly options: Readonly<Record<OptionId, Json>>;
}
