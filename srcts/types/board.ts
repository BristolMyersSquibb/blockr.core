// The board the client holds: plain data, which serializes as it is and names
// the front-end it is made for and each block's class by id, so that no object
// in it carries an R class. Its types are read-only, so that nothing changes a
// board in place: the store makes a new board for every change, sharing what
// did not change, as copy-on-modify did in R.

import type { Json } from "./json.ts";

export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;

// A block class's id in the block registry, `{pkg}-{name}`, such as
// "blockr.core-head_block".
export type RegistryId = string;

// What the board's front-end and its extensions keep, which is empty in core: a
// front-end types its keys by adding them to this interface, as the dock adds
// its views.
// eslint-disable-next-line @typescript-eslint/no-empty-object-type
export interface FrontEndState {}

export interface Board {
  readonly id: string;
  readonly front_end: FrontEnd;
  readonly name: string;
  readonly blocks: Readonly<Record<BlockId, Block>>;
  readonly links: Readonly<Record<LinkId, Link>>;
  readonly stacks: Readonly<Record<StackId, Stack>>;
  // Each board option's value by the board option's id, as { n_rows: 50 }.
  readonly options: Readonly<Record<OptionId, Json>>;
}

export interface FrontEnd {
  // The id of the front-end the board is made for, such as "blockr.dock-dock".
  readonly id: string;
  // The keys that front-ends add to FrontEndState, and those of packages whose
  // types a module does not import.
  readonly state: Readonly<Partial<FrontEndState> & Record<string, unknown>>;
  readonly attrs: Readonly<Record<string, Json>>;
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

// A saved board, the file that only the client writes: the board, the version
// of the file format, and the version of every package that the board's block
// classes, its front-end and its front-end's state come from.
export interface SavedBoard {
  readonly format: number;
  readonly packages: Readonly<Record<string, string>>;
  readonly board: Board;
}

// The opening board that an app script built, as R sends it. R mints no ids, so
// it holds the blocks, links and stacks as arrays, in their order, each with
// its id where the script gave one, and the board's own id and name where it
// has them. The client keys the collections and mints each missing id as it
// opens the board.
export interface OpeningBoard {
  readonly id?: string;
  readonly front_end: FrontEnd;
  readonly name?: string;
  readonly blocks: readonly (Block & { readonly id?: BlockId })[];
  readonly links: readonly (Link & { readonly id?: LinkId })[];
  readonly stacks: readonly (Stack & { readonly id?: StackId })[];
  readonly options: Readonly<Record<OptionId, Json>>;
}
