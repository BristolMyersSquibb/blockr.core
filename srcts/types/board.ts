// The board the client holds: plain data, which serializes as it is and names
// the front-end it is made for and each block's class by id, so that no object
// in it carries an R class.

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
  id: string;
  front_end: FrontEnd;
  name: string;
  blocks: Record<BlockId, Block>;
  links: Record<LinkId, Link>;
  stacks: Record<StackId, Stack>;
  // Each board option's value by the board option's id, as { n_rows: 50 }.
  options: Record<OptionId, Json>;
}

export interface FrontEnd {
  // The id of the front-end the board is made for, such as "blockr.dock-dock".
  id: string;
  // The keys that front-ends add to FrontEndState, and those of packages whose
  // types a module does not import.
  state: Partial<FrontEndState> & Record<string, unknown>;
  attrs: Record<string, Json>;
}

export interface Block {
  registry_id: RegistryId;
  // Its values for the formals of its class's `expr`, as { n: 6 }.
  state: Record<string, Json>;
  name: string;
  // Its attributes, values that are not its state, as { visible: ["outputs"] }.
  attrs: Record<string, Json>;
}

export interface Link {
  from: BlockId;
  to: BlockId;
  // The input of the target it feeds, empty for a positional link into a
  // variadic block.
  input: string;
}

export interface Stack {
  name: string;
  blocks: BlockId[];
}

// A saved board, the file that only the client writes: the board, the version
// of the file format, and the version of every package that the board's block
// classes, its front-end and its front-end's state come from.
export interface SavedBoard {
  format: number;
  packages: Record<string, string>;
  board: Board;
}

// The opening board that an app script built, as R sends it. R mints no ids, so
// it holds the blocks, links and stacks as arrays, in their order, each with
// its id where the script gave one, and the board's own id and name where it
// has them. The client keys the collections and mints each missing id as it
// opens the board.
export interface OpeningBoard {
  id?: string;
  front_end: FrontEnd;
  name?: string;
  blocks: (Block & { id?: BlockId })[];
  links: (Link & { id?: LinkId })[];
  stacks: (Stack & { id?: StackId })[];
  options: Record<OptionId, Json>;
}
