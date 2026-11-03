// Deltas, the one shape in which every change to the board travels, whoever
// makes it: core's objects, the board options, the board's name and the
// front-end's state and attributes alike, in the `add`, `mod` and `rm` shape of
// today's board updates in R.

import type {
  Block,
  BlockId,
  Link,
  LinkId,
  OptionId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json } from "../types/json.ts";

// Who made a change: "user" unless its maker names another, such as an
// extension like the assistant, or a peer once a sync add-on exists. Each
// origin undoes its own steps.
export type Origin = string;

// Within each collection, `rm` applies before `add`, since a delta may reuse an
// id it drops, and `mod` applies last.
export interface Delta {
  readonly blocks?: {
    readonly add?: Readonly<Record<BlockId, Block>>;
    readonly mod?: Readonly<Record<BlockId, BlockMod>>;
    readonly rm?: readonly BlockId[];
  };
  readonly links?: {
    readonly add?: Readonly<Record<LinkId, Link>>;
    readonly mod?: Readonly<Record<LinkId, Partial<Link>>>;
    readonly rm?: readonly LinkId[];
    // Where an added link goes among the links, before the one named, as a
    // variadic target takes its inputs in link order.
    readonly before?: Readonly<Record<LinkId, LinkId>>;
  };
  readonly stacks?: {
    readonly add?: Readonly<Record<StackId, Stack>>;
    readonly mod?: Readonly<Record<StackId, Partial<Stack>>>;
    readonly rm?: readonly StackId[];
  };
  readonly options?: Values<OptionId>;
  readonly name?: string;
  readonly front_end?: {
    readonly state?: readonly KeyChange[];
    readonly attrs?: Values<string>;
  };
}

// Values by name that a delta sets, and names whose values it removes, as a
// collection entry's change sets and removes its fields: board option values
// by id, and attributes by name. Undo removes a value where its step set one
// that had none.
export interface Values<K extends string> {
  readonly set?: Readonly<Record<K, Json>>;
  readonly unset?: readonly K[];
}

export interface BlockMod {
  // The block's edit id that the change was built on, which a command leaves
  // out and the command registry fills in.
  readonly edit?: number;
  readonly state?: Readonly<Record<string, Json>>;
  // State args to remove, since an arg without a default starts unset and a
  // null would reach `expr` as NULL.
  readonly unset?: readonly string[];
  readonly name?: string;
  readonly attrs?: Values<string>;
}

// A change to the key of the front-end's state that is registered at the path.
export interface KeyChange {
  readonly path: readonly string[];
  readonly change: KeyDelta;
}

// An entry of a collection is an object.
export type Entry = Readonly<Record<string, Json>>;

export type KeyDelta = CollectionDelta | ValueDelta;

// A collection's change, by entry id. A `mod` gives an entry the keys its `set`
// names and removes those its `unset` names.
export interface CollectionDelta {
  readonly add?: Readonly<Record<string, Entry>>;
  readonly mod?: Readonly<
    Record<
      string,
      {
        readonly edit?: number;
        readonly set?: Entry;
        readonly unset?: readonly string[];
      }
    >
  >;
  readonly rm?: readonly string[];
}

// Whether a change to front-end state is a value's, rather than a collection's.
export function isValueDelta(change: KeyDelta): change is ValueDelta {
  return Object.hasOwn(change, "set") || Object.hasOwn(change, "unset");
}

// A value's change, which sets the value or removes it.
export type ValueDelta =
  | { readonly edit?: number; readonly set: Json; readonly unset?: never }
  | { readonly edit?: number; readonly unset: true; readonly set?: never };
