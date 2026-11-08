// Deltas, the one shape in which every change to the board travels,
// whoever makes it: core's objects, the board options and the board's
// name in the `add`, `mod` and `rm` shape of today's board updates in R,
// and the board's plugins by patches at nested paths.

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

// Who made a change: "user" unless its maker names another, such as a
// plugin like the assistant, or a peer once a package syncs boards between
// people. Each origin undoes its own steps.
export type Origin = string;

// Within each collection, `rm` applies before `add`,
// since a delta may reuse an id it drops, and `mod` applies last.
// The patches apply in their order, after core's parts.
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
    // variadic target takes its inputs in link order. A link that the delta
    // removes leaves its place to the one named to go before it.
    readonly before?: Readonly<Record<LinkId, LinkId>>;
  };
  readonly stacks?: {
    readonly add?: Readonly<Record<StackId, Stack>>;
    readonly mod?: Readonly<Record<StackId, Partial<Stack>>>;
    readonly rm?: readonly StackId[];
  };
  readonly options?: Values<OptionId>;
  readonly name?: string;
  readonly patches?: readonly Patch[];
}

// Whether a step is given as several deltas rather than one. The store's
// apply() and a command's run() take either.
export function isDeltas(delta: Delta | readonly Delta[]): delta is readonly Delta[] {
  return Array.isArray(delta);
}

// Values by name that a delta sets, and names whose values it removes, as a
// collection entry's change sets and removes its fields: board option values
// by id. Undo removes a value where its step set one that had none.
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
}

// A change to the board's plugins, in the shape of a JSON Patch operation
// (RFC 6902): its path is a JSON Pointer from the board's top, under
// "/plugins", whose second part names the plugin.
// An `add` or a `replace` at the plugin's own path, as "/plugins/dag", sets the
// whole plugin, and a `remove` there removes it, while a patch below that path
// changes the plugin's name or its state, as "/plugins/dag/state/positions/a"
// moves a block in the DAG.
// A patch names the edit id of the plugin that it was built on,
// which a command leaves out and the command registry fills in.
export interface Patch {
  readonly op: "add" | "remove" | "replace";
  readonly path: string;
  // What an `add` or a `replace` puts at the path.
  readonly value?: Json;
  readonly edit?: number;
}
