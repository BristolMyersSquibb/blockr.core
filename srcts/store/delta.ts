// Deltas, the one shape in which every change to the board travels,
// whoever makes it: core's objects, the board options and the board's
// name in the `add`, `mod` and `rm` shape of today's board updates in R,
// and the state of the board's plugins by patches at nested paths.

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
// people.
// The store keeps the steps of every origin in one undo history, so that the
// user's Undo takes back the assistant's edits too.
// An undo given an origin takes back that origin's last step (./store.ts).
export type Origin = string;

// Within each collection, `rm` applies before `add`,
// since a delta may reuse an id it drops, and `mod` applies last.
// The links that `before` moves move between the two, after `rm` and before
// `add`, so that the inverse of a delta, which applies in the same order,
// puts every link back in its place.
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
    // Where a link goes among the links, as a variadic target takes its
    // inputs in link order: before the link named, or last for null.
    // An added link goes there as the delta adds it.
    // A link that the board holds moves there, keeping its id and its ends,
    // in the order that `before` names the links.
    // A link that the delta removes leaves its place to the one named to go
    // before it.
    readonly before?: Readonly<Record<LinkId, LinkId | null>>;
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
// collection entry's change sets and removes its properties: board option
// values by id.
// Undo and redo leave them as they are (./commit.ts).
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
  // The block's lock: true, false or null, which follows the board lock.
  readonly locked?: boolean | null;
}

// A change to the state of the board's plugins, in the shape of a JSON Patch
// operation (RFC 6902): its path is a JSON Pointer from the board's root, under
// "/plugins", whose second segment names the plugin by the id of its class.
// A patch at or below the plugin's own path changes the plugin's state, as
// "/plugins/blockr.dag-dag_plugin/positions/a" moves a block in the DAG, and
// none adds or removes what the board holds for a plugin, since the app sets
// the plugins that run.
// A patch names the edit id of the plugin that it was built on,
// which a command leaves out and the command registry fills in.
export interface Patch {
  readonly op: "add" | "remove" | "replace";
  readonly path: string;
  // What an `add` or a `replace` puts at the path.
  readonly value?: Json;
  readonly edit?: number;
}
