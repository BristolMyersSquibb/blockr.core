// Applying a delta to a board: the new board, which shares with the old one
// whatever the delta did not change, and the inverse that undo applies, taken
// from the board the delta replaces, so that no package writes undo code.
// Setting what a value holds already changes nothing, so the inverse holds
// only what the delta changed.
// It holds no board option values, since undo leaves them as they are:
// a board option never changes what the board computes
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#which-board-options-a-board-holds).
// A delta that changes nothing has an empty inverse and sets or removes no
// board option value, and the store applies it as no step.

import type {
  Block,
  BlockId,
  Board,
  Link,
  LinkId,
  OptionId,
  PluginClassId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual, own, without } from "../util/index.ts";
import type { BlockMod, Delta, Patch, Values } from "./delta.ts";
import { applyPatch, malformed, segmentsFromPath } from "./patch.ts";

// Where each entry that an inverse adds back goes, before the entry named or
// last, by collection: "blocks", "links" or "stacks".
// Under "patches", where each member of an object that an inverse's patch adds
// back goes, by the patch's path.
export type Places = Readonly<
  Record<string, Readonly<Record<string, string | null>>>
>;

export interface Inverse {
  readonly delta: Delta;
  readonly places: Places;
}

// What a delta changed, for the checks that look at that alone, and for the
// edit ids of what it touched.
export interface Changed {
  // Added or changed, by id.
  readonly blocks: ReadonlySet<BlockId>;
  readonly links: ReadonlySet<LinkId>;
  readonly stacks: ReadonlySet<StackId>;
  // Set or removed, by id, which the inverse leaves out.
  readonly options: ReadonlySet<OptionId>;
  // The plugins whose state a patch changed, by the ids of their classes.
  readonly plugins: ReadonlySet<PluginClassId>;
  // What has an edit id that the delta added, changed or removed, as target()
  // names it.
  readonly targets: ReadonlySet<string>;
}

export interface Committed {
  readonly board: Board;
  readonly inverse: Inverse;
  readonly changed: Changed;
}

// What has an edit id: a block, or a plugin by the id of its class.
export type Target = { readonly block: BlockId } | { readonly plugin: PluginClassId };

export function target(of: Target): string {
  return "block" in of
    ? JSON.stringify(["block", of.block])
    : JSON.stringify(["plugin", of.plugin]);
}

export function commit(
  board: Board,
  delta: Delta,
  places: Places = {},
): Committed | { readonly failures: readonly string[] } {
  const failures: string[] = [];
  const targets = new Set<string>();

  const blocks = collection<Block, BlockMod>(
    (id) => `block "${id}"`,
    board.blocks,
    delta.blocks,
    (old, mod, id) => modifyBlock(old, mod, id, failures),
    places.blocks,
    undefined,
    failures,
  );
  for (const id of blocks.touched) targets.add(target({ block: id }));

  const links = collection<Link, Partial<Link>>(
    (id) => `link "${id}"`,
    board.links,
    delta.links,
    modifyFields,
    places.links,
    delta.links?.before,
    failures,
  );

  const stacks = collection<Stack, Partial<Stack>>(
    (id) => `stack "${id}"`,
    board.stacks,
    delta.stacks,
    modifyFields,
    places.stacks,
    undefined,
    failures,
  );

  const options = values(
    board.options,
    delta.options,
    (id) => `the board option "${id}"`,
    failures,
  );

  let next: Board = {
    ...board,
    name: delta.name ?? board.name,
    blocks: blocks.next,
    links: links.next,
    stacks: stacks.next,
    options: options.next,
  };

  const patched = patch(next, delta.patches ?? [], places.patches ?? {}, failures);
  for (const plugin of patched.plugins) targets.add(target({ plugin }));
  next = patched.board;

  if (failures.length > 0) return { failures };

  const inverse: Delta = {
    ...(empty(blocks.inverse) ? {} : { blocks: blocks.inverse }),
    ...(empty(links.inverse) ? {} : { links: links.inverse }),
    ...(empty(stacks.inverse) ? {} : { stacks: stacks.inverse }),
    ...(delta.name === undefined || delta.name === board.name ? {} : { name: board.name }),
    ...(patched.inverse.length === 0 ? {} : { patches: patched.inverse }),
  };

  return {
    board: next,
    inverse: {
      delta: inverse,
      places: {
        blocks: blocks.places,
        links: links.places,
        stacks: stacks.places,
        patches: patched.places,
      },
    },
    changed: {
      blocks: new Set(blocks.changed),
      links: new Set(links.changed),
      stacks: new Set(stacks.changed),
      options: options.changed,
      plugins: patched.plugins,
      targets,
    },
  };
}

// The board after the patches, applied in their order,
// with the patches that take them back, in the order that does,
// and the plugins they changed.
// An `add` of a member that `placed` gives a place for goes in that place, and
// each member that a patch removes leaves its place for the patch that adds
// it back.
// A patch changes the state of the board's plugins, never one of core's
// objects, and never adds or removes what the board holds for a plugin, since
// the app sets the plugins that run.
function patch(
  board: Board,
  patches: readonly Patch[],
  placed: Readonly<Record<string, string | null>>,
  failures: string[],
): {
  board: Board;
  inverse: Patch[];
  places: Readonly<Record<string, string | null>>;
  plugins: ReadonlySet<PluginClassId>;
} {
  let next: Readonly<Record<string, unknown>> = board as unknown as Readonly<Record<string, unknown>>;
  const inverse: Patch[] = [];
  const places: Record<string, string | null> = {};
  const plugins = new Set<PluginClassId>();

  for (const [i, given] of patches.entries()) {
    const name = `The patch ${String(i + 1)}`;
    const problems = malformed(given);
    if (problems.length > 0) {
      failures.push(`${name} ${problems.join(", ")}.`);
      continue;
    }
    const segments = segmentsFromPath(given.path) ?? [];
    const misplaced = outside(segments, given.op);
    if (misplaced !== undefined) {
      failures.push(`${name}, ${given.op} at "${given.path}", ${misplaced}.`);
      continue;
    }
    const place = given.op === "add" ? own(placed, given.path) : undefined;
    const applied = applyPatch(next, given, segments, place);
    if ("failure" in applied) {
      failures.push(`${name}, ${given.op} at "${given.path}", ${applied.failure}.`);
      continue;
    }
    next = applied.next;
    if (applied.inverse === undefined) continue;
    inverse.unshift(applied.inverse);
    if (applied.place !== undefined) places[applied.inverse.path] = applied.place;
    plugins.add(segments[1] ?? "");
  }

  return { board: next as unknown as Board, inverse, places, plugins };
}

// Why a patch at the path `segments` changes what no patch may, in a clause,
// or undefined where it changes a plugin's state, at or below
// `/plugins/{id}`, the id of the plugin's class.
// No patch adds or removes what the board holds for a plugin, as an `add` or a
// `remove` at `/plugins/{id}` would, since the app sets the plugins that run,
// while a `replace` there replaces the plugin's state as a whole.
function outside(segments: readonly string[], op: Patch["op"]): string | undefined {
  const [boardProperty, plugin] = segments;
  if (boardProperty !== "plugins") return `changes "${boardProperty ?? ""}", where a patch changes the state of the board's plugins alone`;
  if (plugin === undefined) return "changes the board's plugins as a whole, where a patch changes the state of one plugin";
  if (segments.length === 2 && op !== "replace") {
    return `${op === "add" ? "adds" : "removes"} what the board holds for the plugin "${plugin}", where a patch changes a plugin's state alone, since the app sets the plugins that run`;
  }
  return undefined;
}

interface Part<T, M> {
  readonly add?: Readonly<Record<string, T>>;
  readonly mod?: Readonly<Record<string, M>>;
  readonly rm?: readonly string[];
}

interface Collection<T, M> {
  readonly next: Readonly<Record<string, T>>;
  // The inverse of the part, with the moves that put back the entries that
  // `before` moved.
  readonly inverse: Part<T, M> & { readonly before?: Readonly<Record<string, string | null>> };
  readonly places: Readonly<Record<string, string | null>>;
  // Added or changed.
  readonly changed: readonly string[];
  // Added, changed or removed.
  readonly touched: readonly string[];
}

// A collection's part of a delta, applied with `rm` first, then the moves of
// the entries that `before` names and the collection holds, then `add`, and
// `mod` last.
// An entry that `places` or `before` names a successor for goes before it,
// and one that `before` names null for goes last, as does every other entry
// added. Where `before` names an entry that the delta removes, the entry takes
// its place.
// Each move keeps the entry that followed the moved entry, or null where it
// was last. The inverse moves each entry back before that one, the last move
// first. The inverse applies in the same order as a delta: its moves come
// after it removes what the delta added, and before it adds back what the
// delta removed. So every entry gets back its place.
// A move to where the entry is, and a `mod` that changes nothing, as its empty
// inverse says, leave the entry as it is.
function collection<T, M extends object>(
  name: (id: string) => string,
  current: Readonly<Record<string, T>>,
  part: Part<T, M> | undefined,
  modify: (old: T, mod: M, id: string) => { value: T; inverse: M },
  places: Readonly<Record<string, string | null>> | undefined,
  before: Readonly<Record<string, string | null>> | undefined,
  failures: string[],
): Collection<T, M> {
  if (part === undefined) {
    return { next: current, inverse: {}, places: {}, changed: [], touched: [] };
  }

  const order = Object.keys(current);
  const removed = new Set<string>();
  for (const id of part.rm ?? []) {
    if (Object.hasOwn(current, id)) removed.add(id);
    else failures.push(`The board holds no ${name(id)} to remove.`);
  }

  const readd: Record<string, T> = {};
  const readdPlaces: Record<string, string | null> = {};
  for (const id of removed) {
    readd[id] = current[id] as T;
    const after = order.slice(order.indexOf(id) + 1);
    readdPlaces[id] = after.find((other) => !removed.has(other)) ?? null;
  }

  const kept: [string, T][] = order
    .filter((id) => !removed.has(id))
    .map((id) => [id, current[id] as T]);

  // The entry that `before` puts an entry before: the one named, the one that
  // followed it where the delta removes it, or null for the last place.
  const successorOf = (named: string | null): string | null =>
    named === null ? null : removed.has(named) ? (own(readdPlaces, named) ?? null) : named;

  const moves: [string, string | null][] = [];
  for (const [id, named] of Object.entries(before ?? {})) {
    if (Object.hasOwn(part.add ?? {}, id)) continue;
    const at = kept.findIndex(([other]) => other === id);
    const entry = kept[at];
    if (entry === undefined) {
      failures.push(`The board holds no ${name(id)} to move.`);
      continue;
    }
    const to = successorOf(named);
    const was = kept[at + 1]?.[0] ?? null;
    if (to === was || to === id) continue;
    if (to !== null && !kept.some(([other]) => other === to)) {
      failures.push(`The board holds no ${name(to)} for ${name(id)} to go before.`);
      continue;
    }
    kept.splice(at, 1);
    if (to === null) kept.push(entry);
    else kept.splice(kept.findIndex(([other]) => other === to), 0, entry);
    moves.unshift([id, was]);
  }

  const added: string[] = [];
  for (const [id, value] of Object.entries(part.add ?? {})) {
    if (kept.some(([other]) => other === id)) {
      failures.push(`The board holds ${name(id)} already.`);
      continue;
    }
    const named = own(before ?? {}, id);
    const successor =
      named === undefined ? (own(places ?? {}, id) ?? null) : successorOf(named);
    const at =
      successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && named !== undefined) {
      failures.push(
        `The board holds no ${name(successor)} for ${name(id)} to go before.`,
      );
      continue;
    }
    if (at < 0) kept.push([id, value]);
    else kept.splice(at, 0, [id, value]);
    added.push(id);
  }

  const remod: Record<string, M> = {};
  const modded: string[] = [];
  for (const [id, mod] of Object.entries(part.mod ?? {})) {
    const at = kept.findIndex(([other]) => other === id);
    const entry = kept[at];
    if (entry === undefined) {
      failures.push(`The board holds no ${name(id)} to change.`);
      continue;
    }
    const { value, inverse } = modify(entry[1], mod, id);
    if (Object.keys(inverse).length === 0) continue;
    kept[at] = [id, value];
    if (!added.includes(id)) remod[id] = inverse;
    modded.push(id);
  }

  return {
    next: Object.fromEntries(kept),
    inverse: {
      ...(added.length === 0 ? {} : { rm: added }),
      ...(removed.size === 0 ? {} : { add: readd }),
      ...(Object.keys(remod).length === 0 ? {} : { mod: remod }),
      ...(moves.length === 0 ? {} : { before: Object.fromEntries(moves) }),
    },
    places: readdPlaces,
    changed: [...new Set([...added, ...modded])],
    touched: [...new Set([...removed, ...added, ...modded])],
  };
}

function empty(part: Part<unknown, unknown>): boolean {
  return Object.keys(part).length === 0;
}

// What setting and unsetting values by name changes in a record: the values
// that differ from those it holds, and the names it holds of those to unset,
// with what sets them back, the old values and the names that held none. The
// record stays the same object where nothing changes, and setting and unsetting
// one name fails.
interface Changes {
  readonly next: Readonly<Record<string, Json>>;
  readonly set: readonly string[];
  readonly unset: readonly string[];
  readonly restore: Readonly<Record<string, Json>>;
  readonly remove: readonly string[];
}

function changes(
  current: Readonly<Record<string, Json>>,
  set: Readonly<Record<string, Json>>,
  unset: readonly string[],
  both: (name: string) => string,
  failures: string[],
): Changes {
  const setting = Object.entries(set).filter(
    ([name, value]) => !jsonEqual(own(current, name), value),
  );
  const restore: Record<string, Json> = {};
  const remove: string[] = [];
  const removing: string[] = [];

  for (const name of unset) {
    if (Object.hasOwn(set, name)) failures.push(both(name));
    const was = own(current, name);
    if (was === undefined) continue;
    removing.push(name);
    restore[name] = was;
  }
  for (const [name] of setting) {
    const was = own(current, name);
    if (was === undefined) remove.push(name);
    else restore[name] = was;
  }

  return {
    next:
      setting.length === 0 && removing.length === 0
        ? current
        : { ...without(current, removing), ...Object.fromEntries(setting) },
    set: setting.map(([name]) => name),
    unset: removing,
    restore,
    remove,
  };
}

// Values by name that a delta sets and removes, with the names of those it
// changes: each that it sets to another value, and each that it removes where
// the record holds one.
function values(
  current: Readonly<Record<string, Json>>,
  change: Values<string> | undefined,
  what: (name: string) => string,
  failures: string[],
): {
  next: Readonly<Record<string, Json>>;
  changed: ReadonlySet<string>;
} {
  if (change === undefined) {
    return { next: current, changed: new Set() };
  }
  const result = changes(
    current,
    change.set ?? {},
    change.unset ?? [],
    (name) => `The change both sets and unsets ${what(name)}.`,
    failures,
  );

  return {
    next: result.next,
    changed: new Set([...result.set, ...result.unset]),
  };
}

function modifyBlock(
  old: Block,
  mod: BlockMod,
  id: BlockId,
  failures: string[],
): { value: Block; inverse: BlockMod } {
  const state = changes(
    old.state,
    mod.state ?? {},
    mod.unset ?? [],
    (arg) => `The change to block "${id}" both sets and unsets "${arg}".`,
    failures,
  );
  const name = mod.name === old.name ? undefined : mod.name;
  // A lock of null is a value, which follows the board lock.
  const locked = mod.locked === undefined || mod.locked === old.locked ? undefined : mod.locked;

  return {
    value: {
      ...old,
      state: state.next,
      name: name ?? old.name,
      locked: locked === undefined ? old.locked : locked,
    },
    inverse: {
      ...(Object.keys(state.restore).length === 0 ? {} : { state: state.restore }),
      ...(state.remove.length === 0 ? {} : { unset: state.remove }),
      ...(name === undefined ? {} : { name: old.name }),
      ...(locked === undefined ? {} : { locked: old.locked }),
    },
  };
}

// A link's or a stack's change, which sets the properties it names.
function modifyFields<T extends object>(
  old: T,
  mod: Partial<T>,
): { value: T; inverse: Partial<T> } {
  const inverse: Partial<T> = {};
  const changed: Partial<T> = {};
  for (const field of Object.keys(mod) as (keyof T)[]) {
    if (jsonEqual(old[field], mod[field])) continue;
    inverse[field] = old[field];
    changed[field] = mod[field];
  }
  return { value: { ...old, ...changed }, inverse };
}
