// Applying a delta to a board: the new board, which shares with the old one
// whatever the delta did not change, and the inverse that undo applies, taken
// from the board the delta replaces, so that no package writes undo code.

import type {
  Block,
  BlockId,
  Board,
  Link,
  LinkId,
  OptionId,
  Stack,
  StackId,
} from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type {
  BlockMod,
  CollectionDelta,
  Delta,
  Entry,
  KeyChange,
} from "./delta.ts";
import type { Registrations } from "./registrations.ts";
import { getAt, isObject, own, pathText, setAt, without } from "./util.ts";

// Where each entry that an inverse adds back goes, before the entry named or
// last, by collection: "blocks", "links", "stacks", or a key's path as JSON.
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
  // Set rather than removed, by id or name.
  readonly options: ReadonlySet<OptionId>;
  readonly attrs: ReadonlySet<string>;
  // The entries added or changed in each collection, and the values set, by
  // the key's path as JSON.
  readonly entries: ReadonlyMap<string, ReadonlySet<string>>;
  readonly values: ReadonlySet<string>;
  // What has an edit id that the delta added, changed or removed, as target()
  // names it.
  readonly targets: ReadonlySet<string>;
}

export interface Committed {
  readonly board: Board;
  readonly inverse: Inverse;
  readonly changed: Changed;
}

// What has an edit id: a block, an entry of a collection of the front-end's
// state, or a value of it.
export type Target =
  | { readonly block: BlockId }
  | { readonly path: readonly string[]; readonly entry?: string };

export function target(of: Target): string {
  if ("block" in of) return JSON.stringify(["block", of.block]);
  return of.entry === undefined
    ? JSON.stringify(["value", of.path])
    : JSON.stringify(["entry", of.path, of.entry]);
}

export function commit(
  board: Board,
  delta: Delta,
  registrations: Registrations,
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

  const options = values(board.options, delta.options);
  const attrs = values(board.front_end.attrs, delta.front_end?.attrs);

  let state = board.front_end.state;
  const stateInverse: KeyChange[] = [];
  const statePlaces: Record<string, Readonly<Record<string, string | null>>> =
    {};
  const entries = new Map<string, Set<string>>();
  const setValues = new Set<string>();

  for (const { path, change } of delta.front_end?.state ?? []) {
    const key = registrations.key(path);
    const at = JSON.stringify(path);
    if (key === undefined) {
      failures.push(
        `No key of the front-end's state is registered at ${pathText(path)}.`,
      );
      continue;
    }

    if (key.def.kind === "value") {
      if (!Object.hasOwn(change, "set")) {
        failures.push(`The change to ${pathText(path)} sets no value.`);
        continue;
      }
      const old = getAt(state, path) as Json | undefined;
      const value = (change as { set: Json | undefined }).set;
      state = setAt(state, path, value);
      stateInverse.unshift({ path, change: { set: old } });
      targets.add(target({ path }));
      if (value !== undefined) setValues.add(at);
      continue;
    }

    if (Object.hasOwn(change, "set")) {
      failures.push(
        `The change to ${pathText(path)} sets a value, where it holds a collection.`,
      );
      continue;
    }
    const current = getAt(state, path) ?? {};
    if (!isObject(current)) {
      failures.push(`The key ${pathText(path)} holds no collection.`);
      continue;
    }
    const result = collection<Entry, NonNullable<CollectionDelta["mod"]>[string]>(
      (id) => `entry "${id}" of ${pathText(path)}`,
      current as Readonly<Record<string, Entry>>,
      change as CollectionDelta,
      (old, mod, id) => modifyEntry(old, mod, id, path, failures),
      places[at],
      undefined,
      failures,
    );
    state = setAt(state, path, result.next);
    stateInverse.unshift({ path, change: result.inverse });
    statePlaces[at] = { ...statePlaces[at], ...result.places };
    for (const id of result.touched) targets.add(target({ path, entry: id }));
    entries.set(at, new Set([...(entries.get(at) ?? []), ...result.changed]));
  }

  if (failures.length > 0) return { failures };

  const name = delta.name ?? board.name;

  const next: Board = {
    ...board,
    name,
    blocks: blocks.next,
    links: links.next,
    stacks: stacks.next,
    options: options.next,
    front_end:
      attrs.next === board.front_end.attrs && state === board.front_end.state
        ? board.front_end
        : { ...board.front_end, state, attrs: attrs.next },
  };

  const inverse: Delta = {
    ...(empty(blocks.inverse) ? {} : { blocks: blocks.inverse }),
    ...(empty(links.inverse) ? {} : { links: links.inverse }),
    ...(empty(stacks.inverse) ? {} : { stacks: stacks.inverse }),
    ...(options.inverse === undefined ? {} : { options: options.inverse }),
    ...(delta.name === undefined ? {} : { name: board.name }),
    ...(attrs.inverse === undefined && stateInverse.length === 0
      ? {}
      : {
          front_end: {
            ...(stateInverse.length === 0 ? {} : { state: stateInverse }),
            ...(attrs.inverse === undefined ? {} : { attrs: attrs.inverse }),
          },
        }),
  };

  return {
    board: next,
    inverse: {
      delta: inverse,
      places: {
        blocks: blocks.places,
        links: links.places,
        stacks: stacks.places,
        ...statePlaces,
      },
    },
    changed: {
      blocks: new Set(blocks.changed),
      links: new Set(links.changed),
      stacks: new Set(stacks.changed),
      options: options.set,
      attrs: attrs.set,
      entries,
      values: setValues,
      targets,
    },
  };
}

interface Part<T, M> {
  readonly add?: Readonly<Record<string, T>>;
  readonly mod?: Readonly<Record<string, M>>;
  readonly rm?: readonly string[];
}

interface Collection<T, M> {
  readonly next: Readonly<Record<string, T>>;
  readonly inverse: Part<T, M>;
  readonly places: Readonly<Record<string, string | null>>;
  // Added or changed.
  readonly changed: readonly string[];
  // Added, changed or removed.
  readonly touched: readonly string[];
}

// A collection's part of a delta, applied with `rm` before `add` and `mod`
// last. An entry that `places` or `before` names a successor for goes before
// it, and every other entry added goes last.
function collection<T, M>(
  name: (id: string) => string,
  current: Readonly<Record<string, T>>,
  part: Part<T, M> | undefined,
  modify: (old: T, mod: M, id: string) => { value: T; inverse: M },
  places: Readonly<Record<string, string | null>> | undefined,
  before: Readonly<Record<string, string>> | undefined,
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

  const added: string[] = [];
  for (const [id, value] of Object.entries(part.add ?? {})) {
    if (kept.some(([other]) => other === id)) {
      failures.push(`The board holds ${name(id)} already.`);
      continue;
    }
    const successor = own(before ?? {}, id) ?? own(places ?? {}, id) ?? null;
    const at =
      successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && own(before ?? {}, id) !== undefined) {
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
    },
    places: readdPlaces,
    changed: [...new Set([...added, ...modded])],
    touched: [...new Set([...removed, ...added, ...modded])],
  };
}

function empty(part: Part<unknown, unknown>): boolean {
  return Object.keys(part).length === 0;
}

// Values by id that a delta sets, or removes where it gives undefined.
function values(
  current: Readonly<Record<string, Json>>,
  change: Readonly<Record<string, Json | undefined>> | undefined,
): {
  next: Readonly<Record<string, Json>>;
  inverse: Readonly<Record<string, Json | undefined>> | undefined;
  set: ReadonlySet<string>;
} {
  if (change === undefined) {
    return { next: current, inverse: undefined, set: new Set() };
  }
  let next = current;
  const inverse: Record<string, Json | undefined> = {};
  const set = new Set<string>();
  for (const [id, value] of Object.entries(change)) {
    inverse[id] = own(current, id);
    if (value === undefined) {
      next = without(next, [id]);
    } else {
      next = { ...next, [id]: value };
      set.add(id);
    }
  }
  return { next, inverse, set };
}

function modifyBlock(
  old: Block,
  mod: BlockMod,
  id: BlockId,
  failures: string[],
): { value: Block; inverse: BlockMod } {
  const set = mod.state ?? {};
  const unset = mod.unset ?? [];
  for (const arg of unset) {
    if (Object.hasOwn(set, arg)) {
      failures.push(`The change to block "${id}" both sets and unsets "${arg}".`);
    }
  }

  const state: Record<string, Json> = {};
  const restore: string[] = [];
  for (const arg of Object.keys(set)) {
    const was = own(old.state, arg);
    if (was === undefined) restore.push(arg);
    else state[arg] = was;
  }
  for (const arg of unset) {
    const was = own(old.state, arg);
    if (was !== undefined) state[arg] = was;
  }

  const attrs = values(old.attrs, mod.attrs);

  return {
    value: {
      ...old,
      state:
        mod.state === undefined && mod.unset === undefined
          ? old.state
          : { ...without(old.state, unset), ...set },
      name: mod.name ?? old.name,
      attrs: attrs.next,
    },
    inverse: {
      ...(Object.keys(state).length === 0 ? {} : { state }),
      ...(restore.length === 0 ? {} : { unset: restore }),
      ...(mod.name === undefined ? {} : { name: old.name }),
      ...(attrs.inverse === undefined ? {} : { attrs: attrs.inverse }),
    },
  };
}

// A link's or a stack's change, which sets the fields it names.
function modifyFields<T extends object>(
  old: T,
  mod: Partial<T>,
): { value: T; inverse: Partial<T> } {
  const inverse: Partial<T> = {};
  for (const field of Object.keys(mod) as (keyof T)[]) {
    inverse[field] = old[field];
  }
  return { value: { ...old, ...mod }, inverse };
}

function modifyEntry(
  old: Entry,
  mod: NonNullable<CollectionDelta["mod"]>[string],
  id: string,
  path: readonly string[],
  failures: string[],
): { value: Entry; inverse: NonNullable<CollectionDelta["mod"]>[string] } {
  const set = mod.set ?? {};
  const unset = mod.unset ?? [];
  for (const key of unset) {
    if (Object.hasOwn(set, key)) {
      failures.push(
        `The change to entry "${id}" of ${pathText(path)} both sets and unsets "${key}".`,
      );
    }
  }

  const restore: Record<string, Json> = {};
  const remove: string[] = [];
  for (const key of Object.keys(set)) {
    const was = own(old, key);
    if (was === undefined) remove.push(key);
    else restore[key] = was;
  }
  for (const key of unset) {
    const was = own(old, key);
    if (was !== undefined) restore[key] = was;
  }

  return {
    value: { ...without(old, unset), ...set },
    inverse: {
      ...(Object.keys(restore).length === 0 ? {} : { set: restore }),
      ...(remove.length === 0 ? {} : { unset: remove }),
    },
  };
}
