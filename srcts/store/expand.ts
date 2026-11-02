// Expanding a delta with what follows from it, so that it reaches everything in
// the board that refers to what it changes: core's part as R's
// augment_board_update() did, removing a removed block's links and its places
// in stacks, and each key of the front-end's state through its `expand`. Every
// `expand` runs again on what the rounds before added, until none adds
// anything, as ProseMirror calls `appendTransaction` again for what another
// plugin appended.

import type { Board, Stack, StackId } from "../types/board.ts";
import type {
  CollectionDelta,
  Delta,
  Entry,
  KeyChange,
  KeyDelta,
} from "./delta.ts";
import type { Registrations } from "./registrations.ts";
import { jsonEqual, messageOf, own, pathText } from "./util.ts";

// How many rounds a delta may expand in before the store rejects it, naming
// the keys that kept adding.
export const ROUNDS = 10;

export function expand(
  delta: Delta,
  board: Board,
  registrations: Registrations,
): Delta | { readonly failures: readonly string[] } {
  let expanded = delta;

  for (let round = 0; round < ROUNDS; round++) {
    const adding: string[] = [];
    const failures: string[] = [];

    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;

    for (const { def } of registrations.keys()) {
      if (def.expand === undefined) continue;
      let change: KeyDelta | undefined;
      try {
        change = def.expand(expanded, board);
      } catch (error) {
        failures.push(`The key ${pathText(def.path)} failed to expand the delta: ${messageOf(error)}`);
        continue;
      }
      if (change === undefined) continue;
      const merged = merge(expanded, def.path, def.kind, change);
      if (merged !== expanded) adding.push(pathText(def.path));
      expanded = merged;
    }

    if (failures.length > 0) return { failures };
    if (!grew && adding.length === 0) return expanded;
    if (round === ROUNDS - 1) {
      return {
        failures: [
          `The delta kept expanding after ${String(ROUNDS)} rounds, through ${adding.join(", ")}.`,
        ],
      };
    }
  }

  return expanded;
}

// Removing a block removes the links into and out of it and takes it out of
// its stack.
function expandCore(delta: Delta, board: Board): Delta {
  const gone = new Set(delta.blocks?.rm ?? []);
  if (gone.size === 0) return delta;

  const linksRm = delta.links?.rm ?? [];
  const incident = Object.entries(board.links)
    .filter(([id, link]) => (gone.has(link.from) || gone.has(link.to)) && !linksRm.includes(id))
    .map(([id]) => id);

  const stacksRm = delta.stacks?.rm ?? [];
  const stacksMod = delta.stacks?.mod ?? {};
  const placed: Record<StackId, Partial<Stack>> = {};
  for (const [id, stack] of Object.entries(board.stacks)) {
    if (stacksRm.includes(id)) continue;
    const mod = own(stacksMod, id);
    const blocks = mod?.blocks ?? stack.blocks;
    if (blocks.some((block) => gone.has(block))) {
      placed[id] = { ...mod, blocks: blocks.filter((block) => !gone.has(block)) };
    }
  }

  if (incident.length === 0 && Object.keys(placed).length === 0) return delta;

  return {
    ...delta,
    ...(incident.length === 0
      ? {}
      : { links: { ...delta.links, rm: [...linksRm, ...incident] } }),
    ...(Object.keys(placed).length === 0
      ? {}
      : { stacks: { ...delta.stacks, mod: { ...stacksMod, ...placed } } }),
  };
}

// The delta with what an `expand` returned for its key merged in, or the same
// delta where that adds nothing new, as an `expand` may return what the delta
// holds already.
function merge(
  delta: Delta,
  path: readonly string[],
  kind: "collection" | "value",
  change: KeyDelta,
): Delta {
  const changes = delta.front_end?.state ?? [];
  const at = changes.findIndex((other) => jsonEqual(other.path, path));
  const current = changes[at]?.change;

  const merged =
    kind === "value"
      ? mergeValue(current, change)
      : mergeCollection(current as CollectionDelta | undefined, change as CollectionDelta);
  if (merged === undefined || merged === current) return delta;

  const next: KeyChange = { path, change: merged };
  return {
    ...delta,
    front_end: {
      ...delta.front_end,
      state:
        at < 0
          ? [...changes, next]
          : changes.map((other, i) => (i === at ? next : other)),
    },
  };
}

// A value's change from an `expand`, which keeps the edit id that the delta's
// own change of the value names.
function mergeValue(current: KeyDelta | undefined, change: KeyDelta): KeyDelta | undefined {
  if (!("set" in change)) return current;
  if (current === undefined) return { set: change.set };
  if ("set" in current && jsonEqual(current.set, change.set)) return current;
  return { ...current, set: change.set };
}

function mergeCollection(
  current: CollectionDelta | undefined,
  change: CollectionDelta,
): CollectionDelta | undefined {
  const base = current ?? {};
  let grew = false;

  const rm = [...(base.rm ?? [])];
  for (const id of change.rm ?? []) {
    if (!rm.includes(id)) {
      rm.push(id);
      grew = true;
    }
  }

  const add: Record<string, Entry> = { ...base.add };
  for (const [id, entry] of Object.entries(change.add ?? {})) {
    if (!jsonEqual(own(add, id), entry)) {
      add[id] = entry;
      grew = true;
    }
  }

  const mod: Record<string, NonNullable<CollectionDelta["mod"]>[string]> = {
    ...base.mod,
  };
  for (const [id, entry] of Object.entries(change.mod ?? {})) {
    const was = own(mod, id);
    const set = { ...was?.set, ...entry.set };
    const unset = [...new Set([...(was?.unset ?? []), ...(entry.unset ?? [])])];
    const next = {
      ...was,
      ...(Object.keys(set).length === 0 ? {} : { set }),
      ...(unset.length === 0 ? {} : { unset }),
    };
    if (!jsonEqual(was, next)) {
      mod[id] = next;
      grew = true;
    }
  }

  if (!grew) return current;
  return {
    ...(rm.length === 0 ? {} : { rm }),
    ...(Object.keys(add).length === 0 ? {} : { add }),
    ...(Object.keys(mod).length === 0 ? {} : { mod }),
  };
}
