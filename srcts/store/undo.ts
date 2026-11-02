// Undo and redo, which apply a step's deltas again as a new edit on the board
// as it is now, as Figma, Google Slides and tldraw undo: what the step changed
// is set back where it is still there, over what another origin has changed
// since, and nothing happens where it is gone.

import type { Board } from "../types/board.ts";
import type { CollectionDelta, Delta, KeyChange } from "./delta.ts";
import type { Registrations } from "./registrations.ts";
import { getAt, isObject } from "./util.ts";

// The part of a delta that still applies to the board: without the removal or
// change of what is gone, the addition of what is there already, and the
// links and the places in stacks of blocks that are gone.
export function fit(delta: Delta, board: Board, registrations: Registrations): Delta {
  const blocks = collection(board.blocks, delta.blocks);
  const kept = new Set([
    ...Object.keys(board.blocks).filter((id) => !(blocks.rm ?? []).includes(id)),
    ...Object.keys(blocks.add ?? {}),
  ]);
  const holds = (id: string) => kept.has(id);

  const links = collection(board.links, delta.links, (link) =>
    (link.from === undefined || holds(link.from)) && (link.to === undefined || holds(link.to)),
  );
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).filter(([id]) => Object.hasOwn(links.add ?? {}, id)),
  );

  const stacks = collection(board.stacks, delta.stacks);
  const placed = <T extends { blocks?: readonly string[] }>(stack: T): T =>
    stack.blocks === undefined ? stack : { ...stack, blocks: stack.blocks.filter(holds) };

  const state: KeyChange[] = [];
  for (const { path, change } of delta.front_end?.state ?? []) {
    const key = registrations.key(path);
    if (key === undefined) continue;
    if (key.def.kind === "value") {
      state.push({ path, change });
      continue;
    }
    const at = getAt(board.front_end.state, path);
    const entries = isObject(at) ? at : {};
    const fitted = collection(entries, change as CollectionDelta);
    if (Object.keys(fitted).length > 0) state.push({ path, change: fitted });
  }

  return prune({
    blocks,
    links: { ...links, ...(Object.keys(before).length === 0 ? {} : { before }) },
    stacks: {
      ...(stacks.rm === undefined ? {} : { rm: stacks.rm }),
      ...(stacks.add === undefined
        ? {}
        : { add: Object.fromEntries(Object.entries(stacks.add).map(([id, s]) => [id, placed(s)])) }),
      ...(stacks.mod === undefined
        ? {}
        : { mod: Object.fromEntries(Object.entries(stacks.mod).map(([id, s]) => [id, placed(s)])) }),
    },
    ...(delta.options === undefined ? {} : { options: delta.options }),
    ...(delta.name === undefined ? {} : { name: delta.name }),
    front_end: {
      ...(state.length === 0 ? {} : { state }),
      ...(delta.front_end?.attrs === undefined ? {} : { attrs: delta.front_end.attrs }),
    },
  });
}

interface Part<T, M> {
  readonly add?: Readonly<Record<string, T>>;
  readonly mod?: Readonly<Record<string, M>>;
  readonly rm?: readonly string[];
}

// A collection's part of a delta without the removal or change of an entry
// that is gone, and without the addition of one that is there already, once
// what the part removes is gone, or that `keep` refuses.
function collection<T, M>(
  current: Readonly<Record<string, unknown>>,
  part: Part<T, M> | undefined,
  keep: (entry: T | M) => boolean = () => true,
): Part<T, M> {
  if (part === undefined) return {};
  const rm = (part.rm ?? []).filter((id) => Object.hasOwn(current, id));
  const there = (id: string) => Object.hasOwn(current, id) && !rm.includes(id);
  const add = Object.entries(part.add ?? {}).filter(([id, entry]) => !there(id) && keep(entry));
  const mod = Object.entries(part.mod ?? {}).filter(([id, change]) => there(id) && keep(change));
  return {
    ...(rm.length === 0 ? {} : { rm }),
    ...(add.length === 0 ? {} : { add: Object.fromEntries(add) }),
    ...(mod.length === 0 ? {} : { mod: Object.fromEntries(mod) }),
  };
}

// The delta without its empty parts.
function prune(delta: Delta): Delta {
  const parts: [string, unknown][] = Object.entries(delta);
  return Object.fromEntries(
    parts.filter(([, part]) => !isObject(part) || Object.keys(part).length > 0),
  );
}

export function isEmpty(delta: Delta): boolean {
  return Object.keys(prune(delta)).length === 0;
}
