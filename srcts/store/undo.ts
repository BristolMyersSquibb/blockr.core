// Undo and redo, which apply a step's deltas again as a new edit on the board
// as it is now, as Figma, Google Slides and tldraw undo: what the step changed
// is set back where it is still there, over what another origin has changed
// since, and nothing happens where it is gone.

import type { Board } from "../types/board.ts";
import { isObject } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";
import { applyPatch, segmentsFromPath } from "./patch.ts";

// The part of a delta that still applies to the board: without the removal
// or change of what is gone, the addition of what is there already,
// the links and the places in stacks of blocks that are gone,
// the moves of links that are gone,
// and the patches that no longer find their path,
// as one that changes what another origin removed.
// A link moves last where the link that it went before is gone, as an entry
// that undo adds back goes last where the entry that followed it is gone.
// A patch at an array's index changes whatever the array holds there now.
export function fit(delta: Delta, board: Board): Delta {
  const blocks = collection(board.blocks, delta.blocks);
  const kept = new Set([
    ...Object.keys(board.blocks).filter((id) => !(blocks.rm ?? []).includes(id)),
    ...Object.keys(blocks.add ?? {}),
  ]);
  const holds = (id: string) => kept.has(id);

  const links = collection(board.links, delta.links, (link) =>
    (link.from === undefined || holds(link.from)) && (link.to === undefined || holds(link.to)),
  );
  const held = (id: string) => Object.hasOwn(board.links, id) && !(links.rm ?? []).includes(id);
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).flatMap(([id, named]): [string, string | null][] => {
      if (Object.hasOwn(links.add ?? {}, id)) return [[id, named]];
      if (!held(id)) return [];
      return [[id, named !== null && Object.hasOwn(board.links, named) ? named : null]];
    }),
  );

  const stacks = collection(board.stacks, delta.stacks);
  const placed = <T extends { blocks?: readonly string[] }>(stack: T): T =>
    stack.blocks === undefined ? stack : { ...stack, blocks: stack.blocks.filter(holds) };

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
    patches: patches(delta.patches ?? [], board),
  });
}

// The patches that apply, one after another, to the board's plugins.
function patches(given: readonly Patch[], board: Board): Patch[] {
  let at = board as unknown as Readonly<Record<string, unknown>>;
  const fitted: Patch[] = [];
  for (const patch of given) {
    const segments = segmentsFromPath(patch.path) ?? [];
    if (segments[0] !== "plugins") continue;
    const applied = applyPatch(at, patch, segments);
    if ("failure" in applied) continue;
    at = applied.next;
    fitted.push(patch);
  }
  return fitted;
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
    parts.filter(([, part]) =>
      Array.isArray(part) ? part.length > 0 : !isObject(part) || Object.keys(part).length > 0,
    ),
  );
}

export function isEmpty(delta: Delta): boolean {
  return Object.keys(prune(delta)).length === 0;
}
