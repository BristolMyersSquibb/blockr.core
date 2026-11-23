// Expanding a delta with what follows from it,
// so that it reaches everything in the board that refers to what it changes:
// core's part as R's augment_board_update() did,
// removing a removed block's links and its places in stacks,
// and the state of each plugin through the `update` of its class, which the
// store calls once for each plugin whose state the board holds.
// Every `update` runs again on what the rounds before added,
// until none adds anything, as ProseMirror calls `appendTransaction` again for
// what another plugin appended.
// Once the last delta of a step is in, the `afterStep` of each plugin's class
// gives what follows from the step as a whole, which the store applies as one
// more delta of the step (afterStep(), below).

import type { Board, PluginClassId, Stack, StackId } from "../types/board.ts";
import { errorMessage, jsonEqual, own, quoted } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";
import { malformed, overlap, segmentsFromPath } from "./patch.ts";
import type { Registrations } from "./registrations.ts";

// How many rounds a delta may expand in before the store rejects it,
// naming the plugins whose updates kept adding.
export const ROUNDS = 10;

export function expand(
  delta: Delta,
  board: Board,
  registrations: Pick<Registrations, "plugin">,
): Delta | { readonly failures: readonly string[] } {
  let expanded = delta;

  for (let round = 0; round < ROUNDS; round++) {
    const adding: PluginClassId[] = [];
    const failures: string[] = [];

    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;

    for (const id of Object.keys(board.plugins)) {
      const def = registrations.plugin(id)?.def;
      if (def?.update === undefined) continue;
      let patches: unknown;
      try {
        patches = def.update(expanded, board, id);
      } catch (error) {
        failures.push(`The plugin class "${id}" failed to update the delta for its plugin: ${errorMessage(error)}`);
        continue;
      }
      const wrong = misplaced(id, patches, `The plugin class "${id}" updated the delta for its plugin with`);
      if (wrong.length > 0) {
        failures.push(...wrong);
        continue;
      }
      const merged = merge(expanded, delta, patches as readonly Patch[], board);
      if (merged !== expanded) adding.push(id);
      expanded = merged;
    }

    if (failures.length > 0) return { failures };
    if (!grew && adding.length === 0) return expanded;
    if (round === ROUNDS - 1) {
      return {
        failures: [
          `The delta kept expanding after ${String(ROUNDS)} rounds, through the update of the plugin${adding.length > 1 ? "s" : ""} ${quoted(adding)}.`,
        ],
      };
    }
  }

  return expanded;
}

// The delta that follows from a step as a whole, once its last delta is in:
// the patches that the `afterStep` of each plugin's class returns for its
// plugin, in the order of the board's plugins, or undefined where none
// returns any.
// Every class gets the same two boards: the board that the step's own deltas
// leave and the board that the step started from. So what one class returns
// does not depend on the order of the app's plugins. A class that follows
// another plugin's state still sees the other class's patches, in its
// `update`, since the store expands this delta as any other.
// The store calls each `afterStep` once in a step, and not again on what the
// others return, so that every step ends.
export function afterStep(
  board: Board,
  before: Board,
  registrations: Pick<Registrations, "plugin">,
): Delta | undefined | { readonly failures: readonly string[] } {
  const patches: Patch[] = [];
  const failures: string[] = [];

  for (const id of Object.keys(board.plugins)) {
    const def = registrations.plugin(id)?.def;
    if (def?.afterStep === undefined) continue;
    let returned: unknown;
    try {
      returned = def.afterStep(board, before, id);
    } catch (error) {
      failures.push(`The afterStep of the plugin class "${id}" failed: ${errorMessage(error)}`);
      continue;
    }
    const wrong = misplaced(id, returned, `The afterStep of the plugin class "${id}" returned`);
    if (wrong.length > 0) {
      failures.push(...wrong);
      continue;
    }
    patches.push(...(returned as readonly Patch[]));
  }

  if (failures.length > 0) return { failures };
  return patches.length === 0 ? undefined : { patches };
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

// What is wrong with what the `update` or the `afterStep` of the plugin class
// `id` returned for its plugin: no array of patches, or a patch outside that
// plugin's state. Each failure is a sentence that `of` starts, which names the
// function.
function misplaced(id: PluginClassId, patches: unknown, of: string): string[] {
  if (!Array.isArray(patches)) return [`${of} no array of patches.`];
  const failures: string[] = [];
  for (const patch of patches as unknown[]) {
    const problems = malformed(patch);
    if (problems.length > 0) {
      failures.push(`${of} a patch that ${problems.join(", ")}.`);
      continue;
    }
    const { path } = patch as Patch;
    const [boardProperty, plugin] = segmentsFromPath(path) ?? [];
    if (boardProperty !== "plugins" || plugin !== id) {
      failures.push(`${of} a patch at "${path}", outside that plugin's state.`);
    }
  }
  return failures;
}

// The delta with the patches that an `update` returned appended, or the same
// delta where that adds nothing new, since an `update` may return what the
// delta holds already.
// What follows from a delta never overrides what the delta given to the store
// says itself.
// So a patch of an `update` is left out where its path is at, under or above a
// path that one of the given delta's own patches changes.
// An undo that puts back a plugin's old state then puts back that state, and
// not one that an `update` makes up for a new object.
// An array's index, or its end as "-", names no element, so that the store
// keeps what an `update` adds to an array that the delta adds to as well.
// The board before the delta tells which segments of a path index an array.
// Where that board lacks a member on the way, one of the delta's own patches
// adds the member.
// That patch lies above the update's patch, which is left out all the same.
function merge(delta: Delta, given: Delta, patches: readonly Patch[], board: Board): Delta {
  const held = delta.patches ?? [];
  const fixed = (given.patches ?? []).map(({ path }) => segmentsFromPath(path) ?? []);

  const added: Patch[] = [];
  for (const patch of patches) {
    const segments = segmentsFromPath(patch.path) ?? [];
    if ([...held, ...added].some((other) => same(other, patch))) continue;
    if (fixed.some((other) => overlap(other, segments, board))) continue;
    added.push(patch);
  }

  if (added.length === 0) return delta;
  return { ...delta, patches: [...held, ...added] };
}

function same(a: Patch, b: Patch): boolean {
  return a.op === b.op && a.path === b.path && jsonEqual(a.value, b.value);
}
