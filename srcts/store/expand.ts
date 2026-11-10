// Expanding a delta with what follows from it,
// so that it reaches everything in the board that refers to what it changes:
// core's part as R's augment_board_update() did,
// removing a removed block's links and its places in stacks,
// and the state of each plugin through the `update` of its class, which the
// store calls once for each plugin of the class.
// Every `update` runs again on what the rounds before added,
// until none adds anything, as ProseMirror calls `appendTransaction` again for
// what another plugin appended.

import type { Board, PluginId, Stack, StackId } from "../types/board.ts";
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
    const adding: PluginId[] = [];
    const failures: string[] = [];

    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;

    for (const [id, plugin] of Object.entries(board.plugins)) {
      const def = registrations.plugin(plugin.class)?.def;
      if (def?.update === undefined) continue;
      let patches: unknown;
      try {
        patches = def.update(expanded, board, id);
      } catch (error) {
        failures.push(`The plugin class "${def.id}" failed to update the delta for the plugin "${id}": ${errorMessage(error)}`);
        continue;
      }
      const wrong = misplaced(def.id, id, patches);
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

// What is wrong with what the `update` of a plugin's class returned for the
// plugin `id`: no array of patches, or a patch outside that plugin's state.
function misplaced(cls: string, id: PluginId, patches: unknown): string[] {
  const of = `The plugin class "${cls}" updated the delta for the plugin "${id}"`;
  if (!Array.isArray(patches)) return [`${of} with no array of patches.`];
  const failures: string[] = [];
  for (const patch of patches as unknown[]) {
    const problems = malformed(patch);
    if (problems.length > 0) {
      failures.push(`${of} with a patch that ${problems.join(", ")}.`);
      continue;
    }
    const { path } = patch as Patch;
    const [boardProperty, plugin, pluginProperty] = segmentsFromPath(path) ?? [];
    if (boardProperty !== "plugins" || plugin !== id || pluginProperty !== "state") {
      failures.push(`${of} with a patch at "${path}", outside that plugin's state.`);
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
