// Core's board actions that create and edit a link, from the table of Core's
// board actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions):
// - Create link, on a block that a new link can join to another, on an input
//   that no link feeds, or on the board, where it holds such a block. On a
//   block, its quick action lists the blocks that the block can feed, under
//   "Send to", and those that can feed it, under "Take from", each by its
//   name, with its ID at its end. Picking one adds the link, or first asks for
//   the input, where the block that the link goes into has several free ones,
//   as blockr.dock#549 had them. On an input, it lists the blocks that can
//   feed the input, under "Take from", and picking one adds the link into that
//   input, or without a name into the next place of a variadic block. On the
//   board, its quick action gives no level, since a link from no block takes
//   three picks, so that its row opens the advanced action at once. Its
//   advanced action is the form of a link (./link-form.tsx), starting from the
//   block or the input, or from no block on the board.
// - Edit link, on a link. Its quick action offers Change source…, which lists
//   the blocks that can feed the link's input instead, Move to input…, which
//   lists the other inputs of the block that the link goes into, and Rename
//   input…, a text row that holds the name of the link's input, as
//   blockr.dock#549 had them. Move to input… shows where that block takes
//   each input by its name, and Rename input… where it is variadic. An input
//   that another link feeds says so at its end, since moving the link there
//   swaps the inputs of the two links, as blockr.core-change_link does. Its
//   advanced action is the form of a link, editing it.
// Each quick action lists only the links for which the preflight of
// blockr.core-add_link or blockr.core-change_link passes, so that a menu offers
// no link that would close a cycle, join a block to itself or feed an input
// that a link feeds already. Create link fails its offers() where no row would
// be left, as on an input that a link feeds or that the block's class lacks,
// and Edit link leaves out a row that would lead to no row.
// A row that adds a link mints the link's ID with CoreApi's mintId() as the
// user picks it.
// Under the board lock, both fail their offers(), since the store refuses
// what they run.
// Create link plays add_link_role, and Edit link edit_link_role (./roles.ts).

import { freeInputs, linkFeeding, takesLink } from "../commands/index.ts";
import { createGraph } from "../graph/index.ts";
import type { Graph } from "../graph/index.ts";
import { inputNameError, namesInto } from "../picker/index.ts";
import type { BlockId, Board, Link, LinkId } from "../types/board.ts";
import type { BlockRegistry, CoreApi } from "../types/core-api.ts";
import { blockName, classInputs, isVariadicBlock } from "../ui/index.ts";
import { own } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import type { CoreParts } from "./core.ts";
import { PLUS } from "./icons.ts";
import { createLinkForm, editLinkForm } from "./link-form.tsx";
import { levelItem } from "./quick.ts";
import type { QuickLevel, QuickRow } from "./quick.ts";
import { heldBlock, inputOf } from "./targets.ts";

const CREATE_LINK = "blockr.core-add_link_action";
const EDIT_LINK = "blockr.core-edit_link_action";
const ADD_LINK = "blockr.core-add_link";
const CHANGE_LINK = "blockr.core-change_link";

// Create link, whose rows ask the command registry's preflights through
// `parts`, since offers() gets no CoreApi.
export function createLinkAction(parts: CoreParts): BoardAction {
  return {
    id: CREATE_LINK,
    label: "Create link",
    icon: PLUS,
    on: ["block", "input", "board"],
    category: "add",
    offers(board, target, lock) {
      if (lock.board) return false;
      const joining = joiningOn(board, parts);
      if ("board" in target) return [...joining.takers].some((to) => fedBy(joining, to, true).length > 0);
      const block = heldBlock(board, target);
      if (block === undefined) return false;
      const input = inputOf(target);
      if (input !== undefined) return fedBy(joining, block, true, input).length > 0;
      return feeds(joining, block, true).length > 0 || fedBy(joining, block, true).length > 0;
    },
    quick(board, target, core) {
      const block = heldBlock(board, target);
      if (block === undefined) return undefined;
      const joining = joiningOn(board, parts);
      const input = inputOf(target);
      if (input !== undefined) {
        const rows = fedBy(joining, block, false, input).map(
          (from): QuickRow => ({ ...quickBlock(board, joining.registry, from), heading: "Take from", pick: () => newLink(core, parts, from, block, input) }),
        );
        return rows.length === 0 ? undefined : { filter: true, rows };
      }
      const rows = [
        ...feeds(joining, block).map(
          (to): QuickRow => ({ ...quickBlock(board, joining.registry, to), heading: "Send to", pick: () => newLink(core, parts, block, to) }),
        ),
        ...fedBy(joining, block).map(
          (from): QuickRow => ({ ...quickBlock(board, joining.registry, from), heading: "Take from", pick: () => newLink(core, parts, from, block) }),
        ),
      ];
      return rows.length === 0 ? undefined : { filter: true, rows };
    },
    advanced: createLinkForm,
    role: "add_link_role",
  };
}

// Edit link, whose rows ask the command registry's preflights through
// `parts`.
export function editLinkAction(parts: CoreParts): BoardAction {
  return {
    id: EDIT_LINK,
    label: "Edit link",
    on: ["link"],
    category: "edit",
    offers: (board, target, lock) => !lock.board && heldLink(board, target) !== undefined,
    quick(board, target) {
      const held = heldLink(board, target);
      if (held === undefined) return undefined;
      const [id, link] = held;
      const registry = parts.blockRegistry();
      const into = blockName(board, link.to);
      const rows: QuickRow[] = [];
      const sources = sourceRows(board, registry, parts, id, link);
      if (sources.length > 0) {
        rows.push(levelItem("Change source…", { title: `Feed ${into} from`, filter: true, rows: sources }));
      }
      if (isVariadicBlock(board, registry, link.to)) {
        rows.push(levelItem("Rename input…", { title: `Rename input of ${into}`, rows: [renameRow(board, id, link)] }));
      } else {
        const inputs = inputRows(board, registry, parts, id, link);
        if (inputs.length > 0) rows.push(levelItem("Move to input…", { title: `Move to which input of ${into}?`, rows: inputs }));
      }
      return rows.length === 0 ? undefined : { rows };
    },
    advanced: editLinkForm,
    role: "edit_link_role",
  };
}

// The batch that adds a link from block `from` to block `to`, under an ID that
// CoreApi mints, into the input given, where the target names one. Otherwise
// it is the level that asks for the input first, where the block that the
// link goes into takes each input by its name and several are free. The level
// hands the advanced action the two blocks, through its open icon.
function newLink(core: CoreApi, parts: CoreParts, from: BlockId, to: BlockId, input?: string): readonly CommandCall[] | QuickLevel {
  const board = core.store.board();
  const adding = (into?: string): CommandCall[] => [
    {
      id: ADD_LINK,
      args: { id: core.mintId((id) => Object.hasOwn(core.store.board().links, id)), from, to, ...(into === undefined ? {} : { input: into }) },
    },
  ];
  if (input !== undefined) return adding(input);
  const free = freeInputs(board, parts.blockRegistry(), to) ?? [];
  if (free.length < 2) return adding();
  return {
    title: `Into which input of ${blockName(board, to)}?`,
    picked: { from, to },
    rows: free
      .filter((input) => parts.preflight(ADD_LINK, { from, to, input }))
      .map((input): QuickRow => ({ kind: "item", label: input, pick: () => adding(input) })),
  };
}

// What finds the blocks that a new link can join on the board: the board, the
// block registry, the graph of the board's links, the blocks that take a new
// link and the preflights of the command registry.
interface Joining {
  readonly board: Board;
  readonly registry: BlockRegistry;
  readonly graph: Graph;
  readonly takers: ReadonlySet<BlockId>;
  readonly preflight: CoreParts["preflight"];
}

function joiningOn(board: Board, parts: CoreParts): Joining {
  const registry = parts.blockRegistry();
  return {
    board,
    registry,
    graph: createGraph(board),
    takers: new Set(Object.keys(board.blocks).filter((block) => takesLink(board, registry, block))),
    preflight: parts.preflight,
  };
}

// The blocks that a new link from the block can go to, in the board's order:
// each that takes a link and is not upstream of the block, so that the link
// closes no cycle, where the preflight of add_link passes. Those are what the
// preflight checks, and cheap to find, so that the preflight runs only for a
// block that it is likely to pass, while it has the last word. With `first`,
// it stops at the first.
function feeds(joining: Joining, block: BlockId, first = false): BlockId[] {
  const upstream = joining.graph.withUpstream([block]);
  return passing(
    [...joining.takers].filter((to) => !upstream.has(to)),
    (to) => joining.preflight(ADD_LINK, { from: block, to }),
    first,
  );
}

// The blocks from which a new link can go to the block, where the block takes
// a link, or into the input given, where no link feeds it: each that is not
// downstream of the block, where the preflight of add_link passes. The input
// "" is the next place of a variadic block. With `first`, it stops at the
// first.
function fedBy(joining: Joining, block: BlockId, first = false, input?: string): BlockId[] {
  const takes =
    input === undefined ? joining.takers.has(block) : freeInputs(joining.board, joining.registry, block)?.includes(input) === true;
  if (!takes) return [];
  const downstream = joining.graph.withDownstream([block]);
  return passing(
    Object.keys(joining.board.blocks).filter((from) => !downstream.has(from)),
    (from) => joining.preflight(ADD_LINK, { from, to: block, ...(input === undefined ? {} : { input }) }),
    first,
  );
}

// The blocks for which the check passes, or with `first`, the first of them.
function passing(blocks: readonly BlockId[], check: (block: BlockId) => boolean, first: boolean): BlockId[] {
  if (!first) return blocks.filter(check);
  const found = blocks.find(check);
  return found === undefined ? [] : [found];
}

// The rows of Change source…: each block that can feed the link's input in
// place of the block that it comes from, which is no block downstream of the
// block that the link goes into, where the preflight of change_link passes.
function sourceRows(board: Board, registry: BlockRegistry, parts: CoreParts, id: LinkId, link: Link): QuickRow[] {
  const downstream = createGraph(board).withDownstream([link.to]);
  return Object.keys(board.blocks)
    .filter((from) => from !== link.from && !downstream.has(from) && parts.preflight(CHANGE_LINK, { id, from }))
    .map((from) => ({ ...quickBlock(board, registry, from), pick: () => [{ id: CHANGE_LINK, args: { id, from } }] }));
}

// The rows of Move to input…: each other input of the block that the link goes
// into, where the preflight of change_link passes. One that another link
// feeds says which block that link comes from, since the two links swap their
// inputs.
function inputRows(board: Board, registry: BlockRegistry, parts: CoreParts, id: LinkId, link: Link): QuickRow[] {
  return classInputs(board, registry, link.to)
    .filter((input) => input !== link.input && parts.preflight(CHANGE_LINK, { id, input }))
    .map((input): QuickRow => {
      const other = linkFeeding(board, registry, link.to, input, id);
      const from = other === undefined ? undefined : own(board.links, other)?.from;
      return {
        kind: "item",
        label: input,
        ...(from === undefined ? {} : { detail: `swaps with ${blockName(board, from)}` }),
        pick: () => [{ id: CHANGE_LINK, args: { id, input } }],
      };
    });
}

// The text row of Rename input…, which holds the name that the link gives the
// input of the variadic block, and refuses a name that another link into the
// block gives already. An empty name leaves the input without one, and a name
// that the user leaves as it was runs nothing.
function renameRow(board: Board, id: LinkId, link: Link): QuickRow {
  const taken = namesInto(board, link.to, id);
  return {
    kind: "text",
    label: "Name",
    value: link.input,
    check: (value) => inputNameError(value, taken),
    submit(value) {
      const input = value.trim();
      return input === link.input ? [] : [{ id: CHANGE_LINK, args: { id, input } }];
    },
  };
}

// A block as a row of a quick action shows it: by its name, with its class's
// icon, and with its ID at its end, by which the filter finds it too.
function quickBlock(board: Board, registry: BlockRegistry, block: BlockId): { kind: "item"; label: string; detail: string; icon?: string } {
  const icon = own(registry, own(board.blocks, block)?.registry_id ?? "")?.icon;
  return { kind: "item", label: blockName(board, block), detail: block, ...(icon === undefined || icon === "" ? {} : { icon }) };
}

// The link that the target names, with its ID, where the board holds it.
function heldLink(board: Board, target: Target): [LinkId, Link] | undefined {
  if (!("link" in target)) return undefined;
  const link = own(board.links, target.link);
  return link === undefined ? undefined : [target.link, link];
}
