// Core's four board actions that add a block, from the table of Core's board
// actions
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#cores-board-actions),
// in its order, each of the category "add":
// - Add block, `add_block_action`, on the board, adds a block on
//   its own, with add_block.
// - Append block, `append_block_action`, on a block, adds a block
//   that takes the block's result, with add_block and add_link.
// - Prepend block, `prepend_block_action`, on a block with an
//   input that no link feeds, or on such an input, adds a block that feeds
//   that input, with add_block and add_link.
// - Insert block, `insert_block_action`, on a link, adds a block
//   between the two blocks that the link joins, with add_block, add_link and
//   change_link. The link then comes from the new block, and keeps its id,
//   its input and its place among the links.
// Their ids keep the names of the dock's actions that they replace. Each plays
// the role named after its id, as Append block plays append_block_role
// (./roles.ts).
//
// The quick action of each is a level with a filter, and a row for each block
// class that the board action can add, under the heading of its category, with
// the class's icon and its package, as the add menu of blockr.ui's design
// system lists them
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1199-L1207).
// Append block and Insert block list the classes that have an input, since the
// new block takes a block's result. Picking a row adds a block of the class
// with an id that core.mintId() mints and the class's name, and its link
// through the first free input. That is the new block's first input after a
// block or on a link, and before a block the block's first input that no link
// feeds. Into a variadic block, the link goes without a name. On an input,
// Prepend block's link goes into that input, and on the next place of a
// variadic block, the input "", without a name. The batch runs as one step of
// the user.
// The advanced action of each is the block picker (../picker), which holds the
// new block's class, its id, its name and the input that its link feeds:
// on its own for Add block, after the block for Append block, before it for
// Prepend block, and on the link for Insert block. On an input, the picker
// before the block takes the link into that input. It names the input in a
// line in place of the field of the input, since the target fixes the input,
// as prepend_block_role has it (./roles.ts). On the next place of a variadic
// block, the input "", the picker asks for the name of the input and its
// place among the block's links, as before the block.
// Opened from the quick action, the picker starts its search from the text in
// the level's filter, and takes the keyboard's focus there. It runs nothing
// before the user submits it, mints the id that the user leaves empty, and
// hands its batch to done().
// A plugin may show the advanced action of Add block in its room, as the Edit
// board plugin does. There core renders the picker again once it has added a
// block. Where the keyboard's focus was in the picker as the user added the
// block, the new picker takes the focus, so that it is ready for the next one.
//
// Each carries the icon of a block, Lucide's box, so that a menu tells it
// apart from Create link and Create stack, which carry the icons of what they
// add (./icons.ts).
// Each fails its offers() in these cases:
// - The board lock is on, since the store refuses what the board action runs.
// - The board does not hold what the board action acts on.
// - Its quick action would hold no row, as where the block registry holds no
//   class that it can add.
// - For Prepend block on a block, links feed every input of the block.
// - For Prepend block on an input, a link feeds the input, or the block has no
//   such input. A variadic block's only input there is its next place, "",
//   and a block of fixed inputs has no input "".
// A row that the user picks once the board no longer holds the board action's
// target runs nothing.

import { CORE, freeInputs } from "../commands/index.ts";
import { blockClasses, BlockPicker } from "../picker/index.ts";
import type { AddMode } from "../picker/index.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { drawForm } from "../ui/index.ts";
import { interned, own } from "../util/index.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import type { AdvancedModule } from "./advanced.ts";
import { BLOCK } from "./icons.ts";
import type { QuickLevel } from "./quick.ts";
import { heldBlock, inputOf } from "./targets.ts";

// The block registry's entries by registry id, as the store holds them for the
// open board.
type Registry = () => ReadonlyMap<RegistryId, RegistryEntry>;

// Core's board actions that add a block, in the order of their table, which
// read the block registry through `registry`.
export function addingBoardActions(registry: Registry): Omit<BoardAction, "package">[] {
  // Whether the block registry holds a class that a board action can add,
  // one with an input where `taking` is true.
  const adds = (taking: boolean): boolean => [...registry().values()].some((entry) => !taking || hasInput(entry));
  return [
    {
      id: "add_block_action",
      label: "Add block",
      icon: BLOCK,
      on: ["board"],
      category: "add",
      offers: (_board, _target, lock) => !lock.board && adds(false),
      quick: (_board, _target, core) => classLevel(core, false, (entry, block) => [addBlockCall(entry, block)]),
      advanced: pickerModule(() => ({ kind: "add" })),
      role: "add_block_role",
    },
    {
      id: "append_block_action",
      label: "Append block",
      icon: BLOCK,
      on: ["block"],
      category: "add",
      offers: (board, target, lock) => !lock.board && heldBlock(board, target) !== undefined && adds(true),
      quick: (_board, target, core) =>
        classLevel(core, true, (entry, block, board) => {
          const from = heldBlock(board, target);
          return from === undefined ? [] : [addBlockCall(entry, block), addLinkCall(from, block, newBlockInput(entry))];
        }),
      advanced: pickerModule((target) => ("block" in target ? { kind: "after", from: target.block } : undefined)),
      role: "append_block_role",
    },
    {
      id: "prepend_block_action",
      label: "Prepend block",
      icon: BLOCK,
      on: ["block", "input"],
      category: "add",
      offers: (board, target, lock) =>
        !lock.board && freeInputOf(board, registry(), target) !== undefined && adds(false),
      quick: (_board, target, core) =>
        classLevel(core, false, (entry, block, board) => {
          const to = heldBlock(board, target);
          // On an input, the link goes into that input even where a link has
          // come to feed it since the menu opened, so that the command refuses
          // it, rather than the link going into another input.
          const input = inputOf(target) ?? freeInputOf(board, core.blockRegistry(), target);
          return to === undefined ? [] : [addBlockCall(entry, block), addLinkCall(block, to, input)];
        }),
      advanced: pickerModule((target) => {
        if (!("block" in target)) return undefined;
        const input = inputOf(target);
        return { kind: "before", to: target.block, ...(input === undefined ? {} : { input }) };
      }),
      role: "prepend_block_role",
    },
    {
      id: "insert_block_action",
      label: "Insert block",
      icon: BLOCK,
      on: ["link"],
      category: "add",
      offers: (board, target, lock) => !lock.board && "link" in target && Object.hasOwn(board.links, target.link) && adds(true),
      quick: (_board, target, core) =>
        classLevel(core, true, (entry, block, board) => {
          const id = "link" in target ? target.link : undefined;
          const link = id === undefined ? undefined : own(board.links, id);
          return id === undefined || link === undefined
            ? []
            : [
                addBlockCall(entry, block),
                addLinkCall(link.from, block, newBlockInput(entry)),
                { id: interned(CORE, "change_link"), args: { id, from: block } },
              ];
        }),
      advanced: pickerModule((target) => ("link" in target ? { kind: "on", link: target.link } : undefined)),
      role: "insert_block_role",
    },
  ];
}

// Whether a block class has an input, which a block that takes another's
// result needs.
function hasInput(entry: RegistryEntry): boolean {
  return entry.inputs.length > 0;
}

// The level of a quick action that adds a block: a filter, and a row for each
// block class of the block registry, only those with an input where `taking`
// is true. Picking a row mints the new block's id and runs the batch that
// `batch` returns for the class, the id and the board as it is then.
function classLevel(
  core: CoreApi,
  taking: boolean,
  batch: (entry: RegistryEntry, block: BlockId, board: Board) => readonly CommandCall[],
): QuickLevel {
  const registry = core.blockRegistry();
  const entries = [...registry.values()].filter((entry) => !taking || hasInput(entry));
  return {
    filter: true,
    rows: blockClasses(entries).map((item) => ({
      kind: "item",
      label: item.name,
      heading: item.category,
      ...(item.icon === "" ? {} : { icon: item.icon }),
      ...(item.pkg === null ? {} : { detail: item.pkg }),
      pick() {
        const entry = registry.get(item.registryId);
        const board = core.store.board();
        if (entry === undefined) return [];
        return batch(entry, core.mintId((id) => Object.hasOwn(board.blocks, id)), board);
      },
    })),
  };
}

// The command that adds a block of the class under the id given, which takes
// the class's name.
function addBlockCall(entry: RegistryEntry, block: BlockId): CommandCall {
  return { id: interned(CORE, "add_block"), args: { registry_id: { package: entry.package, id: entry.id }, id: block } };
}

// The command that links two blocks, into the input given, which is empty for
// a variadic block. Without one, the command finds the input, and refuses
// where none is free.
function addLinkCall(from: BlockId, to: BlockId, input: string | undefined): CommandCall {
  return { id: interned(CORE, "add_link"), args: { from, to, ...(input === undefined ? {} : { input }) } };
}

// The first input of a new block of the class, which takes the link that the
// block comes with: empty for a variadic class, whose link goes in without a
// name.
function newBlockInput(entry: RegistryEntry): string | undefined {
  return entry.inputs.includes("...") ? "" : entry.inputs[0];
}

// The input that a block that Prepend block adds feeds, of the block that the
// target names: on a block, its first input that no link feeds, and on an
// input, that input, where no link feeds it. It is "" for a variadic block,
// whose link goes in without a name. It is undefined in these cases:
// - The board does not hold the block.
// - Links feed every input of the block.
// - On an input, a link feeds the input, or the block has no input of that
//   name. A variadic block has none but "", its next place.
function freeInputOf(board: Board, registry: ReadonlyMap<RegistryId, RegistryEntry>, target: Target): string | undefined {
  const block = heldBlock(board, target);
  const free = block === undefined ? undefined : freeInputs(board, registry, block);
  const input = inputOf(target);
  if (input === undefined) return free?.[0];
  return free?.includes(input) === true ? input : undefined;
}

// The block picker as the advanced action of a board action, in the mode that
// `mode` gives for the target. It renders nothing for a target that gives no
// mode, which core hands it for no board action of this file.
function pickerModule(mode: (target: Target) => AddMode | undefined): AdvancedModule {
  // The elements in which a picker added a block while the keyboard's focus
  // was in it, so that the picker that core renders there next takes the
  // focus.
  const focused = new WeakSet<HTMLElement>();
  return {
    render({ el, core, target, filter, done }) {
      const at = mode(target);
      if (at === undefined) return () => undefined;
      const autofocus = filter !== undefined || focused.delete(el);
      return drawForm(
        el,
        <BlockPicker
          core={core}
          mode={at}
          {...(filter === undefined ? {} : { filter })}
          autofocus={autofocus}
          done={(calls, opts) => {
            if (el.contains(el.ownerDocument.activeElement)) focused.add(el);
            return done(calls, opts).finally(() => {
              focused.delete(el);
            });
          }}
        />,
      );
    },
  };
}
