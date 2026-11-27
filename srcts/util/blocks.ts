// The board's blocks and the block classes as core's board actions and their
// forms name, list and check them: a block's name, what its class says of its
// inputs, the names that the links into a variadic block give its inputs, and
// the block classes as the block picker and the quick actions that add a block
// list them.
// Core's runtime holds them for its board actions, and each form that core
// loads on demand holds a copy, as the bundle of the Edit board plugin does
// (../actions, ../picker, ../modules/edit-board).

import type { BlockId, Board } from "../types/board.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own, packageFromId } from "./util.ts";

// A block's name, or its ID where the board lacks it.
export function blockName(board: Pick<Board, "blocks">, block: BlockId): string {
  return own(board.blocks, block)?.name ?? block;
}

// The inputs of the block's class, in their order, or none where the board or
// the block registry lacks it.
export function classInputs(board: Pick<Board, "blocks">, registry: BlockRegistry, block: BlockId): readonly string[] {
  const held = own(board.blocks, block);
  return (held === undefined ? undefined : own(registry, held.registry_id)?.inputs) ?? [];
}

// Whether the block is variadic, taking any number of links in their order.
export function isVariadicBlock(board: Pick<Board, "blocks">, registry: BlockRegistry, block: BlockId): boolean {
  return classInputs(board, registry, block).includes("...");
}

// The names that the links into a variadic block give its inputs.
export function namesInto(board: Pick<Board, "links">, to: BlockId, except?: string): readonly string[] {
  return Object.entries(board.links)
    .filter(([id, link]) => id !== except && link.to === to && link.input !== "")
    .map(([, link]) => link.input);
}

// Why the name of a variadic block's input does not do, where it does not: a
// name that another of its links gives already would leave two arguments of
// one name. An empty name does, for an input without a name.
export function inputNameError(value: string, taken: readonly string[]): string | undefined {
  const trimmed = value.trim();
  if (trimmed === "") return undefined;
  if (taken.includes(trimmed)) return `Another link gives the input the name "${trimmed}" already.`;
  return undefined;
}

// A block class as the select and the quick actions that add a block offer
// it: by its name, with its description, its icon and its package, under its
// category, and found by those, by its keywords and by its registry id too.
export interface ClassItem {
  readonly id: string;
  readonly name: string;
  readonly description: string;
  readonly icon: string;
  readonly pkg: string | null;
  readonly category: string;
  readonly keywords: readonly string[];
}

// The block classes of the entries, in the order in which the picker and the
// quick actions that add a block list them (byGroupAndName(), below).
export function blockClasses(entries: readonly RegistryEntry[]): ClassItem[] {
  return entries.map(classItem).sort(byGroupAndName);
}

function classItem(entry: RegistryEntry): ClassItem {
  return {
    id: entry.id,
    name: entry.name,
    description: entry.description,
    icon: entry.icon,
    pkg: packageFromId(entry.id),
    category: entry.category === "" ? UNCATEGORIZED : entry.category,
    keywords: entry.keywords,
  };
}

// The category of a class that gives none, as R's registry names it.
const UNCATEGORIZED = "uncategorized";

// The classes by category and then by name, the categories by name, but for
// the classes without a category, which come last.
function byGroupAndName(a: ClassItem, b: ClassItem): number {
  const last = (item: ClassItem): number => (item.category.toLowerCase() === UNCATEGORIZED ? 1 : 0);
  return last(a) - last(b) || a.category.localeCompare(b.category) || a.name.localeCompare(b.name);
}
