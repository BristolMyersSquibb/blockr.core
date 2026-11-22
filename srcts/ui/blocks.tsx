// The board's blocks as the forms of core's board actions and the tables of
// its Edit board plugin show them: by name, with the ID and the class, and
// what their classes say of their inputs.

import type { ReactElement } from "react";
import { ComboBoxItem, Text } from "../react/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import { own } from "../util/index.ts";

// A block as a select offers it: by its name, with its ID and its class
// after it, by which the select finds it too.
export interface BlockItem {
  readonly id: BlockId;
  readonly name: string;
  readonly detail: string;
}

export function blockItem(board: Pick<Board, "blocks">, registry: BlockRegistry, id: BlockId): BlockItem {
  const block = own(board.blocks, id);
  const cls = block === undefined ? undefined : (own(registry, block.registry_id)?.name ?? block.registry_id);
  return { id, name: block?.name ?? id, detail: cls === undefined ? id : `${id} · ${cls}` };
}

// The texts by which a select finds a block.
export function blockTexts(item: BlockItem): readonly string[] {
  return [item.name, item.detail, item.id];
}

// A block's row in a select's list.
export function blockRow(item: BlockItem): ReactElement {
  return (
    <ComboBoxItem id={item.id} textValue={item.name}>
      <Text slot="label" className="bc:shrink-0 bc:truncate">
        {item.name}
      </Text>
      <Text slot="description">{item.detail}</Text>
    </ComboBoxItem>
  );
}

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
