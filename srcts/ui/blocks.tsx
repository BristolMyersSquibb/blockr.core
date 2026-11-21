// A block as the selects of core's UIs offer it: by its name, with its id and
// its class after it, by which a select finds it too. The Edit board plugin's
// forms pick blocks so.

import type { ReactElement } from "react";
import { ComboBoxItem, Text } from "../react/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { own } from "../util/index.ts";

export interface BlockItem {
  readonly id: BlockId;
  readonly name: string;
  readonly detail: string;
}

export function blockItem(board: Pick<Board, "blocks">, registry: Readonly<Record<string, RegistryEntry>>, id: BlockId): BlockItem {
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

// A block's name, or its id where the board lacks it.
export function blockName(board: Pick<Board, "blocks">, block: BlockId): string {
  return own(board.blocks, block)?.name ?? block;
}
