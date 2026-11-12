// The Edit board plugin's section of blocks: the picker of the block
// registry's entries, in its add mode, which adds a block under the ID and the
// name that the user gives with add_block, and below it each of the board's
// blocks, with its name, which rename_block changes, and a button for
// remove_block, which removes the block with its links and its place in its
// stack.

import { useCallback, useState } from "react";
import type { ReactElement } from "react";
import { BlockPicker } from "../../picker/index.ts";
import type { AddMode } from "../../picker/index.ts";
import { styles } from "../../styles/index.ts";
import type { Block, BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { classes, NameField } from "../../ui/index.ts";
import { Section } from "./board.tsx";
import { runCommand } from "./run.ts";

const ADD: AddMode = { kind: "add" };

export function BlocksSection({ core, board }: { core: CoreApi; board: Board }): ReactElement {
  const run = useCallback((command: string, args: Json) => runCommand(core, command, args), [core]);
  const blocks = Object.entries(board.blocks);
  return (
    <Section title="Blocks">
      <BlockPicker core={core} mode={ADD} run={run} />
      {blocks.length === 0 ? (
        <p className={styles.hint}>The board holds no blocks yet.</p>
      ) : (
        <ul aria-label="Blocks" className={`${classes.list} bc:border-t bc:border-border-default bc:pt-2`}>
          {blocks.map(([id, block]) => (
            <BlockItem key={id} core={core} id={id} block={block} />
          ))}
        </ul>
      )}
    </Section>
  );
}

function BlockItem({ core, id, block }: { core: CoreApi; id: BlockId; block: Block }): ReactElement {
  const [renaming, setRenaming] = useState(false);
  return (
    <li aria-label={block.name} className="bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:text-sm">
      {renaming ? (
        <NameField
          label="Block name"
          name={block.name}
          onSubmit={(name) => {
            void runCommand(core, "blockr.core-rename_block", { id, name });
          }}
          onDone={() => {
            setRenaming(false);
          }}
        />
      ) : (
        <span className="bc:min-w-0 bc:flex-1">
          {block.name} <span className="bc:text-text-muted">({id})</span>
        </span>
      )}
      {renaming ? null : (
        <button
          type="button"
          className={classes.quiet}
          aria-label={`Rename the block ${block.name}`}
          onClick={() => {
            setRenaming(true);
          }}
        >
          Rename
        </button>
      )}
      <button
        type="button"
        className={`${classes.button} bc:ml-auto`}
        aria-label={`Remove the block ${block.name}`}
        onClick={() => {
          void runCommand(core, "blockr.core-remove_block", { id });
        }}
      >
        Remove
      </button>
    </li>
  );
}
