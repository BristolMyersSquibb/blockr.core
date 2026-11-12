// The Edit board plugin's section of blocks:
// - A form that adds a block of a class from the block registry, under the ID
//   and the name that the user gives, with add_block, once the user submits
//   it.
// - A table of the board's blocks, with each block's name, ID and class, which
//   a search narrows and which shows a page of rows at a time. The name of
//   each block renames it in place with rename_block, and a button removes it
//   with remove_block, which removes its links and its place in its stack too.

import { useCallback, useState } from "react";
import type { ReactElement } from "react";
import { BlockPicker } from "../../picker/index.ts";
import type { AddMode } from "../../picker/index.ts";
import { styles } from "../../styles/index.ts";
import type { Block, BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { classes, NameField, Pager, TableSearch, useTableView } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { Part, Section, table } from "./board.tsx";
import { runCommand } from "./run.ts";

const ADD: AddMode = { kind: "add" };

export function BlocksSection({ core, board }: { core: CoreApi; board: Board }): ReactElement {
  const run = useCallback((command: string, args: Json) => runCommand(core, command, args), [core]);
  const registry = core.blockRegistry();
  const className = (block: Block): string => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id, block]) => `${block.name} ${id} ${className(block)}`);
  const count = Object.keys(board.blocks).length;

  return (
    <Section title="Blocks" count={count}>
      <Part title="Add a block">
        <BlockPicker core={core} mode={ADD} run={run} />
      </Part>
      <Part title="Blocks on the board">
        {count === 0 ? (
          <p className={styles.hint}>The board holds no blocks yet.</p>
        ) : (
          <>
            <TableSearch label="Search blocks" view={view} />
            {view.found === 0 ? (
              <p className={styles.hint}>No block matches the search.</p>
            ) : (
              <table className={table.table}>
                <caption className="bc:sr-only">The board's blocks</caption>
                <thead>
                  <tr>
                    <th scope="col" className={table.head}>
                      Name
                    </th>
                    <th scope="col" className={table.head}>
                      ID
                    </th>
                    <th scope="col" className={table.head}>
                      Class
                    </th>
                    <th scope="col" className={`${table.head} bc:w-28`}>
                      <span className="bc:sr-only">Actions</span>
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {view.rows.map(([id, block]) => (
                    <BlockRow key={id} core={core} id={id} block={block} className={className(block)} />
                  ))}
                </tbody>
              </table>
            )}
            <Pager label="Pages of blocks" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

function BlockRow({ core, id, block, className }: { core: CoreApi; id: BlockId; block: Block; className: string }): ReactElement {
  const [renaming, setRenaming] = useState(false);
  return (
    <tr aria-label={block.name} className={table.row}>
      <td className={table.cell}>
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
          <span className="bc:font-medium bc:break-words">{block.name}</span>
        )}
      </td>
      <td className={`${table.cell} bc:font-mono bc:text-xs bc:break-all`}>{id}</td>
      <td className={`${table.cell} bc:text-text-muted`}>{className}</td>
      <td className={`${table.cell} bc:text-right bc:whitespace-nowrap`}>
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
            className={classes.quiet}
            aria-label={`Remove the block ${block.name}`}
            onClick={() => {
              void runCommand(core, "blockr.core-remove_block", { id });
            }}
          >
            Remove
          </button>
      </td>
    </tr>
  );
}
