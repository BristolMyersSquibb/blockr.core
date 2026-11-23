// The Edit board plugin's section of blocks:
// - The advanced action of core's board action Add block on the board, the
//   block picker, which adds a block of a class from the block registry,
//   under the ID and the name that the user gives, once the user submits it.
//   The plugin mounts it through mountAdvanced(), so that the plugin and the
//   menus share it, and only where core.boardActions.on() offers Add block on
//   the board
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows).
//   Core renders it again once it has added a block, ready for the next.
// - A table of the board's blocks, with each block's name, ID and class, which
//   a search narrows and which shows a page of rows at a time. Each row's "…"
//   opens the menu of its block, which offers Rename and Remove block among
//   core's board actions, and the board actions of other packages on a block.
//   An advanced action that the menu opens, such as the block picker of
//   Append block, shows in the row.
// Where the store or the command registry refuses an edit, the picker says why
// below its button. Where it refuses a board action that a row's menu ran,
// the plugin shows why in a banner below the table, naming the block, until
// the user opens the menu of one of the table's rows again.

import type { ReactElement } from "react";
import { styles } from "../../styles/index.ts";
import type { Block, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// Core's board action whose advanced action the section mounts, by its id, as
// any plugin names it.
const ADD_BLOCK = "blockr.core-add_block_action";

const COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "id", name: "ID" },
  { id: "class", name: "Class" },
] as const;

export function BlocksSection({ core, board }: { core: CoreApi; board: Board }): ReactElement {
  const registry = core.blockRegistry();
  const className = (block: Block): string => own(registry, block.registry_id)?.name ?? block.registry_id;
  const view = useTableView(Object.entries(board.blocks), ([id, block]) => `${block.name} ${id} ${className(block)}`);
  const count = Object.keys(board.blocks).length;
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Blocks" count={count}>
      {offers(core, ADD_BLOCK, BOARD) ? (
        <Part title="Add a block">
          <Advanced core={core} id={ADD_BLOCK} target={BOARD} />
        </Part>
      ) : null}
      <Part title="Blocks on the board">
        {count === 0 ? (
          <p className={styles.hint}>The board holds no blocks yet.</p>
        ) : (
          <>
            <TableSearch label="Search blocks" view={view} />
            {view.found === 0 ? (
              <p className={styles.hint}>No block matches the search.</p>
            ) : (
              <ObjectsTable
                core={core}
                menus={menus}
                kind="block"
                label="The board's blocks"
                columns={COLUMNS}
                rows={view.rows.map(([id, block]) => ({
                  id,
                  text: block.name,
                  menuLabel: `Actions of the block ${block.name}`,
                  cells: [
                    { content: <span className="bc:font-medium">{block.name}</span>, className: WRAP },
                    { content: id, className: "bc:font-mono bc:text-xs bc:whitespace-normal! bc:break-all" },
                    { content: className(block), className: `${WRAP} bc:text-text-muted` },
                  ],
                }))}
              />
            )}
            <Refusal refusal={menus.refusal} />
            <Pager label="Pages of blocks" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}
