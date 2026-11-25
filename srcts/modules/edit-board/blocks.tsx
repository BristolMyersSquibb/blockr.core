// The Edit board plugin's section of blocks:
// - The form that adds a block, the advanced action of the board action that
//   plays add_block_role on the board. That is core's Add block, unless the
//   app picks another board action for the role. The advanced action of Add
//   block is the block picker, which adds a block of a class from the block
//   registry, under the ID and the name that the user gives, once the user
//   submits it. The plugin mounts the form through mountAdvanced() by the
//   role's id, so that the plugin and the menus share it. The plugin mounts
//   the form only where core.boardActions.on() offers the board action that
//   plays the role on the board
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
// The section takes from the board only each block's ID, name and class. So
// it draws again only where one of those changes, and not for a change of a
// block's state.

import type { ReactElement } from "react";
import { useBoard } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board } from "../../types/board.ts";
import type { BlockRegistry, CoreApi } from "../../types/core-api.ts";
import { Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// The role of the board action whose advanced action the section mounts.
// Given the role's id, mountAdvanced() renders the advanced action of the
// board action that the app picks for the role, or else of core's Add block.
const ROLE = "add_block_role";

const COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "id", name: "ID" },
  { id: "class", name: "Class" },
] as const;

export function BlocksSection({ core }: { core: CoreApi }): ReactElement {
  const blocks = useBoard(core, (board) => blockRows(board, core.blockRegistry()));
  const adds = useBoard(core, () => offers(core, ROLE, BOARD));
  const view = useTableView(blocks, (block) => `${block.name} ${block.id} ${block.cls}`);
  const count = blocks.length;
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Blocks" count={count}>
      {adds ? (
        <Part title="Add a block">
          <Advanced core={core} role={ROLE} target={BOARD} />
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
                rows={view.rows.map((block) => ({
                  id: block.id,
                  text: block.name,
                  menuLabel: `Actions of the block ${block.name}`,
                  cells: [
                    { content: <span className="bc:font-medium">{block.name}</span>, className: WRAP },
                    { content: block.id, className: "bc:font-mono bc:text-xs bc:whitespace-normal! bc:break-all" },
                    { content: block.cls, className: `${WRAP} bc:text-text-muted` },
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

// Each block of the board as its row shows it: its ID, its name and the name
// of its class, or the class's registry id where the block registry lacks it.
function blockRows(board: Board, registry: BlockRegistry): { readonly id: BlockId; readonly name: string; readonly cls: string }[] {
  return Object.entries(board.blocks).map(([id, block]) => ({
    id,
    name: block.name,
    cls: own(registry, block.registry_id)?.name ?? block.registry_id,
  }));
}
