// The Edit board plugin's section of links:
// - The form that adds a link, the advanced action of core's board action
//   Create link on the board, which the plugin mounts where a menu of the
//   board offers Create link, as where the board holds a block that a new
//   link can join to another. Elsewhere the section says why it holds no
//   form.
// - A table of the board's links, with the block that each comes from, the
//   block it goes to and the input it feeds, which a search narrows and which
//   shows a page of rows at a time. For a link into a variadic block, the
//   table shows the link's place among the block's links.
//   Each row's "…" opens the menu of its link, which offers Insert block,
//   Edit link and Remove link among core's board actions. An advanced action
//   that the menu opens, such as the form of Edit link, shows in the row,
//   which turns into the form until done() ends it or the user closes it.
// Where the store or the command registry refuses a board action that a row's
// menu ran, the table says so below itself, naming the link, until the next
// board action that one of the table's menus runs. The forms say why below
// themselves.
// Under the board lock, it lists the links alone, and a row shows no "…"
// unless another package's board action applies to its link.

import type { ReactElement } from "react";
import { createGraph } from "../../graph/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../../types/board.ts";
import type { BlockRegistry, CoreApi } from "../../types/core-api.ts";
import { blockName, isVariadicBlock, Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// Core's board action whose advanced action the section mounts, by its id, as
// any plugin names it.
const CREATE_LINK = "blockr.core-add_link_action";

const COLUMNS = [
  { id: "from", name: "From", rowHeader: true },
  { id: "to", name: "To" },
  { id: "input", name: "Input" },
] as const;

export function LinksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const links = Object.entries(board.links);
  const view = useTableView(links, ([, link]) =>
    [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" "),
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Links" count={links.length}>
      {locked ? null : (
        <Part title="Add a link">
          {offers(core, CREATE_LINK, BOARD) ? (
            <Advanced core={core} id={CREATE_LINK} target={BOARD} />
          ) : (
            <p className={styles.hint}>
              {Object.keys(board.blocks).length < 2
                ? "A link joins two blocks, so the board needs two blocks first."
                : "No new link can join two blocks of the board."}
            </p>
          )}
        </Part>
      )}
      <Part title="Links on the board">
        {links.length === 0 ? (
          <p className={styles.hint}>No link joins two blocks yet.</p>
        ) : (
          <>
            <TableSearch label="Search links" view={view} />
            {view.found === 0 ? (
              <p className={styles.hint}>No link matches the search.</p>
            ) : (
              <ObjectsTable
                core={core}
                menus={menus}
                kind="link"
                label="The board's links"
                columns={COLUMNS}
                rows={view.rows.map(([id, link]) => {
                  const from = blockName(board, link.from);
                  const to = blockName(board, link.to);
                  return {
                    id,
                    text: `${from} → ${to}`,
                    menuLabel: `Actions of the link from ${from} to ${to}`,
                    cells: [
                      { content: <BlockLabel board={board} id={link.from} />, className: WRAP },
                      { content: <BlockLabel board={board} id={link.to} />, className: WRAP },
                      {
                        content: variadic.has(link.to) ? (
                          <VariadicInput board={board} id={id} link={link} />
                        ) : (
                          <span className="bc:font-mono bc:text-xs">{link.input}</span>
                        ),
                        className: WRAP,
                      },
                    ],
                  };
                })}
              />
            )}
            <Refusal refusal={menus.refusal} />
            <Pager label="Pages of links" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

// A block as a table shows it: its name, and its id below.
function BlockLabel({ board, id }: { board: Board; id: BlockId }): ReactElement {
  return (
    <span className="bc:grid">
      <span>{blockName(board, id)}</span>
      <span className="bc:font-mono bc:text-xs bc:text-text-muted bc:break-all">{id}</span>
    </span>
  );
}

// The input of a link into a variadic block: its name, if any, and its place
// among the block's links, in which the block takes them.
function VariadicInput({ board, id, link }: { board: Board; id: LinkId; link: Link }): ReactElement {
  const place = createGraph(board).linksInto(link.to).indexOf(id) + 1;
  return (
    <span className="bc:grid">
      {link.input === "" ? <span className="bc:text-text-muted">No name</span> : <span className="bc:font-mono bc:text-xs">{link.input}</span>}
      <span className="bc:text-xs bc:text-text-muted">Position {place}</span>
    </span>
  );
}

// The board's variadic blocks.
function variadicBlocks(board: Pick<Board, "blocks">, registry: BlockRegistry): ReadonlySet<BlockId> {
  return new Set(Object.keys(board.blocks).filter((id) => isVariadicBlock(board, registry, id)));
}
