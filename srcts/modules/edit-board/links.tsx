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
//   A row's Edit button turns the row into the form that changes the link,
//   the advanced action of Edit link on the link, which applies each edit as
//   the user makes it, until the user closes it. Its Remove button removes
//   the link with remove_link.
// Where the store or the command registry refuses a removal, the table says
// so below itself, naming the link, until the user's next edit in the table.
// The forms say why below themselves.
// Under the board lock, it lists the links alone.

import type { ReactElement } from "react";
import { createGraph } from "../../graph/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../../types/board.ts";
import type { BlockRegistry, CoreApi } from "../../types/core-api.ts";
import { blockName, classes, isVariadicBlock, Pager, Refusal, TableSearch, useEdits, useTableView } from "../../ui/index.ts";
import type { Edits } from "../../ui/index.ts";
import { Advanced, offers, Part, RowAdvanced, Section, table, useRowEditing } from "./board.tsx";

// Core's board actions whose advanced actions the section mounts, by their
// ids, as any plugin names them.
const CREATE_LINK = "blockr.core-add_link_action";
const EDIT_LINK = "blockr.core-edit_link_action";
const BOARD = { board: true } as const;

export function LinksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const links = Object.entries(board.links);
  const view = useTableView(links, ([, link]) =>
    [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" "),
  );
  const variadic = variadicBlocks(board, core.blockRegistry());
  // The edits of the table's rows, which say why one was refused below the
  // table.
  const edits = useEdits(core);

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
              <table className={table.table}>
                <caption className="bc:sr-only">The board's links</caption>
                <thead>
                  <tr>
                    <th scope="col" className={table.head}>
                      From
                    </th>
                    <th scope="col" className={table.head}>
                      To
                    </th>
                    <th scope="col" className={table.head}>
                      Input
                    </th>
                    {locked ? null : (
                      <th scope="col" className={`${table.head} bc:w-24`}>
                        <span className="bc:sr-only">Actions</span>
                      </th>
                    )}
                  </tr>
                </thead>
                <tbody>
                  {view.rows.map(([id, link]) => (
                    <LinkRow
                      key={id}
                      core={core}
                      edits={edits}
                      board={board}
                      id={id}
                      link={link}
                      variadic={variadic.has(link.to)}
                      locked={locked}
                    />
                  ))}
                </tbody>
              </table>
            )}
            <Refusal refusal={edits.refusal} />
            <Pager label="Pages of links" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

function LinkRow({
  core,
  edits,
  board,
  id,
  link,
  variadic,
  locked,
}: {
  core: CoreApi;
  edits: Edits;
  board: Board;
  id: LinkId;
  link: Link;
  variadic: boolean;
  locked: boolean;
}): ReactElement {
  const { editing, open, close, button } = useRowEditing();
  const label = `${blockName(board, link.from)} → ${blockName(board, link.to)}`;
  if (editing) {
    return <RowAdvanced core={core} id={EDIT_LINK} target={{ link: id }} label={label} columns={4} onDone={close} />;
  }
  return (
    <tr aria-label={label} className={table.row}>
      <td className={`${table.cell} bc:break-words`}>
        <BlockLabel board={board} id={link.from} />
      </td>
      <td className={`${table.cell} bc:break-words`}>
        <BlockLabel board={board} id={link.to} />
      </td>
      <td className={`${table.cell} bc:break-words`}>
        {variadic ? <VariadicInput board={board} id={id} link={link} /> : <span className="bc:font-mono bc:text-xs">{link.input}</span>}
      </td>
      {locked ? null : (
        <td className={`${table.cell} bc:text-right bc:whitespace-nowrap`}>
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Edit the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
            ref={button}
            onClick={() => {
              // Opening the form is the user's next edit in the table, so the
              // table's refusal goes. The form says why its own edits are
              // refused.
              edits.clear();
              open();
            }}
          >
            Edit
          </button>
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
            onClick={() => {
              const title = `Could not remove the link from "${blockName(board, link.from)}" to "${blockName(board, link.to)}".`;
              void edits.run(title, "blockr.core-remove_link", { id });
            }}
          >
            Remove
          </button>
        </td>
      )}
    </tr>
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
