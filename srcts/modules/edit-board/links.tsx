// The Edit board plugin's section of links:
// - The form that adds a link, the advanced action of the board action that
//   plays add_link_role on the board. That is core's Create link, unless the
//   app picks another board action for the role. The plugin mounts the form
//   by the role's id where a menu of the board offers the board action that
//   plays the role. A menu of the board offers Create link where the board
//   holds a block that a new link can join to another. Elsewhere the section
//   says why it holds no form.
// - A table of the board's links, with the block that each comes from, the
//   block it goes to and the input it feeds, which a search narrows and which
//   shows a page of rows at a time. For a link into a variadic block, the
//   table shows the link's place among the block's links.
//   Each row's "…" opens the menu of its link, which offers Insert block,
//   Edit link and Remove link among core's board actions. An advanced action
//   that the menu opens, such as the form of Edit link, shows in the row,
//   which turns into the form until done() ends it or the user closes it.
// Where the store or the command registry refuses a board action that a row's
// menu ran, the plugin shows why in a banner below the table, naming the
// link, until the user opens the menu of one of the table's rows again. The
// forms say why below themselves.
// Under the board lock, it lists the links alone, and a row shows no "…"
// unless another package's board action applies to its link.
// The section takes from the board only what its rows show of each link, and
// whether the board holds two blocks. So it draws again only where those
// change, and not for a change of a block's state.

import type { ReactElement } from "react";
import { createGraph } from "../../graph/index.ts";
import { useBoard } from "../../react/index.ts";
import type { BlockId, Board, LinkId } from "../../types/board.ts";
import type { BlockRegistry, CoreApi } from "../../types/core-api.ts";
import { classes, Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { blockName, isVariadicBlock } from "../../util/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// The role of the board action whose advanced action the section mounts.
// Given the role's id, mountAdvanced() renders the advanced action of the
// board action that the app picks for the role, or else of core's Create link.
const ROLE = "add_link_role";

const COLUMNS = [
  { id: "from", name: "From", rowHeader: true },
  { id: "to", name: "To" },
  { id: "input", name: "Input" },
] as const;

export function LinksSection({ core, locked }: { core: CoreApi; locked: boolean }): ReactElement {
  const links = useBoard(core, (board) => linkRows(board, core.blockRegistry()));
  const adds = useBoard(core, () => offers(core, ROLE, BOARD));
  // Whether the board holds two blocks, which a link joins.
  const two = useBoard(core, (board) => Object.keys(board.blocks).length >= 2);
  const view = useTableView(links, (link) => [link.fromName, link.from, link.toName, link.to, link.input].join(" "));
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Links" count={links.length}>
      {locked ? null : (
        <Part title="Add a link">
          {adds ? (
            <Advanced core={core} role={ROLE} target={BOARD} />
          ) : (
            <p className={classes.hint}>
              {two ? "No new link can join two blocks of the board." : "A link joins two blocks, so the board needs two blocks first."}
            </p>
          )}
        </Part>
      )}
      <Part title="Links on the board">
        {links.length === 0 ? (
          <p className={classes.hint}>No link joins two blocks yet.</p>
        ) : (
          <>
            <TableSearch label="Search links" view={view} />
            {view.found === 0 ? (
              <p className={classes.hint}>No link matches the search.</p>
            ) : (
              <ObjectsTable
                core={core}
                menus={menus}
                kind="link"
                label="The board's links"
                columns={COLUMNS}
                rows={view.rows.map((link) => ({
                  id: link.id,
                  text: `${link.fromName} → ${link.toName}`,
                  menuLabel: `Actions of the link from ${link.fromName} to ${link.toName}`,
                  cells: [
                    { content: <BlockLabel id={link.from} name={link.fromName} />, className: WRAP },
                    { content: <BlockLabel id={link.to} name={link.toName} />, className: WRAP },
                    {
                      content:
                        link.place === null ? (
                          <span className="bc:font-mono bc:text-xs">{link.input}</span>
                        ) : (
                          <VariadicInput input={link.input} place={link.place} />
                        ),
                      className: WRAP,
                    },
                  ],
                }))}
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

// Each link of the board as its row shows it: its ID, the blocks that it
// comes from and goes to, each by its ID and its name, and the input that it
// feeds. For a link into a variadic block, its place among the block's links,
// from 1, in which the block takes them, and null for a link into another
// block.
function linkRows(
  board: Board,
  registry: BlockRegistry,
): {
  readonly id: LinkId;
  readonly from: BlockId;
  readonly fromName: string;
  readonly to: BlockId;
  readonly toName: string;
  readonly input: string;
  readonly place: number | null;
}[] {
  const graph = createGraph(board);
  return Object.entries(board.links).map(([id, link]) => ({
    id,
    from: link.from,
    fromName: blockName(board, link.from),
    to: link.to,
    toName: blockName(board, link.to),
    input: link.input,
    place: isVariadicBlock(board, registry, link.to) ? graph.linksInto(link.to).indexOf(id) + 1 : null,
  }));
}

// A block as a table shows it: its name, and its id below.
function BlockLabel({ id, name }: { id: BlockId; name: string }): ReactElement {
  return (
    <span className="bc:grid">
      <span>{name}</span>
      <span className="bc:font-mono bc:text-xs bc:text-muted-foreground bc:break-all">{id}</span>
    </span>
  );
}

// The input of a link into a variadic block: its name, if any, and its place
// among the block's links, in which the block takes them.
function VariadicInput({ input, place }: { input: string; place: number }): ReactElement {
  return (
    <span className="bc:grid">
      {input === "" ? <span className="bc:text-muted-foreground">No name</span> : <span className="bc:font-mono bc:text-xs">{input}</span>}
      <span className="bc:text-xs bc:text-muted-foreground">Position {place}</span>
    </span>
  );
}
