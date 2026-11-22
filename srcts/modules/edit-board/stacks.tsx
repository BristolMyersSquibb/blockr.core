// The Edit board plugin's section of stacks:
// - The form that adds a stack, the advanced action of core's board action
//   Create stack on the board, which the plugin mounts where a menu of the
//   board offers Create stack.
// - A table of the board's stacks, with each stack's name, colour and blocks,
//   which a search narrows and which shows a page of rows at a time. Each
//   row's "…" opens the menu of its stack, which offers Rename, Edit stack and
//   Remove stack among core's board actions. An advanced action that the menu
//   opens, such as the form of Edit stack, shows in the row, which turns into
//   the form until done() ends it or the user closes it. Remove stack leaves
//   the stack's blocks on the board.
// The table draws a stack's colour with the swatch of core's React SDK.
// Where the store or the command registry refuses a board action that a row's
// menu ran, the table says so below itself, naming the stack, until the next
// board action that one of the table's menus runs. The forms say why below
// themselves.
// Under the board lock, it lists the stacks alone, and a row shows no "…"
// unless another package's board action applies to its stack.

import type { ReactElement } from "react";
import { ColorSwatch } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { blockName, Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// Core's board action whose advanced action the section mounts, by its id, as
// any plugin names it.
const CREATE_STACK = "blockr.core-add_stack_action";

const COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "blocks", name: "Blocks" },
] as const;

export function StacksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id, stack]) => [stack.name, id, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Stacks" count={stacks.length}>
      {locked || !offers(core, CREATE_STACK, BOARD) ? null : (
        <Part title="Add a stack">
          <Advanced core={core} id={CREATE_STACK} target={BOARD} />
        </Part>
      )}
      <Part title="Stacks on the board">
        {stacks.length === 0 ? (
          <p className={styles.hint}>The board has no stacks yet.</p>
        ) : (
          <>
            <TableSearch label="Search stacks" view={view} />
            {view.found === 0 ? (
              <p className={styles.hint}>No stack matches the search.</p>
            ) : (
              <ObjectsTable
                core={core}
                menus={menus}
                kind="stack"
                label="The board's stacks"
                columns={COLUMNS}
                rows={view.rows.map(([id, stack]) => ({
                  id,
                  text: stack.name,
                  menuLabel: `Actions of the stack ${stack.name}`,
                  cells: [
                    {
                      content: (
                        <span className="bc:flex bc:items-center bc:gap-1.5">
                          <Swatch color={stack.color} />
                          <span className="bc:font-medium">{stack.name}</span>
                        </span>
                      ),
                      className: WRAP,
                    },
                    { content: <StackBlocks board={board} blocks={stack.blocks} />, className: WRAP },
                  ],
                }))}
              />
            )}
            <Refusal refusal={menus.refusal} />
            <Pager label="Pages of stacks" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

// How many blocks of a stack its row names before it counts the rest.
const SHOWN_BLOCKS = 6;

// A stack's blocks, each by its name and its id, which tells apart blocks of
// one name, the first few of a large stack and how many more it holds.
function StackBlocks({ board, blocks }: { board: Board; blocks: readonly BlockId[] }): ReactElement {
  if (blocks.length === 0) return <span className="bc:text-text-muted">No blocks</span>;
  const rest = blocks.length - SHOWN_BLOCKS;
  return (
    <ul aria-label="Blocks" className="bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0">
      {blocks.slice(0, SHOWN_BLOCKS).map((block) => (
        <li key={block} className="bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs">
          {blockName(board, block)} <span className="bc:font-mono bc:text-text-muted">{block}</span>
        </li>
      ))}
      {rest > 0 ? <li className="bc:px-1 bc:text-xs bc:text-text-muted">and {rest} more</li> : null}
    </ul>
  );
}

// The colour of a stack, as the SDK's swatch, which a screen reader names by
// its colour, or a ring where it has none, which a screen reader skips.
// Each is a dot of 0.75rem. The SDK's swatch is a rounded square of 2rem,
// whose size and shape the dot's classes win over with Tailwind's important
// modifier.
function Swatch({ color }: { color: string | null }): ReactElement {
  return color === null ? <span aria-hidden="true" className={RING} /> : <ColorSwatch color={color} className={DOT} />;
}

const RING = "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default";
const DOT = "bc:size-3! bc:rounded-full!";
