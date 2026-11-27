// The Edit board plugin's section of stacks:
// - The form that adds a stack, the advanced action of the board action that
//   plays add_stack_role on the board. That is core's Create stack, unless
//   the app picks another board action for the role. The plugin mounts the
//   form by the role's id where a menu of the board offers the board action
//   that plays the role.
// - A table of the board's stacks, with each stack's name, colour and blocks,
//   which a search narrows and which shows a page of rows at a time. Each
//   row's "…" opens the menu of its stack, which offers Rename, Edit stack and
//   Remove stack among core's board actions. An advanced action that the menu
//   opens, such as the form of Edit stack, shows in the row, which turns into
//   the form until done() ends it or the user closes it. Remove stack leaves
//   the stack's blocks on the board.
// The table draws a stack's colour with blockr.ui's swatch.
// Where the store or the command registry refuses a board action that a row's
// menu ran, the plugin shows why in a banner below the table, naming the
// stack, until the user opens the menu of one of the table's rows again. The
// forms say why below themselves.
// Under the board lock, it lists the stacks alone, and a row shows no "…"
// unless another package's board action applies to its stack.
// The section takes from the board only what its rows show of each stack. So
// it draws again only where that changes, and not for a change of a block's
// state.

import type { ReactElement } from "react";
import { ColorSwatch } from "@blockr/ui";
import { useBoard } from "../../react/index.ts";
import type { BlockId, Board, StackId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { blockName, classes, Pager, Refusal, TableSearch, useTableView } from "../../ui/index.ts";
import { Advanced, BOARD, ObjectsTable, offers, Part, Section, WRAP } from "./board.tsx";
import { useMenus } from "./menus.tsx";

// The role of the board action whose advanced action the section mounts.
// Given the role's id, mountAdvanced() renders the advanced action of the
// board action that the app picks for the role, or else of core's Create
// stack.
const ROLE = "add_stack_role";

const COLUMNS = [
  { id: "name", name: "Name", rowHeader: true },
  { id: "blocks", name: "Blocks" },
] as const;

export function StacksSection({ core, locked }: { core: CoreApi; locked: boolean }): ReactElement {
  const stacks = useBoard(core, stackRows);
  const adds = useBoard(core, () => offers(core, ROLE, BOARD));
  const view = useTableView(stacks, (stack) => [stack.name, stack.id, ...stack.blocks.map((block) => block.name)].join(" "));
  // The menus of the table's rows, which say why one was refused below the
  // table.
  const menus = useMenus(core);

  return (
    <Section title="Stacks" count={stacks.length}>
      {locked || !adds ? null : (
        <Part title="Add a stack">
          <Advanced core={core} role={ROLE} target={BOARD} />
        </Part>
      )}
      <Part title="Stacks on the board">
        {stacks.length === 0 ? (
          <p className={classes.hint}>The board has no stacks yet.</p>
        ) : (
          <>
            <TableSearch label="Search stacks" view={view} />
            {view.found === 0 ? (
              <p className={classes.hint}>No stack matches the search.</p>
            ) : (
              <ObjectsTable
                core={core}
                menus={menus}
                kind="stack"
                label="The board's stacks"
                columns={COLUMNS}
                rows={view.rows.map((stack) => ({
                  id: stack.id,
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
                    { content: <StackBlocks blocks={stack.blocks} />, className: WRAP },
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

// Each stack of the board as its row shows it: its ID, its name, its colour
// and its blocks, each by its ID and its name.
function stackRows(board: Board): {
  readonly id: StackId;
  readonly name: string;
  readonly color: string | null;
  readonly blocks: readonly { readonly id: BlockId; readonly name: string }[];
}[] {
  return Object.entries(board.stacks).map(([id, stack]) => ({
    id,
    name: stack.name,
    color: stack.color,
    blocks: stack.blocks.map((block) => ({ id: block, name: blockName(board, block) })),
  }));
}

// How many blocks of a stack its row names before it counts the rest.
const SHOWN_BLOCKS = 6;

// A stack's blocks, each by its name and its id, which tells apart blocks of
// one name, the first few of a large stack and how many more it holds.
function StackBlocks({ blocks }: { blocks: readonly { readonly id: BlockId; readonly name: string }[] }): ReactElement {
  if (blocks.length === 0) return <span className="bc:text-muted-foreground">No blocks</span>;
  const rest = blocks.length - SHOWN_BLOCKS;
  return (
    <ul aria-label="Blocks" className="bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0">
      {blocks.slice(0, SHOWN_BLOCKS).map((block) => (
        <li key={block.id} className="bc:rounded-sm bc:border bc:border-border bc:px-1 bc:text-xs">
          {block.name} <span className="bc:font-mono bc:text-muted-foreground">{block.id}</span>
        </li>
      ))}
      {rest > 0 ? <li className="bc:px-1 bc:text-xs bc:text-muted-foreground">and {rest} more</li> : null}
    </ul>
  );
}

// The colour of a stack, as blockr.ui's swatch, which a screen reader names by
// its colour, or a ring where it has none, which a screen reader skips.
// Each is a dot of 0.75rem. The swatch of blockr.ui is a rounded square of
// 2rem, whose size and shape the dot's classes win over with Tailwind's
// important modifier.
function Swatch({ color }: { color: string | null }): ReactElement {
  return color === null ? <span aria-hidden="true" className={RING} /> : <ColorSwatch color={color} className={DOT} />;
}

const RING = "bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border";
const DOT = "bc:size-3! bc:rounded-full!";
