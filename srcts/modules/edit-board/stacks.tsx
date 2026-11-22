// The Edit board plugin's section of stacks:
// - The form that adds a stack, the advanced action of core's board action
//   Create stack on the board, which the plugin mounts where a menu of the
//   board offers Create stack.
// - A table of the board's stacks, with each stack's name, colour and blocks,
//   which a search narrows and which shows a page of rows at a time. A row's
//   Edit button turns the row into the form that changes the stack, the
//   advanced action of Edit stack on the stack, which changes the stack once
//   the user saves it, until the user closes it. Its Remove button removes
//   the stack with remove_stack, which leaves the stack's blocks on the
//   board.
// The table draws a stack's colour with the swatch of core's React SDK.
// Where the store or the command registry refuses a removal, the table says
// so below itself, naming the stack, until the user's next edit in the table.
// The forms say why below themselves.
// Under the board lock, it lists the stacks alone.

import type { ReactElement } from "react";
import { ColorSwatch } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Stack, StackId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { blockName, classes, Pager, Refusal, TableSearch, useEdits, useTableView } from "../../ui/index.ts";
import type { Edits } from "../../ui/index.ts";
import { Advanced, BOARD, offers, Part, RowAdvanced, Section, table, useRowEditing } from "./board.tsx";

// Core's board actions whose advanced actions the section mounts, by their
// ids, as any plugin names them.
const CREATE_STACK = "blockr.core-add_stack_action";
const EDIT_STACK = "blockr.core-edit_stack_action";

export function StacksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id, stack]) => [stack.name, id, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  // The edits of the table's rows, which say why one was refused below the
  // table.
  const edits = useEdits(core);

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
              <table className={table.table}>
                <caption className="bc:sr-only">The board's stacks</caption>
                <thead>
                  <tr>
                    <th scope="col" className={table.head}>
                      Name
                    </th>
                    <th scope="col" className={table.head}>
                      Blocks
                    </th>
                    {locked ? null : (
                      <th scope="col" className={`${table.head} bc:w-24`}>
                        <span className="bc:sr-only">Actions</span>
                      </th>
                    )}
                  </tr>
                </thead>
                <tbody>
                  {view.rows.map(([id, stack]) => (
                    <StackRow key={id} core={core} edits={edits} board={board} id={id} stack={stack} locked={locked} />
                  ))}
                </tbody>
              </table>
            )}
            <Refusal refusal={edits.refusal} />
            <Pager label="Pages of stacks" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

function StackRow({
  core,
  edits,
  board,
  id,
  stack,
  locked,
}: {
  core: CoreApi;
  edits: Edits;
  board: Board;
  id: StackId;
  stack: Stack;
  locked: boolean;
}): ReactElement {
  const { editing, open, close, button } = useRowEditing();
  if (editing) {
    return <RowAdvanced core={core} id={EDIT_STACK} target={{ stack: id }} label={stack.name} columns={3} onDone={close} />;
  }
  return (
    <tr aria-label={stack.name} className={table.row}>
      <td className={`${table.cell} bc:break-words`}>
        <span className="bc:flex bc:items-center bc:gap-1.5">
          <Swatch color={stack.color} />
          <span className="bc:font-medium">{stack.name}</span>
        </span>
      </td>
      <td className={`${table.cell} bc:break-words`}>
        <StackBlocks board={board} blocks={stack.blocks} />
      </td>
      {locked ? null : (
        <td className={`${table.cell} bc:text-right bc:whitespace-nowrap`}>
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Edit the stack ${stack.name}`}
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
            aria-label={`Remove the stack ${stack.name}`}
            onClick={() => {
              void edits.run(`Could not remove the stack "${stack.name}".`, "blockr.core-remove_stack", { id });
            }}
          >
            Remove
          </button>
        </td>
      )}
    </tr>
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
