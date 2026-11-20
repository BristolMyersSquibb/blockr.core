// The Edit board plugin's section of stacks:
// - A form that adds a stack with add_stack, with its name, its colour and its
//   blocks, once the user submits it. It asks for no id, which the command
//   mints.
// - A table of the board's stacks, with each stack's name, colour and blocks,
//   which a search narrows and which shows a page of rows at a time. A row's
//   Edit button turns the row into the same form, which renames the stack with
//   rename_stack and sets its colour and blocks with change_stack once the
//   user saves it, each where it changed. Its Remove button removes the stack
//   with remove_stack, which leaves the stack's blocks on the board.
// The form picks the blocks in a select of several, whose list the user
// filters, so that it stays usable on a board of many blocks.
// A block sits in one stack at most. So the select lists the blocks in no
// stack first, and apart from them those that another stack holds, naming the
// stack, which a block that the user picks leaves.
// A new stack starts with a name and a colour that no other stack has, such
// as "Stack 2" and the first colour of a palette that no stack uses, which
// the user may change or, for the colour, leave out.
// Where the store or the command registry refuses an edit, the form that adds
// a stack says why below its buttons. Where it refuses an edit in the table, a
// removal or the save of a row's form, the table says so below itself, naming
// the stack, until the user's next edit in the table.
// Under the board lock, it lists the stacks alone.

import { useId, useState } from "react";
import type { ReactElement, SyntheticEvent } from "react";
import { ComboBox, ComboBoxSection, Form, TextField } from "../../react/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Stack, StackId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { classes, Pager, Refusal, TableSearch, useEdits, useSearch, useTableView } from "../../ui/index.ts";
import type { Edits } from "../../ui/index.ts";
import { Part, Section, table, useRowEditing } from "./board.tsx";
import { blockItem, blockName, blockRow, blockTexts } from "./links.tsx";
import type { BlockItem } from "./links.tsx";

// The colours that a new stack starts from, ColorBrewer's Set2, of which a new
// stack takes the first that no stack has.
const PALETTE = ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"] as const;

// The first colour of the palette that no stack has, or else the one that
// follows the number of stacks round the palette.
function suggestedColor(stacks: readonly Stack[]): string {
  const used = new Set(stacks.map((stack) => stack.color?.toLowerCase()));
  return PALETTE.find((color) => !used.has(color)) ?? PALETTE[stacks.length % PALETTE.length] ?? PALETTE[0];
}

// "Stack 1", or the first "Stack n" that no stack has.
function suggestedName(stacks: readonly Stack[]): string {
  const names = new Set(stacks.map((stack) => stack.name));
  let n = 1;
  while (names.has(`Stack ${String(n)}`)) n++;
  return `Stack ${String(n)}`;
}

export function StacksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const stacks = Object.entries(board.stacks);
  const view = useTableView(stacks, ([id, stack]) => [stack.name, id, ...stack.blocks.map((block) => blockName(board, block))].join(" "));
  // The edits of the table's rows, which say why one was refused below the
  // table.
  const edits = useEdits(core);

  return (
    <Section title="Stacks" count={stacks.length}>
      {locked ? null : (
        <Part title="Add a stack">
          <StackForm core={core} board={board} submitLabel="Add stack" />
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
    return (
      <tr aria-label={stack.name} className={table.row}>
        <td colSpan={3} className={`${table.cell} bc:py-2`}>
          <StackForm
            core={core}
            board={board}
            editing={{ id, stack }}
            tableEdits={edits}
            submitLabel="Save"
            onDone={close}
          />
        </td>
      </tr>
    );
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

// The colour of a stack, or a ring where it has none.
function Swatch({ color }: { color: string | null }): ReactElement {
  return (
    <span
      aria-hidden="true"
      className="bc:inline-block bc:size-3 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default"
      style={color === null ? undefined : { backgroundColor: color }}
    />
  );
}

// The form that adds a stack, or that changes the stack given.
// The form that adds a stack says why the store or the command registry
// refused it below its buttons. The form that changes a stack runs its edits
// through `tableEdits`, the edits of the table whose row it takes, and the
// table says why below itself.
function StackForm({
  core,
  board,
  editing,
  tableEdits,
  submitLabel,
  onDone,
}: {
  core: CoreApi;
  board: Board;
  editing?: { readonly id: StackId; readonly stack: Stack };
  tableEdits?: Edits;
  submitLabel: string;
  onDone?: () => void;
}): ReactElement {
  const registry = core.blockRegistry();
  const stacks = Object.values(board.stacks);
  const [name, setName] = useState(editing?.stack.name ?? "");
  const [color, setColor] = useState<string | null>(editing === undefined ? suggestedColor(stacks) : editing.stack.color);
  const [blocks, setBlocks] = useState<readonly BlockId[]>(editing?.stack.blocks ?? []);
  const [busy, setBusy] = useState(false);
  const ownEdits = useEdits(core);
  const edits = tableEdits ?? ownEdits;
  const fallbackName = editing === undefined ? suggestedName(stacks) : editing.stack.name;

  // The stack that holds each block, but the one that the form changes.
  const holder = new Map<BlockId, string>();
  for (const [id, stack] of Object.entries(board.stacks)) {
    if (id === editing?.id) continue;
    for (const block of stack.blocks) holder.set(block, stack.name);
  }
  // The blocks, those in no stack first, and those in another stack, which
  // say that they leave it.
  const options = Object.keys(board.blocks).map((id): BlockItem & { readonly free: boolean } => {
    const item = blockItem(board, registry, id);
    const other = holder.get(id);
    return other === undefined ? { ...item, free: true } : { ...item, free: false, detail: `${item.detail} · leaves the stack ${other}` };
  });
  const search = useSearch(options, blockTexts, "The board holds no block.");
  const sections = [
    { id: "free", title: "In no stack", items: search.items.filter((item) => item.free) },
    { id: "held", title: "In another stack", items: search.items.filter((item) => !item.free) },
  ].filter((section) => section.items.length > 0);
  // The blocks that the board still holds, in the order that the user picked
  // them.
  const kept = blocks.filter((block) => Object.hasOwn(board.blocks, block));

  async function submit(event: SyntheticEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    if (busy) return;
    const trimmed = name.trim();
    setBusy(true);
    let ran: boolean;
    if (editing === undefined) {
      const args: Record<string, Json> = { blocks: kept, color, name: trimmed === "" ? fallbackName : trimmed };
      ran = await edits.run("Could not add the stack.", "blockr.core-add_stack", args);
    } else {
      const { id, stack } = editing;
      const renamed = trimmed !== "" && trimmed !== stack.name;
      const changed: Record<string, Json> = {
        ...(sameBlocks(kept, stack.blocks) ? {} : { blocks: kept }),
        ...(color === stack.color ? {} : { color }),
      };
      // The sentence that names the edit, which names the stack as its row
      // did.
      const title = `Could not change the stack "${stack.name}".`;
      ran = !renamed || (await edits.run(title, "blockr.core-rename_stack", { id, name: trimmed }));
      if (ran && Object.keys(changed).length > 0) ran = await edits.run(title, "blockr.core-change_stack", { id, ...changed });
    }
    setBusy(false);
    if (!ran) return;
    if (editing === undefined) {
      setName("");
      setColor(suggestedColor(Object.values(core.store.board().stacks)));
      setBlocks([]);
    }
    onDone?.();
  }

  return (
    // React Aria's Form takes no handler of keys, so the box around it
    // hears Escape, which closes the form of a row.
    <div
      className="bc:contents"
      onKeyDown={(event) => {
        if (event.key === "Escape" && editing !== undefined && !event.defaultPrevented) onDone?.();
      }}
    >
      <Form
        aria-label={editing === undefined ? "Add stack" : "Change stack"}
        validationBehavior="aria"
        onSubmit={(event) => {
          void submit(event);
        }}
      >
        <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
          <TextField
            label="Name"
            value={name}
            placeholder={fallbackName}
            description={editing === undefined ? `Optional. "${fallbackName}" where left empty.` : undefined}
            autoComplete="off"
            onChange={setName}
          />
          <ColorField color={color} onChange={setColor} />
        </div>
        <ComboBox
          selectionMode="multiple"
          label="Blocks"
          {...search}
          items={sections}
          value={kept}
          placeholder="No blocks"
          description="A block sits in one stack at most."
          onChange={(keys) => {
            setBlocks(keys.map(String));
          }}
        >
          {(section) => (
            <ComboBoxSection id={section.id} title={section.title} items={section.items}>
              {blockRow}
            </ComboBoxSection>
          )}
        </ComboBox>
        <div className="bc:flex bc:gap-2">
          <button type="submit" className={classes.primary} disabled={busy}>
            {submitLabel}
          </button>
          {editing === undefined ? null : (
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                onDone?.();
              }}
            >
              Cancel
            </button>
          )}
        </div>
        {tableEdits === undefined ? <Refusal refusal={ownEdits.refusal} /> : null}
      </Form>
    </div>
  );
}

// A stack's colour, or none: a checkbox that gives the stack a colour, and the
// browser's colour picker, which picks it.
function ColorField({ color, onChange }: { color: string | null; onChange: (color: string | null) => void }): ReactElement {
  const id = useId();
  const [last, setLast] = useState(color ?? PALETTE[0]);
  return (
    <fieldset className={styles.fieldset}>
      <legend className={styles.legend}>Colour</legend>
      <div className="bc:flex bc:items-center bc:gap-2">
        <label className={styles.check}>
          <input
            type="checkbox"
            className={styles.checkbox}
            checked={color !== null}
            onChange={(event) => {
              onChange(event.target.checked ? last : null);
            }}
          />
          A colour of its own
        </label>
        <input
          id={id}
          type="color"
          aria-label="The stack's colour"
          disabled={color === null}
          value={color ?? last}
          className="bc:h-7 bc:w-10 bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:p-0.5 bc:disabled:cursor-default bc:disabled:opacity-50"
          onChange={(event) => {
            setLast(event.target.value);
            onChange(event.target.value);
          }}
        />
      </div>
    </fieldset>
  );
}

function sameBlocks(a: readonly BlockId[], b: readonly BlockId[]): boolean {
  return a.length === b.length && a.every((block, i) => block === b[i]);
}
