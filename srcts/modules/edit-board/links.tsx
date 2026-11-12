// The Edit board plugin's section of links:
// - A form that adds a link with add_link, once the user submits it. It asks
//   for no id, which the command mints.
// - A table of the board's links, with the block that each comes from, the
//   block it goes to and the input it feeds, which a search narrows and which
//   shows a page of rows at a time. A row's Edit button turns the row into a
//   form that changes the link with change_link once the user saves it, and
//   its Remove button removes the link with remove_link.
// - In the row of a link into a variadic block, buttons that move the link up
//   and down among the block's links with move_link, keeping its id and its
//   ends. The block's first link has no Move up, and its last no Move down.
// The form and the edit offer only links that the store takes:
// - As a link's target, a block with an input that no other link feeds, or a
//   variadic block, which takes any number of links. A block without inputs
//   takes none.
// - Neither end where the link would close a cycle, such as a link from a
//   block into a block upstream of it, nor a link from a block into itself.
// - Where the target's inputs are fixed, one that no other link feeds, which
//   the user picks where there are several.
// - Where the target is variadic, a name for the input, which the user may
//   give, as `b2` in `rbind(a, b2 = b)`, or leave empty for an input without
//   a name. A name that another link into the block gives already does not
//   do. Such a block takes its links in their order on the board, which the
//   table shows as each link's place among the block's links.
// Under the board lock, it lists the links alone.

import { useRef, useState } from "react";
import type { MouseEvent, ReactElement, RefObject, SubmitEvent } from "react";
import { flushSync } from "react-dom";
import { freeInputs } from "../../commands/index.ts";
import { createGraph } from "../../graph/index.ts";
import { focusFirstInvalid, inputNameError, isVariadic, namesInto, without } from "../../picker/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import type { RegistryEntry } from "../../types/registry.ts";
import { classes, Combobox, Pager, TableSearch, TextField, useTableView } from "../../ui/index.ts";
import type { ComboOption } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { Part, Section, table, useRowEditing } from "./board.tsx";
import { runCommand } from "./run.ts";

type Registry = Readonly<Record<string, RegistryEntry>>;

export function LinksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const links = Object.entries(board.links);
  const view = useTableView(links, ([, link]) =>
    [blockName(board, link.from), link.from, blockName(board, link.to), link.to, link.input].join(" "),
  );
  const variadic = variadicBlocks(board, core.blockRegistry());

  return (
    <Section title="Links" count={links.length}>
      {locked ? null : (
        <Part title="Add a link">
          <LinkForm core={core} board={board} submitLabel="Add link" />
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
                    <LinkRow key={id} core={core} board={board} id={id} link={link} variadic={variadic.has(link.to)} locked={locked} />
                  ))}
                </tbody>
              </table>
            )}
            <Pager label="Pages of links" view={view} />
          </>
        )}
      </Part>
    </Section>
  );
}

function LinkRow({
  core,
  board,
  id,
  link,
  variadic,
  locked,
}: {
  core: CoreApi;
  board: Board;
  id: LinkId;
  link: Link;
  variadic: boolean;
  locked: boolean;
}): ReactElement {
  const { editing, open, close, button } = useRowEditing();
  const label = `${blockName(board, link.from)} → ${blockName(board, link.to)}`;
  if (editing) {
    return (
      <tr aria-label={label} className={table.row}>
        <td colSpan={4} className={`${table.cell} bc:py-2`}>
          <LinkForm
            core={core}
            board={board}
            editing={{ id, link }}
            submitLabel="Save"
            onDone={close}
          />
        </td>
      </tr>
    );
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
        {variadic ? (
          <VariadicInput core={core} board={board} id={id} link={link} locked={locked} />
        ) : (
          <span className="bc:font-mono bc:text-xs">{link.input}</span>
        )}
      </td>
      {locked ? null : (
        <td className={`${table.cell} bc:text-right bc:whitespace-nowrap`}>
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Edit the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
            ref={button}
            onClick={open}
          >
            Edit
          </button>
          <button
            type="button"
            className={classes.quiet}
            aria-label={`Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
            onClick={() => {
              void runCommand(core, "blockr.core-remove_link", { id });
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
// among the block's links, in which the block takes them, with the buttons
// that move the link up and down among them, but under the board lock.
function VariadicInput({
  core,
  board,
  id,
  link,
  locked,
}: {
  core: CoreApi;
  board: Board;
  id: LinkId;
  link: Link;
  locked: boolean;
}): ReactElement {
  const into = createGraph(board).linksInto(link.to);
  const at = into.indexOf(id);
  return (
    <span className="bc:grid">
      {link.input === "" ? <span className="bc:text-text-muted">No name</span> : <span className="bc:font-mono bc:text-xs">{link.input}</span>}
      <span className="bc:flex bc:items-center bc:gap-1 bc:text-xs bc:text-text-muted">
        <span>Position {at + 1}</span>
        {locked ? null : <MoveButtons core={core} board={board} id={id} link={link} into={into} at={at} />}
      </span>
    </span>
  );
}

// The buttons that move the link `id` up and down among the links into its
// block, `into`, where it is at the index `at`, with move_link. Move up puts
// it before the link before it, and Move down before the link after the next
// one, or last where the next one is the block's last link.
// A move that takes the link to the first or the last place takes away the
// button that the user pressed. The focus then goes to the row's other one,
// so that the keyboard stays at the row.
function MoveButtons({
  core,
  board,
  id,
  link,
  into,
  at,
}: {
  core: CoreApi;
  board: Board;
  id: LinkId;
  link: Link;
  into: readonly LinkId[];
  at: number;
}): ReactElement {
  const up = useRef<HTMLButtonElement | null>(null);
  const down = useRef<HTMLButtonElement | null>(null);
  const previous = into[at - 1];
  const next = into[at + 1];
  const after = into[at + 2];
  const label = `the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`;

  function move(event: MouseEvent<HTMLButtonElement>, before: LinkId | undefined, other: RefObject<HTMLButtonElement | null>): void {
    const pressed = event.currentTarget;
    void runCommand(core, "blockr.core-move_link", { id, ...(before === undefined ? {} : { before }) }).then((ran) => {
      if (ran && !pressed.isConnected) other.current?.focus();
    });
  }

  return (
    <>
      {previous === undefined ? null : (
        <button
          type="button"
          ref={up}
          className={`${classes.quiet} bc:px-1`}
          aria-label={`Move ${label} up`}
          onClick={(event) => {
            move(event, previous, down);
          }}
        >
          <span aria-hidden="true">↑</span>
        </button>
      )}
      {next === undefined ? null : (
        <button
          type="button"
          ref={down}
          className={`${classes.quiet} bc:px-1`}
          aria-label={`Move ${label} down`}
          onClick={(event) => {
            move(event, after, up);
          }}
        >
          <span aria-hidden="true">↓</span>
        </button>
      )}
    </>
  );
}

type Errors = Partial<Record<"from" | "to" | "input", string>>;

// The form that adds a link, or that changes the link given, which offers
// only links that the store takes (above).
function LinkForm({
  core,
  board,
  editing,
  submitLabel,
  onDone,
}: {
  core: CoreApi;
  board: Board;
  editing?: { readonly id: LinkId; readonly link: Link };
  submitLabel: string;
  onDone?: () => void;
}): ReactElement {
  const registry = core.blockRegistry();
  const except = editing?.id;
  const [from, setFrom] = useState<BlockId | undefined>(editing?.link.from);
  const [to, setTo] = useState<BlockId | undefined>(editing?.link.to);
  const [input, setInput] = useState<string | undefined>(editing?.link.input);
  const [name, setName] = useState(editing !== undefined && isVariadicBlock(board, registry, editing.link.to) ? editing.link.input : "");
  const [errors, setErrors] = useState<Errors>({});
  const [busy, setBusy] = useState(false);

  // The blocks that the form offers, with the links of the board but the one
  // that the form changes, which is about to move.
  const others = withoutLink(board, except);
  const graph = createGraph(others);
  const held = (block: BlockId | undefined): block is BlockId => block !== undefined && own(board.blocks, block) !== undefined;
  const source = held(from) ? from : undefined;
  const target = held(to) ? to : undefined;
  const upstream = source === undefined ? new Set<BlockId>() : graph.withUpstream([source]);
  const downstream = target === undefined ? new Set<BlockId>() : graph.withDownstream([target]);

  const fromOptions = Object.entries(board.blocks)
    .filter(([id]) => !downstream.has(id))
    .map(([id]) => blockOption(board, registry, id));
  const toOptions = Object.entries(board.blocks)
    .filter(([id]) => !upstream.has(id) && takesLink(others, registry, id))
    .map(([id]) => blockOption(board, registry, id));

  const free = target === undefined ? [] : (freeInputs(others, registry, target) ?? []);
  const variadic = isVariadic(free);
  const chosen = variadic ? undefined : input !== undefined && free.includes(input) ? input : free[0];
  const taken = target === undefined ? [] : namesInto(board, target, except);

  function check(): Errors {
    return {
      ...(source === undefined ? { from: "Pick the block that the link comes from." } : {}),
      ...(target === undefined ? { to: "Pick the block that the link goes to." } : {}),
      ...(variadic ? optional("input", inputNameError(name, taken)) : {}),
    };
  }

  async function submit(event: SubmitEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    const form = event.currentTarget;
    const found = check();
    flushSync(() => {
      setErrors(found);
    });
    if (Object.keys(found).length > 0 || source === undefined || target === undefined || busy) {
      focusFirstInvalid(form);
      return;
    }
    const linkInput = variadic ? name.trim() : chosen;
    const args: Record<string, Json> = { from: source, to: target, ...(linkInput === undefined ? {} : { input: linkInput }) };
    setBusy(true);
    const ran =
      editing === undefined
        ? await runCommand(core, "blockr.core-add_link", args)
        : await runCommand(core, "blockr.core-change_link", { id: editing.id, ...args });
    setBusy(false);
    if (!ran) return;
    if (editing === undefined) {
      setTo(undefined);
      setInput(undefined);
      setName("");
      setErrors({});
    }
    onDone?.();
  }

  return (
    <form
      aria-label={editing === undefined ? "Add link" : "Change link"}
      noValidate
      className="bc:grid bc:gap-3"
      onSubmit={(event) => {
        void submit(event);
      }}
      onKeyDown={(event) => {
        if (event.key === "Escape" && editing !== undefined && !event.defaultPrevented) onDone?.();
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:items-start bc:gap-2">
        <Combobox
          label="From"
          options={fromOptions}
          value={source}
          placeholder="Search blocks"
          empty="No block matches."
          error={errors.from}
          onChange={(value) => {
            setFrom(value);
            setErrors((now) => without(now, "from"));
          }}
        />
        <Combobox
          label="To"
          options={toOptions}
          value={target}
          placeholder="Search blocks"
          empty="No block that takes a link matches."
          error={errors.to}
          hint={toOptions.length === 0 ? "No block takes a link from this block." : undefined}
          onChange={(value) => {
            setTo(value);
            setInput(undefined);
            setErrors((now) => without(without(now, "to"), "input"));
          }}
        />
      </div>
      {target === undefined ? null : variadic ? (
        <TextField
          label="Input name"
          value={name}
          hint={`Optional. The block "${blockName(board, target)}" takes any number of links, each by its name or, where it has none, by its position.`}
          error={errors.input}
          identifier
          onChange={(value) => {
            setName(value);
            setErrors((now) => without(now, "input"));
          }}
          onLeave={() => {
            setErrors((now) => ({ ...now, ...optional("input", inputNameError(name, taken)) }));
          }}
        />
      ) : (
        <label className={styles.field}>
          <span className={styles.label}>Input</span>
          <select
            className={styles.box}
            value={chosen}
            disabled={free.length < 2}
            onChange={(event) => {
              setInput(event.target.value);
            }}
          >
            {free.map((option) => (
              <option key={option} value={option}>
                {option}
              </option>
            ))}
          </select>
        </label>
      )}
      {editing === undefined && Object.keys(board.blocks).length < 2 ? (
        <p className={styles.hint}>A link joins two blocks, so the board needs two blocks first.</p>
      ) : null}
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
    </form>
  );
}

// A block as a dropdown offers it: by its name, with its id and its class.
export function blockOption(board: Pick<Board, "blocks">, registry: Registry, id: BlockId): ComboOption {
  const block = own(board.blocks, id);
  const cls = block === undefined ? undefined : (own(registry, block.registry_id)?.name ?? block.registry_id);
  return {
    value: id,
    label: block?.name ?? id,
    detail: cls === undefined ? id : `${id} · ${cls}`,
    keywords: [id],
  };
}

// Whether the block takes a link: a variadic block always, and another block
// where one of its inputs is free. A block without inputs takes none.
function takesLink(board: Pick<Board, "blocks" | "links">, registry: Registry, id: BlockId): boolean {
  return (freeInputs(board, registry, id)?.length ?? 0) > 0;
}

function isVariadicBlock(board: Pick<Board, "blocks">, registry: Registry, id: BlockId): boolean {
  const block = own(board.blocks, id);
  return block !== undefined && (own(registry, block.registry_id)?.inputs.includes("...") ?? false);
}

// The board's variadic blocks.
function variadicBlocks(board: Pick<Board, "blocks">, registry: Registry): ReadonlySet<BlockId> {
  return new Set(Object.keys(board.blocks).filter((id) => isVariadicBlock(board, registry, id)));
}

// The board without the link given, as one that a form is about to change.
function withoutLink(board: Board, id: LinkId | undefined): Board {
  if (id === undefined || !Object.hasOwn(board.links, id)) return board;
  const links = Object.fromEntries(Object.entries(board.links).filter(([other]) => other !== id));
  return { ...board, links };
}

function optional<K extends string>(key: K, error: string | undefined): Partial<Record<K, string>> {
  return error === undefined ? {} : ({ [key]: error } as Partial<Record<K, string>>);
}

// A block's name, or its id where the board lacks it.
export function blockName(board: Pick<Board, "blocks">, block: BlockId): string {
  return own(board.blocks, block)?.name ?? block;
}
