// The Edit board plugin's section of links. It lists each link with the block
// it comes from, the block it goes to and the input it feeds, which
// change_link changes as the user picks another, and remove_link removes one.
// Its form adds a link with add_link, and asks for no id, which the command
// mints.
// For a link and for the form alike, it offers the inputs of the target that
// no other link feeds. It offers none for a variadic block, which takes its
// links in order, and as a link's target only a block with an input that no
// other link feeds.
// Under the board lock, it lists the links alone.

import { useState } from "react";
import type { ReactElement } from "react";
import { freeInputs } from "../../commands/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board, Link, LinkId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { classes } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { Section } from "./board.tsx";
import { runCommand } from "./run.ts";

export function LinksSection({ core, board, locked }: { core: CoreApi; board: Board; locked: boolean }): ReactElement {
  const links = Object.entries(board.links);
  return (
    <Section title="Links">
      {links.length === 0 ? (
        <p className={styles.hint}>No link joins two blocks yet.</p>
      ) : (
        <ul aria-label="Links" className={classes.list}>
          {links.map(([id, link]) =>
            locked ? (
              <li key={id} className="bc:text-sm">
                <LinkText board={board} link={link} />
              </li>
            ) : (
              <LinkItem key={id} core={core} board={board} id={id} link={link} />
            ),
          )}
        </ul>
      )}
      {locked ? null : <AddLink core={core} board={board} />}
    </Section>
  );
}

// A link as the user reads it: the block it comes from, the block it goes to,
// and the input it feeds, unless it goes to a variadic block.
function LinkText({ board, link }: { board: Board; link: Link }): ReactElement {
  return (
    <>
      {blockName(board, link.from)} → {blockName(board, link.to)}
      {link.input === "" ? null : <span className="bc:text-text-muted"> as {link.input}</span>}
    </>
  );
}

function LinkItem({ core, board, id, link }: { core: CoreApi; board: Board; id: LinkId; link: Link }): ReactElement {
  const registry = core.blockRegistry();
  const free = freeInputs(board, registry, link.to, id) ?? [];

  function change(args: Readonly<Record<string, Json>>): void {
    void runCommand(core, "blockr.core-change_link", { id, ...args });
  }

  // A link that goes to another block feeds the first input of that block that
  // no other link feeds, which the user may then change, or its one input
  // where it is variadic, which the command finds.
  function retarget(to: BlockId): void {
    const [first] = freeInputs(board, registry, to, id) ?? [];
    change(first === undefined || first === "" ? { to } : { to, input: first });
  }

  return (
    <li
      aria-label={`${blockName(board, link.from)} → ${blockName(board, link.to)}`}
      className="bc:flex bc:flex-wrap bc:items-end bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:p-2"
    >
      <BlockSelect
        label="From"
        board={board}
        value={link.from}
        disabled={(block) => block === link.to}
        onChange={(from) => {
          change({ from });
        }}
      />
      <BlockSelect
        label="To"
        board={board}
        value={link.to}
        disabled={(block) => block === link.from || (block !== link.to && (freeInputs(board, registry, block, id)?.length ?? 0) === 0)}
        onChange={retarget}
      />
      {isVariadic(free) ? null : (
        <InputSelect
          inputs={free}
          value={link.input}
          onChange={(input) => {
            change({ input });
          }}
        />
      )}
      <button
        type="button"
        className={`${classes.button} bc:ml-auto`}
        aria-label={`Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
        onClick={() => {
          void runCommand(core, "blockr.core-remove_link", { id });
        }}
      >
        Remove
      </button>
    </li>
  );
}

// The form that adds a link, which offers the inputs of its target that no
// link feeds.
// It stays drawn as the board changes, so a block that the user picked and
// that the board no longer holds gives way to the board's first block, or its
// second for the target, as for a form on a board that held no blocks yet.
function AddLink({ core, board }: { core: CoreApi; board: Board }): ReactElement {
  const ids = Object.keys(board.blocks);
  const [pickedFrom, setFrom] = useState<BlockId | undefined>(undefined);
  const [pickedTo, setTo] = useState<BlockId | undefined>(undefined);
  const [input, setInput] = useState<string | undefined>(undefined);
  const held = (block: BlockId | undefined): block is BlockId => block !== undefined && Object.hasOwn(board.blocks, block);
  const from = held(pickedFrom) ? pickedFrom : (ids[0] ?? "");
  const to = held(pickedTo) ? pickedTo : (ids[1] ?? ids[0] ?? "");

  const free = freeInputs(board, core.blockRegistry(), to) ?? [];
  const chosen = isVariadic(free) ? undefined : input !== undefined && free.includes(input) ? input : free[0];
  const args: Record<string, Json> = { from, to, ...(chosen === undefined ? {} : { input: chosen }) };

  return (
    <form
      aria-label="Add link"
      className="bc:grid bc:gap-2 bc:border-t bc:border-border-default bc:pt-2"
      onSubmit={(event) => {
        event.preventDefault();
        setInput(undefined);
        void runCommand(core, "blockr.core-add_link", args);
      }}
    >
      <div className="bc:grid bc:grid-cols-2 bc:gap-2">
        <BlockSelect label="From" board={board} value={from} onChange={setFrom} />
        <BlockSelect
          label="To"
          board={board}
          value={to}
          onChange={(next) => {
            setTo(next);
            setInput(undefined);
          }}
        />
      </div>
      {isVariadic(free) ? null : <InputSelect inputs={free} value={chosen ?? ""} onChange={setInput} />}
      <div>
        <button type="submit" className={classes.primary} disabled={!core.commands.enabled("blockr.core-add_link", args)}>
          Add link
        </button>
      </div>
    </form>
  );
}

function BlockSelect({
  label,
  board,
  value,
  disabled,
  onChange,
}: {
  label: string;
  board: Board;
  value: BlockId;
  // Whether the user may not pick the block.
  disabled?: (block: BlockId) => boolean;
  onChange: (block: BlockId) => void;
}): ReactElement {
  return (
    <label className={styles.field}>
      <span className={styles.label}>{label}</span>
      <select
        className={styles.box}
        value={value}
        onChange={(event) => {
          onChange(event.target.value);
        }}
      >
        {Object.entries(board.blocks).map(([id, block]) => (
          <option key={id} value={id} disabled={id !== value && disabled?.(id) === true}>
            {block.name} ({id})
          </option>
        ))}
      </select>
    </label>
  );
}

// The input that a link feeds, among the inputs of its target that no other
// link feeds.
function InputSelect({
  inputs,
  value,
  onChange,
}: {
  inputs: readonly string[];
  value: string;
  onChange: (input: string) => void;
}): ReactElement {
  return (
    <label className={styles.field}>
      <span className={styles.label}>Input</span>
      <select
        className={styles.box}
        value={value}
        disabled={inputs.length === 0}
        onChange={(event) => {
          onChange(event.target.value);
        }}
      >
        {inputs.length === 0 ? <option value="">No free input</option> : null}
        {inputs.map((name) => (
          <option key={name} value={name}>
            {name}
          </option>
        ))}
      </select>
    </label>
  );
}

// Whether the free inputs are those of a variadic block, the one empty input
// that takes its links in order.
function isVariadic(free: readonly string[]): boolean {
  return free.length === 1 && free[0] === "";
}

// A block's name, or its id where the board lacks it.
export function blockName(board: Pick<Board, "blocks">, block: BlockId): string {
  return own(board.blocks, block)?.name ?? block;
}
