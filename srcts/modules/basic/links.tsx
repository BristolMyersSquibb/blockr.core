// The dialog that edits the board's links through core's commands.
// It lists each link with the blocks it joins and the input it feeds, and
// remove_link removes one.
// Its form adds a link with add_link. The form offers the inputs of the target
// that no link feeds. It offers none for a variadic block, which takes its
// links in order.

import { useState } from "react";
import type { ReactElement } from "react";
import { freeInputs } from "../../commands/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId, Board } from "../../types/board.ts";
import type { Json } from "../../types/json.ts";
import { own } from "../../util/index.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { useBoard } from "./hooks.ts";
import { classes, Dialog } from "./ui.tsx";

export function LinksDialog({ onClose }: { onClose: () => void }): ReactElement {
  const front = useFrontEnd();
  const { core } = front;
  const board = useBoard(core);
  const ids = Object.keys(board.blocks);
  const [from, setFrom] = useState<BlockId>(ids[0] ?? "");
  const [to, setTo] = useState<BlockId>(ids[1] ?? ids[0] ?? "");
  const [input, setInput] = useState<string | undefined>(undefined);

  const free = freeInputs(board, core.blockRegistry(), to) ?? [];
  const variadic = free.length === 1 && free[0] === "";
  const chosen = variadic ? undefined : input !== undefined && free.includes(input) ? input : free[0];
  const args: Record<string, Json> = { from, to, ...(chosen === undefined ? {} : { input: chosen }) };
  const links = Object.entries(board.links);

  return (
    <Dialog title="Links" onClose={onClose}>
      {links.length === 0 ? (
        <p className={styles.hint}>No link joins two blocks yet.</p>
      ) : (
        <ul aria-label="Links" className={classes.list}>
          {links.map(([id, link]) => (
            <li key={id} className="bc:flex bc:items-center bc:gap-2 bc:text-sm">
              <span className="bc:flex-1">
                {blockName(board, link.from)} → {blockName(board, link.to)}
                {link.input === "" ? null : <span className="bc:text-text-muted"> as {link.input}</span>}
              </span>
              <button
                type="button"
                className={classes.button}
                aria-label={`Remove the link from ${blockName(board, link.from)} to ${blockName(board, link.to)}`}
                onClick={() => {
                  void runCommand(front, "blockr.core-remove_link", { id });
                }}
              >
                Remove
              </button>
            </li>
          ))}
        </ul>
      )}
      <form
        aria-label="Add link"
        className="bc:grid bc:gap-2 bc:border-t bc:border-border-default bc:pt-3"
        onSubmit={(event) => {
          event.preventDefault();
          setInput(undefined);
          void runCommand(front, "blockr.core-add_link", args);
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
        {variadic ? null : (
          <label className={styles.field}>
            <span className={styles.label}>Input</span>
            <select
              className={styles.box}
              value={chosen ?? ""}
              disabled={free.length === 0}
              onChange={(event) => {
                setInput(event.target.value);
              }}
            >
              {free.length === 0 ? <option value="">No free input</option> : null}
              {free.map((name) => (
                <option key={name} value={name}>
                  {name}
                </option>
              ))}
            </select>
          </label>
        )}
        <div>
          <button type="submit" className={classes.primary} disabled={!core.commands.enabled("blockr.core-add_link", args)}>
            Add link
          </button>
        </div>
      </form>
    </Dialog>
  );
}

function BlockSelect({
  label,
  board,
  value,
  onChange,
}: {
  label: string;
  board: Board;
  value: BlockId;
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
          <option key={id} value={id}>
            {block.name} ({id})
          </option>
        ))}
      </select>
    </label>
  );
}

// A block's name, or its id where the board lacks it.
export function blockName(board: Pick<Board, "blocks">, block: BlockId): string {
  return own(board.blocks, block)?.name ?? block;
}
