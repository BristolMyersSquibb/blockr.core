// What the Edit board plugin draws: three sections, for the board's blocks,
// its links and its stacks, as What it shows has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows).
// Each section holds a form that adds one, and a table of those that the board
// holds, with the controls that change and remove each. A table shows a page
// of rows at a time, which a search narrows, so that it stays usable on a
// board of many blocks.
// Each section collapses by its heading, which counts what the board holds.
// It draws nothing of a block's state, which the block's UI sets, nor the
// board's name or its board options, which the frontend draws.
// It follows the board through the store, so the next edit reads the board as
// the last one left it, a block that the user just added among it.
// Under the board lock, the store refuses every edit that the plugin makes,
// so the plugin draws no control there, as it reads from core.lock.board. It
// then lists only the board's links and stacks, since a frontend draws the
// blocks on their cards.

import { useCallback, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode, RefObject } from "react";
import type { CoreApi } from "../../types/core-api.ts";
import { useBoard } from "../../ui/index.ts";
import { BlocksSection } from "./blocks.tsx";
import { LinksSection } from "./links.tsx";
import { StacksSection } from "./stacks.tsx";

export function EditBoard({ core }: { core: CoreApi }): ReactElement {
  const board = useBoard(core);
  const locked = core.lock.board;
  return (
    <div className="bc:grid bc:gap-3 bc:font-sans bc:text-text-default">
      {locked ? null : <BlocksSection core={core} board={board} />}
      <LinksSection core={core} board={board} locked={locked} />
      <StacksSection core={core} board={board} locked={locked} />
    </div>
  );
}

// One of the plugin's sections, a region named by its title, which the user
// collapses by its heading, which counts what the board holds of it.
export function Section({ title, count, children }: { title: string; count: number; children: ReactNode }): ReactElement {
  return (
    <section aria-label={title} className="bc:rounded-md bc:border bc:border-border-default">
      <details open className="bc:group">
        <summary className="bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:px-3 bc:py-2 bc:font-sans bc:text-sm bc:font-semibold">
          <h3 className="bc:m-0 bc:text-sm bc:font-semibold">{title}</h3>
          <span className="bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted">
            {count}
          </span>
        </summary>
        <div className="bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3">{children}</div>
      </details>
    </section>
  );
}

// A part of a section, such as its form or its table, under a heading of its
// own.
export function Part({ title, children }: { title: string; children: ReactNode }): ReactElement {
  return (
    <div role="group" aria-label={title} className="bc:grid bc:gap-2">
      <h4 className="bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted">{title}</h4>
      {children}
    </div>
  );
}

// The classes of the plugin's tables.
export const table = {
  table: "bc:w-full bc:table-fixed bc:border-collapse bc:text-sm",
  head: "bc:border-b bc:border-border-default bc:px-1 bc:py-1 bc:text-left bc:text-xs bc:font-semibold bc:text-text-muted",
  row: "bc:border-b bc:border-border-default bc:align-top",
  cell: "bc:px-1 bc:py-1.5",
} as const;

// Whether a table's row shows its edit form, and the ref of the row's Edit
// button, which gets the focus back as the form closes, so that the keyboard
// stays at the row that the user edited.
export function useRowEditing(): {
  editing: boolean;
  open: () => void;
  close: () => void;
  button: RefObject<HTMLButtonElement | null>;
} {
  const [editing, setEditing] = useState(false);
  const closed = useRef(false);
  const button = useRef<HTMLButtonElement | null>(null);
  useLayoutEffect(() => {
    if (editing || !closed.current) return;
    closed.current = false;
    button.current?.focus();
  }, [editing]);
  const open = useCallback(() => {
    setEditing(true);
  }, []);
  const close = useCallback(() => {
    closed.current = true;
    setEditing(false);
  }, []);
  return { editing, open, close, button };
}
