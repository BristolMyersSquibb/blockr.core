// The page that core's basic frontend draws: the toolbar, the notices and
// the board's blocks, each stack as a group of its blocks' cards. One dialog
// at a time shows over the page.
// The page follows the board through the store, so a removed block's card
// goes because the board changed.

import { useCallback, useMemo, useState } from "react";
import type { ReactElement } from "react";
import { freeInputs } from "../../commands/index.ts";
import type { Condition } from "../../protocol/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId } from "../../types/board.ts";
import type { Json } from "../../types/json.ts";
import { BlockCard, StackGroup } from "./card.tsx";
import { FrontEndContext, useFrontEnd } from "./context.ts";
import type { FrontEnd } from "./context.ts";
import { useBoard, useNotices } from "./hooks.ts";
import { itemsOf } from "./layout.ts";
import { LinksDialog } from "./links.tsx";
import { OptionsDialog } from "./options.tsx";
import { AddBlockDialog } from "./picker.tsx";
import type { AddMode } from "./picker.tsx";
import { StacksDialog } from "./stacks.tsx";
import { Toolbar } from "./toolbar.tsx";
import type { ToolbarDialog } from "./toolbar.tsx";
import { classes } from "./ui.tsx";

type Shown = { readonly kind: "add"; readonly mode: AddMode } | { readonly kind: Exclude<ToolbarDialog, "add"> };

export function BasicBoard({ front }: { front: FrontEnd }): ReactElement {
  return (
    <FrontEndContext value={front}>
      <Page />
    </FrontEndContext>
  );
}

function Page(): ReactElement {
  const { core } = useFrontEnd();
  const board = useBoard(core);
  const items = useMemo(() => itemsOf(board), [board]);
  // The same array until show_conditions changes, so that a card draws again
  // only as its own block changes.
  const shown = board.options.show_conditions;
  const severities = useMemo(() => asSeverities(shown), [shown]);
  const [dialog, setDialog] = useState<Shown | null>(null);
  const add = useCallback((mode: AddMode) => {
    setDialog({ kind: "add", mode });
  }, []);
  const close = useCallback(() => {
    setDialog(null);
  }, []);

  const card = (id: BlockId): ReactElement | null => {
    const block = board.blocks[id];
    if (block === undefined) return null;
    return (
      <BlockCard
        key={`block-${id}`}
        id={id}
        block={block}
        severities={severities}
        prependable={(freeInputs(board, core.blockRegistry(), id)?.length ?? 0) > 0}
        add={add}
      />
    );
  };

  return (
    <div className={`${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`}>
      <Toolbar
        board={board}
        show={(kind) => {
          setDialog(kind === "add" ? { kind, mode: { kind: "add" } } : { kind });
        }}
      />
      <Notices />
      {items.length === 0 ? <p className={styles.hint}>The board holds no blocks yet.</p> : null}
      <div className="bc:grid bc:gap-4">
        {items.map((item) => {
          if ("block" in item) return card(item.block);
          const stack = board.stacks[item.stack];
          if (stack === undefined) return null;
          return (
            <StackGroup key={`stack-${item.stack}`} stack={stack}>
              {item.blocks.length === 0 ? <p className={styles.hint}>The stack holds no blocks.</p> : item.blocks.map(card)}
            </StackGroup>
          );
        })}
      </div>
      {dialog?.kind === "add" ? <AddBlockDialog mode={dialog.mode} onClose={close} /> : null}
      {dialog?.kind === "links" ? <LinksDialog onClose={close} /> : null}
      {dialog?.kind === "stacks" ? <StacksDialog onClose={close} /> : null}
      {dialog?.kind === "options" ? <OptionsDialog onClose={close} /> : null}
    </div>
  );
}

// The edits that the store or the command registry refused, each until the
// user dismisses it.
function Notices(): ReactElement | null {
  const { notices } = useFrontEnd();
  const list = useNotices(notices);
  if (list.length === 0) return null;
  return (
    <ul aria-label="Notices" className={classes.list}>
      {list.map((notice) => (
        <li
          key={notice.id}
          role="alert"
          className="bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-danger bc:p-2 bc:text-sm bc:text-text-danger"
        >
          <span className="bc:flex-1 bc:whitespace-pre-wrap">{notice.text}</span>
          <button
            type="button"
            className={classes.quiet}
            onClick={() => {
              notices.dismiss(notice.id);
            }}
          >
            Dismiss
          </button>
        </li>
      ))}
    </ul>
  );
}

// The severities that the board option show_conditions names. Core's basic
// frontend requires the board option, so the board that it draws holds a value
// of it, and a value that is no array of severities fails, rather than show
// none.
export function asSeverities(value: Json | undefined): readonly Condition["severity"][] {
  if (Array.isArray(value) && value.every(isSeverity)) return value;
  throw new TypeError(
    `The board option show_conditions is ${JSON.stringify(value)}, where the board takes an array of "message", "warning" and "error".`,
  );
}

function isSeverity(value: Json): value is Condition["severity"] {
  return value === "error" || value === "warning" || value === "message";
}
