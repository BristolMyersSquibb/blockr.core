// The page that core's basic frontend draws: the toolbar, the board's blocks,
// each stack as a group of its blocks' cards, which the user collapses, the
// plugins that it shows, and the toasts (./toasts.ts). One dialog at a time
// shows over the page: the picker that a card's menu opens, or the board
// options.
// The page follows the board through the store, so a removed block's card
// goes because the board changed.
// Adding a block on its own and editing links and stacks are the Edit board
// plugin's, which the page shows as it shows any plugin.

import { useCallback, useEffect, useMemo, useState } from "react";
import type { ReactElement } from "react";
import { createPortal } from "react-dom";
import { MARKS, SeverityIcon } from "../../card/index.ts";
import { freeInputs } from "../../commands/index.ts";
import { BlockPicker } from "../../picker/index.ts";
import type { AddMode } from "../../picker/index.ts";
import type { Condition } from "../../protocol/index.ts";
import { styles } from "../../styles/index.ts";
import type { BlockId } from "../../types/board.ts";
import type { Json } from "../../types/json.ts";
import { classes, runCommand, useBoard } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { BlockCard, StackGroup } from "./card.tsx";
import { FrontEndContext, useFrontEnd } from "./context.ts";
import type { FrontEnd } from "./context.ts";
import { layoutItems } from "./layout.ts";
import { OptionsDialog } from "./options.tsx";
import { Plugins } from "./plugins.tsx";
import { concerned, isReload, sourcePackage, useShownIn, useToasts } from "./toasts.ts";
import type { Toast } from "./toasts.ts";
import { Toolbar } from "./toolbar.tsx";
import { Dialog } from "./ui.tsx";

type Shown = { readonly kind: "add"; readonly mode: AddMode } | { readonly kind: "options" };

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
  const items = useMemo(() => layoutItems(board), [board]);
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
        showOptions={() => {
          setDialog({ kind: "options" });
        }}
      />
      {/* The plugins come before the board, so that a narrow page shows them
          above the board and the keyboard reaches them first. A wide page
          shows them beside the board. */}
      <div className="bc:flex bc:flex-col bc:gap-4 bc:md:flex-row-reverse bc:md:items-start">
        <Plugins />
        <div className="bc:grid bc:min-w-0 bc:flex-1 bc:gap-4">
          {items.length === 0 ? <p className={styles.hint}>The board holds no blocks yet.</p> : null}
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
      </div>
      {dialog?.kind === "add" ? <AddBlockDialog mode={dialog.mode} onClose={close} /> : null}
      {dialog?.kind === "options" ? <OptionsDialog onClose={close} /> : null}
      <Toasts />
    </div>
  );
}

// The form that a card's menu opens, which adds a block before or after the
// card's block, and closes once the block is on the board.
function AddBlockDialog({ mode, onClose }: { mode: AddMode; onClose: () => void }): ReactElement {
  const front = useFrontEnd();
  const board = useBoard(front.core);
  const target = mode.kind === "add" ? undefined : own(board.blocks, mode.kind === "after" ? mode.from : mode.to);
  const title = target === undefined ? "Add block" : `Add block ${mode.kind} "${target.name}"`;
  const run = useCallback((command: string, args: Json) => runCommand(front, command, args), [front]);
  return (
    <Dialog title={title} onClose={onClose}>
      <BlockPicker core={front.core} mode={mode} run={run} onAdded={onClose} autofocus />
    </Dialog>
  );
}

// The toasts that show, one above the other in the window's bottom right
// corner, the latest lowest. While a dialog is open, they show inside it.
// The column scrolls where the toasts outgrow the window. Its padding keeps
// their shadows from being cut off there, and it lets clicks through between
// the toasts, so that the board below takes them. Core's stylesheet leaves
// out Tailwind's reset, so the column counts its padding into its width
// itself.
function Toasts(): ReactElement | null {
  const { core, toasts } = useFrontEnd();
  const list = useToasts(toasts);
  const inside = useShownIn(toasts);
  const board = useBoard(core);
  if (list.length === 0) return null;
  const shown = (
    <section
      aria-label="Notifications"
      className="bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-50 bc:m-0 bc:box-border bc:grid bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans"
    >
      {list.map((toast) => (
        <ToastItem key={toast.notification.id} toast={toast} title={concerned(toast.notification.about, core, board)} />
      ))}
    </section>
  );
  return inside === null ? shown : createPortal(shown, inside);
}

// A toast, in the colour of its notification's severity: what it concerns,
// with the package that its source names, its message, and a button that
// closes it. Where core asks for the page to be reloaded, it also offers
// to reload it.
// A toast that moves between the page and a dialog is drawn anew, and the
// browser may send its old element no blur and no pointerleave as it goes. So
// the toast lets go of what kept it from fading once its element goes.
function ToastItem({ toast, title }: { toast: Toast; title: string }): ReactElement {
  const { toasts } = useFrontEnd();
  const { notification, fading } = toast;
  const { id, severity } = notification;
  const mark = MARKS[severity];
  const pkg = sourcePackage(notification);
  useEffect(
    () => () => {
      toasts.release(id, "pointer");
      toasts.release(id, "focus");
    },
    [toasts, id],
  );
  return (
    <div
      role={severity === "message" ? "status" : "alert"}
      data-severity={severity}
      onPointerEnter={() => {
        toasts.keep(id, "pointer");
      }}
      onPointerLeave={() => {
        toasts.release(id, "pointer");
      }}
      onFocus={() => {
        toasts.keep(id, "focus");
      }}
      onBlur={(event) => {
        if (!(event.relatedTarget instanceof Node && event.currentTarget.contains(event.relatedTarget))) {
          toasts.release(id, "focus");
        }
      }}
      className={`${classes.surface} bc:pointer-events-auto bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 bc:shadow-lg bc:transition-opacity bc:duration-300 bc:motion-reduce:transition-none ${mark.border} ${fading ? "bc:opacity-0" : ""}`}
    >
      <span className={`bc:mt-0.5 ${mark.text}`}>
        <SeverityIcon severity={severity} />
      </span>
      <div className="bc:grid bc:min-w-0 bc:flex-1 bc:gap-1">
        <p className="bc:m-0 bc:text-xs bc:font-semibold">
          {title}
          {pkg === undefined ? null : <span className="bc:font-normal bc:text-text-muted"> · {pkg}</span>}
        </p>
        <p className="bc:m-0 bc:text-sm bc:break-words bc:whitespace-pre-wrap">{notification.message}</p>
        {isReload(notification) ? (
          <div>
            <button
              type="button"
              className={classes.button}
              onClick={() => {
                location.reload();
              }}
            >
              Reload
            </button>
          </div>
        ) : null}
      </div>
      <button
        type="button"
        aria-label="Close"
        className={classes.quiet}
        onClick={() => {
          toasts.close(id);
        }}
      >
        <span aria-hidden="true">×</span>
      </button>
    </div>
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
