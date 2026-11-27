// The page that core's basic frontend draws: the toolbar, the board's blocks,
// each stack as a group of its blocks' cards, which the user collapses, the
// plugins that it shows, and the toasts (./toasts.ts). The dialog of the board
// options shows over the page, and core's dialog shows the advanced actions
// that a card's menu opens, such as the block picker.
// The page follows the board through the store, so a removed block's card
// goes because the board changed.
// Adding a block on its own and editing links and stacks are the Edit board
// plugin's, which the page shows as it shows any plugin.

import { useCallback, useMemo, useState } from "react";
import type { ReactElement } from "react";
import { Button, Icon, Text, Toast, ToastContent, ToastRegion } from "@blockr/ui";
import { X } from "@blockr/ui/icons";
import type { QueuedToast } from "@blockr/ui";
import { MARKS, SeverityIcon } from "../../card/index.ts";
import { useBoard } from "../../react/index.ts";
import type { BlockId } from "../../types/board.ts";
import type { Notification } from "../../types/core-api.ts";
import { classes } from "../../ui/index.ts";
import { BlockCard, StackGroup } from "./card.tsx";
import { FrontEndContext, useFrontEnd } from "./context.ts";
import type { FrontEnd } from "./context.ts";
import { layoutItems } from "./layout.ts";
import { OptionsDialog } from "./options.tsx";
import { Plugins } from "./plugins.tsx";
import { concerned, isReload, sourcePackage } from "./toasts.ts";
import { Toolbar } from "./toolbar.tsx";

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
  const [options, setOptions] = useState(false);
  const close = useCallback(() => {
    setOptions(false);
  }, []);

  const card = (id: BlockId): ReactElement | null => {
    const block = board.blocks[id];
    if (block === undefined) return null;
    return <BlockCard key={`block-${id}`} id={id} block={block} />;
  };

  return (
    <div className={`${classes.page} bc:flex bc:min-h-full bc:flex-col bc:gap-4 bc:p-4 bc:font-sans`}>
      <Toolbar
        board={board}
        showOptions={() => {
          setOptions(true);
        }}
      />
      {/* The plugins come before the board, so that a narrow page shows them
          above the board and the keyboard reaches them first. A wide page
          shows them beside the board. */}
      <div className="bc:flex bc:flex-col bc:gap-4 bc:md:flex-row-reverse bc:md:items-start">
        <Plugins />
        <div className="bc:grid bc:min-w-0 bc:flex-1 bc:gap-4">
          {items.length === 0 ? <p className={classes.hint}>The board holds no blocks yet.</p> : null}
          {items.map((item) => {
            if ("block" in item) return card(item.block);
            const stack = board.stacks[item.stack];
            if (stack === undefined) return null;
            return (
              <StackGroup key={`stack-${item.stack}`} id={item.stack} stack={stack}>
                {item.blocks.length === 0 ? <p className={classes.hint}>The stack holds no blocks.</p> : item.blocks.map(card)}
              </StackGroup>
            );
          })}
        </div>
      </div>
      {options ? <OptionsDialog onClose={close} /> : null}
      <Toasts />
    </div>
  );
}

// The toasts, in blockr.ui's ToastRegion, which draws them one above the other
// in the window's bottom right corner, the latest lowest (./toasts.ts).
function Toasts(): ReactElement {
  const { core, toasts } = useFrontEnd();
  const board = useBoard(core);
  return (
    <ToastRegion queue={toasts.queue}>
      {({ toast }) => <ToastItem toast={toast} title={concerned(toast.content.about, core, board)} />}
    </ToastRegion>
  );
}

// A toast, blockr.ui's Toast, marked in the colour of its notification's
// severity at its left: what it concerns, with the package that its source
// names, its message, and a button that closes it, which shows Lucide's
// cross. Where core asks for the page to be reloaded, it also offers to
// reload it.
function ToastItem({ toast, title }: { toast: QueuedToast<Notification>; title: string }): ReactElement {
  const notification = toast.content;
  const { severity } = notification;
  const mark = MARKS[severity];
  const pkg = sourcePackage(notification);
  return (
    <Toast toast={toast} data-severity={severity} className={`bc:border-l-4 ${LEFT[severity]}`}>
      <span className={`bc:mt-0.5 ${mark.text}`}>
        <SeverityIcon severity={severity} />
      </span>
      <ToastContent role={severity === "message" ? "status" : undefined} className="bc:grid bc:min-w-0 bc:flex-1 bc:gap-1">
        <Text slot="title" elementType="p" className="bc:m-0 bc:text-xs bc:font-semibold">
          {title}
          {pkg === undefined ? null : <span className="bc:font-normal bc:text-muted-foreground"> · {pkg}</span>}
        </Text>
        <Text slot="description" elementType="p" className="bc:m-0 bc:text-sm bc:break-words bc:whitespace-pre-wrap">
          {notification.message}
        </Text>
        {isReload(notification) ? (
          <div>
            <Button
              variant="secondary"
              onPress={() => {
                location.reload();
              }}
            >
              Reload
            </Button>
          </div>
        ) : null}
      </ToastContent>
      <Button slot="close" variant="quiet">
        <Icon icon={X} />
      </Button>
    </Toast>
  );
}

// The colour of the mark at a toast's left, for each severity, as a card's
// pills mark it (../../card/index.ts).
const LEFT: Readonly<Record<Notification["severity"], string>> = {
  error: "bc:border-l-destructive",
  warning: "bc:border-l-warning",
  message: "bc:border-l-primary",
};
