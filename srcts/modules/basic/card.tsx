// A block's card and a stack's group, as core's basic frontend draws them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
//
// A card shows these, top to bottom:
// - The block's name, its class and its status, and a menu to rename or
//   remove the block or to add a block before or after it.
// - The block's UI and its output, which core mounts, and in whose elements
//   core draws nothing of its own, not even where they fail.
// - The footer, the last line of the output's box, which shows a ready
//   block's summary.
// - Below the box, the pills: one for the reason of an invalid or a waiting
//   block, and one for each severity of the notifications about the block
//   that the board option show_conditions names, whoever raised them, as the
//   conditions of R's reply, what failed of the block's UI module, its output
//   and its requests, and what its modules raise.
// - The banners of the pills that are open.
// The footer, the pills and the banners are those of srcts/card, which the
// card of a block that serve() serves alone draws as well.
//
// Core keeps drawing a block's last output whatever the block's status. So
// the card greys the output out while the block is not ready.
// It greys out the output alone, and not the pills, so that they keep the
// colours of their severities.
// Core asks R for an output only once the block reads ready and the output's
// element measures more than 0 by 0, and R draws a plot at that size.
// So the card gives the output a height of its own from the first time the
// block reads ready on, which the user may drag to another.
// Before then there is nothing to draw, and the card of a block that fails or
// reads invalid at once shows the pills and the banners alone, as that of a
// waiting block does.
// The box then has no footer either, since a block that never read ready has
// no summary.
// The handle sits at the end of the footer, clear of the output's own
// controls.
// The card always shows the block's UI and its output, with no toggles to
// hide them, which a frontend built on core's may add.
// The card gives its block focus as the user moves into it, by a click or by
// moving the keyboard's focus into it, so that a plugin such as the assistant
// knows which block the user is looking at. Hovering over the card does not.
// Under the board lock, the card draws no control for what the lock fixes. It
// has no menu, and it mounts the UI only of a block that the board lock leaves
// open, as the store's locked() says.

import { memo, useCallback, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { Banners, Footer, Mounted, Pills, useFooter, useNotified, useSized, useStatus } from "../../card/index.ts";
import type { Condition } from "../../protocol/index.ts";
import type { BlockStatus, Status } from "../../scheduler/index.ts";
import type { Block, BlockId, Stack } from "../../types/board.ts";
import type { AddMode } from "../../picker/index.ts";
import { classes, NameField } from "../../ui/index.ts";
import { own } from "../../util/index.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { Menu } from "./ui.tsx";

type Severity = Condition["severity"];

export interface CardProps {
  readonly id: BlockId;
  readonly block: Block;
  // The severities of the conditions that the card shows.
  readonly severities: readonly Severity[];
  // Whether the block takes a link from a new block, which it does while an
  // input of it is free.
  readonly prependable: boolean;
  readonly add: (mode: AddMode) => void;
}

export const BlockCard = memo(function BlockCard({
  id,
  block,
  severities,
  prependable,
  add,
}: CardProps): ReactElement {
  const front = useFrontEnd();
  const { core, inView } = front;
  const status = useStatus(core, id);
  const notified = useNotified(core, id);
  const footer = useFooter(status, notified, severities);
  const sized = useSized(status);
  const [renaming, setRenaming] = useState(false);
  const ref = useRef<HTMLElement>(null);

  useLayoutEffect(() => {
    const el = ref.current;
    return el === null ? undefined : inView.watch(id, el);
  }, [inView, id]);

  const mountUi = useCallback((el: HTMLElement) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);
  const focus = useCallback(() => {
    if (core.focus.get() !== id) core.focus.set(id);
  }, [core, id]);

  const locked = core.lock.board;
  const inputs = !core.store.locked(id);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";

  return (
    <article
      ref={ref}
      aria-label={block.name}
      className={`${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`}
      onPointerDown={focus}
      onFocus={focus}
    >
      <header className="bc:flex bc:flex-wrap bc:items-center bc:gap-2">
        {renaming ? (
          <NameField
            label="Block name"
            name={block.name}
            onSubmit={(name) => {
              void runCommand(front, "blockr.core-rename_block", { id, name });
            }}
            onDone={() => {
              setRenaming(false);
            }}
          />
        ) : (
          <h3 className={classes.heading}>{block.name}</h3>
        )}
        <span className="bc:text-xs bc:text-text-muted">{entry?.name ?? block.registry_id}</span>
        <StatusBadge status={status} />
        {locked ? null : (
          <div className="bc:ml-auto bc:flex bc:items-center bc:gap-1">
            <Menu
              label={`Actions of ${block.name}`}
              items={[
                {
                  label: "Rename",
                  onSelect: () => {
                    setRenaming(true);
                  },
                },
                {
                  label: "Add block before",
                  disabled: !prependable,
                  onSelect: () => {
                    add({ kind: "before", to: id });
                  },
                },
                {
                  label: "Add block after",
                  onSelect: () => {
                    add({ kind: "after", from: id });
                  },
                },
                {
                  label: "Remove",
                  onSelect: () => {
                    void runCommand(front, "blockr.core-remove_block", { id });
                  },
                },
              ]}
            />
          </div>
        )}
      </header>
      {inputs ? <Mounted mount={mountUi} label="Inputs" className="bc:min-w-0" /> : null}
      <div
        className={`bc:min-w-0 ${sized ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`}
      >
        <Mounted
          mount={mountOutput}
          label="Output"
          data={{ "data-ready": String(ready) }}
          className={`bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${
            sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"
          }`}
        />
        {sized ? <Footer {...footer} /> : null}
      </div>
      <Pills {...footer} />
      <Banners {...footer} />
    </article>
  );
});

// A stack's group, drawn in the stack's colour, or in the colour of a border
// where the stack has none. The user collapses and expands it by its name, as
// a plugin's section, and it starts open. A collapsed group hides the cards of
// its blocks and leaves them mounted.
export function StackGroup({ stack, children }: { stack: Stack; children: ReactNode }): ReactElement {
  const color = stack.color ?? undefined;
  return (
    <section
      aria-label={stack.name}
      className="bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3"
      style={color === undefined ? undefined : { borderColor: color }}
    >
      <details open>
        <summary className={`${classes.heading} bc:cursor-pointer`}>{stack.name}</summary>
        <div className="bc:mt-3 bc:grid bc:gap-3">{children}</div>
      </details>
    </section>
  );
}

const TONES: Readonly<Record<Status, string>> = {
  ready: "bc:text-text-success",
  failed: "bc:text-text-danger",
  invalid: "bc:text-text-warning",
  waiting: "bc:text-text-warning",
  stale: "bc:text-text-muted",
  unevaluated: "bc:text-text-muted",
};

// The block's status, and whether R is evaluating it.
function StatusBadge({ status }: { status: BlockStatus | undefined }): ReactElement | null {
  if (status === undefined) return null;
  return (
    <span
      data-status={status.status}
      aria-busy={status.running}
      className={`bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:px-2 bc:py-0.5 bc:text-xs ${TONES[status.status]}`}
    >
      <span aria-hidden="true" className={`bc:size-2 bc:rounded-full bc:bg-current ${status.running ? "bc:animate-pulse" : ""}`} />
      {status.running ? `${status.status}, running` : status.status}
    </span>
  );
}
