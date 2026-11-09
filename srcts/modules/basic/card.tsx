// A block's card and a stack's group, as core's basic frontend draws them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
//
// A card shows these, top to bottom:
// - The block's name, its class and its status, and a menu to rename or
//   remove the block or to add a block before or after it.
// - The block's UI and its output, which core mounts, with the summary of the
//   block's result below the output.
// - The conditions of the block's latest evaluation, of the severities that
//   the board option show_conditions names.
//
// Core keeps drawing a block's last output whatever the block's status. So
// the card greys the output out while the block is not ready.
// Core asks R for an output only once the output's element measures more than
// 0 by 0, and R draws a plot at that size. So the card gives the output a
// height of its own, which the user may drag to another. The handle sits at
// the end of the summary's line, clear of the output's own controls. The card
// does so from the block's first reply on, since there is nothing to draw
// before.
// The card always shows the block's UI and its output, with no toggles to
// hide them, which a frontend built on core's may add.
// On a locked board, the card draws no control for what the lock fixes. It
// has no menu, and it shows the UI only of a block that the lock leaves open.

import { memo, useCallback, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import type { Condition } from "../../protocol/index.ts";
import type { BlockStatus, Status } from "../../scheduler/index.ts";
import type { Block, BlockId, Stack } from "../../types/board.ts";
import { own } from "../../util/index.ts";
import { runCommand, useFrontEnd } from "./context.ts";
import { useStatus } from "./hooks.ts";
import type { AddMode } from "./picker.tsx";
import { classes, Menu, Mounted, NameField } from "./ui.tsx";

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
  const [renaming, setRenaming] = useState(false);
  const ref = useRef<HTMLElement>(null);

  useLayoutEffect(() => {
    const el = ref.current;
    return el === null ? undefined : inView.watch(id, el);
  }, [inView, id]);

  const mountUi = useCallback((el: HTMLElement) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);

  const { lock } = core;
  const inputs = lock === null || lock.open.includes(id);
  const entry = own(core.blockRegistry(), block.registry_id);
  const ready = status?.status === "ready";
  const replied = status !== undefined && status.evaluated !== null;

  return (
    <article
      ref={ref}
      aria-label={block.name}
      className={`${classes.card} bc:grid bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:p-3`}
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
        {lock === null ? (
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
        ) : null}
      </header>
      {inputs ? <Mounted mount={mountUi} label="Inputs" className="bc:min-w-0" /> : null}
      <div
        role="region"
        aria-label="Output"
        data-ready={ready}
        className={`bc:min-w-0 bc:transition-opacity ${ready ? "" : "bc:opacity-50 bc:grayscale"} ${
          replied ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""
        }`}
      >
        <Mounted
          mount={mountOutput}
          className={replied ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}
        />
        {/* The line of the summary, the last of the box, whose end holds the
            handle, so it keeps a line's height without a summary too. */}
        {replied ? (
          <p className="bc:m-0 bc:min-h-4 bc:shrink-0 bc:px-5 bc:text-center bc:font-mono bc:text-xs bc:text-text-muted">
            {status.summary}
          </p>
        ) : null}
      </div>
      <Conditions status={status} severities={severities} />
    </article>
  );
});

// A stack's group, drawn in the stack's colour, or in the colour of a border
// where the stack has none.
export function StackGroup({ stack, children }: { stack: Stack; children: ReactNode }): ReactElement {
  const color = stack.color ?? undefined;
  return (
    <section
      aria-label={stack.name}
      className="bc:grid bc:gap-3 bc:rounded-xl bc:border-2 bc:border-border-default bc:p-3"
      style={color === undefined ? undefined : { borderColor: color }}
    >
      <header className="bc:flex bc:items-center bc:gap-2">
        <span
          aria-hidden="true"
          className="bc:size-3 bc:rounded-full bc:bg-current bc:text-text-muted"
          style={color === undefined ? undefined : { backgroundColor: color }}
        />
        <h2 className={classes.heading}>{stack.name}</h2>
      </header>
      {children}
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

const SEVERITY_TONES: Readonly<Record<Severity, string>> = {
  error: "bc:text-text-danger",
  warning: "bc:text-text-warning",
  message: "bc:text-text-muted",
};

// The conditions of the block's latest evaluation of the severities given,
// and how many more of them R counted rather than kept.
function Conditions({ status, severities }: { status: BlockStatus | undefined; severities: readonly Severity[] }): ReactElement | null {
  if (status === undefined) return null;
  const shown = status.conditions.filter((condition) => severities.includes(condition.severity));
  const omitted = (["warning", "message"] as const).filter((severity) => severities.includes(severity) && status.omitted[severity] > 0);
  if (shown.length === 0 && omitted.length === 0) return null;
  return (
    <ul aria-label="Conditions" className="bc:m-0 bc:grid bc:list-none bc:gap-1 bc:p-0 bc:text-sm">
      {shown.map((condition, i) => (
        <li key={i} data-severity={condition.severity} className={`bc:whitespace-pre-wrap ${SEVERITY_TONES[condition.severity]}`}>
          <span className="bc:font-medium">{condition.severity}:</span> {condition.message}
        </li>
      ))}
      {omitted.map((severity) => (
        <li key={`omitted-${severity}`} className="bc:text-text-muted">
          {String(status.omitted[severity])} more {severity === "warning" ? "warnings" : "messages"} not kept
        </li>
      ))}
    </ul>
  );
}
