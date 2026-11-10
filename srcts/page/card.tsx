// The card in which core draws a block that serve() serves alone, in the
// page's element, since no frontend draws such a block
// (https://github.com/BristolMyersSquibb/blockr.core/issues/442).
//
// The card shows these, top to bottom:
// - The block's name.
// - The block's UI, which core mounts where the board lock leaves the block
//   open, as the store's locked() says.
// - The block's output, as R's chosen renderer draws it, which core mounts.
// - The footer, one centred line of the block's status: a ready block's
//   summary, or why a waiting block waits, and a pill for each severity of the
//   conditions of R's reply. The pill of the errors reads "error", and those of
//   the warnings and the messages count them, as "2 warnings". The footer
//   writes out no message of R's, which may be of any length.
// - The banners of the conditions whose pills are open, in the order R raised
//   them: each condition in full, in the colour of its severity. Each pill
//   opens and closes its banners, and keeps its state when R evaluates the
//   block again. The pill of the errors is open from the start, so that a
//   failed block shows its error at once.
// While R evaluates the block, the footer and the output dim, and keep what
// they show. The output also dims while the block is not ready, and keeps the
// last output that it drew. The card shows conditions in its footer and its
// banners alone, and never as toasts.
// The card draws in the page's theme, which the board option dark_mode
// switches, as on every page that core starts.
// Core asks R for an output only once the output's element measures more than
// 0 by 0, and R draws a plot at that size. So the card gives the output a
// height of its own from the block's first reply on, which the user may drag
// to another. The footer is the last line of the output's box, so that the
// handle sits at the end of the footer, clear of the output's own controls.

import { useCallback, useId, useLayoutEffect, useRef, useState, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { loadingNote } from "../core/index.ts";
import { showError } from "../mount/index.ts";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { own } from "../util/index.ts";

type Severity = Condition["severity"];

// The owner of the card's demand.
export const OWNER = "blockr.core";

// What the card reads of CoreApi.
export type CardCore = Pick<CoreApi, "mountBlock" | "mountOutput"> & {
  readonly store: Pick<CoreApi["store"], "board" | "subscribe" | "locked">;
  readonly scheduler: Pick<CoreApi["scheduler"], "demand" | "status" | "subscribe">;
};

// Draws the board's one block in its card in the element, and demands it,
// until the signal aborts, as the next board opens. The element then says that
// the board is loading, until core draws the next. Where the board holds no
// block or several, the element says so instead.
export function drawCard(el: HTMLElement, core: CardCore, signal: AbortSignal): void {
  if (signal.aborted) return;
  const ids = Object.keys(core.store.board().blocks);
  const [id] = ids;
  if (id === undefined || ids.length > 1) {
    showError(el, `A block that serve() serves alone comes as a board of one block, and this board holds ${String(ids.length)}.`);
    return;
  }
  const demand = core.scheduler.demand(OWNER, [id]);
  el.replaceChildren();
  const root = createRoot(el);
  flushSync(() => {
    root.render(<AloneCard core={core} id={id} />);
  });
  signal.addEventListener(
    "abort",
    () => {
      demand.release();
      root.unmount();
      el.replaceChildren(loadingNote());
    },
    { once: true },
  );
}

function AloneCard({ core, id }: { core: CardCore; id: BlockId }): ReactElement {
  const name = useBlockName(core, id);
  const status = useStatus(core, id);
  const shown = useShown(status);
  const [open, setOpen] = useState<ReadonlySet<Severity>>(OPEN_AT_START);
  const panel = useId();

  const mountUi = useCallback((el: HTMLElement) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);

  const inputs = !core.store.locked(id);
  const replied = status !== undefined && status.evaluated !== null;
  const current = status?.status === "ready" && !status.running;
  const settled = status !== undefined && status === shown && !status.running;
  const listed = shown === undefined ? [] : statusConditions(shown).filter((condition) => open.has(condition.severity));

  return (
    <article
      aria-label={name}
      className="bc:mx-auto bc:my-4 bc:grid bc:max-w-5xl bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-subtle bc:p-3 bc:font-sans bc:text-gray-900 bc:dark:text-gray-100"
    >
      <header>
        <h1 className="bc:m-0 bc:font-sans bc:text-base bc:font-semibold">{name}</h1>
      </header>
      {inputs ? <Mounted mount={mountUi} label="Inputs" className="bc:min-w-0" /> : null}
      <div
        className={`bc:min-w-0 ${replied ? "bc:flex bc:h-96 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`}
      >
        <Mounted
          mount={mountOutput}
          label="Output"
          data={{ "data-current": String(current) }}
          className={`bc:min-w-0 bc:transition-opacity ${current ? "" : "bc:opacity-50 bc:grayscale"} ${
            replied ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"
          }`}
        />
        <footer
          aria-busy={status?.running === true}
          data-status={shown?.status}
          className={`bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:transition-opacity ${
            settled ? "" : "bc:opacity-50"
          }`}
        >
          {shown === undefined ? null : (
            <StatusLine
              status={shown}
              panel={panel}
              open={open}
              toggle={(severity) => {
                const next = new Set(open);
                if (!next.delete(severity)) next.add(severity);
                setOpen(next);
              }}
            />
          )}
        </footer>
      </div>
      {listed.length > 0 ? (
        <ul id={panel} aria-label="Conditions" className="bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0">
          {listed.map((condition, i) => (
            <Banner key={i} condition={condition} />
          ))}
        </ul>
      ) : null}
    </article>
  );
}

// How the card marks a condition of each severity, in its pill and in its
// banner: in the theme's danger colour for an error, its warning colour for a
// warning, and its accent, blue, for a message.
const MARKS: Readonly<Record<Severity, { label: string; noun: string; text: string; tint: string; border: string }>> = {
  error: {
    label: "Error",
    noun: "error",
    text: "bc:text-text-danger",
    tint: "bc:bg-red-500/10",
    border: "bc:border-border-danger",
  },
  warning: {
    label: "Warning",
    noun: "warning",
    text: "bc:text-text-warning",
    tint: "bc:bg-amber-500/10",
    border: "bc:border-amber-500",
  },
  message: {
    label: "Message",
    noun: "message",
    text: "bc:text-blue-600 bc:dark:text-blue-400",
    tint: "bc:bg-blue-500/10",
    border: "bc:border-border-accent",
  },
};

// The line of the footer for a status that has one: a ready block's summary,
// or why a waiting block waits, and a pill for each severity of the conditions
// of R's reply. The pill of the errors reads "error", and those of the
// warnings and the messages count them.
function StatusLine({
  status,
  panel,
  open,
  toggle,
}: {
  status: BlockStatus;
  panel: string;
  open: ReadonlySet<Severity>;
  toggle: (severity: Severity) => void;
}): ReactElement | null {
  if (status.status === "waiting") {
    return <span className="bc:text-text-muted">{status.conditions[0]?.message}</span>;
  }

  const pills = (["error", "warning", "message"] as const).flatMap((severity) => {
    const n = status.conditions.filter((condition) => condition.severity === severity).length;
    if (n === 0) return [];
    const mark = MARKS[severity];
    return [
      <button
        key={severity}
        type="button"
        aria-expanded={open.has(severity)}
        aria-controls={panel}
        onClick={() => {
          toggle(severity);
        }}
        className={`bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1 bc:rounded-full bc:border-0 bc:px-1.5 bc:py-0.5 bc:font-mono bc:text-xs bc:hover:underline bc:aria-expanded:ring-1 bc:aria-expanded:ring-current ${mark.tint} ${mark.text}`}
      >
        <SeverityIcon severity={severity} />
        {severity === "error" ? mark.noun : `${String(n)} ${mark.noun}${n === 1 ? "" : "s"}`}
      </button>,
    ];
  });

  return (
    <span className="bc:inline-flex bc:flex-wrap bc:items-center bc:justify-center bc:gap-2">
      {status.status === "ready" && status.summary !== null ? <span className="bc:text-text-muted">{status.summary}</span> : null}
      {pills}
    </span>
  );
}

// A condition in full, as a banner in the colour of its severity, with the
// severity's icon and name.
function Banner({ condition }: { condition: Condition }): ReactElement {
  const mark = MARKS[condition.severity];
  return (
    <li
      data-severity={condition.severity}
      className={`bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`}
    >
      <span className={`bc:mt-0.5 ${mark.text}`}>
        <SeverityIcon severity={condition.severity} />
      </span>
      <span className="bc:grid bc:min-w-0 bc:gap-0.5">
        <span className={`bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`}>{mark.label}</span>
        <span className="bc:font-mono bc:text-xs bc:whitespace-pre-wrap">{condition.message}</span>
      </span>
    </li>
  );
}

// The icon of a severity, drawn in the colour of the text around it: a cross in
// a circle for an error, an exclamation mark in a triangle for a warning, and
// an i in a circle for a message.
function SeverityIcon({ severity }: { severity: Severity }): ReactElement {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 16 16"
      className="bc:size-3 bc:shrink-0"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.6}
      strokeLinecap="round"
      strokeLinejoin="round"
    >
      {severity === "warning" ? (
        <>
          <path d="M8 1.8 14.6 13.6H1.4Z" />
          <path d="M8 6.2v3.2M8 11.6v.1" />
        </>
      ) : (
        <>
          <circle cx="8" cy="8" r="6.4" />
          <path d={severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1"} />
        </>
      )}
    </svg>
  );
}

// The status that the footer shows: the block's, where it has a line of its
// own, and otherwise the last that had one. A block that reads stale or
// unevaluated has no reply for its revision yet, so the footer keeps what it
// showed while R evaluates the block.
function useShown(status: BlockStatus | undefined): BlockStatus | undefined {
  const [kept, setKept] = useState<BlockStatus | undefined>(undefined);
  const now = status !== undefined && status.status !== "stale" && status.status !== "unevaluated" ? status : undefined;
  if (now !== undefined && now !== kept) setKept(now);
  return now ?? kept;
}

// The severities whose banners are open as the card starts: the errors, since
// their pill names no more of them than their severity.
const OPEN_AT_START: ReadonlySet<Severity> = new Set<Severity>(["error"]);

// The conditions that the footer opens to, in the order R raised them: those
// of R's reply, which a waiting block has none of.
function statusConditions(status: BlockStatus): readonly Condition[] {
  return status.status === "waiting" ? [] : status.conditions;
}

// The block's name, which the store keeps until a change renames the block.
function useBlockName(core: Pick<CardCore, "store">, id: BlockId): string {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => own(core.store.board().blocks, id)?.name ?? id);
}

// The block's status, which the scheduler keeps the same object until it
// announces that it changed.
function useStatus(core: Pick<CardCore, "scheduler">, id: BlockId): BlockStatus | undefined {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.scheduler.subscribe((changed) => {
        if (changed.includes(id)) notify();
      }),
    [core, id],
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(id));
}

// The element in which core mounts a module until the component goes, as the
// basic frontend's is. Core mounts the module as React commits the card, so
// that the card has mounted the block once drawCard() returns. Core's cleanup
// runs once React has finished the work in which the component went, so that
// a module written in React unmounts its own root before core empties the
// element.
function Mounted({
  mount,
  label,
  className,
  data,
}: {
  mount: (el: HTMLElement) => () => void;
  label: string;
  className?: string;
  data?: Readonly<Record<`data-${string}`, string>>;
}): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return <div ref={ref} role="region" aria-label={label} className={className} {...data} />;
}
