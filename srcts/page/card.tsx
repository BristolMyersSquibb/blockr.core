// The card in which core draws a block that serve() serves alone, in the
// page's element, since no frontend draws such a block
// (https://github.com/BristolMyersSquibb/blockr.core/issues/442).
//
// The card shows these, top to bottom:
// - The block's name.
// - The block's UI, which core mounts where the board lock leaves the block
//   open, as the store's locked() says.
// - The block's output, as R's chosen renderer draws it, which core mounts.
// - The footer, one centred line of the block's status:
//   - A ready block shows its summary, with the count of its warnings after
//     it, as "<data.frame> [0 × 5] · 1 warning".
//   - A failed block shows the first line of R's error, in red.
//   - A waiting block shows why it waits, and an invalid block why its state
//     cannot run.
//   The count of warnings and the error each open to their full messages,
//   below the output.
// While R evaluates the block, the footer and the output dim, and keep what
// they show. The output also dims while the block is not ready, and keeps the
// last output that it drew. The card shows conditions in its footer alone,
// and never as toasts.
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
  const [open, setOpen] = useState(false);
  const panel = useId();

  const mountUi = useCallback((el: HTMLElement) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);

  const inputs = !core.store.locked(id);
  const replied = status !== undefined && status.evaluated !== null;
  const current = status?.status === "ready" && !status.running;
  const settled = status !== undefined && status === shown && !status.running;
  const messages = shown === undefined ? [] : opened(shown);

  return (
    <article
      aria-label={name}
      className="bc:mx-auto bc:my-4 bc:grid bc:max-w-5xl bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-subtle bc:p-3 bc:font-sans bc:text-gray-900"
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
          className={`bc:min-h-4 bc:shrink-0 bc:px-5 bc:text-center bc:font-mono bc:text-xs bc:transition-opacity ${
            settled ? "" : "bc:opacity-50"
          }`}
        >
          {shown === undefined ? null : (
            <StatusLine
              status={shown}
              panel={panel}
              open={open}
              toggle={() => {
                setOpen(!open);
              }}
            />
          )}
        </footer>
      </div>
      {open && messages.length > 0 ? (
        <ul
          id={panel}
          aria-label={shown?.status === "ready" ? "Warnings" : "Errors"}
          className="bc:m-0 bc:grid bc:list-none bc:gap-1 bc:p-0 bc:font-mono bc:text-xs bc:whitespace-pre-wrap"
        >
          {messages.map((condition, i) => (
            <li key={i} className={condition.severity === "error" ? "bc:text-text-danger" : "bc:text-text-warning"}>
              {condition.message}
            </li>
          ))}
        </ul>
      ) : null}
    </article>
  );
}

// The line of the footer for a status that has one.
function StatusLine({
  status,
  panel,
  open,
  toggle,
}: {
  status: BlockStatus;
  panel: string;
  open: boolean;
  toggle: () => void;
}): ReactElement | null {
  const opens = (label: string, tone: string): ReactElement => (
    <button
      type="button"
      aria-expanded={open}
      aria-controls={panel}
      className={`bc:cursor-pointer bc:border-0 bc:bg-transparent bc:p-0 bc:font-mono bc:text-xs bc:underline-offset-2 bc:hover:underline ${tone}`}
      onClick={toggle}
    >
      {label}
    </button>
  );

  switch (status.status) {
    case "ready": {
      const warnings = status.conditions.filter((condition) => condition.severity === "warning").length;
      return (
        <span className="bc:text-text-muted">
          {status.summary}
          {warnings === 0 ? null : (
            <>
              {status.summary === null ? null : " · "}
              {opens(`${String(warnings)} ${warnings === 1 ? "warning" : "warnings"}`, "bc:text-text-warning")}
            </>
          )}
        </span>
      );
    }
    case "failed":
      return opens(firstLine(errorsOf(status)[0]), "bc:text-text-danger");
    case "invalid":
      return <span className="bc:text-text-warning">{firstLine(errorsOf(status)[0])}</span>;
    case "waiting":
      return <span className="bc:text-text-muted">{status.conditions[0]?.message}</span>;
    default:
      return null;
  }
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

// The conditions that the footer's line opens to: a ready block's warnings,
// and a failed block's errors.
function opened(status: BlockStatus): readonly Condition[] {
  if (status.status === "ready") return status.conditions.filter((condition) => condition.severity === "warning");
  if (status.status === "failed") return errorsOf(status);
  return [];
}

function errorsOf(status: BlockStatus): readonly Condition[] {
  const errors = status.conditions.filter((condition) => condition.severity === "error");
  return errors.length > 0 ? errors : status.conditions;
}

function firstLine(condition: Condition | undefined): string {
  return condition?.message.split("\n", 1)[0] ?? "";
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
