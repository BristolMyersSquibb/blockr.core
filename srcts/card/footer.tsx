// The footer of a block's card and the banners below it, which the card of a
// block that serve() serves alone and the cards of core's basic frontend draw
// alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve).
//
// - The footer is one centred line of the block's status: a ready block's
//   summary, or the reason of an invalid or a waiting block, and a pill for
//   each severity of the conditions of R's reply that the card shows.
//   The pill of the errors reads "error", and those of the warnings and the
//   messages count them, as "2 warnings".
//   The footer writes out no condition's message, which may be of any length.
// - The banners are those of the conditions whose pills are open, in the
//   order R raised them: each condition in full, in the colour of its
//   severity.
//   Each pill opens and closes its banners, and keeps its state when R
//   evaluates the block again.
//   The pill of the errors is open from the start, so that a failed block
//   shows its error at once.
// While R evaluates the block, the footer dims and keeps what it shows.
//
// A card draws the footer with Footer, as the last line of its output's box,
// and the banners with Banners, below the box.
// Both components draw from what useFooter() returns.

import { useCallback, useId, useState } from "react";
import type { ReactElement } from "react";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";

type Severity = Condition["severity"];

// Every severity, in the order of the pills.
const SEVERITIES: readonly Severity[] = ["error", "warning", "message"];

// What Footer and Banners draw from.
export interface FooterProps {
  // The block's status.
  readonly status: BlockStatus | undefined;
  // The status that the footer shows: the block's, where it has a line of its
  // own, and otherwise the last that had one.
  readonly shown: BlockStatus | undefined;
  // The severities whose pills and banners the card shows.
  readonly severities: readonly Severity[];
  // The severities whose banners are open.
  readonly open: ReadonlySet<Severity>;
  // Opens the banners of the severity, or closes them where they are open.
  readonly toggle: (severity: Severity) => void;
  // The id of the list of banners, which each pill controls.
  readonly panel: string;
}

// What the footer and the banners of a block draw from, for its status and
// the severities given, or every severity where none are given.
export function useFooter(status: BlockStatus | undefined, severities: readonly Severity[] = SEVERITIES): FooterProps {
  const shown = useShown(status);
  const [open, setOpen] = useState<ReadonlySet<Severity>>(OPEN_AT_START);
  const panel = useId();
  const toggle = useCallback((severity: Severity) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(severity)) next.add(severity);
      return next;
    });
  }, []);
  return { status, shown, severities, open, toggle, panel };
}

// The footer, which dims while R evaluates the block.
export function Footer({ status, shown, severities, open, toggle, panel }: FooterProps): ReactElement {
  const settled = status !== undefined && status === shown && !status.running;
  return (
    <footer
      aria-busy={status?.running === true}
      data-status={shown?.status}
      className={`bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:transition-opacity ${
        settled ? "" : "bc:opacity-50"
      }`}
    >
      {shown === undefined ? null : (
        <StatusLine status={shown} severities={severities} panel={panel} open={open} toggle={toggle} />
      )}
    </footer>
  );
}

// The banners of the conditions whose pills are open, in the order R raised
// them.
export function Banners({ shown, severities, open, panel }: FooterProps): ReactElement | null {
  const listed =
    shown === undefined
      ? []
      : shown.conditions.filter((condition) => severities.includes(condition.severity) && open.has(condition.severity));
  if (listed.length === 0) return null;
  return (
    <ul id={panel} aria-label="Conditions" className="bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0">
      {listed.map((condition, i) => (
        <Banner key={i} condition={condition} />
      ))}
    </ul>
  );
}

// How a card marks a condition of each severity, in its pill and in its
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
// or the reason of an invalid or a waiting block, in muted text, and a pill
// for each severity given of the conditions of R's reply.
// The pill of the errors reads "error", and those of the warnings and the
// messages count them.
function StatusLine({
  status,
  severities,
  panel,
  open,
  toggle,
}: {
  status: BlockStatus;
  severities: readonly Severity[];
  panel: string;
  open: ReadonlySet<Severity>;
  toggle: (severity: Severity) => void;
}): ReactElement | null {
  const said =
    status.status === "ready"
      ? status.summary
      : status.status === "invalid" || status.status === "waiting"
        ? (status.reason?.message ?? null)
        : null;

  const pills = SEVERITIES.filter((severity) => severities.includes(severity)).flatMap((severity) => {
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
      {said === null ? null : <span className="bc:text-text-muted">{said}</span>}
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
// own, and otherwise the last that had one.
// A block that reads stale or unevaluated has no reply for its revision yet,
// so the footer keeps what it showed while R evaluates the block.
function useShown(status: BlockStatus | undefined): BlockStatus | undefined {
  const [kept, setKept] = useState<BlockStatus | undefined>(undefined);
  const now = status !== undefined && status.status !== "stale" && status.status !== "unevaluated" ? status : undefined;
  if (now !== undefined && now !== kept) setKept(now);
  return now ?? kept;
}

// The severities whose banners are open as the card starts: the errors, since
// their pill names no more of them than their severity.
const OPEN_AT_START: ReadonlySet<Severity> = new Set<Severity>(["error"]);
