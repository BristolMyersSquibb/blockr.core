// The footer of a block's card, and the pills and the banners below the
// output's box, which the card of a block that serve() serves alone and the
// cards of core's basic frontend draw alike
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve).
//
// - The footer is one centred line, the last of the output's box: a ready
//   block's summary.
// - The pills sit on a centred line of their own below the box, since the box
//   holds what the block's renderer draws.
//   An invalid or a waiting block has a pill that reads "reason".
//   Then comes a pill for each severity of the conditions of R's reply that
//   the card shows.
//   The pill of the errors reads "error", and those of the warnings and the
//   messages count them, as "2 warnings".
//   No pill writes out a message, which may be of any length.
// - The banners are those of the pills that are open: the reason first, and
//   then the conditions in the order R raised them, each in full, in the
//   colour of its severity.
//   Each pill opens and closes its banners, and keeps its state when R
//   evaluates the block again.
//   The pill of the errors is open from the start, so that a failed block
//   shows its error at once.
// While R evaluates the block, the footer and the pills dim, and keep what
// they show.
//
// A card draws the footer with Footer, as the last line of its output's box,
// the pills with Pills below the box, and the banners with Banners below the
// pills.
// The three components draw from what useFooter() returns.

import { useCallback, useId, useState } from "react";
import type { ReactElement } from "react";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";

type Severity = Condition["severity"];

// What a pill opens: the reason of an invalid or a waiting block, or the
// conditions of a severity.
type Pill = "reason" | Severity;

// Every severity, in the order of the pills.
const SEVERITIES: readonly Severity[] = ["error", "warning", "message"];

// What Footer, Pills and Banners draw from.
export interface FooterProps {
  // The block's status.
  readonly status: BlockStatus | undefined;
  // The status that the footer shows: the block's, where it has a line of its
  // own, and otherwise the last that had one.
  readonly shown: BlockStatus | undefined;
  // The severities whose pills and banners the card shows.
  readonly severities: readonly Severity[];
  // The pills whose banners are open.
  readonly open: ReadonlySet<Pill>;
  // Opens the banners of the pill, or closes them where they are open.
  readonly toggle: (pill: Pill) => void;
  // The id of the list of banners, which each pill controls.
  readonly panel: string;
}

// What the footer, the pills and the banners of a block draw from, for its
// status and the severities given, or every severity where none are given.
export function useFooter(status: BlockStatus | undefined, severities: readonly Severity[] = SEVERITIES): FooterProps {
  const shown = useShown(status);
  const [open, setOpen] = useState<ReadonlySet<Pill>>(OPEN_AT_START);
  const panel = useId();
  const toggle = useCallback((pill: Pill) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(pill)) next.add(pill);
      return next;
    });
  }, []);
  return { status, shown, severities, open, toggle, panel };
}

// The footer, which dims while R evaluates the block.
export function Footer({ status, shown }: FooterProps): ReactElement {
  return (
    <footer
      aria-busy={status?.running === true}
      data-status={shown?.status}
      className={`bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:text-text-muted bc:transition-opacity ${
        settled(status, shown) ? "" : "bc:opacity-50"
      }`}
    >
      {shown?.status === "ready" ? shown.summary : null}
    </footer>
  );
}

// The pills, which dim while R evaluates the block: the reason's, and one for
// each severity given of the conditions of R's reply. A block with none has no
// line of pills.
export function Pills({ status, shown, severities, open, toggle, panel }: FooterProps): ReactElement | null {
  if (shown === undefined) return null;
  const pills: Pill[] = [
    ...(shown.reason === null ? [] : ["reason" as const]),
    ...SEVERITIES.filter((severity) => severities.includes(severity) && count(shown, severity) > 0),
  ];
  if (pills.length === 0) return null;
  return (
    <div
      role="group"
      aria-label="Status"
      aria-busy={status?.running === true}
      className={`bc:flex bc:flex-wrap bc:items-center bc:justify-center bc:gap-2 bc:px-5 bc:font-mono bc:text-xs bc:transition-opacity ${
        settled(status, shown) ? "" : "bc:opacity-50"
      }`}
    >
      {pills.map((pill) => {
        const mark = MARKS[pill];
        const n = pill === "reason" ? 1 : count(shown, pill);
        return (
          <button
            key={pill}
            type="button"
            aria-expanded={open.has(pill)}
            aria-controls={panel}
            onClick={() => {
              toggle(pill);
            }}
            className={`bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1 bc:rounded-full bc:border-0 bc:px-1.5 bc:py-0.5 bc:font-mono bc:text-xs bc:hover:underline bc:aria-expanded:ring-1 bc:aria-expanded:ring-current ${mark.tint} ${mark.text}`}
          >
            <PillIcon pill={pill} />
            {pill === "reason" || pill === "error" ? mark.noun : `${String(n)} ${mark.noun}${n === 1 ? "" : "s"}`}
          </button>
        );
      })}
    </div>
  );
}

// The banners of the pills that are open: the reason's first, and then the
// conditions in the order R raised them.
export function Banners({ shown, severities, open, panel }: FooterProps): ReactElement | null {
  if (shown === undefined) return null;
  const reason = open.has("reason") ? shown.reason : null;
  const listed = shown.conditions.filter(
    (condition) => severities.includes(condition.severity) && open.has(condition.severity),
  );
  if (reason === null && listed.length === 0) return null;
  return (
    <ul id={panel} aria-label="Reason and conditions" className="bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0">
      {reason === null ? null : <Banner key="reason" pill="reason" message={reason} />}
      {listed.map((condition, i) => (
        <Banner key={i} pill={condition.severity} message={condition.message} />
      ))}
    </ul>
  );
}

// Whether the footer and the pills show the block's status as it is: R
// evaluates nothing of it, and it has a line of its own.
function settled(status: BlockStatus | undefined, shown: BlockStatus | undefined): boolean {
  return status !== undefined && status === shown && !status.running;
}

// How many conditions of the severity R's reply holds.
function count(status: BlockStatus, severity: Severity): number {
  return status.conditions.filter((condition) => condition.severity === severity).length;
}

// How a card marks each pill, and its banners: in the theme's danger colour for
// an error, its warning colour for a warning, its accent, blue, for a message,
// and in muted grey for the reason, which is no condition.
const MARKS: Readonly<Record<Pill, { label: string; noun: string; text: string; tint: string; border: string }>> = {
  reason: {
    label: "Reason",
    noun: "reason",
    text: "bc:text-text-muted",
    tint: "bc:bg-gray-500/10",
    border: "bc:border-gray-400",
  },
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

// The reason or a condition in full, as a banner in the colour of its pill,
// with the pill's icon and name.
function Banner({ pill, message }: { pill: Pill; message: string }): ReactElement {
  const mark = MARKS[pill];
  return (
    <li
      data-pill={pill}
      className={`bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`}
    >
      <span className={`bc:mt-0.5 ${mark.text}`}>
        <PillIcon pill={pill} />
      </span>
      <span className="bc:grid bc:min-w-0 bc:gap-0.5">
        <span className={`bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`}>{mark.label}</span>
        <span className="bc:font-mono bc:text-xs bc:whitespace-pre-wrap">{message}</span>
      </span>
    </li>
  );
}

// The icon of a pill, drawn in the colour of the text around it: a cross in a
// circle for an error, an exclamation mark in a triangle for a warning, and an
// i in a circle for a message or the reason.
function PillIcon({ pill }: { pill: Pill }): ReactElement {
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
      {pill === "warning" ? (
        <>
          <path d="M8 1.8 14.6 13.6H1.4Z" />
          <path d="M8 6.2v3.2M8 11.6v.1" />
        </>
      ) : (
        <>
          <circle cx="8" cy="8" r="6.4" />
          <path d={pill === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1"} />
        </>
      )}
    </svg>
  );
}

// The status that the footer and the pills show: the block's, where it has a
// line of its own, and otherwise the last that had one.
// A block that reads stale or unevaluated has no reply for its revision yet,
// so they keep what they showed while R evaluates the block.
function useShown(status: BlockStatus | undefined): BlockStatus | undefined {
  const [kept, setKept] = useState<BlockStatus | undefined>(undefined);
  const now = status !== undefined && status.status !== "stale" && status.status !== "unevaluated" ? status : undefined;
  if (now !== undefined && now !== kept) setKept(now);
  return now ?? kept;
}

// The pills whose banners are open as the card starts: the errors, since their
// pill names no more of them than their severity.
const OPEN_AT_START: ReadonlySet<Pill> = new Set<Pill>(["error"]);
