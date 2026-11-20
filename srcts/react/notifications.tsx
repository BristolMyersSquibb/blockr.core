// The list of notifications that core's React SDK offers a UI, in the message
// style of blockr.ui's design system
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1374-L1377).
// A UI hands it the notifications as useNotified() gives them.
// The UI places the list, and wraps it in what it needs, such as a live
// region or a fold.
// - The list holds an item for each notification, in the order given.
//   Each item holds the notification's severity in `data-severity`, and its
//   message as text.
// - An error takes the danger set of core's theme: text-danger on bg-danger,
//   with an edge of border-danger-subtle.
//   A warning takes the warning set in the same way.
//   The design system gives a message no colour, so a message takes the
//   neutral set: text-default on bg-subtle, with an edge of border-default.
// - Each item is 13px, with a radius of 6px.
//   It keeps the message's line breaks, and wraps a long word, such as a path.
// - The caller names the list, as "Errors".
// - Where no notification is given, the list draws nothing.

import type { ReactElement } from "react";
import type { Notification, Severity } from "../types/core-api.ts";

export interface NotificationListProps {
  // The notifications, in the order to show them in.
  readonly notifications: readonly Notification[];
  // The name by which a screen reader reads the list.
  readonly "aria-label"?: string;
}

export function NotificationList({ notifications, "aria-label": label }: NotificationListProps): ReactElement | null {
  if (notifications.length === 0) return null;
  return (
    <ul aria-label={label} className="bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0">
      {notifications.map(({ id, severity, message }) => (
        <li key={id} data-severity={severity} className={`${ITEM} ${SETS[severity]}`}>
          {message}
        </li>
      ))}
    </ul>
  );
}

// What every item takes: 13px with a line height of 1.5, a radius of 6px, a
// padding of 8px by 12px, and the message's line breaks.
const ITEM =
  "bc:rounded-md bc:border bc:px-3 bc:py-2 bc:font-sans bc:text-[0.8125rem] bc:leading-normal bc:whitespace-pre-wrap bc:wrap-anywhere";

// The colours of an item, by its severity: its edge, its fill and its text.
const SETS: Readonly<Record<Severity, string>> = {
  error: "bc:border-border-danger-subtle bc:bg-bg-danger bc:text-text-danger",
  warning: "bc:border-border-warning-subtle bc:bg-bg-warning bc:text-text-warning",
  message: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default",
};
