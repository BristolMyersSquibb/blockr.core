// srcts/components/notifications.tsx
import { jsx } from "react/jsx-runtime";
function NotificationList({ notifications, "aria-label": label }) {
  if (notifications.length === 0) return null;
  return /* @__PURE__ */ jsx("ul", { "aria-label": label, className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: notifications.map(({ id, severity, message }) => /* @__PURE__ */ jsx("li", { "data-severity": severity, className: `${ITEM} ${SETS[severity]}`, children: message }, id)) });
}
var ITEM = "bc:rounded-md bc:border bc:px-3 bc:py-2 bc:font-sans bc:text-[0.8125rem] bc:leading-normal bc:whitespace-pre-wrap bc:wrap-anywhere";
var SETS = {
  error: "bc:border-destructive/35 bc:bg-destructive/10 bc:text-destructive-text",
  warning: "bc:border-warning/45 bc:bg-warning/10 bc:text-warning-text",
  message: "bc:border-border bc:bg-muted"
};
export {
  NotificationList
};
