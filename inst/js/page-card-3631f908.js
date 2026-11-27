// srcts/page/card.tsx
import { useCallback as useCallback2, useSyncExternalStore } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/card/footer.tsx
import { useCallback, useId, useState } from "react";
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
var SEVERITIES = ["error", "warning", "message"];
function useFooter(status, notifications) {
  const shown = useShown(status);
  const opened = useOpened(OPEN_AT_START);
  return { status, shown, settled: isSettled(status, shown), notifications, ...opened };
}
function useOpened(atStart) {
  const [open, setOpen] = useState(atStart);
  const panel = useId();
  const toggle = useCallback((severity) => {
    setOpen((was) => {
      const next = new Set(was);
      if (!next.delete(severity)) next.add(severity);
      return next;
    });
  }, []);
  return { open, toggle, panel };
}
function Footer({ status, shown, settled }) {
  return /* @__PURE__ */ jsx(
    "footer",
    {
      "aria-busy": status?.running === true,
      "data-status": shown?.status,
      className: `bc:min-h-4 bc:shrink-0 bc:px-5 bc:pb-1 bc:text-center bc:font-mono bc:text-xs bc:text-muted-foreground bc:transition-opacity ${settled ? "" : "bc:opacity-50"}`,
      children: shown?.status === "ready" ? shown.summary : null
    }
  );
}
function Pills({
  status,
  shown,
  settled,
  notifications,
  open,
  toggle,
  panel,
  className = "bc:justify-center bc:px-5"
}) {
  const pills = SEVERITIES.flatMap((severity) => {
    const n = counted(shown, notifications, severity);
    return n === 0 ? [] : [{ severity, n }];
  });
  if (pills.length === 0) return null;
  return /* @__PURE__ */ jsx(
    "div",
    {
      role: "group",
      "aria-label": "Status",
      "aria-busy": status?.running === true,
      className: `bc:flex bc:flex-wrap bc:items-center bc:gap-2 bc:font-mono bc:text-xs bc:transition-opacity ${className} ${settled ? "" : "bc:opacity-50"}`,
      children: pills.map(({ severity, n }) => {
        const mark = MARKS[severity];
        return /* @__PURE__ */ jsxs(
          "button",
          {
            type: "button",
            "aria-expanded": open.has(severity),
            "aria-controls": panel,
            onClick: () => {
              toggle(severity);
            },
            className: `bc:inline-flex bc:cursor-pointer bc:items-center bc:gap-1 bc:rounded-full bc:border-0 bc:px-1.5 bc:py-0.5 bc:font-mono bc:text-xs bc:hover:underline bc:aria-expanded:ring-1 bc:aria-expanded:ring-current ${mark.tint} ${mark.text}`,
            children: [
              /* @__PURE__ */ jsx(SeverityIcon, { severity }),
              severity === "error" ? mark.noun : `${String(n)} ${mark.noun}${n === 1 ? "" : "s"}`
            ]
          },
          severity
        );
      })
    }
  );
}
function Banners({ shown, notifications, open, panel }) {
  const listed = notifications.filter(
    (notification) => open.has(notification.severity)
  );
  const reason = shown?.reason ?? null;
  if (reason !== null && open.has("message")) listed.push({ severity: "message", message: reason });
  if (listed.length === 0) return null;
  return /* @__PURE__ */ jsx("ul", { id: panel, "aria-label": "Conditions", className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: listed.map(({ severity, message }, i) => /* @__PURE__ */ jsx(Banner, { severity, message }, i)) });
}
function isSettled(status, shown) {
  return status !== void 0 && status === shown && !status.running;
}
function counted(status, notifications, severity) {
  const n = notifications.filter((notification) => notification.severity === severity).length;
  return severity === "message" && (status?.reason ?? null) !== null ? n + 1 : n;
}
var MARKS = {
  error: {
    label: "Error",
    noun: "error",
    text: "bc:text-destructive-text",
    tint: "bc:bg-destructive/10",
    border: "bc:border-destructive"
  },
  warning: {
    label: "Warning",
    noun: "warning",
    text: "bc:text-warning-text",
    tint: "bc:bg-warning/10",
    border: "bc:border-warning"
  },
  message: {
    label: "Message",
    noun: "message",
    text: "bc:text-primary-text",
    tint: "bc:bg-primary/10",
    border: "bc:border-primary"
  }
};
function Banner({ severity, message }) {
  const mark = MARKS[severity];
  return /* @__PURE__ */ jsxs(
    "li",
    {
      "data-severity": severity,
      className: `bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`,
      children: [
        /* @__PURE__ */ jsx("span", { className: `bc:mt-0.5 ${mark.text}`, children: /* @__PURE__ */ jsx(SeverityIcon, { severity }) }),
        /* @__PURE__ */ jsxs("span", { className: "bc:grid bc:min-w-0 bc:gap-0.5", children: [
          /* @__PURE__ */ jsx("span", { className: `bc:font-sans bc:text-xs bc:font-semibold ${mark.text}`, children: mark.label }),
          /* @__PURE__ */ jsx("span", { className: "bc:font-mono bc:text-xs bc:whitespace-pre-wrap", children: message })
        ] })
      ]
    }
  );
}
function SeverityIcon({ severity }) {
  return /* @__PURE__ */ jsx(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      className: "bc:size-3 bc:shrink-0",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.6,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: severity === "warning" ? /* @__PURE__ */ jsxs(Fragment, { children: [
        /* @__PURE__ */ jsx("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ] }) : /* @__PURE__ */ jsxs(Fragment, { children: [
        /* @__PURE__ */ jsx("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx("path", { d: severity === "error" ? "M5.7 5.7l4.6 4.6M10.3 5.7l-4.6 4.6" : "M8 7.4v3.6M8 4.9v.1" })
      ] })
    }
  );
}
function useShown(status) {
  const [kept, setKept] = useState(void 0);
  const now = status !== void 0 && status.status !== "stale" && status.status !== "unevaluated" ? status : void 0;
  if (now !== void 0 && now !== kept) setKept(now);
  return now ?? kept;
}
var OPEN_AT_START = /* @__PURE__ */ new Set(["error"]);

// srcts/card/mounted.tsx
import { useLayoutEffect, useRef } from "react";
import { jsx as jsx2 } from "react/jsx-runtime";
function Mounted({
  mount,
  label,
  className,
  data,
  deferred = false
}) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    let cleanup;
    let gone = false;
    const start = () => {
      if (!gone) cleanup = mount(el);
    };
    if (deferred) queueMicrotask(start);
    else start();
    return () => {
      gone = true;
      queueMicrotask(() => {
        cleanup?.();
      });
    };
  }, [mount, deferred]);
  return /* @__PURE__ */ jsx2("div", { ref, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/card/status.ts
import { useState as useState2 } from "react";
function useSized(status) {
  const ready = status?.status === "ready";
  const [sized, setSized] = useState2(ready);
  if (ready && !sized) setSized(true);
  return sized || ready;
}

// srcts/page/card.tsx
import { useNotified, useStatus } from "@blockr/core/react";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}

// srcts/page/card.tsx
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function drawCard(el, core, id) {
  const root = createRoot(el);
  flushSync(() => {
    root.render(/* @__PURE__ */ jsx3(AloneCard, { core, id }));
  });
  return () => {
    root.unmount();
  };
}
function AloneCard({ core, id }) {
  const name = useBlockName(core, id);
  const status = useStatus(core, id);
  const notified = useNotified(core, null);
  const footer = useFooter(status, notified);
  const sized = useSized(status);
  const mountUi = useCallback2((el) => core.mountBlock(el, id), [core, id]);
  const mountOutput = useCallback2((el) => core.mountOutput(el, id), [core, id]);
  const inputs = !core.store.locked(id);
  const current = status?.status === "ready" && !status.running;
  return /* @__PURE__ */ jsxs2(
    "article",
    {
      "aria-label": name,
      className: "bc:mx-auto bc:my-4 bc:grid bc:max-w-5xl bc:min-w-0 bc:gap-3 bc:rounded-lg bc:border bc:border-border bc:bg-card bc:p-3 bc:font-sans bc:text-card-foreground",
      children: [
        /* @__PURE__ */ jsx3("header", { children: /* @__PURE__ */ jsx3("h1", { className: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold", children: name }) }),
        inputs ? /* @__PURE__ */ jsx3(Mounted, { mount: mountUi, label: "Inputs", className: "bc:min-w-0" }) : null,
        /* @__PURE__ */ jsxs2(
          "div",
          {
            className: `bc:min-w-0 ${sized ? "bc:flex bc:h-104 bc:min-h-24 bc:resize-y bc:flex-col bc:gap-2 bc:overflow-hidden" : ""}`,
            children: [
              /* @__PURE__ */ jsx3(
                Mounted,
                {
                  mount: mountOutput,
                  label: "Output",
                  data: { "data-current": String(current) },
                  className: `bc:min-w-0 bc:transition-opacity ${current ? "" : "bc:opacity-50 bc:grayscale"} ${sized ? "bc:min-h-0 bc:flex-1 bc:overflow-auto" : "bc:h-0 bc:overflow-hidden"}`
                }
              ),
              sized ? /* @__PURE__ */ jsx3(Footer, { ...footer }) : null
            ]
          }
        ),
        /* @__PURE__ */ jsx3(Pills, { ...footer }),
        /* @__PURE__ */ jsx3(Banners, { ...footer })
      ]
    }
  );
}
function useBlockName(core, id) {
  const subscribe = useCallback2(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore(subscribe, () => own(core.store.board().blocks, id)?.name ?? id);
}
export {
  drawCard
};
