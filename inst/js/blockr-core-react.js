// srcts/react/hooks.ts
import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import { bind, follow } from "@blockr/core/binding";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}
function jsonEqual(a, b) {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return Array.isArray(b) && a.length === b.length && a.every((x, i) => jsonEqual(x, b[i]));
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]));
  }
  return false;
}

// srcts/react/hooks.ts
var snapshots = /* @__PURE__ */ new WeakMap();
function useOutput(host) {
  const subscribe = useCallback((notify) => host.on("output", notify), [host]);
  const read = useCallback(() => outputSnapshot(host), [host]);
  return useSyncExternalStore(subscribe, read);
}
function outputSnapshot(host) {
  const next = {
    output: host.output(),
    body: host.body(),
    params: host.params(),
    loading: host.loading(),
    error: host.error()
  };
  const last = snapshots.get(host);
  if (last !== void 0 && Object.keys(next).every((key) => last[key] === next[key])) {
    return last;
  }
  snapshots.set(host, next);
  return next;
}
function useValue(host, key, change) {
  const read = typeof key === "function" ? key : null;
  const name = typeof key === "function" ? null : key ?? ("option" in host ? host.option : "");
  const given = read === null || change === void 0 ? null : { read, change };
  const [value, setValue] = useState(() => held(host, name, { current: given }));
  const [error, setError] = useState(null);
  const binding = useRef(null);
  const functions = useRef(given);
  useLayoutEffect(() => {
    binding.current = start(host, name, functions, setValue, setError);
    return () => {
      const bound = binding.current;
      binding.current = null;
      bound?.release();
    };
  }, [host, name]);
  useLayoutEffect(() => {
    if (read === null || change === void 0) return;
    const last = functions.current;
    if (last !== null && last.read === read && last.change === change) return;
    const moved = last === null || !readAlike(last.read, read, host.state());
    if (moved) binding.current?.release();
    functions.current = { read, change };
    if (moved) binding.current = start(host, name, functions, setValue, setError);
  }, [read, change, host, name]);
  return useMemo(
    () => ({
      value,
      error,
      get: () => binding.current?.get() ?? held(host, name, functions),
      commit: (next) => binding.current?.commit(next) ?? Promise.resolve(false),
      unset: () => binding.current?.unset() ?? Promise.resolve(false),
      type: (next) => {
        binding.current?.type(next);
      },
      flush: () => {
        binding.current?.flush();
      }
    }),
    [value, error, host, name]
  );
}
function useRequest(host, name, options = {}) {
  const [answered, setAnswered] = useState({ answer: void 0, error: null, loading: true });
  const reads = JSON.stringify([options.args ?? {}, options.reads ?? null]);
  useLayoutEffect(() => {
    const [args, extra] = JSON.parse(reads);
    const followed = follow(host, name, {
      args: () => args,
      reads: () => extra,
      loading: () => {
        setAnswered((last) => last.loading ? last : { ...last, loading: true });
      },
      answer: (answer) => {
        setAnswered({ answer, error: null, loading: false });
      },
      failed: (failure) => {
        setAnswered((last) => ({ answer: last.answer, error: errorMessage(failure), loading: false }));
      }
    });
    return () => {
      followed.release();
    };
  }, [host, name, reads]);
  return answered;
}
function start(host, name, functions, show, refused) {
  const options = {
    settled: (error) => {
      refused(error === null ? null : errorMessage(error));
    },
    notifier: () => void 0
  };
  if (name !== null) return bind(host, name, show, options);
  return bind(
    host,
    (state) => functions.current?.read(state),
    (value, state) => functions.current?.change(value, state) ?? {},
    show,
    options
  );
}
function held(host, name, functions) {
  if ("option" in host) return host.value();
  return name === null ? functions.current?.read(host.state()) : own(host.state(), name);
}
function readAlike(a, b, state) {
  try {
    return jsonEqual(a(state), b(state));
  } catch {
    return false;
  }
}
function useBoard(core, select) {
  const subscribe = useCallback(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  const read = useMemo(() => {
    if (select === void 0) return () => core.store.board();
    let last;
    return () => {
      const board = core.store.board();
      if (last?.board !== board) {
        const value = select(board);
        last = { board, value: last !== void 0 && jsonEqual(last.value, value) ? last.value : value };
      }
      return last.value;
    };
  }, [core, select]);
  return useSyncExternalStore(subscribe, read);
}
function useStatus(core, block) {
  const subscribe = useCallback(
    (notify) => core.scheduler.subscribe((changed) => {
      if (changed.includes(block)) notify();
    }),
    [core, block]
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}
function useNotified(core, about) {
  const subscribe = useCallback(
    (notify) => core.notifications.subscribe(() => {
      notify();
    }),
    [core]
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  const [kind, id] = about === null ? [] : Object.entries(about)[0] ?? [];
  return useMemo(
    () => kind === void 0 ? all : all.filter((notification) => notification.about !== void 0 && own(notification.about, kind) === id),
    [all, kind, id]
  );
}

// srcts/react/module.tsx
import { Component } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { jsx } from "react/jsx-runtime";
function reactModule(Draw) {
  return {
    render({ el, host, signal }) {
      const root = createRoot(el);
      flushSync(() => {
        root.render(/* @__PURE__ */ jsx(Draw, { host, signal }));
      });
      return () => {
        root.unmount();
      };
    }
  };
}
function outputRenderer(Draw) {
  function Renderer({ host }) {
    const now = useOutput(host);
    return /* @__PURE__ */ jsx("div", { className: "bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default", "aria-busy": now.loading, children: /* @__PURE__ */ jsx(Boundary, { host, output: now.output, children: /* @__PURE__ */ jsx("div", { className: `bc:flex bc:min-h-0 bc:flex-col ${now.loading ? "bc:opacity-60" : ""}`, children: /* @__PURE__ */ jsx(
      Draw,
      {
        output: now.output,
        body: now.body,
        params: now.params,
        loading: now.loading,
        error: now.error,
        host
      }
    ) }) }) });
  }
  return reactModule(Renderer);
}
var Boundary = class extends Component {
  state = { failed: false };
  // What withdraws the notification of the latest throw, while one holds.
  failure;
  static getDerivedStateFromError() {
    return { failed: true };
  }
  componentDidCatch(error) {
    this.props.host.log.error(asError(error));
    const before = this.failure;
    this.failure = this.props.host.notify({
      severity: "error",
      code: "render_failed",
      message: `The renderer failed to draw the output: ${errorMessage(error)}`
    });
    before?.();
  }
  // Another output draws the component again, but not the one that it threw
  // on, which may have come with the update in which it threw.
  componentDidUpdate(before, was) {
    if (this.state.failed) {
      if (was.failed && before.output !== this.props.output) this.setState({ failed: false });
    } else {
      this.withdrawFailure();
    }
  }
  componentWillUnmount() {
    this.withdrawFailure();
  }
  withdrawFailure() {
    const before = this.failure;
    this.failure = void 0;
    before?.();
  }
  render() {
    return this.state.failed ? null : this.props.children;
  }
};

// srcts/inputs/button.tsx
import { Button as AriaButton, composeRenderProps } from "./vendor-c5b8952d.js";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent bc:aria-invalid:accent-border-danger",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};
var buttons = {
  primary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  secondary: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  destructive: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-red-700 bc:bg-red-700 bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-white bc:cursor-pointer bc:hover:bg-red-800 bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/inputs/button.tsx
import { jsx as jsx2 } from "react/jsx-runtime";
function Button({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }) {
  return /* @__PURE__ */ jsx2(
    AriaButton,
    {
      ...props,
      render: shortcuts === void 0 || props.render !== void 0 ? props.render : (dom) => /* @__PURE__ */ jsx2("button", { ...dom, "aria-keyshortcuts": shortcuts }),
      className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)
    }
  );
}
function FieldButton({ className, ...props }) {
  return /* @__PURE__ */ jsx2(AriaButton, { ...props, className: composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`) });
}
var FIELD_BUTTON = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/calendar.tsx
import {
  Calendar as AriaCalendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  composeRenderProps as composeRenderProps2,
  RangeCalendar as AriaRangeCalendar,
  Text,
  useLocale
} from "./vendor-c5b8952d.js";

// srcts/inputs/bounddate.ts
import { useRef as useRef2, useState as useState2 } from "react";

// srcts/inputs/dates.ts
import { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "./vendor-c5b8952d.js";
var DATE = /^\d{4}-\d{2}-\d{2}$/;
var DATE_TIME = /^(\d{4}-\d{2}-\d{2})(?: (\d{2}:\d{2}(?::\d{2})?))?$/;
var TIME = /^\d{2}:\d{2}(?::\d{2})?$/;
function takesTime(granularity) {
  return granularity !== void 0 && granularity !== "day";
}
function readDate(value, time) {
  if (typeof value !== "string") return void 0;
  const parts = (time ? DATE_TIME : DATE).exec(value);
  if (parts === null) return void 0;
  try {
    return time ? parseDateTime(parts[2] === void 0 ? value : `${parts[1] ?? ""}T${parts[2]}`) : parseDate(value);
  } catch {
    return void 0;
  }
}
function writeDate(value, time) {
  return time ? toCalendarDateTime(value).toString().replace("T", " ") : toCalendarDate(value).toString();
}
function readTime(value) {
  if (typeof value !== "string" || !TIME.test(value)) return void 0;
  try {
    return parseTime(value);
  } catch {
    return void 0;
  }
}
function writeTime(value) {
  return ("day" in value ? toTime(value) : value).toString();
}
function readRange(value, time) {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return void 0;
  const record = value;
  const start2 = readDate(own(record, "start") ?? null, time);
  const end = readDate(own(record, "end") ?? null, time);
  return start2 === void 0 || end === void 0 ? void 0 : { start: start2, end };
}
function writeRange(value, time) {
  return { start: writeDate(value.start, time), end: writeDate(value.end, time) };
}
function dateFits(value, { minValue, maxValue, isDateUnavailable }) {
  return (minValue === void 0 || minValue === null || value.compare(minValue) >= 0) && (maxValue === void 0 || maxValue === null || value.compare(maxValue) <= 0) && isDateUnavailable?.(value) !== true;
}
function rangeFits(value, bounds) {
  return value.start.compare(value.end) <= 0 && dateFits(value.start, bounds) && dateFits(value.end, bounds);
}
function timeFits(value, { minValue, maxValue }) {
  const time = "day" in value ? toTime(value) : value;
  const at = (bound) => time.compare("day" in bound ? toTime(bound) : bound);
  return (minValue === void 0 || minValue === null || at(minValue) >= 0) && (maxValue === void 0 || maxValue === null || at(maxValue) <= 0);
}
function shownValue(value) {
  return typeof value === "string" ? value : JSON.stringify(value);
}

// srcts/inputs/bounddate.ts
var STEPS = /* @__PURE__ */ new Set(["ArrowUp", "ArrowDown", "PageUp", "PageDown", "Home", "End"]);
function useBoundDate(bound, { read, write, fits, required }) {
  const held3 = bound.value === void 0 ? null : read(bound.value);
  const [kept, setKept] = useState2(null);
  const latest = useRef2(null);
  const keep = (next) => {
    latest.current = next;
    setKept(next);
  };
  const stepped = useRef2(false);
  const [seen, setSeen] = useState2(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    if (kept?.submitted === true) setKept(null);
  }
  const submit = (value) => {
    const next = value === null ? void 0 : write(value);
    if (value === null ? required : !fits(value)) {
      keep(null);
      return;
    }
    if (jsonEqual(next, bound.value)) {
      keep(null);
      return;
    }
    keep({ value, submitted: true });
    void (next === void 0 ? bound.unset() : bound.commit(next)).then((took) => {
      if (!took) keep(null);
    });
  };
  return {
    value: kept === null ? held3 ?? null : kept.value,
    missing: kept === null && bound.value !== void 0 && held3 === void 0 ? shownValue(bound.value) : null,
    heard: (key) => {
      stepped.current = STEPS.has(key);
    },
    change: (value) => {
      if (stepped.current) {
        stepped.current = false;
        submit(value);
      } else {
        keep({ value, submitted: false });
      }
    },
    pick: submit,
    flush: () => {
      const edit = latest.current;
      if (edit !== null && !edit.submitted) submit(edit.value);
    }
  };
}

// srcts/inputs/inputs.tsx
import { useState as useState3 } from "react";
import { jsx as jsx3, jsxs } from "react/jsx-runtime";
function Stack({ children }) {
  return /* @__PURE__ */ jsx3("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx3("span", { className: styles.label, children: label }),
    children
  ] });
}
function NotAvailable({
  values,
  onRemove
}) {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-2", children: [
    /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: `Not available: ${named}` }),
    onRemove === void 0 ? null : /* @__PURE__ */ jsx3(Button, { variant: "quiet", "aria-label": `Remove ${named}`, onPress: onRemove, children: "Remove" })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = useState3(null);
  const [seen, setSeen] = useState3(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/inputs/calendar.tsx
import { Fragment, jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function Calendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx4(DrawnCalendar, { ...props }) : /* @__PURE__ */ jsx4(BoundCalendar, { ...props, bound });
}
function RangeCalendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx4(DrawnRange, { ...props }) : /* @__PURE__ */ jsx4(BoundRange, { ...props, bound });
}
function BoundCalendar({ bound, onChange, ...props }) {
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true
  });
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsx4(
      DrawnCalendar,
      {
        ...props,
        value: date.value,
        missing: date.missing,
        onChange: (value) => {
          date.pick(value);
          onChange?.(value);
        }
      }
    ),
    /* @__PURE__ */ jsx4(Refusal, { error: bound.error })
  ] });
}
function BoundRange({ bound, onChange, ...props }) {
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true
  });
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsx4(
      DrawnRange,
      {
        ...props,
        value: range.value,
        missing: range.missing,
        onChange: (value) => {
          range.pick(value);
          onChange?.(value);
        }
      }
    ),
    /* @__PURE__ */ jsx4(Refusal, { error: bound.error })
  ] });
}
function DrawnCalendar({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs2(
    AriaCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps2(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx4(Months, { count: props.visibleDuration?.months ?? 1, cell: (date) => /* @__PURE__ */ jsx4(CalendarCell, { date, className: CELL }) }),
        errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx4(Text, { slot: "errorMessage", className: styles.alert, children: errorMessage2 }),
        missing === null ? null : /* @__PURE__ */ jsx4(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function DrawnRange({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs2(
    AriaRangeCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps2(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx4(
          Months,
          {
            count: props.visibleDuration?.months ?? 1,
            cell: (date) => /* @__PURE__ */ jsx4(CalendarCell, { date, className: RANGE_CELL, children: ({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => /* @__PURE__ */ jsx4("span", { className: isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`, children: formattedDate }) })
          }
        ),
        errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx4(Text, { slot: "errorMessage", className: styles.alert, children: errorMessage2 }),
        missing === null ? null : /* @__PURE__ */ jsx4(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function Months({ count, cell }) {
  const { direction } = useLocale();
  return /* @__PURE__ */ jsx4(Fragment, { children: Array.from({ length: count }, (_, i) => /* @__PURE__ */ jsxs2("div", { className: MONTH, children: [
    /* @__PURE__ */ jsxs2("header", { className: HEADER, children: [
      i === 0 ? /* @__PURE__ */ jsx4(Button, { variant: "quiet", slot: "previous", className: PAGE, children: /* @__PURE__ */ jsx4(Arrow, { back: direction !== "rtl" }) }) : null,
      /* @__PURE__ */ jsx4(CalendarHeading, { offset: { months: i }, className: HEADING }),
      i === count - 1 ? /* @__PURE__ */ jsx4(Button, { variant: "quiet", slot: "next", className: PAGE, children: /* @__PURE__ */ jsx4(Arrow, { back: direction === "rtl" }) }) : null
    ] }),
    /* @__PURE__ */ jsxs2(CalendarGrid, { offset: { months: i }, className: GRID, children: [
      /* @__PURE__ */ jsx4(CalendarGridHeader, { children: (day) => /* @__PURE__ */ jsx4(CalendarHeaderCell, { className: WEEKDAY, children: day }) }),
      /* @__PURE__ */ jsx4(CalendarGridBody, { children: cell })
    ] })
  ] }, i)) });
}
function Arrow({ back }) {
  return /* @__PURE__ */ jsx4(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx4("polyline", { points: back ? "7.5 3 4.5 6 7.5 9" : "4.5 3 7.5 6 4.5 9" })
    }
  );
}
var CALENDAR = "bc:flex bc:w-fit bc:max-w-full bc:gap-3 bc:overflow-auto bc:font-sans bc:text-sm bc:text-text-default";
var MONTH = "bc:flex bc:flex-col bc:gap-2";
var HEADER = "bc:flex bc:items-center bc:gap-1";
var HEADING = "bc:m-0 bc:flex-1 bc:text-center bc:text-sm bc:font-semibold";
var PAGE = "bc:size-7 bc:justify-center bc:px-0 bc:text-text-default";
var GRID = "bc:border-collapse bc:border-spacing-0";
var WEEKDAY = "bc:size-8 bc:p-0 bc:text-xs bc:font-medium bc:text-text-muted";
var CELL = "bc:flex bc:size-8 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded-full bc:text-sm bc:tabular-nums bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-today:font-semibold bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-40 bc:data-unavailable:cursor-default bc:data-unavailable:line-through bc:data-unavailable:opacity-40 bc:data-invalid:data-selected:bg-border-danger";
var RANGE_CELL = "bc:group bc:size-8 bc:cursor-pointer bc:text-sm bc:tabular-nums bc:outline-none bc:data-selected:bg-bg-accent/15 bc:data-selection-start:rounded-s-full bc:data-selection-end:rounded-e-full bc:[td:first-child_&]:rounded-s-full bc:[td:last-child_&]:rounded-e-full bc:data-disabled:cursor-default bc:data-disabled:opacity-40 bc:data-unavailable:cursor-default bc:data-unavailable:line-through bc:data-unavailable:opacity-40 bc:data-invalid:data-selected:bg-border-danger/15";
var DAY = "bc:flex bc:size-full bc:items-center bc:justify-center bc:rounded-full bc:group-data-today:font-semibold bc:group-data-focus-visible:outline-2 bc:group-data-focus-visible:outline-offset-1 bc:group-data-focus-visible:outline-border-accent";
var END = "bc:bg-bg-accent bc:text-text-on-accent bc:group-data-invalid:bg-border-danger";
var UNPICKED = "bc:group-data-hovered:bg-bg-subtle bc:group-data-pressed:bg-bg-subtle";

// srcts/inputs/checkbox.tsx
import { useRef as useRef4 } from "react";
import { strings } from "@blockr/core/binding";
import {
  CheckboxButton,
  CheckboxField,
  CheckboxGroup as AriaCheckboxGroup,
  composeRenderProps as composeRenderProps3,
  FieldError,
  Label,
  Text as Text2
} from "./vendor-c5b8952d.js";

// srcts/inputs/icons.tsx
import { jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
function Chevron() {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted bc:transition-transform bc:group-data-open:rotate-180",
      children: /* @__PURE__ */ jsx5("polyline", { points: "3 4.5 6 7.5 9 4.5" })
    }
  );
}
function ChevronRight({ size = 12 }) {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: size,
      height: size,
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted",
      children: /* @__PURE__ */ jsx5("polyline", { points: "4.5 3 7.5 6 4.5 9" })
    }
  );
}
function Tick({ size = 14 }) {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: size,
      height: size,
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx5("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function Dash() {
  return /* @__PURE__ */ jsx5("svg", { width: "12", height: "12", viewBox: "0 0 16 16", fill: "none", stroke: "currentColor", strokeWidth: "1.5", strokeLinecap: "round", "aria-hidden": "true", children: /* @__PURE__ */ jsx5("path", { d: "M4 8h8" }) });
}
function Cross() {
  return /* @__PURE__ */ jsx5(
    "svg",
    {
      width: "10",
      height: "10",
      viewBox: "0 0 10 10",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx5("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" })
    }
  );
}
function Magnifier() {
  return /* @__PURE__ */ jsxs3(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      "aria-hidden": "true",
      className: "bc:shrink-0 bc:text-text-muted",
      children: [
        /* @__PURE__ */ jsx5("circle", { cx: "7", cy: "7", r: "4.5" }),
        /* @__PURE__ */ jsx5("path", { d: "M10.5 10.5L14 14" })
      ]
    }
  );
}

// srcts/inputs/items.ts
import { createContext, useCallback as useCallback2, useContext, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef3, useState as useState4 } from "react";
var ItemsContext = createContext(null);
function useItems() {
  const entries = useRef3(/* @__PURE__ */ new Map());
  const [known2, setKnown] = useState4(null);
  const sync = useCallback2(() => {
    const next = inPageOrder([...entries.current.values()]);
    setKnown((last) => last !== null && sameValues(last, next) ? last : next);
  }, []);
  const items = useMemo2(
    () => ({
      add(value, element) {
        const key = Symbol(value);
        entries.current.set(key, { value, element });
        sync();
        return () => {
          entries.current.delete(key);
          sync();
        };
      }
    }),
    [sync]
  );
  useLayoutEffect2(() => {
    sync();
  });
  return { known: known2, items };
}
function useItem(value, ref) {
  const items = useContext(ItemsContext);
  useLayoutEffect2(() => {
    if (items === null || value === void 0) return void 0;
    return items.add(value, () => ref.current);
  }, [items, value, ref]);
}
function inPageOrder(entries) {
  const placed = entries.map((entry) => ({ value: entry.value, element: entry.element() }));
  placed.sort((a, b) => {
    if (a.element === null || b.element === null) return a.element === null ? b.element === null ? 0 : 1 : -1;
    return a.element.compareDocumentPosition(b.element) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
  });
  return [...new Set(placed.map((entry) => entry.value))];
}
function sameValues(a, b) {
  return a.length === b.length && a.every((value, i) => value === b[i]);
}
function inOrder(values, order) {
  return [...order.filter((id) => values.includes(id)), ...values.filter((value) => !order.includes(value))];
}
function merged(now, before, picks) {
  const added = picks.filter((value) => !before.includes(value));
  const removed = before.filter((value) => !picks.includes(value));
  if (added.length === 0 && removed.length === 0) return null;
  return [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
}
function lacking(values, known2) {
  return known2 === null ? [] : values.filter((value) => !known2.includes(value));
}
function held2(values, known2) {
  return known2 === null ? [...values] : values.filter((value) => known2.includes(value));
}

// srcts/inputs/checkbox.tsx
import { Fragment as Fragment2, jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";
function Checkbox({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx6(DrawnCheckbox, { ...props }) : /* @__PURE__ */ jsx6(BoundCheckbox, { ...props, bound });
}
function BoundCheckbox({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs4(Fragment2, { children: [
    /* @__PURE__ */ jsx6(
      DrawnCheckbox,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx6(Refusal, { error: bound.error })
  ] });
}
function DrawnCheckbox({ children, description, errorMessage: errorMessage2, className, ...props }) {
  const ref = useRef4(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs4(CheckboxField, { ...props, ref, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsx6(CheckboxButton, { className: composeRenderProps3(className, (given) => `bc:group ${CHECK} ${given ?? ""}`), children: composeRenderProps3(children, (children2, { isSelected, isIndeterminate }) => /* @__PURE__ */ jsxs4(Fragment2, { children: [
      /* @__PURE__ */ jsx6("span", { className: BOX, children: isIndeterminate ? /* @__PURE__ */ jsx6(Dash, {}) : isSelected ? /* @__PURE__ */ jsx6(Tick, { size: 12 }) : null }),
      children2
    ] })) }),
    /* @__PURE__ */ jsx6(Text2, { slot: "description", className: `${styles.hint} bc:ml-6 bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx6(FieldError, { className: `${styles.alert} bc:ml-6`, children: errorMessage2 })
  ] });
}
function CheckboxGroup({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx6(DrawnGroup, { ...props }) : /* @__PURE__ */ jsx6(BoundGroup, { ...props, bound, outdated, ordered });
}
function BoundGroup({
  bound,
  outdated,
  ordered,
  onChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const shown = strings(bound.value);
  const gone = lacking(shown, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs4(Fragment2, { children: [
    /* @__PURE__ */ jsx6(
      DrawnGroup,
      {
        ...props,
        value: [...shown],
        isInvalid: props.isInvalid === true || marking,
        onChange: (picks) => {
          const value = merged(strings(bound.get()), shown, picks);
          if (value !== null) void bound.commit(ordered && known2 !== null ? inOrder(value, known2) : value);
          onChange?.(picks);
        },
        children: /* @__PURE__ */ jsx6(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx6(
      NotAvailable,
      {
        values: gone,
        onRemove: () => {
          void bound.commit(held2(strings(bound.get()), known2));
        }
      }
    ) : null,
    /* @__PURE__ */ jsx6(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup({ label, description, errorMessage: errorMessage2, children, className, ...props }) {
  return /* @__PURE__ */ jsxs4(AriaCheckboxGroup, { ...props, className: composeRenderProps3(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx6(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx6("div", { className: "bc:grid bc:gap-1.5", children }),
    /* @__PURE__ */ jsx6(Text2, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx6(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
var CHECK = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var BOX = "bc:box-border bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent bc:group-data-selected:text-text-on-accent bc:group-data-indeterminate:border-border-accent bc:group-data-indeterminate:bg-bg-accent bc:group-data-indeterminate:text-text-on-accent bc:group-data-invalid:border-border-danger bc:group-data-selected:group-data-invalid:bg-border-danger";

// srcts/inputs/colorfield.tsx
import { useContext as useContext2, useState as useState6 } from "react";
import { ColorField as AriaColorField, composeRenderProps as composeRenderProps4, FieldError as FieldError2, Input, Label as Label2, parseColor as parseColor2, Text as Text3 } from "./vendor-c5b8952d.js";

// srcts/inputs/colors.ts
import { createContext as createContext2, useState as useState5 } from "react";
import { parseColor } from "./vendor-c5b8952d.js";
var FALLBACK = "#000000";
function hex(color) {
  return color.getChannelValue("alpha") < 1 ? color.toString("hexa") : color.toString("hex");
}
function valueColor(value) {
  if (typeof value !== "string") return void 0;
  try {
    parseColor(value);
  } catch {
    return void 0;
  }
  return value;
}
var Ended = createContext2(null);
function useDragged(bound) {
  const value = valueColor(bound.value);
  const [kept, setKept] = useState5(null);
  const [seen, setSeen] = useState5(value);
  if (seen !== value) {
    setSeen(value);
    if (kept !== null && !kept.dragging && (value === void 0 || hex(parseColor(value)) !== hex(kept.color))) setKept(null);
  }
  return {
    shown: kept?.color ?? value ?? FALLBACK,
    change: (color) => {
      setKept({ color, dragging: true });
    },
    end: (color) => {
      setKept({ color, dragging: false });
      void bound.commit(hex(color)).then((took) => {
        if (!took) setKept((now) => now?.dragging === true ? now : null);
      });
    }
  };
}

// srcts/inputs/colorfield.tsx
import { Fragment as Fragment3, jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
function ColorField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Drawn, { ...props }) : /* @__PURE__ */ jsx7(BoundField, { ...props, bound });
}
function BoundField({ bound, onChange, onBlur, ...props }) {
  const value = valueColor(bound.value);
  const [typing, setTyping] = useState6(null);
  const shown = typing === null ? value : typing.value;
  return /* @__PURE__ */ jsxs5(Fragment3, { children: [
    /* @__PURE__ */ jsx7(
      Drawn,
      {
        ...props,
        value: shown ?? null,
        onInput: () => {
          setTyping((now) => now ?? { value: shown });
        },
        onChange: (color) => {
          setTyping(null);
          if (color !== null) {
            const alpha = shown === void 0 ? 1 : parseColor2(shown).getChannelValue("alpha");
            void bound.commit(hex(color.withChannelValue("alpha", alpha)));
          }
          onChange?.(color);
        },
        onBlur: (event) => {
          setTyping(null);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx7(Refusal, { error: bound.error })
  ] });
}
function Drawn({ label, description, errorMessage: errorMessage2, className, onChange, onInput, ...props }) {
  const ended = useContext2(Ended);
  return /* @__PURE__ */ jsxs5(
    AriaColorField,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        if (color !== null) ended?.(color);
      },
      className: composeRenderProps4(className, (given) => `${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx7(Label2, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx7(Input, { onInput, className: styles.box }),
        /* @__PURE__ */ jsx7(Text3, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx7(FieldError2, { className: styles.alert, children: errorMessage2 })
      ]
    }
  );
}

// srcts/inputs/colorparts.tsx
import { useContext as useContext3 } from "react";
import {
  ColorArea as AriaColorArea,
  ColorSlider as AriaColorSlider,
  ColorThumb as AriaColorThumb,
  ColorWheel as AriaColorWheel,
  ColorWheelTrack,
  composeRenderProps as composeRenderProps5,
  Label as Label3,
  SliderOutput,
  SliderTrack
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment4, jsx as jsx8, jsxs as jsxs6 } from "react/jsx-runtime";
function ColorThumb({ className, style, ...props }) {
  return /* @__PURE__ */ jsx8(
    AriaColorThumb,
    {
      ...props,
      className: composeRenderProps5(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps5(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Area, { ...props }) : /* @__PURE__ */ jsx8(BoundPart, { Part: Area, props, bound });
}
function ColorSlider({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Slider, { ...props }) : /* @__PURE__ */ jsx8(BoundPart, { Part: Slider, props, bound });
}
function ColorWheel({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Wheel, { ...props }) : /* @__PURE__ */ jsx8(BoundPart, { Part: Wheel, props, bound });
}
function BoundPart({
  Part,
  props,
  bound
}) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs6(Fragment4, { children: [
    /* @__PURE__ */ jsx8(
      Part,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          props.onChange?.(color);
        },
        onChangeEnd: (color) => {
          dragged.end(color);
          props.onChangeEnd?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx8(Refusal, { error: bound.error })
  ] });
}
function useEnd(onChangeEnd) {
  const ended = useContext3(Ended);
  return (color) => {
    onChangeEnd?.(color);
    ended?.(color);
  };
}
function Area({ className, style, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsx8(
    AriaColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps5(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps5(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx8(ColorThumb, {})
    }
  );
}
function Slider({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs6(
    AriaColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps5(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx8(Label3, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx8(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx8(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx8(ColorThumb, {})
          }
        )
      ]
    }
  );
}
function Wheel({ onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs6(AriaColorWheel, { ...props, onChangeEnd: useEnd(onChangeEnd), outerRadius: 100, innerRadius: 74, children: [
    /* @__PURE__ */ jsx8(
      ColorWheelTrack,
      {
        className: WHEEL_TRACK,
        style: ({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
        })
      }
    ),
    /* @__PURE__ */ jsx8(ColorThumb, {})
  ] });
}
var THUMB = "bc:box-border bc:size-4.5 bc:top-[50%] bc:left-[50%] bc:rounded-full bc:border-2 bc:border-white bc:data-focus-visible:size-8 bc:data-dragging:bg-gray-700 bc:dark:data-dragging:bg-gray-300 bc:forced-colors:data-dragging:bg-[ButtonBorder] bc:data-disabled:border-gray-300 bc:data-disabled:bg-gray-300 bc:dark:data-disabled:border-gray-700 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:border-[GrayText] bc:forced-colors:data-disabled:bg-[GrayText]";
var AREA = "bc:aspect-square bc:w-full bc:max-w-56 bc:rounded-lg bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var SLIDER = "bc:grid-cols-[1fr_auto] bc:flex-col bc:items-center bc:gap-2 bc:font-sans bc:text-text-default bc:data-[orientation=horizontal]:grid bc:data-[orientation=horizontal]:w-56 bc:data-[orientation=vertical]:flex";
var OUTPUT = "bc:text-sm bc:font-medium bc:text-text-muted bc:data-[orientation=vertical]:hidden";
var TRACK = "bc:group bc:col-span-2 bc:rounded-md";
var DISABLED = "bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var WHEEL_TRACK = "bc:data-disabled:bg-gray-300 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:bg-[GrayText]";
var CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";

// srcts/inputs/colorpicker.tsx
import { ColorPicker as AriaColorPicker, Button as Button3 } from "./vendor-c5b8952d.js";

// srcts/overlays/dialog.tsx
import { Button as Button2, composeRenderProps as composeRenderProps6, Dialog as AriaDialog, Heading, Modal as AriaModal, ModalOverlay } from "./vendor-c5b8952d.js";
import { Fragment as Fragment5, jsx as jsx9, jsxs as jsxs7 } from "react/jsx-runtime";
function Modal({ className, ...props }) {
  return /* @__PURE__ */ jsx9(ModalOverlay, { ...props, className: OVERLAY, children: /* @__PURE__ */ jsx9("div", { className: CENTER, children: /* @__PURE__ */ jsx9(AriaModal, { ...props, className: composeRenderProps6(className, (given) => `${MODAL} ${given ?? ""}`) }) }) });
}
function Dialog({ className, ...props }) {
  return /* @__PURE__ */ jsx9(AriaDialog, { ...props, className: `${DIALOG} ${className ?? ""}` });
}
function AlertDialog({
  title,
  variant = "info",
  actionLabel,
  cancelLabel = "Cancel",
  onAction,
  children,
  ...props
}) {
  const destructive = variant === "destructive";
  return /* @__PURE__ */ jsx9(Dialog, { role: "alertdialog", ...props, children: ({ close }) => /* @__PURE__ */ jsxs7(Fragment5, { children: [
    /* @__PURE__ */ jsx9(Heading, { slot: "title", className: TITLE, children: title }),
    /* @__PURE__ */ jsx9("span", { className: `bc:absolute bc:top-4 bc:right-4 ${destructive ? "bc:text-text-danger" : "bc:text-blue-700 bc:dark:text-blue-400"}`, children: /* @__PURE__ */ jsx9(Mark, { destructive }) }),
    /* @__PURE__ */ jsx9("p", { className: "bc:mt-3 bc:mb-0 bc:font-sans bc:text-sm bc:text-text-muted", children }),
    /* @__PURE__ */ jsxs7("div", { className: "bc:mt-6 bc:flex bc:justify-end bc:gap-2", children: [
      /* @__PURE__ */ jsx9(Button2, { className: buttons.secondary, onPress: close, children: cancelLabel }),
      /* @__PURE__ */ jsx9(
        Button2,
        {
          className: destructive ? buttons.destructive : buttons.primary,
          autoFocus: true,
          onPress: () => {
            onAction?.();
            close();
          },
          children: actionLabel
        }
      )
    ] })
  ] }) });
}
function Mark({ destructive }) {
  return /* @__PURE__ */ jsxs7(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      className: "bc:block bc:size-5",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.4,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: [
        /* @__PURE__ */ jsx9("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx9("path", { d: destructive ? "M8 4.6v4.2M8 11.2v.1" : "M8 7.4v3.6M8 4.9v.1" })
      ]
    }
  );
}
var OVERLAY = "bc:absolute bc:top-0 bc:left-0 bc:isolate bc:z-overlay bc:h-(--page-height) bc:w-full bc:bg-black/40";
var CENTER = "bc:sticky bc:top-0 bc:left-0 bc:box-border bc:flex bc:h-(--visual-viewport-height) bc:w-full bc:items-center bc:justify-center bc:p-4";
var MODAL = "bc:box-border bc:max-h-[calc(var(--visual-viewport-height)*.9)] bc:w-full bc:max-w-xl bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-xl bc:outline-none bc:dark:text-gray-100";
var DIALOG = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";
var TITLE = "bc:m-0 bc:pr-8 bc:font-sans bc:text-lg bc:font-semibold";

// srcts/overlays/menu.tsx
import { Children } from "react";
import {
  Collection,
  composeRenderProps as composeRenderProps9,
  Header,
  Menu as AriaMenu,
  MenuItem as AriaMenuItem,
  MenuSection as AriaMenuSection,
  MenuTrigger as AriaMenuTrigger,
  SubmenuTrigger as AriaSubmenuTrigger
} from "./vendor-c5b8952d.js";

// srcts/overlays/popover.tsx
import { composeRenderProps as composeRenderProps8, Popover as AriaPopover } from "./vendor-c5b8952d.js";

// srcts/overlays/stacking.ts
import { composeRenderProps as composeRenderProps7 } from "./vendor-c5b8952d.js";
var OVERLAY_Z_INDEX = "var(--bc-z-index-overlay)";
function overlayStyle(style) {
  return composeRenderProps7(style, (given) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}

// srcts/overlays/popover.tsx
import { jsx as jsx10 } from "react/jsx-runtime";
function Popover({ className, style, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx10(
    AriaPopover,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps8(className, (given) => `${POPOVER} ${given ?? ""}`)
    }
  );
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment6, jsx as jsx11, jsxs as jsxs8 } from "react/jsx-runtime";
function MenuTrigger({ placement, children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs8(AriaMenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx11(Popover, { placement, className: "bc:min-w-44", children: menu })
  ] });
}
function SubmenuTrigger({ children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs8(AriaSubmenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx11(Popover, { offset: -2, crossOffset: -4, className: "bc:min-w-44", children: menu })
  ] });
}
function Menu({ className, ...props }) {
  return /* @__PURE__ */ jsx11(AriaMenu, { ...props, className: composeRenderProps9(className, (given) => `${MENU} ${given ?? ""}`) });
}
function MenuItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx11(AriaMenuItem, { ...props, textValue, className: composeRenderProps9(props.className, (given) => `${ITEM} ${given ?? ""}`), children: composeRenderProps9(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => /* @__PURE__ */ jsxs8(Fragment6, { children: [
    selectionMode === "none" ? null : /* @__PURE__ */ jsx11("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx11(Tick, {}) : null }),
    /* @__PURE__ */ jsx11("span", { className: CONTENT, children }),
    hasSubmenu ? /* @__PURE__ */ jsx11(ChevronRight, { size: 14 }) : null
  ] })) });
}
function MenuSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs8(AriaMenuSection, { ...props, children: [
    title === void 0 ? null : /* @__PURE__ */ jsx11(Header, { className: HEADING2, children: title }),
    /* @__PURE__ */ jsx11(Collection, { items, children })
  ] });
}
var MENU = "bc:grid bc:max-h-[inherit] bc:gap-0.5 bc:overflow-auto bc:p-1 bc:outline-none";
var ITEM = "bc:box-border bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var HEADING2 = "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
var CONTENT = "bc:grid bc:min-w-0 bc:flex-1 bc:grid-cols-[minmax(0,1fr)_auto] bc:items-center bc:gap-x-4 bc:[&>*]:truncate bc:[&>[slot=description]]:col-start-1 bc:[&>[slot=description]]:text-xs bc:[&>[slot=description]]:text-text-muted bc:[&>kbd]:col-start-2 bc:[&>kbd]:row-start-1 bc:[&>kbd]:font-sans bc:[&>kbd]:text-xs bc:[&>kbd]:text-text-muted";

// srcts/overlays/palette.tsx
import { Autocomplete, useFilter } from "./vendor-c5b8952d.js";
import { jsx as jsx12, jsxs as jsxs9 } from "react/jsx-runtime";
function CommandPalette({
  isOpen,
  onOpenChange,
  placeholder = "Search commands",
  inputValue,
  defaultInputValue,
  onInputChange,
  filter,
  disableAutoFocusFirst,
  disableVirtualFocus,
  onAction,
  shouldCloseOnSelect = true,
  ...menu
}) {
  const { contains } = useFilter({ sensitivity: "base" });
  return /* @__PURE__ */ jsx12(Modal, { isDismissable: true, isOpen, onOpenChange, children: /* @__PURE__ */ jsx12(Dialog, { "aria-label": placeholder, className: "bc:flex bc:max-h-[inherit] bc:flex-col bc:gap-2 bc:p-2", children: /* @__PURE__ */ jsxs9(
    Autocomplete,
    {
      filter: filter ?? contains,
      inputValue,
      defaultInputValue,
      onInputChange,
      disableAutoFocusFirst,
      disableVirtualFocus,
      children: [
        /* @__PURE__ */ jsx12(SearchField, { autoFocus: true, "aria-label": placeholder, placeholder }),
        /* @__PURE__ */ jsx12(
          Menu,
          {
            renderEmptyState: () => /* @__PURE__ */ jsx12("p", { className: NOTE, children: "No results found." }),
            ...menu,
            onAction: (key, value) => {
              onAction?.(key, value);
              if (shouldCloseOnSelect) onOpenChange(false);
            },
            className: "bc:min-h-0 bc:flex-1"
          }
        )
      ]
    }
  ) }) });
}
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/overlays/sheet.tsx
import {
  composeRenderProps as composeRenderProps10,
  Sheet as AriaSheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay
} from "./vendor-c5b8952d.js";
import { jsx as jsx13, jsxs as jsxs10 } from "react/jsx-runtime";
function Sheet({ children, className, overscrollPadding = true, ...props }) {
  const { snapPoints } = props;
  return /* @__PURE__ */ jsxs10(SheetOverlay, { ...props, className: composeRenderProps10(className, (given) => `bc:group/sheet bc:z-overlay ${given ?? ""}`), children: [
    /* @__PURE__ */ jsx13(
      SheetBackdrop,
      {
        className: BACKDROP,
        swipeAnimation: "bc-sheet-backdrop",
        swipeAnimationRange: snapPoints === void 0 ? void 0 : { start: snapPoints.length - 1 }
      }
    ),
    /* @__PURE__ */ jsx13(
      AriaSheet,
      {
        overscrollPadding,
        stackAnimation: "bc-sheet-scale-back",
        className: ({ position }) => `${SHEET} ${POSITIONS[position]}`,
        children: /* @__PURE__ */ jsx13(SheetContent, { className: CONTENT2, children })
      }
    )
  ] });
}
var BACKDROP = "bc:data-[stack-index=0]:bg-black/40";
var SHEET = "bc:z-[1] bc:overflow-clip bc:rounded-2xl bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-2xl bc:outline-hidden bc:will-change-transform bc:[transform:perspective(1000px)] bc:forced-colors:bg-[Canvas] bc:dark:text-gray-100";
var POSITIONS = {
  bottom: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_-150px]",
  top: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_150px]",
  center: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_-150px]",
  left: "bc:h-[calc(100%-1rem)] bc:w-[300px] bc:origin-[150px_center]",
  right: "bc:h-[calc(100%-1rem)] bc:w-[300px] bc:origin-[-150px_center]"
};
var CONTENT2 = "bc:box-border bc:h-full bc:overflow-hidden bc:overscroll-auto bc:p-6 bc:pb-[calc(--spacing(6)+var(--sheet-scroll-padding-y))] bc:outline-hidden bc:[scrollbar-gutter:stable] bc:group-data-[expanded]/sheet:overflow-auto";

// srcts/overlays/toast.tsx
import { composeRenderProps as composeRenderProps11, UNSTABLE_Toast as AriaToast, UNSTABLE_ToastRegion as AriaToastRegion } from "./vendor-c5b8952d.js";
import { UNSTABLE_ToastContent, UNSTABLE_ToastQueue } from "./vendor-c5b8952d.js";
import { jsx as jsx14 } from "react/jsx-runtime";
var FADES_FOR = 300;
function ToastRegion({ className, ...props }) {
  return /* @__PURE__ */ jsx14(AriaToastRegion, { ...props, className: composeRenderProps11(className, (given) => `${REGION} ${given ?? ""}`) });
}
function Toast({ className, style, ...props }) {
  const { timeout } = props.toast;
  const fade = timeout === void 0 ? void 0 : { "--bc-toast-fade-at": `${String(Math.max(0, timeout - FADES_FOR))}ms` };
  return /* @__PURE__ */ jsx14(
    AriaToast,
    {
      ...props,
      style: composeRenderProps11(style, (given) => ({ ...fade, ...given })),
      className: composeRenderProps11(className, (given) => `${TOAST} ${timeout === void 0 ? "" : FADE} ${given ?? ""}`)
    }
  );
}
var REGION = "bc:group/toasts bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-[calc(var(--bc-z-index-overlay)+1)] bc:m-0 bc:box-border bc:flex bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:flex-col-reverse bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var TOAST = "bc:pointer-events-auto bc:box-border bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-3 bc:py-2 bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:dark:text-gray-100";
var FADE = "bc:toast-fade bc:group-data-hovered/toasts:[animation-play-state:paused] bc:group-focus-within/toasts:[animation-play-state:paused]";

// srcts/overlays/tooltip.tsx
import { composeRenderProps as composeRenderProps12, OverlayArrow, Tooltip as AriaTooltip } from "./vendor-c5b8952d.js";
import { jsx as jsx15, jsxs as jsxs11 } from "react/jsx-runtime";
function Tooltip({ children, className, style, offset = 8, ...props }) {
  return /* @__PURE__ */ jsxs11(
    AriaTooltip,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps12(className, (given) => `${TOOLTIP} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx15(OverlayArrow, { children: /* @__PURE__ */ jsx15("svg", { width: 8, height: 8, viewBox: "0 0 8 8", className: ARROW, "aria-hidden": "true", children: /* @__PURE__ */ jsx15("path", { d: "M0 0 L4 4 L8 0" }) }) }),
        children
      ]
    }
  );
}
var TOOLTIP = "bc:group bc:box-border bc:max-w-xs bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-gray-900 bc:shadow-md bc:outline-none bc:will-change-transform bc:dark:text-gray-100";
var ARROW = "bc:block bc:fill-bg-raised bc:stroke-border-default bc:forced-colors:fill-[Canvas] bc:forced-colors:stroke-[ButtonBorder] bc:group-data-[placement=bottom]:rotate-180 bc:group-data-[placement=left]:-rotate-90 bc:group-data-[placement=right]:rotate-90";

// srcts/overlays/index.ts
import { DialogTrigger, Focusable, Heading as Heading2, Keyboard, TooltipTrigger } from "./vendor-c5b8952d.js";

// srcts/inputs/colorswatches.tsx
import { useContext as useContext4 } from "react";
import {
  ColorSwatch as AriaColorSwatch,
  ColorSwatchPicker as AriaColorSwatchPicker,
  ColorSwatchPickerItem as AriaColorSwatchPickerItem,
  composeRenderProps as composeRenderProps13
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment7, jsx as jsx16, jsxs as jsxs12 } from "react/jsx-runtime";
function ColorSwatch({ className, style, ...props }) {
  return /* @__PURE__ */ jsx16(
    AriaColorSwatch,
    {
      ...props,
      className: composeRenderProps13(className, (given) => `${SWATCH} ${given ?? ""}`),
      style: composeRenderProps13(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx16(Drawn2, { ...props }) : /* @__PURE__ */ jsx16(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs12(Fragment7, { children: [
    /* @__PURE__ */ jsx16(
      Drawn2,
      {
        ...props,
        value: valueColor(bound.value) ?? FALLBACK,
        onChange: (color) => {
          void bound.commit(hex(color));
          onChange?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx16(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext4(Ended);
  return /* @__PURE__ */ jsx16(
    AriaColorSwatchPicker,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        ended?.(color);
      },
      className: composeRenderProps13(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem({ className, ...props }) {
  return /* @__PURE__ */ jsx16(AriaColorSwatchPickerItem, { ...props, className: composeRenderProps13(className, (given) => `${ITEM2} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs12(Fragment7, { children: [
    /* @__PURE__ */ jsx16(ColorSwatch, {}),
    isSelected ? /* @__PURE__ */ jsx16("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bc:box-border bc:size-8 bc:shrink-0 bc:rounded-md bc:border bc:border-border-default";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bc:flex bc:gap-1";
var ITEM2 = "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var SELECTED = "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";

// srcts/inputs/colorpicker.tsx
import { Fragment as Fragment8, jsx as jsx17, jsxs as jsxs13 } from "react/jsx-runtime";
function ColorPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx17(Drawn3, { ...props }) : /* @__PURE__ */ jsx17(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs13(Fragment8, { children: [
    /* @__PURE__ */ jsx17(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx17(
      Drawn3,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          onChange?.(color);
        }
      }
    ) }),
    /* @__PURE__ */ jsx17(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx17(AriaColorPicker, { ...props, children: /* @__PURE__ */ jsxs13(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs13(Button3, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx17(ColorSwatch, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx17("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx17(Popover, { placement: "bottom start", children: /* @__PURE__ */ jsx17(Dialog, { className: "bc:flex bc:flex-col bc:gap-2", children: children ?? /* @__PURE__ */ jsxs13(Fragment8, { children: [
      /* @__PURE__ */ jsx17(ColorArea, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx17(ColorSlider, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx17(ColorField, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded-xs bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/combobox.tsx
import { useContext as useContext5, useEffect, useEffectEvent, useRef as useRef5, useState as useState7 } from "react";
import { strings as strings2 } from "@blockr/core/binding";
import {
  Button as Button4,
  Collection as Collection2,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps as composeRenderProps14,
  FieldError as FieldError3,
  Group,
  Header as Header2,
  Input as Input2,
  Label as Label4,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Popover as Popover2,
  Tag,
  TagGroup,
  TagList,
  Text as Text4,
  TooltipTriggerStateContext,
  Virtualizer
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment9, jsx as jsx18, jsxs as jsxs14 } from "react/jsx-runtime";
function ComboBox(props) {
  const { bound, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx18(Drawn4, { ...rest });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx18(BoundSeveral, { ...rest, bound, ordered }) : /* @__PURE__ */ jsx18(BoundOne, { ...rest, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      Drawn4,
      {
        ...props,
        selectionMode: "single",
        outdated,
        marking: !outdated && known(props),
        onClear: () => {
          void bound.unset();
        },
        value: current,
        onChange: (key) => {
          if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onOpenChange,
  ...props
}) {
  const items = props.items ?? props.defaultItems;
  const held3 = strings2(bound.value);
  const [picks, setPicks] = useState7(null);
  const pending = useRef5(null);
  const submit = (next, opened) => {
    const value = merged(strings2(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && items !== void 0 ? inOrder(value, ids(items)) : value);
  };
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      Drawn4,
      {
        ...props,
        selectionMode: "multiple",
        outdated,
        marking: !outdated && known(props),
        value: picks ?? held3,
        onOpenChange: (isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held3, opened: held3 };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen, trigger);
        },
        onChange: (keys) => {
          if (!Array.isArray(keys)) return;
          const next = keys.map(String);
          if (pending.current === null) {
            submit(next, held3);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function known(props) {
  return (props.items ?? props.defaultItems) !== void 0 || typeof props.children !== "function";
}
function ids(items) {
  return [...items].flatMap((item) => {
    const id = item.id;
    return typeof id === "string" || typeof id === "number" ? [String(id)] : [];
  });
}
function Drawn4({
  label,
  description,
  errorMessage: errorMessage2,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  className,
  allowsEmptyCollection = true,
  ...props
}) {
  const multiple = props.selectionMode === "multiple";
  const isDisabled = props.isDisabled === true;
  const editable = !isDisabled && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  const unknown = given === void 0 && typeof children === "function";
  const none = given === void 0 ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading\u2026" : none ? renderEmptyState?.() ?? "No options" : "No matches";
  const value = useSameValue(props.value);
  return /* @__PURE__ */ jsxs14(
    AriaComboBox,
    {
      ...props,
      value,
      defaultItems: unknown ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps14(className, (given2) => `bc:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx18(Label4, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx18(
          Field2,
          {
            placeholder,
            multiple,
            marking,
            isDisabled,
            editable,
            clearable: props.isRequired !== true && editable,
            onClear,
            label
          }
        ),
        /* @__PURE__ */ jsx18(Text4, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx18(FieldError3, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx18(Lacking, { multiple, marking }),
        /* @__PURE__ */ jsx18(Popover2, { offset: 4, className: LIST, style: { zIndex: OVERLAY_Z_INDEX }, children: /* @__PURE__ */ jsx18(Virtualizer, { layout: ListLayout, layoutOptions: LAYOUT, children: /* @__PURE__ */ jsx18(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx18("p", { className: NOTE2, children: empty }), className: LISTBOX, children }) }) })
      ]
    }
  );
}
function useSameValue(value) {
  const [drawn, setDrawn] = useState7(value);
  const same = drawn === value || Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value);
  if (!same) setDrawn(value);
  return same ? drawn : value;
}
function sameKeys(a, b) {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}
function hasChildren(children) {
  return Array.isArray(children) ? children.some((child) => hasChildren(child)) : children !== null && children !== void 0 && children !== false;
}
function Field2({
  placeholder,
  multiple,
  marking,
  isDisabled,
  editable,
  clearable,
  onClear,
  label
}) {
  const { state, keys, one, known: known2, lacked } = usePicks(multiple);
  const item = one === null ? void 0 : known2.get(one);
  const invalid = marking && lacked.length > 0;
  const remove = (gone) => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  const chips = keys.map((key) => ({ key, text: known2.get(key) ?? String(key), gone: marking && !known2.has(key) }));
  const picks = label === void 0 ? "Picks" : `Picks of ${label}`;
  return /* @__PURE__ */ jsxs14(Group, { isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs14("div", { className: VALUES, children: [
      keys.length === 0 ? null : isDisabled ? (
        // The chips of a disabled select take no focus, as its text field
        // takes none.
        // React Aria's TagGroup keeps a tab stop on its list even where
        // every tag is disabled, so a disabled select draws its chips as
        // a plain list.
        /* @__PURE__ */ jsx18("div", { role: "list", "aria-label": picks, className: "bc:flex bc:min-w-0 bc:max-w-full bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx18(PlainChip, { text, gone }, key)) })
      ) : /* @__PURE__ */ jsx18(TagGroup, { "aria-label": picks, onRemove: editable ? remove : void 0, className: "bc:flex bc:min-w-0 bc:max-w-full", children: /* @__PURE__ */ jsx18(TagList, { className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx18(Tag, { id: key, textValue: text, "data-missing": gone || void 0, className: CHIP, children: ({ isFocusVisible }) => /* @__PURE__ */ jsxs14(Fragment9, { children: [
        /* @__PURE__ */ jsx18(ChipText, { text, focused: isFocusVisible }),
        editable ? /* @__PURE__ */ jsx18(Button4, { slot: "remove", "aria-label": `Remove ${text}`, className: CHIP_REMOVE, children: /* @__PURE__ */ jsx18(Cross, {}) }) : null
      ] }) }, key)) }) }),
      /* @__PURE__ */ jsx18(
        Input2,
        {
          value: multiple || state === null || state.isFocused || state.isOpen ? void 0 : item ?? (one === null ? "" : String(one)),
          placeholder: one !== null && item === void 0 ? String(one) : keys.length > 0 ? void 0 : placeholder,
          "aria-invalid": invalid || void 0,
          className: INPUT,
          onClick: () => {
            if (editable && state !== null && !state.isOpen) state.open(null, "manual");
          },
          onKeyDown: (event) => {
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (!editable || event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }
        }
      )
    ] }),
    clearable && (keys.length > 0 || one !== null) ? (
      // A FieldButton whose slot is null, which React Aria's ComboBox then
      // leaves alone, rather than take it for the button that opens the
      // list. It is outside the order of Tab, and keeps the focus in the
      // text field, as a press in the list does.
      // It is the trigger of its tooltip, as a button of React Aria is.
      /* @__PURE__ */ jsxs14(TooltipTrigger, { children: [
        /* @__PURE__ */ jsx18(
          FieldButton,
          {
            slot: null,
            excludeFromTabOrder: true,
            preventFocusOnPress: true,
            "aria-label": label === void 0 ? "Clear" : `Clear ${label}`,
            onPress: () => {
              if (onClear !== void 0) onClear();
              else state?.setValue(multiple ? [] : null);
            },
            children: /* @__PURE__ */ jsx18(Cross, {})
          }
        ),
        /* @__PURE__ */ jsx18(Tooltip, { children: multiple ? "Clear every pick" : "Clear the pick" })
      ] })
    ) : null,
    /* @__PURE__ */ jsx18(Button4, { className: OPENER, children: /* @__PURE__ */ jsx18(Chevron, {}) })
  ] });
}
function PlainChip({ text, gone }) {
  return /* @__PURE__ */ jsx18("div", { role: "listitem", "data-missing": gone || void 0, className: CHIP, children: /* @__PURE__ */ jsx18(ChipText, { text, focused: false }) });
}
function ChipText({ text, focused }) {
  return /* @__PURE__ */ jsx18(TooltipTrigger, { children: /* @__PURE__ */ jsx18(CutText, { text, focused }) });
}
function CutText({ text, focused }) {
  const tip = useContext5(TooltipTriggerStateContext);
  const ref = useRef5(null);
  const [pointed, setPointed] = useState7(false);
  const shown = useRef5({ pointed: false, focused: false });
  const follow2 = useEffectEvent((now) => {
    const before = shown.current;
    shown.current = now;
    const el = ref.current;
    const cut = el !== null && el.scrollWidth > el.clientWidth;
    if (tip === null) return;
    if (cut && now.focused) tip.open(true);
    else if (cut && now.pointed) tip.open();
    else if (before.pointed || before.focused) tip.close(before.focused);
  });
  useEffect(() => {
    follow2({ pointed, focused });
  }, [pointed, focused]);
  return /* @__PURE__ */ jsxs14(Fragment9, { children: [
    /* @__PURE__ */ jsx18(
      "span",
      {
        ref,
        className: "bc:truncate",
        onPointerEnter: (event) => {
          if (event.pointerType !== "touch") setPointed(true);
        },
        onPointerLeave: () => {
          setPointed(false);
        },
        children: text
      }
    ),
    /* @__PURE__ */ jsx18(Tooltip, { triggerRef: ref, children: text })
  ] });
}
function usePicks(multiple) {
  const state = useContext5(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? value : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? value : null;
  const known2 = new Map((state?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = (one === null ? keys : [one]).filter((key) => !known2.has(key));
  return { state, keys, one, known: known2, lacked };
}
function Lacking({ multiple, marking }) {
  const { lacked } = usePicks(multiple);
  return marking ? /* @__PURE__ */ jsx18(NotAvailable, { values: lacked.map(String) }) : null;
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx18(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps14(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs14(Fragment9, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx18("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx18(Tick, {}) : null }) : null,
    /* @__PURE__ */ jsx18(
      "span",
      {
        className: `bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${selectionMode === "single" && isSelected ? "bc:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs14(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx18(Header2, { className: HEADING3, children: title }),
    /* @__PURE__ */ jsx18(Collection2, { items, children })
  ] });
}
var FIELD = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var VALUES = "bc:flex bc:min-w-0 bc:flex-1 bc:flex-wrap bc:items-center bc:gap-1";
var INPUT = "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
var OPENER = "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
var CHIP = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-1.5 bc:pl-1.5 bc:data-allows-removing:pr-0.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
var CHIP_REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
var LAYOUT = { rowHeight: 32, headingHeight: 28, padding: 4 };
var LIST = "bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:outline-none";
var HEADING3 = "bc:box-border bc:h-[28px] bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:box-border bc:flex bc:h-[32px] bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
var NOTE2 = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/datefield.tsx
import {
  composeRenderProps as composeRenderProps15,
  DateField as AriaDateField,
  DateInput as AriaDateInput,
  DateSegment,
  FieldError as FieldError4,
  Label as Label5,
  Text as Text5,
  TimeField as AriaTimeField
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment10, jsx as jsx19, jsxs as jsxs15 } from "react/jsx-runtime";
function DateField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnDate, { ...props }) : /* @__PURE__ */ jsx19(BoundDate, { ...props, bound });
}
function TimeField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnTime, { ...props }) : /* @__PURE__ */ jsx19(BoundTime, { ...props, bound });
}
function BoundDate({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true
  });
  return /* @__PURE__ */ jsxs15(Fragment10, { children: [
    /* @__PURE__ */ jsx19(
      DrawnDate,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
        onKey: date.heard,
        onChange: (value) => {
          date.change(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          date.flush();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter") date.flush();
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function BoundTime({
  bound,
  onChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = useBoundDate(bound, {
    read: readTime,
    write: writeTime,
    fits: (value) => timeFits(value, props),
    required: props.isRequired === true
  });
  return /* @__PURE__ */ jsxs15(Fragment10, { children: [
    /* @__PURE__ */ jsx19(
      DrawnTime,
      {
        ...props,
        validationBehavior,
        value: time.value,
        missing: time.missing,
        onKey: time.heard,
        onChange: (value) => {
          time.change(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          time.flush();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter") time.flush();
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function DrawnDate({
  label,
  description,
  errorMessage: errorMessage2,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsx19(
    AriaDateField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps15(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx19(FieldParts, { label, description, errorMessage: errorMessage2, missing, onKey })
    }
  );
}
function DrawnTime({
  label,
  description,
  errorMessage: errorMessage2,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsx19(
    AriaTimeField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps15(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx19(FieldParts, { label, description, errorMessage: errorMessage2, missing, onKey })
    }
  );
}
function FieldParts({
  label,
  description,
  errorMessage: errorMessage2,
  missing,
  onKey
}) {
  return /* @__PURE__ */ jsxs15(Fragment10, { children: [
    label === void 0 ? null : /* @__PURE__ */ jsx19(Label5, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx19(Segments, { onKey, children: /* @__PURE__ */ jsx19(DateInput, { className: BOX2 }) }),
    /* @__PURE__ */ jsx19(Text5, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx19(FieldError4, { className: styles.alert, children: errorMessage2 }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx19(NotAvailable, { values: [missing] })
  ] });
}
function Segments({ onKey, children }) {
  return onKey === void 0 ? /* @__PURE__ */ jsx19(Fragment10, { children }) : /* @__PURE__ */ jsx19(
    "div",
    {
      className: "bc:contents",
      onKeyDownCapture: (event) => {
        onKey(event.key);
      },
      children
    }
  );
}
function DateInput({ slot, className }) {
  return /* @__PURE__ */ jsx19(AriaDateInput, { slot, className, children: (segment) => /* @__PURE__ */ jsx19(DateSegment, { segment, className: SEGMENT }) });
}
var BOX2 = "bc:flex bc:min-w-0 bc:items-center bc:justify-self-stretch bc:overflow-x-auto bc:whitespace-nowrap bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:[scrollbar-width:none] bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var INLINE = "bc:flex bc:min-w-0 bc:items-center bc:whitespace-nowrap bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:data-disabled:cursor-default";
var SEGMENT = "bc:rounded-sm bc:px-0.5 bc:tabular-nums bc:caret-transparent bc:outline-none bc:data-[type=literal]:px-0 bc:data-placeholder:text-text-muted bc:data-focused:bg-bg-accent bc:data-focused:text-text-on-accent";

// srcts/inputs/datepicker.tsx
import { useRef as useRef6 } from "react";
import {
  composeRenderProps as composeRenderProps16,
  DatePicker as AriaDatePicker,
  DateRangePicker as AriaDateRangePicker,
  FieldError as FieldError5,
  Group as Group2,
  Label as Label6,
  Text as Text6
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment11, jsx as jsx20, jsxs as jsxs16 } from "react/jsx-runtime";
function DatePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx20(DrawnPicker, { ...props }) : /* @__PURE__ */ jsx20(BoundPicker2, { ...props, bound });
}
function DateRangePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx20(DrawnRangePicker, { ...props }) : /* @__PURE__ */ jsx20(BoundRangePicker, { ...props, bound });
}
function usePicking(value) {
  const open = useRef6(false);
  return {
    onOpenChange: (isOpen) => {
      open.current = isOpen;
    },
    onChange: (next) => {
      if (open.current) value.pick(next);
      else value.change(next);
    },
    onBlur: () => {
      value.flush();
    },
    onKeyDown: (event) => {
      if (event.key === "Enter" && event.currentTarget.contains(event.target)) value.flush();
    }
  };
}
function BoundPicker2({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, time),
    write: (value) => writeDate(value, time),
    fits: (value) => dateFits(value, props),
    required: props.isRequired === true
  });
  const picking = usePicking(date);
  return /* @__PURE__ */ jsxs16(Fragment11, { children: [
    /* @__PURE__ */ jsx20(
      DrawnPicker,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
        onKey: date.heard,
        onOpenChange: (isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        },
        onChange: (value) => {
          picking.onChange(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          picking.onBlur();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx20(Refusal, { error: bound.error })
  ] });
}
function BoundRangePicker({
  bound,
  onChange,
  onOpenChange,
  onBlur,
  onKeyDown,
  validationBehavior = "aria",
  ...props
}) {
  const time = takesTime(props.granularity);
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, time),
    write: (value) => writeRange(value, time),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: props.isRequired === true
  });
  const picking = usePicking(range);
  return /* @__PURE__ */ jsxs16(Fragment11, { children: [
    /* @__PURE__ */ jsx20(
      DrawnRangePicker,
      {
        ...props,
        validationBehavior,
        value: range.value,
        missing: range.missing,
        onKey: range.heard,
        onOpenChange: (isOpen) => {
          picking.onOpenChange(isOpen);
          onOpenChange?.(isOpen);
        },
        onChange: (value) => {
          picking.onChange(value);
          onChange?.(value);
        },
        onBlur: (event) => {
          picking.onBlur();
          onBlur?.(event);
        },
        onKeyDown: (event) => {
          picking.onKeyDown(event);
          onKeyDown?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx20(Refusal, { error: bound.error })
  ] });
}
function DrawnPicker({
  label,
  description,
  errorMessage: errorMessage2,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs16(
    AriaDatePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps16(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx20(Label6, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs16(Group2, { className: FIELD2, children: [
          /* @__PURE__ */ jsx20(Segments, { onKey, children: /* @__PURE__ */ jsx20(DateInput, { className: `${INLINE} bc:flex-1` }) }),
          /* @__PURE__ */ jsx20(Opener, {})
        ] }),
        /* @__PURE__ */ jsx20(PickerParts, { description, errorMessage: errorMessage2, missing }),
        /* @__PURE__ */ jsx20(Popover, { className: POPOVER2, children: /* @__PURE__ */ jsx20(Calendar, {}) })
      ]
    }
  );
}
function DrawnRangePicker({
  label,
  description,
  errorMessage: errorMessage2,
  missing = null,
  onKey,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs16(
    AriaDateRangePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps16(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx20(Label6, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs16(Group2, { className: FIELD2, children: [
          /* @__PURE__ */ jsx20(Segments, { onKey, children: /* @__PURE__ */ jsxs16("div", { className: ENDS, children: [
            /* @__PURE__ */ jsx20(DateInput, { slot: "start", className: INLINE }),
            /* @__PURE__ */ jsx20("span", { "aria-hidden": "true", className: "bc:px-1 bc:text-text-muted", children: "\u2013" }),
            /* @__PURE__ */ jsx20(DateInput, { slot: "end", className: `${INLINE} bc:flex-1` })
          ] }) }),
          /* @__PURE__ */ jsx20(Opener, {})
        ] }),
        /* @__PURE__ */ jsx20(PickerParts, { description, errorMessage: errorMessage2, missing }),
        /* @__PURE__ */ jsx20(Popover, { className: POPOVER2, children: /* @__PURE__ */ jsx20(RangeCalendar, {}) })
      ]
    }
  );
}
function PickerParts({
  description,
  errorMessage: errorMessage2,
  missing
}) {
  return /* @__PURE__ */ jsxs16(Fragment11, { children: [
    /* @__PURE__ */ jsx20(Text6, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx20(FieldError5, { className: styles.alert, children: errorMessage2 }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx20(NotAvailable, { values: [missing] })
  ] });
}
function Opener() {
  return /* @__PURE__ */ jsx20(FieldButton, { children: /* @__PURE__ */ jsxs16(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: [
        /* @__PURE__ */ jsx20("rect", { x: "2.5", y: "3.5", width: "11", height: "10", rx: "1.5" }),
        /* @__PURE__ */ jsx20("path", { d: "M2.5 6.5h11M5.5 2v3M10.5 2v3" })
      ]
    }
  ) });
}
var FIELD2 = "bc:flex bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var ENDS = "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:overflow-x-auto bc:[scrollbar-width:none]";
var POPOVER2 = "bc:p-3";

// srcts/inputs/dropzone.tsx
import { composeRenderProps as composeRenderProps17, DropZone as AriaDropZone } from "./vendor-c5b8952d.js";
import { jsx as jsx21 } from "react/jsx-runtime";
function DropZone({ className, ...props }) {
  return /* @__PURE__ */ jsx21(AriaDropZone, { ...props, className: composeRenderProps17(className, (given) => `${ZONE} ${given ?? ""}`) });
}
var ZONE = "bc:box-border bc:flex bc:min-h-20 bc:flex-col bc:items-center bc:justify-center bc:gap-2 bc:rounded-lg bc:border bc:border-dashed bc:border-border-default bc:p-4 bc:text-center bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-drop-target:border-solid bc:data-drop-target:border-border-accent bc:data-drop-target:bg-bg-subtle bc:data-disabled:opacity-50";

// srcts/inputs/form.tsx
import { Form as AriaForm } from "./vendor-c5b8952d.js";
import { jsx as jsx22 } from "react/jsx-runtime";
function Form({ className, ...props }) {
  return /* @__PURE__ */ jsx22(AriaForm, { ...props, className: `bc:grid bc:gap-3 ${className ?? ""}` });
}

// srcts/inputs/listbox.tsx
import { composeRenderProps as composeRenderProps18, ListBox as AriaListBox } from "./vendor-c5b8952d.js";
import { jsx as jsx23 } from "react/jsx-runtime";
function ListBox2({ className, ...props }) {
  return /* @__PURE__ */ jsx23(AriaListBox, { ...props, className: composeRenderProps18(className, (given) => `${LISTBOX2} ${given ?? ""}`) });
}
function ListBoxItem2(props) {
  return /* @__PURE__ */ jsx23(ComboBoxItem, { ...props });
}
var LISTBOX2 = "bc:box-border bc:grid bc:min-w-0 bc:content-start bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-empty:px-2.5 bc:data-empty:py-2 bc:data-empty:italic bc:data-empty:text-text-muted";

// srcts/inputs/numberfield.tsx
import { useState as useState8 } from "react";
import { Button as Button5, composeRenderProps as composeRenderProps19, FieldError as FieldError6, Group as Group3, Input as Input3, Label as Label7, NumberField as AriaNumberField, Text as Text7 } from "./vendor-c5b8952d.js";
import { Fragment as Fragment12, jsx as jsx24, jsxs as jsxs17 } from "react/jsx-runtime";
function NumberField({ bound, outdated = false, maxValue, ...props }) {
  const largest = outdated ? void 0 : maxValue;
  return bound === void 0 ? /* @__PURE__ */ jsx24(Drawn5, { ...props, maxValue: largest }) : /* @__PURE__ */ jsx24(BoundNumber, { ...props, maxValue: largest, bound });
}
function BoundNumber({
  bound,
  onChange,
  onInput,
  onBlur,
  commitBehavior = "validate",
  validationBehavior = "aria",
  ...props
}) {
  const held3 = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState8(null);
  const [seen, setSeen] = useState8(held3);
  if (!Object.is(seen, held3)) {
    setSeen(held3);
    if (kept !== null && !kept.typing) setKept(null);
  }
  const fits = (value) => inRange(value, props.minValue, props.maxValue, props.step);
  return /* @__PURE__ */ jsxs17(Fragment12, { children: [
    /* @__PURE__ */ jsx24(
      Drawn5,
      {
        ...props,
        commitBehavior,
        validationBehavior,
        value: kept?.value ?? held3,
        onInput: (event) => {
          setKept((now) => now?.typing === true ? now : { value: now?.value ?? held3, typing: true });
          onInput?.(event);
        },
        onChange: (value) => {
          if (!fits(value)) {
            setKept(null);
          } else if (kept?.typing === true) {
            bound.type(value);
            bound.flush();
            setKept(Object.is(value, held3) ? null : { value, typing: false });
          } else {
            setKept({ value, typing: false });
            void bound.commit(value).then((took) => {
              if (!took) setKept(null);
            });
          }
          onChange?.(value);
        },
        onBlur: (event) => {
          setKept((now) => now?.typing === true ? null : now);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx24(Refusal, { error: bound.error })
  ] });
}
function inRange(value, min, max, step) {
  if (Number.isNaN(value)) return false;
  if (min !== void 0 && value < min) return false;
  if (max !== void 0 && value > max) return false;
  if (step === void 0 || !(step > 0)) return true;
  const steps = (value - (min ?? 0)) / step;
  return Math.abs(steps - Math.round(steps)) < 1e-9;
}
function Drawn5({ label, description, errorMessage: errorMessage2, placeholder, className, onInput, ...props }) {
  return /* @__PURE__ */ jsxs17(AriaNumberField, { ...props, className: composeRenderProps19(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx24(Label7, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs17(Group3, { className: FIELD3, children: [
      /* @__PURE__ */ jsx24(Input3, { placeholder, onInput, className: INPUT2 }),
      /* @__PURE__ */ jsxs17("div", { className: STEPPERS, children: [
        /* @__PURE__ */ jsx24(Button5, { slot: "increment", className: STEPPER, children: /* @__PURE__ */ jsx24(Chevron2, { up: true }) }),
        /* @__PURE__ */ jsx24(Button5, { slot: "decrement", className: STEPPER, children: /* @__PURE__ */ jsx24(Chevron2, { up: false }) })
      ] })
    ] }),
    /* @__PURE__ */ jsx24(Text7, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx24(FieldError6, { className: styles.alert, children: errorMessage2 })
  ] });
}
function Chevron2({ up }) {
  return /* @__PURE__ */ jsx24(
    "svg",
    {
      width: "10",
      height: "10",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: /* @__PURE__ */ jsx24("polyline", { points: up ? "3 7.5 6 4.5 9 7.5" : "3 4.5 6 7.5 9 4.5" })
    }
  );
}
var FIELD3 = "bc:flex bc:min-w-0 bc:items-stretch bc:justify-self-stretch bc:overflow-hidden bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT2 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted";
var STEPPERS = "bc:flex bc:flex-col bc:border-l bc:border-border-default bc:group-data-invalid:border-border-danger";
var STEPPER = "bc:flex bc:flex-1 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:px-1 bc:py-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/radio.tsx
import { useRef as useRef7 } from "react";
import { composeRenderProps as composeRenderProps20, FieldError as FieldError7, Label as Label8, RadioButton, RadioField, RadioGroup as AriaRadioGroup, Text as Text8 } from "./vendor-c5b8952d.js";
import { Fragment as Fragment13, jsx as jsx25, jsxs as jsxs18 } from "react/jsx-runtime";
function RadioGroup({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx25(DrawnGroup2, { ...props }) : /* @__PURE__ */ jsx25(BoundGroup2, { ...props, bound, outdated });
}
function BoundGroup2({
  bound,
  outdated,
  onChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const held3 = typeof bound.value === "string" ? bound.value : null;
  const gone = lacking(held3 === null ? [] : [held3], known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs18(Fragment13, { children: [
    /* @__PURE__ */ jsx25(
      DrawnGroup2,
      {
        ...props,
        value: held3,
        isInvalid: props.isInvalid === true || marking,
        onChange: (value) => {
          void bound.commit(value);
          onChange?.(value);
        },
        children: /* @__PURE__ */ jsx25(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx25(NotAvailable, { values: gone }) : null,
    /* @__PURE__ */ jsx25(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup2({ label, description, errorMessage: errorMessage2, children, className, ...props }) {
  return /* @__PURE__ */ jsxs18(AriaRadioGroup, { ...props, className: composeRenderProps20(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx25(Label8, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx25("div", { className: "bc:flex bc:gap-1.5 bc:group-data-[orientation=horizontal]:flex-row bc:group-data-[orientation=horizontal]:flex-wrap bc:group-data-[orientation=horizontal]:gap-4 bc:group-data-[orientation=vertical]:flex-col", children }),
    /* @__PURE__ */ jsx25(Text8, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx25(FieldError7, { className: styles.alert, children: errorMessage2 })
  ] });
}
function Radio({ children, description, className, ...props }) {
  const ref = useRef7(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs18(RadioField, { ...props, ref, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsxs18(RadioButton, { className: composeRenderProps20(className, (given) => `bc:group ${RADIO} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx25("span", { className: CIRCLE }),
      children
    ] }),
    /* @__PURE__ */ jsx25(Text8, { slot: "description", className: `${styles.hint} bc:ml-6 bc:empty:hidden`, children: description })
  ] });
}
var RADIO = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var CIRCLE = "bc:box-border bc:size-4 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:transition-[border-width] bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-[5px] bc:group-data-selected:border-bg-accent bc:group-data-invalid:border-border-danger";

// srcts/inputs/searchfield.tsx
import { composeRenderProps as composeRenderProps21, FieldError as FieldError8, Group as Group4, Input as Input4, Label as Label9, SearchField as AriaSearchField, Text as Text9 } from "./vendor-c5b8952d.js";
import { Fragment as Fragment14, jsx as jsx26, jsxs as jsxs19 } from "react/jsx-runtime";
function SearchField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx26(Drawn6, { ...props }) : /* @__PURE__ */ jsx26(BoundSearch, { ...props, bound });
}
function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }) {
  const [text, typed] = useTyped(bound);
  return /* @__PURE__ */ jsxs19(Fragment14, { children: [
    /* @__PURE__ */ jsx26(
      Drawn6,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        },
        onClear: () => {
          bound.flush();
          onClear?.();
        }
      }
    ),
    /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
  ] });
}
function Drawn6({ label, description, errorMessage: errorMessage2, placeholder, className, ...props }) {
  return /* @__PURE__ */ jsxs19(AriaSearchField, { ...props, className: composeRenderProps21(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx26(Label9, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs19(Group4, { className: FIELD4, children: [
      /* @__PURE__ */ jsx26(Magnifier, {}),
      /* @__PURE__ */ jsx26(Input4, { placeholder, className: INPUT3 }),
      /* @__PURE__ */ jsx26(FieldButton, { className: "bc:group-data-empty:invisible", children: /* @__PURE__ */ jsx26(Cross, {}) })
    ] }),
    /* @__PURE__ */ jsx26(Text9, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx26(FieldError8, { className: styles.alert, children: errorMessage2 })
  ] });
}
var FIELD4 = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1.5 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-1 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT3 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:[&::-webkit-search-cancel-button]:hidden";

// srcts/inputs/select.tsx
import { useContext as useContext6, useLayoutEffect as useLayoutEffect3, useMemo as useMemo3, useRef as useRef8, useState as useState9 } from "react";
import { strings as strings3 } from "@blockr/core/binding";
import {
  Button as Button6,
  Collection as Collection3,
  composeRenderProps as composeRenderProps22,
  FieldError as FieldError9,
  Label as Label10,
  ListBox as ListBox3,
  Popover as Popover3,
  Select as AriaSelect,
  SelectStateContext,
  SelectValue,
  Text as Text10,
  useLocale as useLocale2
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment15, jsx as jsx27, jsxs as jsxs20 } from "react/jsx-runtime";
function Select(props) {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx27(Drawn7, { ...rest });
  return /* @__PURE__ */ jsx27(BoundSelect, { ...rest, bound, outdated, ordered });
}
function SelectItem(props) {
  return /* @__PURE__ */ jsx27(ComboBoxItem, { ...props });
}
function SelectSection(props) {
  return /* @__PURE__ */ jsx27(ComboBoxSection, { ...props });
}
function BoundSelect({
  bound,
  outdated,
  ordered,
  onChange,
  onOpenChange,
  ...props
}) {
  const several = props.selectionMode === "multiple";
  const values = several ? strings3(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const [known2, setKnown] = useState9(null);
  const unknown = props.items === void 0 && typeof props.children === "function";
  const ids2 = unknown ? null : known2;
  const [picks, setPicks] = useState9(null);
  const pending = useRef8(null);
  const shown = picks ?? values;
  const asIs = ids2 === null ? values : values.filter((value) => !ids2.includes(value));
  const marking = !outdated && ids2 !== null && asIs.length > 0;
  const submit = (next, opened) => {
    const value = merged(strings3(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && ids2 !== null ? inOrder(value, ids2) : value);
  };
  const common = {
    ...props,
    isInvalid: props.isInvalid === true || marking,
    asIs,
    onKnown: (next) => {
      setKnown((last) => last !== null && sameIds(last, next) ? last : next);
    }
  };
  const line = marking ? /* @__PURE__ */ jsx27(
    NotAvailable,
    {
      values: asIs,
      onRemove: several ? () => {
        void bound.commit(held2(strings3(bound.get()), ids2));
      } : void 0
    }
  ) : null;
  if (!several) {
    return /* @__PURE__ */ jsxs20(Fragment15, { children: [
      /* @__PURE__ */ jsx27(
        Drawn7,
        {
          ...common,
          selectionMode: "single",
          value: values[0] ?? null,
          onChange: (key) => {
            if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
            onChange?.(key);
          },
          onOpenChange
        }
      ),
      line,
      /* @__PURE__ */ jsx27(Refusal, { error: bound.error })
    ] });
  }
  return /* @__PURE__ */ jsxs20(Fragment15, { children: [
    /* @__PURE__ */ jsx27(
      Drawn7,
      {
        ...common,
        selectionMode: "multiple",
        value: [...shown],
        onOpenChange: (isOpen) => {
          if (isOpen) {
            pending.current ??= { picks: values, opened: values };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen);
        },
        onChange: (keys) => {
          if (Array.isArray(keys)) {
            const next = keys.map(String);
            if (pending.current === null) {
              submit(next, values);
            } else {
              pending.current = { ...pending.current, picks: next };
              setPicks(next);
            }
          }
          onChange?.(keys);
        }
      }
    ),
    line,
    /* @__PURE__ */ jsx27(Refusal, { error: bound.error })
  ] });
}
function sameIds(a, b) {
  return a.length === b.length && a.every((id, i) => id === b[i]);
}
function Drawn7({ label, description, errorMessage: errorMessage2, items, children, asIs = [], onKnown, className, ...props }) {
  const value = useSameValue(props.value);
  const { locale } = useLocale2();
  const list = useMemo3(() => new Intl.ListFormat(locale), [locale]);
  return /* @__PURE__ */ jsxs20(
    AriaSelect,
    {
      ...props,
      value,
      className: composeRenderProps22(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx27(Label10, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs20(Button6, { className: TRIGGER, children: [
          /* @__PURE__ */ jsx27(SelectValue, { className: ({ isPlaceholder }) => isPlaceholder && asIs.length === 0 ? `${VALUE} ${PLACEHOLDER}` : VALUE, children: ({ state, selectedText, defaultChildren, isPlaceholder }) => asIs.length > 0 ? list.format([...state.selectedItems.map((item) => item.textValue), ...asIs]) : isPlaceholder ? defaultChildren : selectedText || defaultChildren }),
          /* @__PURE__ */ jsx27(Chevron, {})
        ] }),
        /* @__PURE__ */ jsx27(Text10, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx27(FieldError9, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx27(Popover3, { offset: 4, className: LIST2, children: /* @__PURE__ */ jsx27(ListBox3, { renderEmptyState: () => /* @__PURE__ */ jsx27("p", { className: NOTE3, children: "No options" }), className: LISTBOX3, children: items === void 0 ? typeof children === "function" ? null : children : /* @__PURE__ */ jsx27(Collection3, { items, children }) }) }),
        onKnown === void 0 ? null : /* @__PURE__ */ jsx27(Known, { onKnown })
      ]
    }
  );
}
function Known({ onKnown }) {
  const state = useContext6(SelectStateContext);
  useLayoutEffect3(() => {
    if (state === null) return;
    const { collection } = state;
    const ids2 = [...collection.getKeys()].flatMap(
      (key) => collection.getItem(key)?.type === "item" ? [String(key)] : []
    );
    onKnown(ids2);
  });
  return null;
}
var TRIGGER = "bc:flex bc:min-h-[30px] bc:w-full bc:min-w-0 bc:cursor-pointer bc:items-center bc:gap-2 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-1 bc:pr-1.5 bc:pl-2 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var VALUE = "bc:min-w-0 bc:flex-1 bc:truncate bc:group-data-invalid:text-text-danger";
var PLACEHOLDER = "bc:text-text-muted";
var LIST2 = "bc:box-border bc:min-w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX3 = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:p-1 bc:outline-none";
var NOTE3 = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/slider.tsx
import { useContext as useContext7, useLayoutEffect as useLayoutEffect4, useRef as useRef9, useState as useState10 } from "react";
import {
  composeRenderProps as composeRenderProps23,
  Label as Label11,
  Slider as AriaSlider,
  SliderFill,
  SliderOutput as SliderOutput2,
  SliderStateContext,
  SliderThumb,
  SliderTrack as SliderTrack2,
  useLocale as useLocale3
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment16, jsx as jsx28, jsxs as jsxs21 } from "react/jsx-runtime";
function Slider2({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx28(Drawn8, { ...props }) : /* @__PURE__ */ jsx28(BoundSlider, { ...props, bound, outdated });
}
function BoundSlider({
  bound,
  outdated,
  onChange,
  onChangeEnd,
  ...props
}) {
  const several = Array.isArray(bound.value) || Array.isArray(props.defaultValue);
  const held3 = numbers(bound.value);
  const [moving, setMoving] = useState10(null);
  const [seen, setSeen] = useState10(bound.value);
  if (!jsonEqual(seen, bound.value)) {
    setSeen(bound.value);
    setMoving(null);
  }
  const shown = useRef9([]);
  const start2 = useRef9(null);
  const min = props.minValue ?? 0;
  const fits = (value2) => inRange(value2, min, props.maxValue ?? 100, props.step ?? 1);
  const lacked = moving === null ? held3.filter((value2) => !fits(value2)) : [];
  const marking = !outdated && lacked.length > 0;
  const drawn = moving ?? (held3.length > 0 ? held3 : [min]);
  const value = several ? [...drawn] : drawn[0] ?? min;
  const { locale } = useLocale3();
  return /* @__PURE__ */ jsxs21(Fragment16, { children: [
    /* @__PURE__ */ jsx28(
      Drawn8,
      {
        ...props,
        value,
        invalid: marking,
        said: moving === null && held3.length === 0 ? () => "" : lacked.length > 0 ? (formatted) => held3.map(formatted).join(held3.length === 2 ? "\u2013" : ", ") : void 0,
        onChange: (next) => {
          start2.current ??= shown.current;
          setMoving(numbers(next));
          onChange?.(next);
        },
        onChangeEnd: (next) => {
          const ended = numbers(next);
          const began = start2.current ?? shown.current;
          start2.current = null;
          if (jsonEqual(ended, began)) {
            setMoving(null);
          } else {
            const given = ended.map((x, i) => Object.is(x, began[i]) ? held3[i] ?? x : x);
            setMoving(ended);
            void bound.commit(several ? given : given[0] ?? min).then((took) => {
              if (!took) setMoving(null);
            });
          }
          onChangeEnd?.(next);
        },
        watch: /* @__PURE__ */ jsx28(
          Watch,
          {
            onShown: (values) => {
              shown.current = values;
            }
          }
        )
      }
    ),
    marking ? /* @__PURE__ */ jsx28(NotAvailable, { values: lacked.map((x) => new Intl.NumberFormat(locale, props.formatOptions).format(x)) }) : null,
    /* @__PURE__ */ jsx28(Refusal, { error: bound.error })
  ] });
}
function Watch({ onShown }) {
  const state = useContext7(SliderStateContext);
  useLayoutEffect4(() => {
    if (state !== null && !state.values.some((_, i) => state.isThumbDragging(i))) onShown(state.values);
  });
  return null;
}
function numbers(value) {
  if (typeof value === "number") return [value];
  return Array.isArray(value) ? value.filter((x) => typeof x === "number") : [];
}
function Drawn8({ label, thumbLabels, fillOffset, said, invalid = false, watch, className, ...props }) {
  return /* @__PURE__ */ jsxs21(
    AriaSlider,
    {
      ...props,
      "data-invalid": invalid || void 0,
      className: composeRenderProps23(className, (given) => `bc:group ${SLIDER2} ${given ?? ""}`),
      children: [
        label === void 0 ? /* @__PURE__ */ jsx28("span", {}) : /* @__PURE__ */ jsx28(Label11, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx28(SliderOutput2, { className: OUTPUT2, children: ({ state }) => said === void 0 ? state.getFormattedValue() : said((value) => state.getFormattedValue(value)) }),
        /* @__PURE__ */ jsx28(SliderTrack2, { className: TRACK2, children: ({ state }) => /* @__PURE__ */ jsxs21(Fragment16, { children: [
          /* @__PURE__ */ jsx28("div", { className: BAR, children: /* @__PURE__ */ jsx28(SliderFill, { offset: fillOffset, className: FILL }) }),
          state.values.map((_, i) => /* @__PURE__ */ jsx28(SliderThumb, { index: i, "aria-label": thumbLabels?.[i], className: THUMB2 }, i))
        ] }) }),
        watch
      ]
    }
  );
}
var SLIDER2 = "bc:grid bc:grid-cols-[1fr_auto] bc:items-center bc:gap-x-2 bc:gap-y-1 bc:font-sans bc:text-text-default bc:data-disabled:opacity-50";
var OUTPUT2 = "bc:text-sm bc:text-text-muted bc:tabular-nums bc:group-data-invalid:text-text-danger";
var TRACK2 = "bc:relative bc:col-span-2 bc:flex bc:h-5 bc:items-center bc:mx-2";
var BAR = "bc:relative bc:h-1.5 bc:w-full bc:rounded-full bc:bg-border-default";
var FILL = "bc:rounded-full bc:bg-bg-accent bc:group-data-invalid:bg-border-danger";
var THUMB2 = "bc:top-1/2 bc:box-border bc:size-4 bc:cursor-grab bc:rounded-full bc:border-2 bc:border-bg-accent bc:bg-bg-raised bc:shadow-xs bc:outline-offset-2 bc:outline-border-accent bc:data-dragging:cursor-grabbing bc:data-focus-visible:outline-2 bc:data-disabled:cursor-default bc:group-data-invalid:border-border-danger";

// srcts/inputs/switch.tsx
import { composeRenderProps as composeRenderProps24, FieldError as FieldError10, SwitchButton, SwitchField, Text as Text11 } from "./vendor-c5b8952d.js";
import { Fragment as Fragment17, jsx as jsx29, jsxs as jsxs22 } from "react/jsx-runtime";
function Switch({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx29(Drawn9, { ...props, onChange });
  return /* @__PURE__ */ jsxs22(Fragment17, { children: [
    /* @__PURE__ */ jsx29(
      Drawn9,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx29(Refusal, { error: bound.error })
  ] });
}
function Drawn9({ children, description, errorMessage: errorMessage2, className, ...props }) {
  return /* @__PURE__ */ jsxs22(SwitchField, { ...props, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsxs22(SwitchButton, { className: composeRenderProps24(className, (given) => `bc:group ${SWITCH} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx29("span", { className: TRACK3, children: /* @__PURE__ */ jsx29("span", { className: HANDLE }) }),
      children
    ] }),
    /* @__PURE__ */ jsx29(Text11, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx29(FieldError10, { className: styles.alert, children: errorMessage2 })
  ] });
}
var SWITCH = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var TRACK3 = "bc:box-border bc:flex bc:h-5 bc:w-9 bc:shrink-0 bc:items-center bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:px-px bc:outline-offset-2 bc:outline-border-accent bc:transition-colors bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent";
var HANDLE = "bc:size-4 bc:rounded-full bc:bg-text-muted bc:shadow-xs bc:transition-transform bc:group-data-selected:translate-x-full bc:group-data-selected:bg-text-on-accent";

// srcts/inputs/tags.tsx
import { createContext as createContext3, useContext as useContext8 } from "react";
import { Button as Button7, composeRenderProps as composeRenderProps25, Label as Label12, Tag as AriaTag, TagGroup as AriaTagGroup, TagList as TagList2, Text as Text12 } from "./vendor-c5b8952d.js";
import { Fragment as Fragment18, jsx as jsx30, jsxs as jsxs23 } from "react/jsx-runtime";
var colors = {
  gray: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default",
  accent: "bc:border-border-accent bc:bg-bg-subtle bc:text-text-default"
};
var ColorContext = createContext3("gray");
function TagGroup2({
  label,
  description,
  errorMessage: errorMessage2,
  items,
  children,
  renderEmptyState,
  color = "gray",
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs23(AriaTagGroup, { ...props, className: `${styles.field} ${className ?? ""}`, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx30(Label12, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx30(ColorContext.Provider, { value: color, children: /* @__PURE__ */ jsx30(TagList2, { items, renderEmptyState, className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children }) }),
    description === void 0 ? null : /* @__PURE__ */ jsx30(Text12, { slot: "description", className: styles.hint, children: description }),
    errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx30(Text12, { slot: "errorMessage", className: styles.alert, children: errorMessage2 })
  ] });
}
function Tag2({ children, color, className, ...props }) {
  const textValue = typeof children === "string" ? children : void 0;
  const groupColor = useContext8(ColorContext);
  return /* @__PURE__ */ jsx30(
    AriaTag,
    {
      textValue,
      ...props,
      className: composeRenderProps25(className, (given) => `${TAG} ${colors[color ?? groupColor]} ${given ?? ""}`),
      children: composeRenderProps25(children, (children2, { allowsRemoving }) => /* @__PURE__ */ jsxs23(Fragment18, { children: [
        /* @__PURE__ */ jsx30("span", { className: "bc:truncate", children: children2 }),
        allowsRemoving ? /* @__PURE__ */ jsx30(Button7, { slot: "remove", className: REMOVE, children: /* @__PURE__ */ jsx30(Cross, {}) }) : null
      ] }))
    }
  );
}
var TAG = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:cursor-default bc:items-center bc:gap-0.5 bc:rounded bc:border bc:px-1.5 bc:py-0 bc:font-sans bc:text-xs bc:leading-5 bc:outline-none bc:data-allows-removing:pr-0.5 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-selected:border-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:opacity-50";
var REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:data-hovered:bg-bg-subtle";

// srcts/inputs/textfield.tsx
import { composeRenderProps as composeRenderProps26, FieldError as FieldError11, Input as Input5, Label as Label13, Text as Text13, TextArea, TextField as AriaTextField } from "./vendor-c5b8952d.js";
import { Fragment as Fragment19, jsx as jsx31, jsxs as jsxs24 } from "react/jsx-runtime";
function TextField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx31(Drawn10, { ...props }) : /* @__PURE__ */ jsx31(BoundText, { ...props, bound });
}
function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }) {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
  return /* @__PURE__ */ jsxs24(Fragment19, { children: [
    /* @__PURE__ */ jsx31(
      Drawn10,
      {
        ...props,
        value: text ?? (typeof bound.value === "string" ? bound.value : ""),
        onChange: (value) => {
          typed(value);
          bound.type(value);
          onChange?.(value);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing && (lines <= 1 || event.ctrlKey || event.metaKey)) {
            bound.flush();
          }
          event.continuePropagation();
          onKeyDown?.(event);
        },
        onBlur: (event) => {
          bound.flush();
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx31(Refusal, { error: bound.error })
  ] });
}
function Drawn10({ label, description, errorMessage: errorMessage2, placeholder, lines = 1, className, ...props }) {
  return /* @__PURE__ */ jsxs24(AriaTextField, { ...props, className: composeRenderProps26(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx31(Label13, { className: styles.label, children: label }),
    lines > 1 ? /* @__PURE__ */ jsx31(TextArea, { rows: lines, placeholder, className: styles.box }) : /* @__PURE__ */ jsx31(Input5, { placeholder, className: styles.box }),
    /* @__PURE__ */ jsx31(Text13, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx31(FieldError11, { className: styles.alert, children: errorMessage2 })
  ] });
}

// srcts/inputs/toggle.tsx
import { createContext as createContext4, useContext as useContext9, useId, useRef as useRef10 } from "react";
import { strings as strings4 } from "@blockr/core/binding";
import { composeRenderProps as composeRenderProps27, ToggleButton as AriaToggleButton, ToggleButtonGroup as AriaToggleButtonGroup } from "./vendor-c5b8952d.js";
import { Fragment as Fragment20, jsx as jsx32, jsxs as jsxs25 } from "react/jsx-runtime";
function ToggleButton({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx32(DrawnButton, { ...props, onChange });
  return /* @__PURE__ */ jsxs25(Fragment20, { children: [
    /* @__PURE__ */ jsx32(
      DrawnButton,
      {
        ...props,
        isSelected: bound.value === true,
        onChange: (selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }
      }
    ),
    /* @__PURE__ */ jsx32(Refusal, { error: bound.error })
  ] });
}
var InGroup = createContext4(false);
function DrawnButton({ className, ...props }) {
  const ref = useRef10(null);
  const inGroup = useContext9(InGroup);
  useItem(props.id === void 0 ? void 0 : String(props.id), ref);
  return /* @__PURE__ */ jsx32(
    AriaToggleButton,
    {
      ...props,
      ref,
      className: composeRenderProps27(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)
    }
  );
}
function ToggleButtonGroup({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx32(DrawnGroup3, { ...props }) : /* @__PURE__ */ jsx32(BoundGroup3, { ...props, bound, outdated, ordered });
}
function BoundGroup3({
  bound,
  outdated,
  ordered,
  onSelectionChange,
  children,
  ...props
}) {
  const { known: known2, items } = useItems();
  const several = props.selectionMode === "multiple";
  const shown = several ? strings4(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(shown, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs25(Fragment20, { children: [
    /* @__PURE__ */ jsx32(
      DrawnGroup3,
      {
        ...props,
        selectedKeys: shown,
        invalid: marking,
        onSelectionChange: (keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value = merged(strings4(bound.get()), shown, picks);
            if (value !== null) void bound.commit(ordered && known2 !== null ? inOrder(value, known2) : value);
          } else {
            const [pick] = picks;
            void (pick === void 0 ? bound.unset() : bound.commit(pick));
          }
          onSelectionChange?.(keys);
        },
        children: /* @__PURE__ */ jsx32(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx32(
      NotAvailable,
      {
        values: gone,
        onRemove: several ? () => {
          void bound.commit(held2(strings4(bound.get()), known2));
        } : void 0
      }
    ) : null,
    /* @__PURE__ */ jsx32(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup3({ label, description, invalid = false, children, className, ...props }) {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== void 0 && description !== "";
  const group = /* @__PURE__ */ jsx32(
    AriaToggleButtonGroup,
    {
      ...props,
      "aria-labelledby": props["aria-labelledby"] ?? (label === void 0 ? void 0 : labelId),
      "aria-describedby": described ? descriptionId : void 0,
      "data-invalid": invalid || void 0,
      className: composeRenderProps27(className, (given) => `${GROUP} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx32(InGroup.Provider, { value: true, children })
    }
  );
  if (label === void 0 && !described) return group;
  return /* @__PURE__ */ jsxs25("div", { className: styles.field, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx32("span", { id: labelId, className: styles.label, children: label }),
    group,
    described ? /* @__PURE__ */ jsx32("p", { id: descriptionId, className: styles.hint, children: description }) : null
  ] });
}
var GROUP = "bc:flex bc:w-fit bc:max-w-full bc:flex-wrap bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-0.5 bc:font-sans bc:data-[orientation=vertical]:flex-col bc:data-invalid:border-border-danger";
var TOGGLE = "bc:inline-flex bc:min-h-6 bc:cursor-pointer bc:items-center bc:justify-center bc:gap-1 bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var ALONE = "bc:rounded-md bc:border bc:border-solid bc:border-border-default bc:data-selected:border-border-accent";

// srcts/inputs/tokenfield.tsx
import { composeRenderProps as composeRenderProps28, Label as Label14, Text as Text14, Token as AriaToken, TokenField as AriaTokenField, TokenInput } from "./vendor-c5b8952d.js";
import { jsx as jsx33, jsxs as jsxs26 } from "react/jsx-runtime";
function TokenField({
  label,
  description,
  className,
  inputRef,
  children,
  ...props
}) {
  return /* @__PURE__ */ jsxs26(AriaTokenField, { ...props, className: composeRenderProps28(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx33(Label14, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx33(TokenInput, { ref: inputRef, className: INPUT4, children }),
    description === void 0 ? null : /* @__PURE__ */ jsx33(Text14, { slot: "description", className: styles.hint, children: description })
  ] });
}
function Token({ className, ...props }) {
  return /* @__PURE__ */ jsx33(AriaToken, { ...props, className: composeRenderProps28(className, (given) => `${TOKEN} ${given ?? ""}`) });
}
var INPUT4 = "bc:box-border bc:min-h-[30px] bc:w-full bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focused:outline-2 bc:data-focused:outline-offset-1 bc:data-focused:outline-border-accent bc:data-disabled:opacity-50";
var TOKEN = "bc:mx-0.5 bc:inline-flex bc:h-5 bc:cursor-default bc:items-center bc:whitespace-nowrap bc:rounded bc:border bc:border-border-accent bc:bg-bg-subtle bc:px-1.5 bc:text-xs bc:text-text-default bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent";

// srcts/inputs/index.ts
import { Autocomplete as Autocomplete2, FileTrigger, Text as Text15, useFilter as useFilter2 } from "./vendor-c5b8952d.js";

// srcts/layout/breadcrumbs.tsx
import { Breadcrumb as AriaBreadcrumb, Breadcrumbs as AriaBreadcrumbs, composeRenderProps as composeRenderProps30 } from "./vendor-c5b8952d.js";

// srcts/layout/link.tsx
import { composeRenderProps as composeRenderProps29, Link as AriaLink } from "./vendor-c5b8952d.js";
import { jsx as jsx34 } from "react/jsx-runtime";
function Link({ variant = "primary", className, ...props }) {
  return /* @__PURE__ */ jsx34(AriaLink, { ...props, className: composeRenderProps29(className, (given) => `${LINK} ${VARIANTS[variant]} ${given ?? ""}`) });
}
var LINK = "bc:cursor-pointer bc:rounded-xs bc:underline bc:outline-none bc:transition bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:no-underline bc:forced-colors:data-disabled:text-[GrayText]";
var VARIANTS = {
  primary: "bc:text-blue-700 bc:decoration-blue-700/60 bc:data-hovered:decoration-blue-700 bc:dark:text-blue-400 bc:dark:decoration-blue-400/60 bc:dark:data-hovered:decoration-blue-400",
  secondary: "bc:text-text-default bc:decoration-current/50 bc:data-hovered:decoration-current"
};

// srcts/layout/breadcrumbs.tsx
import { Fragment as Fragment21, jsx as jsx35, jsxs as jsxs27 } from "react/jsx-runtime";
function Breadcrumbs({ className, ...props }) {
  return /* @__PURE__ */ jsx35(AriaBreadcrumbs, { ...props, className: `${BREADCRUMBS} ${className ?? ""}` });
}
function Breadcrumb({ className, id, ...props }) {
  return /* @__PURE__ */ jsx35(AriaBreadcrumb, { ...props, id, className: composeRenderProps30(className, (given) => `${BREADCRUMB} ${given ?? ""}`), children: ({ isCurrent }) => /* @__PURE__ */ jsxs27(Fragment21, { children: [
    /* @__PURE__ */ jsx35(Link, { variant: "secondary", ...props }),
    isCurrent ? null : /* @__PURE__ */ jsx35(ChevronRight, {})
  ] }) });
}
var BREADCRUMBS = "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:items-center bc:gap-1 bc:p-0 bc:font-sans bc:text-sm";
var BREADCRUMB = "bc:flex bc:items-center bc:gap-1";

// srcts/layout/disclosure.tsx
import { useContext as useContext10 } from "react";
import {
  Button as Button8,
  composeRenderProps as composeRenderProps31,
  Disclosure as AriaDisclosure,
  DisclosurePanel as AriaDisclosurePanel,
  DisclosureStateContext,
  Heading as Heading3
} from "./vendor-c5b8952d.js";
import { jsx as jsx36, jsxs as jsxs28 } from "react/jsx-runtime";
function Disclosure({ className, ...props }) {
  return /* @__PURE__ */ jsx36(AriaDisclosure, { ...props, className: composeRenderProps31(className, (given) => `${DISCLOSURE} ${given ?? ""}`) });
}
function DisclosureHeader({ children, level = 3, className }) {
  const expanded = useContext10(DisclosureStateContext)?.isExpanded ?? false;
  return /* @__PURE__ */ jsx36(Heading3, { level, className: `${HEADING4} ${className ?? FACE}`, children: /* @__PURE__ */ jsxs28(Button8, { slot: "trigger", className: TRIGGER2, children: [
    /* @__PURE__ */ jsx36(Marker, { expanded }),
    children
  ] }) });
}
function DisclosurePanel({ className, ...props }) {
  return /* @__PURE__ */ jsx36(AriaDisclosurePanel, { ...props, className: composeRenderProps31(className, (given) => `${PANEL} ${given ?? ""}`) });
}
function Marker({ expanded }) {
  return /* @__PURE__ */ jsx36(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      fill: "currentColor",
      className: `bc:size-3 bc:shrink-0 bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`,
      children: /* @__PURE__ */ jsx36("path", { d: "M5.5 3.5 11 8l-5.5 4.5Z" })
    }
  );
}
var DISCLOSURE = "bc:font-sans bc:text-text-default";
var HEADING4 = "bc:m-0 bc:font-sans";
var FACE = "bc:text-base bc:font-semibold";
var TRIGGER2 = "bc:flex bc:w-full bc:cursor-pointer bc:items-center bc:gap-1.5 bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-left bc:text-inherit bc:[font:inherit] bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var PANEL = "bc:h-(--disclosure-panel-height) bc:overflow-clip bc:motion-safe:transition-[height]";

// srcts/layout/meter.tsx
import { composeRenderProps as composeRenderProps32, Label as Label15, Meter as AriaMeter, ProgressBar as AriaProgressBar } from "./vendor-c5b8952d.js";
import { Fragment as Fragment22, jsx as jsx37, jsxs as jsxs29 } from "react/jsx-runtime";
function Meter({ label, className, ...props }) {
  return /* @__PURE__ */ jsx37(AriaMeter, { ...props, className: composeRenderProps32(className, (given) => `${METER} ${given ?? ""}`), children: ({ percentage, valueText }) => /* @__PURE__ */ jsxs29(Fragment22, { children: [
    /* @__PURE__ */ jsxs29("div", { className: "bc:flex bc:justify-between bc:gap-2", children: [
      /* @__PURE__ */ jsx37(Label15, { className: styles.label, children: label }),
      /* @__PURE__ */ jsxs29("span", { className: `bc:text-sm ${percentage >= 80 ? "bc:text-text-danger" : "bc:text-text-muted"}`, children: [
        percentage >= 80 ? /* @__PURE__ */ jsx37(Alert, {}) : null,
        ` ${valueText ?? ""}`
      ] })
    ] }),
    /* @__PURE__ */ jsx37("div", { className: `${TRACK4} bc:w-64`, children: /* @__PURE__ */ jsx37("div", { className: `${FILL2} bc:left-0 ${fill(percentage)}`, style: { width: `${String(percentage)}%` } }) })
  ] }) });
}
function ProgressBar({ label, className, ...props }) {
  return /* @__PURE__ */ jsx37(AriaProgressBar, { ...props, className: composeRenderProps32(className, (given) => `${PROGRESS} ${given ?? ""}`), children: ({ percentage, valueText, isIndeterminate }) => /* @__PURE__ */ jsxs29(Fragment22, { children: [
    /* @__PURE__ */ jsxs29("div", { className: "bc:flex bc:justify-between bc:gap-2", children: [
      /* @__PURE__ */ jsx37(Label15, { className: styles.label, children: label }),
      /* @__PURE__ */ jsx37("span", { className: "bc:text-sm bc:text-text-muted", children: valueText })
    ] }),
    /* @__PURE__ */ jsx37("div", { className: `${TRACK4} bc:overflow-hidden`, children: /* @__PURE__ */ jsx37(
      "div",
      {
        className: `${FILL2} bc:left-0 bc:bg-bg-accent ${isIndeterminate ? "bc:motion-safe:animate-[bc-progress-slide_1s_ease-out_infinite]" : ""}`,
        style: { width: `${String(isIndeterminate ? 40 : percentage ?? 0)}%` }
      }
    ) })
  ] }) });
}
function fill(percentage) {
  if (percentage < 70) return "bc:bg-green-600";
  if (percentage < 80) return "bc:bg-orange-500";
  return "bc:bg-red-600";
}
function Alert() {
  return /* @__PURE__ */ jsxs29(
    "svg",
    {
      role: "img",
      "aria-label": "Alert",
      viewBox: "0 0 16 16",
      className: "bc:inline-block bc:size-4 bc:align-text-bottom",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: 1.4,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      children: [
        /* @__PURE__ */ jsx37("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx37("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ]
    }
  );
}
var METER = "bc:flex bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
var PROGRESS = "bc:flex bc:w-64 bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
var TRACK4 = "bc:relative bc:h-2 bc:max-w-full bc:rounded-full bc:bg-border-default bc:outline bc:outline-1 bc:-outline-offset-1 bc:outline-transparent";
var FILL2 = "bc:absolute bc:top-0 bc:h-full bc:rounded-full bc:forced-colors:bg-[Highlight]";

// srcts/layout/tabs.tsx
import {
  composeRenderProps as composeRenderProps33,
  SelectionIndicator,
  Tab as AriaTab,
  TabList as AriaTabList,
  TabPanel as AriaTabPanel,
  Tabs as AriaTabs
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment23, jsx as jsx38, jsxs as jsxs30 } from "react/jsx-runtime";
function Tabs({ className, ...props }) {
  return /* @__PURE__ */ jsx38(
    AriaTabs,
    {
      ...props,
      className: composeRenderProps33(className, (given, { orientation }) => `${TABS} ${ORIENTED[orientation]} ${given ?? ""}`)
    }
  );
}
function TabList({ className, ...props }) {
  return /* @__PURE__ */ jsx38(
    AriaTabList,
    {
      ...props,
      className: composeRenderProps33(className, (given, { orientation }) => `${LIST3} ${LISTED[orientation]} ${given ?? ""}`)
    }
  );
}
function Tab({ className, children, ...props }) {
  return /* @__PURE__ */ jsx38(AriaTab, { ...props, className: composeRenderProps33(className, (given) => `${TAB} ${given ?? ""}`), children: composeRenderProps33(children, (children2) => /* @__PURE__ */ jsxs30(Fragment23, { children: [
    children2,
    /* @__PURE__ */ jsx38(SelectionIndicator, { className: PILL })
  ] })) });
}
function TabPanel({ className, ...props }) {
  return /* @__PURE__ */ jsx38(AriaTabPanel, { ...props, className: composeRenderProps33(className, (given) => `${PANEL2} ${given ?? ""}`) });
}
var TABS = "bc:flex bc:max-w-full bc:gap-4 bc:font-sans bc:text-text-default";
var ORIENTED = { horizontal: "bc:flex-col", vertical: "bc:flex-row" };
var LIST3 = "bc:-m-1 bc:flex bc:max-w-full bc:overflow-x-auto bc:overflow-y-clip bc:p-1 bc:[scrollbar-width:none]";
var LISTED = { horizontal: "bc:flex-row", vertical: "bc:flex-col bc:items-start" };
var TAB = "bc:relative bc:isolate bc:flex bc:cursor-default bc:items-center bc:rounded-full bc:px-3 bc:py-1.5 bc:font-sans bc:text-sm bc:font-medium bc:text-text-muted bc:outline-none bc:transition-colors bc:forced-color-adjust-none bc:[-webkit-tap-highlight-color:transparent] bc:data-hovered:text-text-default bc:data-selected:text-text-on-accent bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:forced-colors:data-selected:text-[HighlightText]";
var PILL = "bc:absolute bc:inset-0 bc:-z-1 bc:rounded-full bc:bg-bg-accent bc:motion-safe:transition-[translate,width,height] bc:forced-colors:bg-[Highlight]";
var PANEL2 = "bc:box-border bc:flex-1 bc:rounded bc:p-4 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";

// srcts/layout/toolbar.tsx
import {
  composeRenderProps as composeRenderProps34,
  Separator as AriaSeparator,
  ToggleButtonGroupContext,
  Toolbar as AriaToolbar
} from "./vendor-c5b8952d.js";
import { jsx as jsx39 } from "react/jsx-runtime";
function Toolbar({ className, ...props }) {
  return /* @__PURE__ */ jsx39(ToggleButtonGroupContext, { value: { orientation: props.orientation }, children: /* @__PURE__ */ jsx39(
    AriaToolbar,
    {
      ...props,
      className: composeRenderProps34(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED2[orientation]} ${given ?? ""}`)
    }
  ) });
}
function Separator({ className, ...props }) {
  return /* @__PURE__ */ jsx39(AriaSeparator, { ...props, className: `${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}` });
}
var TOOLBAR = "bc:flex bc:flex-wrap bc:gap-2";
var ORIENTED2 = { horizontal: "bc:flex-row bc:items-center", vertical: "bc:flex-col bc:items-start" };
var SEPARATOR = "bc:m-0 bc:border-none bc:bg-border-default bc:forced-colors:bg-[ButtonBorder]";
var ACROSS = { horizontal: "bc:h-px bc:w-full", vertical: "bc:w-px bc:self-stretch" };

// srcts/layout/index.ts
import { DisclosureGroup } from "./vendor-c5b8952d.js";

// srcts/react/notifications.tsx
import { jsx as jsx40 } from "react/jsx-runtime";
function NotificationList({ notifications, "aria-label": label }) {
  if (notifications.length === 0) return null;
  return /* @__PURE__ */ jsx40("ul", { "aria-label": label, className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: notifications.map(({ id, severity, message }) => /* @__PURE__ */ jsx40("li", { "data-severity": severity, className: `${ITEM3} ${SETS[severity]}`, children: message }, id)) });
}
var ITEM3 = "bc:rounded-md bc:border bc:px-3 bc:py-2 bc:font-sans bc:text-[0.8125rem] bc:leading-normal bc:whitespace-pre-wrap bc:wrap-anywhere";
var SETS = {
  error: "bc:border-border-danger-subtle bc:bg-bg-danger bc:text-text-danger",
  warning: "bc:border-border-warning-subtle bc:bg-bg-warning bc:text-text-warning",
  message: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default"
};

// srcts/dnd/dnd.tsx
import { composeRenderProps as composeRenderProps35, DropIndicator as AriaDropIndicator, useDragAndDrop as useAriaDragAndDrop } from "./vendor-c5b8952d.js";
import { jsx as jsx41, jsxs as jsxs31 } from "react/jsx-runtime";
function useDragAndDrop(options) {
  return useAriaDragAndDrop({ renderDropIndicator: (target) => /* @__PURE__ */ jsx41(DropIndicator, { target }), ...options });
}
function DropIndicator(props) {
  return /* @__PURE__ */ jsx41(AriaDropIndicator, { ...props, className: composeRenderProps35(props.className, (given) => `${INDICATOR} ${given ?? ""}`) });
}
function DragButton() {
  return /* @__PURE__ */ jsx41(Button, { slot: "drag", variant: "quiet", children: /* @__PURE__ */ jsx41(Grip, {}) });
}
function Grip() {
  return /* @__PURE__ */ jsxs31("svg", { width: "12", height: "12", viewBox: "0 0 12 12", fill: "currentColor", "aria-hidden": "true", children: [
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "2.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "2.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "4", cy: "9.5", r: "1" }),
    /* @__PURE__ */ jsx41("circle", { cx: "8", cy: "9.5", r: "1" })
  ] });
}
var INDICATOR = "bc:outline-none bc:data-drop-target:outline-1 bc:data-drop-target:outline-bg-accent bc:forced-colors:data-drop-target:outline-[Highlight]";

// srcts/gridlist/gridlist.tsx
import {
  composeRenderProps as composeRenderProps36,
  GridList as AriaGridList,
  GridListHeader as AriaGridListHeader,
  GridListItem as AriaGridListItem,
  GridListSection
} from "./vendor-c5b8952d.js";
import { Fragment as Fragment24, jsx as jsx42, jsxs as jsxs32 } from "react/jsx-runtime";
function GridList(props) {
  return /* @__PURE__ */ jsx42(AriaGridList, { ...props, className: composeRenderProps36(props.className, (given) => `${LIST4} ${given ?? ""}`) });
}
function GridListItem({ children, ...props }) {
  const textValue = props.textValue ?? (typeof children === "string" ? children : void 0);
  return /* @__PURE__ */ jsx42(AriaGridListItem, { ...props, textValue, className: composeRenderProps36(props.className, (given) => `${ITEM4} ${given ?? ""}`), children: composeRenderProps36(children, (children2, { selectionMode, selectionBehavior, allowsDragging }) => /* @__PURE__ */ jsxs32(Fragment24, { children: [
    allowsDragging && /* @__PURE__ */ jsx42(DragButton, {}),
    selectionMode !== "none" && selectionBehavior === "toggle" && /* @__PURE__ */ jsx42(Checkbox, { slot: "selection" }),
    children2
  ] })) });
}
function GridListHeader({ className, ...props }) {
  return /* @__PURE__ */ jsx42(AriaGridListHeader, { ...props, className: `${HEADER2} ${className ?? ""}` });
}
var LIST4 = "bc:grid bc:content-start bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none bc:data-[orientation=horizontal]:flex bc:data-[orientation=horizontal]:flex-row bc:data-[orientation=horizontal]:flex-nowrap bc:data-empty:px-2 bc:data-empty:py-0.5 bc:data-empty:text-sm bc:data-empty:text-text-muted bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var ITEM4 = "bc:flex bc:shrink-0 bc:cursor-pointer bc:select-none bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-0.5 bc:text-sm bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var HEADER2 = "bc:block bc:px-2 bc:pt-1 bc:text-xs bc:font-semibold bc:text-text-muted";

// srcts/table/table.tsx
import {
  Cell as AriaCell,
  Collection as Collection4,
  Column as AriaColumn,
  composeRenderProps as composeRenderProps37,
  Row as AriaRow,
  Table as AriaTable,
  TableBody as AriaTableBody,
  TableHeader as AriaTableHeader,
  useTableOptions
} from "./vendor-c5b8952d.js";
import { jsx as jsx43, jsxs as jsxs33 } from "react/jsx-runtime";
function Table({ className, ...props }) {
  return /* @__PURE__ */ jsx43("div", { onScroll: props.onScroll, className: `${BOX3} ${className ?? ""}`, children: /* @__PURE__ */ jsx43(AriaTable, { ...props, className: TABLE }) });
}
function Column({ align = "start", ...props }) {
  return /* @__PURE__ */ jsx43(
    AriaColumn,
    {
      ...props,
      className: composeRenderProps37(props.className, (given) => `${COLUMN} ${align === "end" ? "bc:text-end" : "bc:text-start"} ${given ?? ""}`),
      children: composeRenderProps37(props.children, (children, { sortDirection }) => /* @__PURE__ */ jsxs33("span", { className: `${HEADING5} ${align === "end" ? "bc:flex-row-reverse" : ""}`, children: [
        children,
        sortDirection === void 0 ? null : /* @__PURE__ */ jsx43(Arrow2, { down: sortDirection === "descending" })
      ] }))
    }
  );
}
function TableHeader({ columns, children, ...props }) {
  const { selectionBehavior, selectionMode, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs33(AriaTableHeader, { ...props, children: [
    allowsDragging && /* @__PURE__ */ jsx43(AriaColumn, { className: PART }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx43(AriaColumn, { className: PART, children: selectionMode === "multiple" && /* @__PURE__ */ jsx43(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx43(Collection4, { items: columns, children })
  ] });
}
function TableBody(props) {
  return /* @__PURE__ */ jsx43(AriaTableBody, { ...props, className: composeRenderProps37(props.className, (given) => `${BODY} ${given ?? ""}`) });
}
function Row({ id, columns, children, ...props }) {
  const { selectionBehavior, allowsDragging } = useTableOptions();
  return /* @__PURE__ */ jsxs33(AriaRow, { id, ...props, className: composeRenderProps37(props.className, (given) => `${ROW2} ${given ?? ""}`), children: [
    allowsDragging && /* @__PURE__ */ jsx43(Cell, { className: "bc:w-px", children: /* @__PURE__ */ jsx43(DragButton, {}) }),
    selectionBehavior === "toggle" && /* @__PURE__ */ jsx43(Cell, { className: "bc:w-px", children: /* @__PURE__ */ jsx43(Checkbox, { slot: "selection" }) }),
    /* @__PURE__ */ jsx43(Collection4, { items: columns, children })
  ] });
}
function Cell(props) {
  return /* @__PURE__ */ jsx43(AriaCell, { ...props, className: composeRenderProps37(props.className, (given) => `${CELL2} ${given ?? ""}`) });
}
function Arrow2({ down }) {
  return /* @__PURE__ */ jsx43(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: `bc:mb-0.5 bc:shrink-0 bc:forced-colors:text-[ButtonText] ${down ? "bc:rotate-180" : ""}`,
      children: /* @__PURE__ */ jsx43("path", { d: "M8 13V3M4 7l4-4 4 4" })
    }
  );
}
var BOX3 = "bc:min-w-0 bc:overflow-auto bc:font-sans bc:text-text-default";
var TABLE = "bc:w-full bc:border-collapse bc:text-left bc:text-sm";
var COLUMN = "bc:border-b bc:border-border-default bc:px-2 bc:py-1 bc:align-bottom bc:font-semibold bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-allows-sorting:cursor-pointer";
var HEADING5 = "bc:flex bc:items-end bc:gap-1";
var PART = "bc:w-px bc:border-b bc:border-border-default bc:px-2 bc:py-1";
var BODY = "bc:data-empty:text-center bc:data-empty:text-sm bc:data-empty:italic bc:data-empty:text-text-muted";
var ROW2 = "bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50 bc:data-selection-mode:select-none";
var CELL2 = "bc:border-b bc:border-border-default/40 bc:px-2 bc:py-1 bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";

// srcts/tree/navigation-tree.tsx
import { createContext as createContext5, useContext as useContext11 } from "react";
import {
  Button as Button10,
  composeRenderProps as composeRenderProps39,
  Link as Link2,
  NavigationTree as AriaNavigationTree,
  NavigationTreeHeader as AriaNavigationTreeHeader,
  NavigationTreeItem as AriaNavigationTreeItem,
  NavigationTreeItemContent as AriaNavigationTreeItemContent,
  NavigationTreeSection as AriaNavigationTreeSection
} from "./vendor-c5b8952d.js";

// srcts/tree/tree.tsx
import {
  Button as Button9,
  composeRenderProps as composeRenderProps38,
  Tree as AriaTree,
  TreeItem as AriaTreeItem,
  TreeItemContent
} from "./vendor-c5b8952d.js";
import { jsx as jsx44, jsxs as jsxs34 } from "react/jsx-runtime";
function Tree(props) {
  return /* @__PURE__ */ jsx44(AriaTree, { ...props, className: composeRenderProps38(props.className, (given) => `${TREE} ${given ?? ""}`) });
}
function TreeItem({ title, children, ...props }) {
  return /* @__PURE__ */ jsxs34(AriaTreeItem, { textValue: title, ...props, className: composeRenderProps38(props.className, (given) => `${ITEM5} ${given ?? ""}`), children: [
    /* @__PURE__ */ jsx44(TreeItemContent, { children: ({ selectionMode, selectionBehavior, hasChildItems, isExpanded, allowsDragging }) => /* @__PURE__ */ jsxs34("div", { className: "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-1", children: [
      allowsDragging && /* @__PURE__ */ jsx44(DragButton, {}),
      selectionMode !== "none" && selectionBehavior === "toggle" && /* @__PURE__ */ jsx44(Checkbox, { slot: "selection" }),
      /* @__PURE__ */ jsx44("div", { className: INDENT }),
      hasChildItems ? /* @__PURE__ */ jsx44(Button9, { slot: "chevron", className: EXPAND, children: /* @__PURE__ */ jsx44(Chevron3, { expanded: isExpanded }) }) : /* @__PURE__ */ jsx44("div", { className: "bc:size-6 bc:shrink-0" }),
      /* @__PURE__ */ jsx44("span", { className: "bc:min-w-0 bc:truncate", children: title })
    ] }) }),
    children
  ] });
}
function Chevron3({ expanded }) {
  return /* @__PURE__ */ jsx44(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      className: `bc:text-text-muted bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`,
      children: /* @__PURE__ */ jsx44("path", { d: "M6 3.5 10.5 8 6 12.5" })
    }
  );
}
var TREE = "bc:grid bc:content-start bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var ITEM5 = "bc:group bc:relative bc:flex bc:cursor-default bc:items-center bc:rounded bc:px-1 bc:font-sans bc:text-sm bc:text-text-default bc:select-none bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50";
var INDENT = "bc:w-[calc((var(--tree-item-level)_-_1)_*_var(--bc-spacing)_*_3)] bc:shrink-0";
var EXPAND = "bc:flex bc:size-6 bc:shrink-0 bc:cursor-default bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:outline-none bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:opacity-50";

// srcts/tree/navigation-tree.tsx
import { Fragment as Fragment25, jsx as jsx45, jsxs as jsxs35 } from "react/jsx-runtime";
var IndicatorContext = createContext5(void 0);
var RenderLinkContext = createContext5(void 0);
function NavigationTree({ renderLink, ...props }) {
  return /* @__PURE__ */ jsx45(RenderLinkContext.Provider, { value: renderLink, children: /* @__PURE__ */ jsx45(AriaNavigationTree, { ...props, className: composeRenderProps39(props.className, (given) => `${TREE2} ${given ?? ""}`) }) });
}
function NavigationTreeItem(props) {
  return /* @__PURE__ */ jsx45(AriaNavigationTreeItem, { ...props, className: composeRenderProps39(props.className, (given) => `${ITEM6} ${given ?? ""}`) });
}
function NavigationTreeItemContent({ children }) {
  return /* @__PURE__ */ jsx45(AriaNavigationTreeItemContent, { children: ({ level, hasChildItems, isExpanded, isCurrent, isCurrentAncestor }) => /* @__PURE__ */ jsxs35(Fragment25, { children: [
    level > 1 && /* @__PURE__ */ jsx45("div", { className: "bc:shrink-0", style: { width: `calc((${String(level)} - 1) * var(--bc-spacing) * 4)` } }),
    /* @__PURE__ */ jsx45(IndicatorContext.Provider, { value: isCurrent ? "current" : isCurrentAncestor && !isExpanded ? "ancestor" : void 0, children }),
    /* @__PURE__ */ jsx45(Button10, { slot: "chevron", isDisabled: !hasChildItems, className: `${EXPAND2} ${hasChildItems ? "" : "bc:invisible"}`, children: /* @__PURE__ */ jsx45(Chevron3, { expanded: isExpanded }) })
  ] }) });
}
function NavigationTreeSection(props) {
  return /* @__PURE__ */ jsx45(AriaNavigationTreeSection, { ...props, className: "bc:not-first:mt-4" });
}
function NavigationTreeHeader(props) {
  return /* @__PURE__ */ jsx45(AriaNavigationTreeHeader, { ...props, className: "bc:px-2 bc:py-1 bc:text-sm bc:font-semibold bc:text-text-muted" });
}
function NavigationTreeItemLink({ render, children, ...props }) {
  const indicator = useContext11(IndicatorContext);
  const given = useContext11(RenderLinkContext);
  return /* @__PURE__ */ jsx45(Link2, { render: render ?? given, ...props, className: LINK2, children: ({ isHovered }) => /* @__PURE__ */ jsxs35(Fragment25, { children: [
    /* @__PURE__ */ jsx45("span", { "aria-hidden": "true", "data-indicator": indicator ?? (isHovered ? "hover" : void 0), className: MARK }),
    children
  ] }) });
}
var TREE2 = "bc:flex bc:flex-col bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none";
var ITEM6 = "bc:group bc:relative bc:flex bc:cursor-default bc:items-center bc:rounded-md bc:font-sans bc:text-text-default bc:select-none bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:opacity-50";
var LINK2 = "bc:relative bc:flex bc:min-w-0 bc:flex-1 bc:cursor-pointer bc:items-center bc:gap-2 bc:px-2 bc:py-1.5 bc:text-sm bc:text-current bc:no-underline bc:outline-none bc:group-[:not([data-href])]:cursor-default bc:data-disabled:cursor-default";
var MARK = "bc:absolute bc:start-0.5 bc:top-1/2 bc:h-[1lh] bc:w-0.5 bc:-translate-y-1/2 bc:rounded-full bc:forced-color-adjust-none bc:group-data-href:data-[indicator=hover]:bg-border-default bc:data-[indicator=current]:bg-bg-accent bc:data-[indicator=ancestor]:size-1 bc:data-[indicator=ancestor]:bg-border-default bc:forced-colors:data-indicator:bg-[Highlight]";
var EXPAND2 = "bc:flex bc:size-6 bc:shrink-0 bc:cursor-default bc:items-center bc:justify-center bc:rounded-md bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
export {
  AlertDialog,
  Autocomplete2 as Autocomplete,
  Breadcrumb,
  Breadcrumbs,
  Button,
  Calendar,
  Cell,
  Checkbox,
  CheckboxGroup,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorThumb,
  ColorWheel,
  Column,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  CommandPalette,
  DateField,
  DatePicker,
  DateRangePicker,
  Dialog,
  DialogTrigger,
  Disclosure,
  DisclosureGroup,
  DisclosureHeader,
  DisclosurePanel,
  DropIndicator,
  DropZone,
  Field,
  FieldButton,
  FileTrigger,
  Focusable,
  Form,
  GridList,
  GridListHeader,
  GridListItem,
  GridListSection,
  Heading2 as Heading,
  Keyboard,
  Link,
  ListBox2 as ListBox,
  ListBoxItem2 as ListBoxItem,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Meter,
  Modal,
  NavigationTree,
  NavigationTreeHeader,
  NavigationTreeItem,
  NavigationTreeItemContent,
  NavigationTreeItemLink,
  NavigationTreeSection,
  NotificationList,
  NumberField,
  Popover,
  ProgressBar,
  Radio,
  RadioGroup,
  RangeCalendar,
  Row,
  SearchField,
  Select,
  SelectItem,
  SelectSection,
  Separator,
  Sheet,
  Slider2 as Slider,
  Stack,
  SubmenuTrigger,
  Switch,
  Tab,
  TabList,
  TabPanel,
  Table,
  TableBody,
  TableHeader,
  Tabs,
  Tag2 as Tag,
  TagGroup2 as TagGroup,
  Text15 as Text,
  TextField,
  TimeField,
  Toast,
  UNSTABLE_ToastContent as ToastContent,
  UNSTABLE_ToastQueue as ToastQueue,
  ToastRegion,
  ToggleButton,
  ToggleButtonGroup,
  Token,
  TokenField,
  Toolbar,
  Tooltip,
  TooltipTrigger,
  Tree,
  TreeItem,
  outputRenderer,
  reactModule,
  useBoard,
  useDragAndDrop,
  useFilter2 as useFilter,
  useNotified,
  useOutput,
  useRequest,
  useStatus,
  useValue
};
