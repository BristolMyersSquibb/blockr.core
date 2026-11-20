// srcts/dates/calendar.tsx
import { composeRenderProps, Text } from "./vendor-WDKFFI2H.js";
import {
  Calendar as AriaCalendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  CalendarHeading,
  RangeCalendar as AriaRangeCalendar,
  useLocale
} from "./vendor-dates-CAREHF26.js";
import { Button, missing as shownMissing, Refusal } from "@blockr/core/react";

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

// srcts/dates/bound.ts
import { useRef, useState } from "react";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
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

// srcts/dates/values.ts
import { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "./vendor-dates-CAREHF26.js";
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
  const start = readDate(own(record, "start") ?? null, time);
  const end = readDate(own(record, "end") ?? null, time);
  return start === void 0 || end === void 0 ? void 0 : { start, end };
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

// srcts/dates/bound.ts
function useBoundDate(bound, { read, write, fits, required }) {
  const held = bound.value === void 0 ? null : read(bound.value);
  const [kept, setKept] = useState(null);
  const latest = useRef(null);
  const keep = (next) => {
    latest.current = next;
    setKept(next);
  };
  const [seen, setSeen] = useState(bound.value);
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
    value: kept === null ? held ?? null : kept.value,
    missing: kept === null && bound.value !== void 0 && held === void 0 ? shownValue(bound.value) : null,
    edit: (value) => {
      keep({ value, submitted: false });
    },
    pick: submit,
    flush: () => {
      const edit = latest.current;
      if (edit !== null && !edit.submitted) submit(edit.value);
    }
  };
}

// srcts/dates/calendar.tsx
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
function Calendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx(DrawnCalendar, { ...props }) : /* @__PURE__ */ jsx(BoundCalendar, { ...props, bound });
}
function RangeCalendar({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx(DrawnRange, { ...props }) : /* @__PURE__ */ jsx(BoundRange, { ...props, bound });
}
function BoundCalendar({ bound, onChange, ...props }) {
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true
  });
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(
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
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
  ] });
}
function BoundRange({ bound, onChange, ...props }) {
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true
  });
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(
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
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
  ] });
}
function DrawnCalendar({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  const shown = missing === null ? errorMessage2 : shownMissing(missing);
  return /* @__PURE__ */ jsxs(
    AriaCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx(Months, { count: props.visibleDuration?.months ?? 1, cell: (date) => /* @__PURE__ */ jsx(CalendarCell, { date, className: CELL }) }),
        shown === void 0 ? null : /* @__PURE__ */ jsx(Text, { slot: "errorMessage", className: styles.alert, children: shown })
      ]
    }
  );
}
function DrawnRange({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  const shown = missing === null ? errorMessage2 : shownMissing(missing);
  return /* @__PURE__ */ jsxs(
    AriaRangeCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx(
          Months,
          {
            count: props.visibleDuration?.months ?? 1,
            cell: (date) => /* @__PURE__ */ jsx(CalendarCell, { date, className: RANGE_CELL, children: ({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => /* @__PURE__ */ jsx("span", { className: isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`, children: formattedDate }) })
          }
        ),
        shown === void 0 ? null : /* @__PURE__ */ jsx(Text, { slot: "errorMessage", className: styles.alert, children: shown })
      ]
    }
  );
}
function Months({ count, cell }) {
  const { direction } = useLocale();
  return /* @__PURE__ */ jsx(Fragment, { children: Array.from({ length: count }, (_, i) => /* @__PURE__ */ jsxs("div", { className: MONTH, children: [
    /* @__PURE__ */ jsxs("header", { className: HEADER, children: [
      i === 0 ? /* @__PURE__ */ jsx(Button, { variant: "quiet", slot: "previous", className: PAGE, children: /* @__PURE__ */ jsx(Arrow, { back: direction !== "rtl" }) }) : null,
      /* @__PURE__ */ jsx(CalendarHeading, { offset: { months: i }, className: HEADING }),
      i === count - 1 ? /* @__PURE__ */ jsx(Button, { variant: "quiet", slot: "next", className: PAGE, children: /* @__PURE__ */ jsx(Arrow, { back: direction === "rtl" }) }) : null
    ] }),
    /* @__PURE__ */ jsxs(CalendarGrid, { offset: { months: i }, className: GRID, children: [
      /* @__PURE__ */ jsx(CalendarGridHeader, { children: (day) => /* @__PURE__ */ jsx(CalendarHeaderCell, { className: WEEKDAY, children: day }) }),
      /* @__PURE__ */ jsx(CalendarGridBody, { children: cell })
    ] })
  ] }, i)) });
}
function Arrow({ back }) {
  return /* @__PURE__ */ jsx(
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
      children: /* @__PURE__ */ jsx("polyline", { points: back ? "7.5 3 4.5 6 7.5 9" : "4.5 3 7.5 6 4.5 9" })
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

// srcts/dates/field.tsx
import { composeRenderProps as composeRenderProps2, FieldError, Label, Text as Text2 } from "./vendor-WDKFFI2H.js";
import { DateField as AriaDateField, DateInput as AriaDateInput, DateSegment, TimeField as AriaTimeField } from "./vendor-dates-CAREHF26.js";
import { missing as shownMissing2, Refusal as Refusal2 } from "@blockr/core/react";
import { Fragment as Fragment2, jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
function DateField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx2(DrawnDate, { ...props }) : /* @__PURE__ */ jsx2(BoundDate, { ...props, bound });
}
function TimeField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx2(DrawnTime, { ...props }) : /* @__PURE__ */ jsx2(BoundTime, { ...props, bound });
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
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx2(
      DrawnDate,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
        onChange: (value) => {
          date.edit(value);
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
    /* @__PURE__ */ jsx2(Refusal2, { error: bound.error })
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
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx2(
      DrawnTime,
      {
        ...props,
        validationBehavior,
        value: time.value,
        missing: time.missing,
        onChange: (value) => {
          time.edit(value);
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
    /* @__PURE__ */ jsx2(Refusal2, { error: bound.error })
  ] });
}
function DrawnDate({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsx2(
    AriaDateField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps2(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx2(FieldParts, { label, description, errorMessage: missing === null ? errorMessage2 : shownMissing2(missing) })
    }
  );
}
function DrawnTime({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsx2(
    AriaTimeField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps2(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx2(FieldParts, { label, description, errorMessage: missing === null ? errorMessage2 : shownMissing2(missing) })
    }
  );
}
function FieldParts({
  label,
  description,
  errorMessage: errorMessage2
}) {
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    label === void 0 ? null : /* @__PURE__ */ jsx2(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx2(DateInput, { className: BOX }),
    /* @__PURE__ */ jsx2(Text2, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx2(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
function DateInput({ slot, className }) {
  return /* @__PURE__ */ jsx2(AriaDateInput, { slot, className, children: (segment) => /* @__PURE__ */ jsx2(DateSegment, { segment, className: SEGMENT }) });
}
var BOX = "bc:flex bc:min-w-0 bc:items-center bc:justify-self-stretch bc:overflow-x-auto bc:whitespace-nowrap bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:[scrollbar-width:none] bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var INLINE = "bc:flex bc:min-w-0 bc:items-center bc:whitespace-nowrap bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:data-disabled:cursor-default";
var SEGMENT = "bc:rounded-sm bc:px-0.5 bc:tabular-nums bc:caret-transparent bc:outline-none bc:data-[type=literal]:px-0 bc:data-placeholder:text-text-muted bc:data-focused:bg-bg-accent bc:data-focused:text-text-on-accent";

// srcts/dates/picker.tsx
import { useRef as useRef2 } from "react";
import { Button as Button2, composeRenderProps as composeRenderProps3, FieldError as FieldError2, Group, Label as Label2, Text as Text3 } from "./vendor-WDKFFI2H.js";
import { DatePicker as AriaDatePicker, DateRangePicker as AriaDateRangePicker } from "./vendor-dates-CAREHF26.js";
import { missing as shownMissing3, Refusal as Refusal3 } from "@blockr/core/react";
import { Popover } from "@blockr/core/react";
import { Fragment as Fragment3, jsx as jsx3, jsxs as jsxs3 } from "react/jsx-runtime";
function DatePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(DrawnPicker, { ...props }) : /* @__PURE__ */ jsx3(BoundPicker, { ...props, bound });
}
function DateRangePicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(DrawnRangePicker, { ...props }) : /* @__PURE__ */ jsx3(BoundRangePicker, { ...props, bound });
}
function usePicking(value) {
  const open = useRef2(false);
  return {
    onOpenChange: (isOpen) => {
      open.current = isOpen;
    },
    onChange: (next) => {
      if (open.current) value.pick(next);
      else value.edit(next);
    },
    onBlur: () => {
      value.flush();
    },
    onKeyDown: (event) => {
      if (event.key === "Enter" && event.currentTarget.contains(event.target)) value.flush();
    }
  };
}
function BoundPicker({
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
  return /* @__PURE__ */ jsxs3(Fragment3, { children: [
    /* @__PURE__ */ jsx3(
      DrawnPicker,
      {
        ...props,
        validationBehavior,
        value: date.value,
        missing: date.missing,
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
    /* @__PURE__ */ jsx3(Refusal3, { error: bound.error })
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
  return /* @__PURE__ */ jsxs3(Fragment3, { children: [
    /* @__PURE__ */ jsx3(
      DrawnRangePicker,
      {
        ...props,
        validationBehavior,
        value: range.value,
        missing: range.missing,
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
    /* @__PURE__ */ jsx3(Refusal3, { error: bound.error })
  ] });
}
function DrawnPicker({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs3(
    AriaDatePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps3(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx3(Label2, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs3(Group, { className: FIELD, children: [
          /* @__PURE__ */ jsx3(DateInput, { className: `${INLINE} bc:flex-1` }),
          /* @__PURE__ */ jsx3(Opener, {})
        ] }),
        /* @__PURE__ */ jsx3(PickerParts, { description, errorMessage: missing === null ? errorMessage2 : shownMissing3(missing) }),
        /* @__PURE__ */ jsx3(Popover, { className: POPOVER, children: /* @__PURE__ */ jsx3(Calendar, {}) })
      ]
    }
  );
}
function DrawnRangePicker({
  label,
  description,
  errorMessage: errorMessage2,
  missing = null,
  isInvalid,
  className,
  ...props
}) {
  return /* @__PURE__ */ jsxs3(
    AriaDateRangePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps3(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx3(Label2, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs3(Group, { className: FIELD, children: [
          /* @__PURE__ */ jsxs3("div", { className: ENDS, children: [
            /* @__PURE__ */ jsx3(DateInput, { slot: "start", className: INLINE }),
            /* @__PURE__ */ jsx3("span", { "aria-hidden": "true", className: "bc:px-1 bc:text-text-muted", children: "\u2013" }),
            /* @__PURE__ */ jsx3(DateInput, { slot: "end", className: `${INLINE} bc:flex-1` })
          ] }),
          /* @__PURE__ */ jsx3(Opener, {})
        ] }),
        /* @__PURE__ */ jsx3(PickerParts, { description, errorMessage: missing === null ? errorMessage2 : shownMissing3(missing) }),
        /* @__PURE__ */ jsx3(Popover, { className: POPOVER, children: /* @__PURE__ */ jsx3(RangeCalendar, {}) })
      ]
    }
  );
}
function PickerParts({
  description,
  errorMessage: errorMessage2
}) {
  return /* @__PURE__ */ jsxs3(Fragment3, { children: [
    /* @__PURE__ */ jsx3(Text3, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx3(FieldError2, { className: styles.alert, children: errorMessage2 })
  ] });
}
function Opener() {
  return /* @__PURE__ */ jsx3(Button2, { className: OPENER, children: /* @__PURE__ */ jsxs3(
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
        /* @__PURE__ */ jsx3("rect", { x: "2.5", y: "3.5", width: "11", height: "10", rx: "1.5" }),
        /* @__PURE__ */ jsx3("path", { d: "M2.5 6.5h11M5.5 2v3M10.5 2v3" })
      ]
    }
  ) });
}
var FIELD = "bc:flex bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var ENDS = "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:overflow-x-auto bc:[scrollbar-width:none]";
var OPENER = "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default";
var POPOVER = "bc:p-3";
export {
  Calendar,
  DateField,
  DatePicker,
  DateRangePicker,
  RangeCalendar,
  TimeField
};
