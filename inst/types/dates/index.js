// srcts/aria/index.ts
import { Button } from "react-aria-components/Button";
import { composeRenderProps } from "react-aria-components/composeRenderProps";
import { FieldError } from "react-aria-components/FieldError";
import { Group } from "react-aria-components/Group";
import { useLocale } from "react-aria-components/I18nProvider";
import { Label } from "react-aria-components/Label";
import { Popover } from "react-aria-components/Popover";
import { Text } from "react-aria-components/Text";

// srcts/dates/aria.ts
import { Calendar, CalendarCell, CalendarGrid, CalendarGridBody, CalendarGridHeader, CalendarHeaderCell, CalendarHeading } from "react-aria-components/Calendar";
import { DateField, DateInput, DateSegment } from "react-aria-components/DateField";
import { DatePicker } from "react-aria-components/DatePicker";
import { DateRangePicker } from "react-aria-components/DateRangePicker";
import { RangeCalendar } from "react-aria-components/RangeCalendar";
import { TimeField } from "react-aria-components/TimeField";
import { parseDate, parseDateTime, parseTime, toCalendarDate, toCalendarDateTime, toTime } from "@internationalized/date";

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
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/inputs/button.tsx
import { jsx } from "react/jsx-runtime";
function Button2({ variant = "primary", className, ...props }) {
  return /* @__PURE__ */ jsx(Button, { ...props, className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`) });
}
function FieldButton({ className, ...props }) {
  return /* @__PURE__ */ jsx(Button, { ...props, className: composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`) });
}
var FIELD_BUTTON = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/checkbox.tsx
import { useRef as useRef2 } from "react";

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

// srcts/inputs/icons.tsx
import { jsx as jsx2, jsxs } from "react/jsx-runtime";

// srcts/inputs/inputs.tsx
import { useState } from "react";
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function NotAvailable({
  values,
  onRemove
}) {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return /* @__PURE__ */ jsxs2("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-2", children: [
    /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: `Not available: ${named}` }),
    onRemove === void 0 ? null : /* @__PURE__ */ jsx3(Button2, { variant: "quiet", "aria-label": `Remove ${named}`, onPress: onRemove, children: "Remove" })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error });
}

// srcts/inputs/items.ts
import { createContext, useCallback, useContext, useLayoutEffect, useMemo, useRef, useState as useState2 } from "react";
var ItemsContext = createContext(null);

// srcts/inputs/checkbox.tsx
import { Fragment, jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";

// srcts/inputs/combobox.tsx
import { useContext as useContext2, useRef as useRef3, useState as useState3 } from "react";
import { Fragment as Fragment2, jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";

// srcts/inputs/dropzone.tsx
import { jsx as jsx6 } from "react/jsx-runtime";

// srcts/inputs/form.tsx
import { jsx as jsx7 } from "react/jsx-runtime";

// srcts/inputs/listbox.tsx
import { jsx as jsx8 } from "react/jsx-runtime";

// srcts/inputs/numberfield.tsx
import { useContext as useContext3, useEffect, useRef as useRef4, useState as useState4 } from "react";
import { Fragment as Fragment3, jsx as jsx9, jsxs as jsxs5 } from "react/jsx-runtime";

// srcts/inputs/radio.tsx
import { useRef as useRef5 } from "react";
import { Fragment as Fragment4, jsx as jsx10, jsxs as jsxs6 } from "react/jsx-runtime";

// srcts/inputs/searchfield.tsx
import { Fragment as Fragment5, jsx as jsx11, jsxs as jsxs7 } from "react/jsx-runtime";

// srcts/inputs/select.tsx
import { useContext as useContext4, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef6, useState as useState5 } from "react";
import { Fragment as Fragment6, jsx as jsx12, jsxs as jsxs8 } from "react/jsx-runtime";

// srcts/inputs/slider.tsx
import { useContext as useContext5, useLayoutEffect as useLayoutEffect3, useRef as useRef7, useState as useState6 } from "react";
import { Fragment as Fragment7, jsx as jsx13, jsxs as jsxs9 } from "react/jsx-runtime";

// srcts/inputs/switch.tsx
import { Fragment as Fragment8, jsx as jsx14, jsxs as jsxs10 } from "react/jsx-runtime";

// srcts/inputs/tags.tsx
import { createContext as createContext2, useContext as useContext6 } from "react";
import { Fragment as Fragment9, jsx as jsx15, jsxs as jsxs11 } from "react/jsx-runtime";
var ColorContext = createContext2("gray");

// srcts/inputs/textfield.tsx
import { Fragment as Fragment10, jsx as jsx16, jsxs as jsxs12 } from "react/jsx-runtime";

// srcts/inputs/toggle.tsx
import { createContext as createContext3, useContext as useContext7, useId, useRef as useRef8 } from "react";
import { Fragment as Fragment11, jsx as jsx17, jsxs as jsxs13 } from "react/jsx-runtime";
var InGroup = createContext3(false);

// srcts/inputs/tokenfield.tsx
import { jsx as jsx18, jsxs as jsxs14 } from "react/jsx-runtime";

// srcts/dates/bound.ts
import { useRef as useRef9, useState as useState7 } from "react";

// srcts/dates/values.ts
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
  const held2 = bound.value === void 0 ? null : read(bound.value);
  const [kept, setKept] = useState7(null);
  const latest = useRef9(null);
  const keep = (next) => {
    latest.current = next;
    setKept(next);
  };
  const [seen, setSeen] = useState7(bound.value);
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
    value: kept === null ? held2 ?? null : kept.value,
    missing: kept === null && bound.value !== void 0 && held2 === void 0 ? shownValue(bound.value) : null,
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
import { Fragment as Fragment12, jsx as jsx19, jsxs as jsxs15 } from "react/jsx-runtime";
function Calendar2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnCalendar, { ...props }) : /* @__PURE__ */ jsx19(BoundCalendar, { ...props, bound });
}
function RangeCalendar2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnRange, { ...props }) : /* @__PURE__ */ jsx19(BoundRange, { ...props, bound });
}
function BoundCalendar({ bound, onChange, ...props }) {
  const date = useBoundDate(bound, {
    read: (value) => readDate(value, false),
    write: (value) => writeDate(value, false),
    fits: (value) => dateFits(value, props),
    required: true
  });
  return /* @__PURE__ */ jsxs15(Fragment12, { children: [
    /* @__PURE__ */ jsx19(
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
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function BoundRange({ bound, onChange, ...props }) {
  const range = useBoundDate(bound, {
    read: (value) => readRange(value, false),
    write: (value) => writeRange(value, false),
    fits: (value) => rangeFits(value, { minValue: props.minValue, maxValue: props.maxValue }),
    required: true
  });
  return /* @__PURE__ */ jsxs15(Fragment12, { children: [
    /* @__PURE__ */ jsx19(
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
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function DrawnCalendar({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs15(
    Calendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx19(Months, { count: props.visibleDuration?.months ?? 1, cell: (date) => /* @__PURE__ */ jsx19(CalendarCell, { date, className: CELL }) }),
        errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx19(Text, { slot: "errorMessage", className: styles.alert, children: errorMessage2 }),
        missing === null ? null : /* @__PURE__ */ jsx19(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function DrawnRange({ errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs15(
    RangeCalendar,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${CALENDAR} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx19(
          Months,
          {
            count: props.visibleDuration?.months ?? 1,
            cell: (date) => /* @__PURE__ */ jsx19(CalendarCell, { date, className: RANGE_CELL, children: ({ formattedDate, isSelected, isSelectionStart, isSelectionEnd }) => /* @__PURE__ */ jsx19("span", { className: isSelected && (isSelectionStart || isSelectionEnd) ? `${DAY} ${END}` : `${DAY} ${isSelected ? "" : UNPICKED}`, children: formattedDate }) })
          }
        ),
        errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx19(Text, { slot: "errorMessage", className: styles.alert, children: errorMessage2 }),
        missing === null ? null : /* @__PURE__ */ jsx19(NotAvailable, { values: [missing] })
      ]
    }
  );
}
function Months({ count, cell }) {
  const { direction } = useLocale();
  return /* @__PURE__ */ jsx19(Fragment12, { children: Array.from({ length: count }, (_, i) => /* @__PURE__ */ jsxs15("div", { className: MONTH, children: [
    /* @__PURE__ */ jsxs15("header", { className: HEADER, children: [
      i === 0 ? /* @__PURE__ */ jsx19(Button2, { variant: "quiet", slot: "previous", className: PAGE, children: /* @__PURE__ */ jsx19(Arrow, { back: direction !== "rtl" }) }) : null,
      /* @__PURE__ */ jsx19(CalendarHeading, { offset: { months: i }, className: HEADING }),
      i === count - 1 ? /* @__PURE__ */ jsx19(Button2, { variant: "quiet", slot: "next", className: PAGE, children: /* @__PURE__ */ jsx19(Arrow, { back: direction === "rtl" }) }) : null
    ] }),
    /* @__PURE__ */ jsxs15(CalendarGrid, { offset: { months: i }, className: GRID, children: [
      /* @__PURE__ */ jsx19(CalendarGridHeader, { children: (day) => /* @__PURE__ */ jsx19(CalendarHeaderCell, { className: WEEKDAY, children: day }) }),
      /* @__PURE__ */ jsx19(CalendarGridBody, { children: cell })
    ] })
  ] }, i)) });
}
function Arrow({ back }) {
  return /* @__PURE__ */ jsx19(
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
      children: /* @__PURE__ */ jsx19("polyline", { points: back ? "7.5 3 4.5 6 7.5 9" : "4.5 3 7.5 6 4.5 9" })
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
import { Fragment as Fragment13, jsx as jsx20, jsxs as jsxs16 } from "react/jsx-runtime";
function DateField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx20(DrawnDate, { ...props }) : /* @__PURE__ */ jsx20(BoundDate, { ...props, bound });
}
function TimeField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx20(DrawnTime, { ...props }) : /* @__PURE__ */ jsx20(BoundTime, { ...props, bound });
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
  return /* @__PURE__ */ jsxs16(Fragment13, { children: [
    /* @__PURE__ */ jsx20(
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
    /* @__PURE__ */ jsx20(Refusal, { error: bound.error })
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
  return /* @__PURE__ */ jsxs16(Fragment13, { children: [
    /* @__PURE__ */ jsx20(
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
    /* @__PURE__ */ jsx20(Refusal, { error: bound.error })
  ] });
}
function DrawnDate({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsx20(
    DateField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx20(FieldParts, { label, description, errorMessage: errorMessage2, missing })
    }
  );
}
function DrawnTime({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsx20(
    TimeField,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx20(FieldParts, { label, description, errorMessage: errorMessage2, missing })
    }
  );
}
function FieldParts({
  label,
  description,
  errorMessage: errorMessage2,
  missing
}) {
  return /* @__PURE__ */ jsxs16(Fragment13, { children: [
    label === void 0 ? null : /* @__PURE__ */ jsx20(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx20(DateInput2, { className: BOX }),
    /* @__PURE__ */ jsx20(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx20(FieldError, { className: styles.alert, children: errorMessage2 }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx20(NotAvailable, { values: [missing] })
  ] });
}
function DateInput2({ slot, className }) {
  return /* @__PURE__ */ jsx20(DateInput, { slot, className, children: (segment) => /* @__PURE__ */ jsx20(DateSegment, { segment, className: SEGMENT }) });
}
var BOX = "bc:flex bc:min-w-0 bc:items-center bc:justify-self-stretch bc:overflow-x-auto bc:whitespace-nowrap bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:[scrollbar-width:none] bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var INLINE = "bc:flex bc:min-w-0 bc:items-center bc:whitespace-nowrap bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-text bc:data-disabled:cursor-default";
var SEGMENT = "bc:rounded-sm bc:px-0.5 bc:tabular-nums bc:caret-transparent bc:outline-none bc:data-[type=literal]:px-0 bc:data-placeholder:text-text-muted bc:data-focused:bg-bg-accent bc:data-focused:text-text-on-accent";

// srcts/dates/picker.tsx
import { useRef as useRef10 } from "react";

// srcts/overlays/dialog.tsx
import { jsx as jsx21 } from "react/jsx-runtime";

// srcts/overlays/menu.tsx
import { Children } from "react";

// srcts/overlays/popover.tsx
import { jsx as jsx22 } from "react/jsx-runtime";
function Popover2({ className, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx22(Popover, { ...props, offset, className: composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`) });
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment14, jsx as jsx23, jsxs as jsxs17 } from "react/jsx-runtime";

// srcts/overlays/palette.tsx
import { jsx as jsx24, jsxs as jsxs18 } from "react/jsx-runtime";

// srcts/dates/picker.tsx
import { Fragment as Fragment15, jsx as jsx25, jsxs as jsxs19 } from "react/jsx-runtime";
function DatePicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx25(DrawnPicker, { ...props }) : /* @__PURE__ */ jsx25(BoundPicker, { ...props, bound });
}
function DateRangePicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx25(DrawnRangePicker, { ...props }) : /* @__PURE__ */ jsx25(BoundRangePicker, { ...props, bound });
}
function usePicking(value) {
  const open = useRef10(false);
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
  return /* @__PURE__ */ jsxs19(Fragment15, { children: [
    /* @__PURE__ */ jsx25(
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
    /* @__PURE__ */ jsx25(Refusal, { error: bound.error })
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
  return /* @__PURE__ */ jsxs19(Fragment15, { children: [
    /* @__PURE__ */ jsx25(
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
    /* @__PURE__ */ jsx25(Refusal, { error: bound.error })
  ] });
}
function DrawnPicker({ label, description, errorMessage: errorMessage2, missing = null, isInvalid, className, ...props }) {
  return /* @__PURE__ */ jsxs19(
    DatePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx25(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs19(Group, { className: FIELD, children: [
          /* @__PURE__ */ jsx25(DateInput2, { className: `${INLINE} bc:flex-1` }),
          /* @__PURE__ */ jsx25(Opener, {})
        ] }),
        /* @__PURE__ */ jsx25(PickerParts, { description, errorMessage: errorMessage2, missing }),
        /* @__PURE__ */ jsx25(Popover2, { className: POPOVER2, children: /* @__PURE__ */ jsx25(Calendar2, {}) })
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
  return /* @__PURE__ */ jsxs19(
    DateRangePicker,
    {
      ...props,
      isInvalid: missing === null ? isInvalid : true,
      className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx25(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs19(Group, { className: FIELD, children: [
          /* @__PURE__ */ jsxs19("div", { className: ENDS, children: [
            /* @__PURE__ */ jsx25(DateInput2, { slot: "start", className: INLINE }),
            /* @__PURE__ */ jsx25("span", { "aria-hidden": "true", className: "bc:px-1 bc:text-text-muted", children: "\u2013" }),
            /* @__PURE__ */ jsx25(DateInput2, { slot: "end", className: `${INLINE} bc:flex-1` })
          ] }),
          /* @__PURE__ */ jsx25(Opener, {})
        ] }),
        /* @__PURE__ */ jsx25(PickerParts, { description, errorMessage: errorMessage2, missing }),
        /* @__PURE__ */ jsx25(Popover2, { className: POPOVER2, children: /* @__PURE__ */ jsx25(RangeCalendar2, {}) })
      ]
    }
  );
}
function PickerParts({
  description,
  errorMessage: errorMessage2,
  missing
}) {
  return /* @__PURE__ */ jsxs19(Fragment15, { children: [
    /* @__PURE__ */ jsx25(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx25(FieldError, { className: styles.alert, children: errorMessage2 }),
    missing === null || missing === void 0 ? null : /* @__PURE__ */ jsx25(NotAvailable, { values: [missing] })
  ] });
}
function Opener() {
  return /* @__PURE__ */ jsx25(FieldButton, { children: /* @__PURE__ */ jsxs19(
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
        /* @__PURE__ */ jsx25("rect", { x: "2.5", y: "3.5", width: "11", height: "10", rx: "1.5" }),
        /* @__PURE__ */ jsx25("path", { d: "M2.5 6.5h11M5.5 2v3M10.5 2v3" })
      ]
    }
  ) });
}
var FIELD = "bc:flex bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var ENDS = "bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:overflow-x-auto bc:[scrollbar-width:none]";
var POPOVER2 = "bc:p-3";
export {
  Calendar2 as Calendar,
  DateField2 as DateField,
  DatePicker2 as DatePicker,
  DateRangePicker2 as DateRangePicker,
  RangeCalendar2 as RangeCalendar,
  TimeField2 as TimeField
};
