// srcts/controls/controls.tsx
import { Fragment as Fragment3, useLayoutEffect as useLayoutEffect2, useRef as useRef2 } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
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

// srcts/binding/binding.ts
var PAUSE = 500;
function bind(host, key, show, options = {}) {
  const source = createSource(host);
  source.check(key);
  let shown;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || timer !== void 0) return;
    const value = source.get(key);
    if (!again && shown !== void 0 && jsonEqual(shown.value, value)) return;
    shown = { value };
    show(value);
  };
  const submit = async (value) => {
    if (jsonEqual(source.get(key), value)) return true;
    try {
      await source.submit(key, value);
    } catch (error) {
      options.settled?.(error);
      return false;
    }
    options.settled?.(null);
    return true;
  };
  const send = () => {
    const value = typed;
    typed = void 0;
    if (value !== void 0) void submit(value);
  };
  const flush = () => {
    if (timer === void 0) return;
    clearTimeout(timer);
    timer = void 0;
    send();
  };
  const off = source.onValues(() => {
    display();
  });
  display(true);
  return {
    get: () => source.get(key),
    async commit(value) {
      const took = await submit(value);
      if (!took) display(true);
      return took;
    },
    type(value) {
      if (released) return;
      typed = value;
      clearTimeout(timer);
      timer = setTimeout(() => {
        timer = void 0;
        send();
      }, PAUSE);
    },
    flush,
    release() {
      if (released) return;
      released = true;
      off();
      flush();
    }
  };
}
function follow(host, name, options) {
  const source = createSource(host);
  let asked;
  let latest;
  let released = false;
  const ask = () => {
    latest?.abort();
    const controller = new AbortController();
    latest = controller;
    options.loading?.();
    source.request(name, options.args?.() ?? {}, { signal: controller.signal }).then(
      (answer) => {
        if (latest === controller && !controller.signal.aborted) options.answer(answer);
      },
      (error) => {
        if (latest === controller && !controller.signal.aborted) options.failed?.(error);
      }
    );
  };
  const check = () => {
    if (released) return;
    const reads = JSON.stringify([source.inputs(), options.args?.() ?? {}, options.reads?.() ?? null]);
    if (reads === asked) return;
    asked = reads;
    ask();
  };
  const offValues = source.onValues(check);
  const offContext = source.onContext(check);
  check();
  return {
    refresh() {
      asked = void 0;
      check();
    },
    release() {
      if (released) return;
      released = true;
      offValues();
      offContext();
      latest?.abort();
    }
  };
}
function createSource(host) {
  if ("option" in host) {
    const check = (key) => {
      if (key !== host.option) {
        throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
      }
    };
    return {
      check,
      get(key) {
        check(key);
        return host.value();
      },
      submit(key, value) {
        check(key);
        return host.submit(value);
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => void 0,
      inputs: () => null,
      request: (name, args, opts) => host.request(name, args, opts)
    };
  }
  return {
    check: () => void 0,
    get: (key) => own(host.state(), key),
    submit: (key, value) => host.submit({ [key]: value }),
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input) => ({ input: input.input, revision: input.revision })),
    request: (name, args, opts) => host.request(name, args, opts)
  };
}
function asChoices(values) {
  return values.map((value) => ({ value, label: value }));
}
function strings(value) {
  return Array.isArray(value) ? value.filter((x) => typeof x === "string") : [];
}

// srcts/inputs/inputs.tsx
import { useState } from "react";

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

// srcts/inputs/inputs.tsx
import { Fragment, jsx, jsxs } from "react/jsx-runtime";
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx("span", { className: styles.label, children: label }),
    children
  ] });
}
function TextInput({
  bound,
  label,
  placeholder,
  lines = 1
}) {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "string" ? bound.value : "");
  const props = {
    className: styles.box,
    value: shown,
    placeholder,
    spellCheck: false,
    onChange: (event) => {
      typed(event.target.value);
      bound.type(event.target.value);
    },
    onKeyDown: (event) => {
      if (event.key !== "Enter" || event.nativeEvent.isComposing) return;
      if (lines > 1 && !(event.ctrlKey || event.metaKey)) return;
      bound.flush();
    },
    onBlur: () => {
      bound.flush();
    }
  };
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(Field, { label, children: lines > 1 ? /* @__PURE__ */ jsx("textarea", { rows: lines, ...props }) : /* @__PURE__ */ jsx("input", { type: "text", ...props }) }),
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
  ] });
}
function NumberInput({
  bound,
  label,
  min,
  max,
  outdated = false
}) {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "number" ? String(bound.value) : "");
  const largest = outdated ? void 0 : max;
  const read = (input) => {
    const trimmed = input.trim();
    if (trimmed === "") return {};
    const value = Number(trimmed);
    if (!Number.isInteger(value)) return { reason: `${trimmed} is not a whole number.` };
    if (min !== void 0 && value < min) {
      return { reason: `${trimmed} is below the minimum of ${String(min)}.` };
    }
    if (largest !== void 0 && value > largest) {
      return { reason: `${trimmed} is above the maximum of ${String(largest)}.` };
    }
    return { value };
  };
  const settle = () => {
    if (text === null) {
      bound.flush();
      return;
    }
    const { value } = read(text);
    bound.type(value);
    bound.flush();
    if (value === void 0) typed(null);
  };
  const checked = read(shown);
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(Field, { label, children: /* @__PURE__ */ jsx(
      "input",
      {
        type: "number",
        step: 1,
        min,
        max: largest,
        required: true,
        className: styles.box,
        value: shown,
        "aria-invalid": checked.value === void 0,
        onChange: (event) => {
          typed(event.target.value);
          bound.type(read(event.target.value).value);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) settle();
        },
        onBlur: settle
      }
    ) }),
    /* @__PURE__ */ jsx(Refusal, { error: checked.reason ?? bound.error })
  ] });
}
function Checkbox({ bound, label }) {
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsxs("label", { className: styles.check, children: [
      /* @__PURE__ */ jsx(
        "input",
        {
          type: "checkbox",
          className: styles.checkbox,
          checked: bound.value === true,
          onChange: (event) => {
            void bound.commit(event.target.checked);
          }
        }
      ),
      label
    ] }),
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
  ] });
}
function missing(value) {
  return `${value} (not available)`;
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = useState(null);
  const [seen, setSeen] = useState(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/inputs/select.tsx
import { useId, useLayoutEffect, useRef, useState as useState2 } from "react";

// srcts/inputs/place.ts
var MIN_WIDTH = 190;
var GAP = 4;
var EDGE = 8;
function listWidth(field) {
  return Math.max(field.width, MIN_WIDTH);
}
function listSpot(field, list, view) {
  const below = field.bottom + GAP;
  const above = field.top - GAP - list.height;
  const top = below + list.height > view.height - EDGE && above >= EDGE ? above : below;
  return {
    left: clamp(field.left, EDGE, view.width - EDGE - list.width),
    top: clamp(top, EDGE, view.height - EDGE - list.height)
  };
}
function clamp(value, low, high) {
  return Math.max(low, Math.min(value, high));
}

// srcts/inputs/select.tsx
import { Fragment as Fragment2, jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
var MAX_ROWS = 200;
function Select(props) {
  if (props.bound !== void 0) {
    return props.multiple === true ? /* @__PURE__ */ jsx2(BoundSeveral, { ...props }) : /* @__PURE__ */ jsx2(BoundOne, { ...props });
  }
  if (props.multiple === true) {
    const { onChange: onChange2, ...rest2 } = props;
    return /* @__PURE__ */ jsx2(
      Picker,
      {
        ...rest2,
        report: (picks) => {
          onChange2(picks);
        }
      }
    );
  }
  const { value, onChange, ...rest } = props;
  return /* @__PURE__ */ jsx2(
    Picker,
    {
      ...rest,
      multiple: false,
      value: value === void 0 ? [] : [value],
      report: ([pick]) => {
        if (pick !== void 0) onChange(pick);
      }
    }
  );
}
function BoundOne({ bound, choices, outdated = false, placeholder, label, empty, disabled }) {
  const current = typeof bound.value === "string" ? bound.value : void 0;
  const known = choices ?? [];
  const lacked = current !== void 0 && !known.some((choice) => choice.value === current);
  const invalid = lacked && choices !== void 0 && !outdated;
  const shown = choices === void 0 || current === void 0 || !lacked ? choices : [...known, { value: current, label: invalid ? missing(current) : current }];
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx2(
      Picker,
      {
        label,
        choices: shown,
        multiple: false,
        value: current === void 0 ? [] : [current],
        placeholder,
        empty,
        disabled,
        invalid,
        report: ([next]) => {
          if (next !== void 0) void bound.commit(next);
        }
      }
    ),
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
  ] });
}
function BoundSeveral({
  bound,
  choices,
  outdated = false,
  ordered = false,
  placeholder,
  label,
  empty,
  disabled,
  min
}) {
  const held = strings(bound.value);
  const known = choices ?? [];
  const marking = choices !== void 0 && !outdated;
  const lacked = held.filter((value) => !known.some((choice) => choice.value === value));
  const shown = choices === void 0 ? void 0 : [...known, ...lacked.map((value) => ({ value, label: marking ? missing(value) : value }))];
  const report = (picks, opened) => {
    const added = picks.filter((value) => !opened.includes(value));
    const removed = opened.filter((value) => !picks.includes(value));
    const now = strings(bound.get());
    const next = [...now.filter((value) => !removed.includes(value)), ...added.filter((value) => !now.includes(value))];
    void bound.commit(ordered ? inOrder(next, known) : next);
  };
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx2(
      Picker,
      {
        label,
        choices: shown,
        multiple: true,
        value: held,
        placeholder,
        empty,
        disabled,
        min,
        invalid: marking && lacked.length > 0,
        report
      }
    ),
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
  ] });
}
function inOrder(values, choices) {
  const order = choices.map((choice) => choice.value);
  return [...order.filter((value) => values.includes(value)), ...values.filter((value) => !order.includes(value))];
}
function Picker({
  label,
  choices,
  multiple,
  value,
  report,
  placeholder = multiple ? "None" : "",
  hint,
  error,
  empty = "No options",
  disabled = false,
  autofocus = false,
  min = 0,
  invalid = false
}) {
  const id = useId();
  const listId = `${id}-list`;
  const noteId = `${id}-note`;
  const anchor = anchorName(id);
  const box = useRef(null);
  const input = useRef(null);
  const list = useRef(null);
  const [open, setOpen] = useState2(false);
  const [query, setQuery] = useState2("");
  const [keyboard, setKeyboard] = useState2(false);
  const [active, setActive] = useState2(void 0);
  const [picks, setPicks] = useState2([]);
  const pending = useRef(null);
  const now = open && multiple ? picks : value;
  const known = choices ?? [];
  const byValue = new Map(known.map((choice) => [choice.value, choice]));
  const listed = multiple ? [...known, ...value.filter((picked) => !byValue.has(picked)).map((picked) => ({ value: picked, label: picked }))] : known;
  const found = matching(listed, query);
  const rows = found.slice(0, MAX_ROWS);
  const pickable = rows.filter((choice) => choice.disabled !== true);
  const marked = !keyboard ? void 0 : pickable.some((choice) => choice.value === active) ? active : pickable[0]?.value;
  const locked = multiple && min > 0 && now.length <= min;
  const names = now.map((picked) => byValue.get(picked)?.label ?? picked);
  const shown = multiple ? summary(names, choices === void 0 ? void 0 : listed.length) : names[0];
  const shownRow = open ? marked ?? active : void 0;
  useLayoutEffect(() => {
    if (shownRow !== void 0) list.current?.ownerDocument.getElementById(optionId(listId, shownRow))?.scrollIntoView({ block: "nearest" });
  }, [shownRow, listId]);
  function show(byKeyboard, typed = "") {
    if (open || disabled) return;
    if (multiple) {
      pending.current = { picks: value, opened: value };
      setPicks(value);
    }
    setOpen(true);
    setQuery(typed);
    setKeyboard(byKeyboard);
    setActive(multiple || typed !== "" ? void 0 : value[0]);
  }
  function close() {
    setOpen(false);
    setQuery("");
    setKeyboard(false);
    setActive(void 0);
    const left = pending.current;
    pending.current = null;
    if (left !== null && !sameList(left.picks, left.opened)) report(left.picks, left.opened);
  }
  function pick(choice) {
    if (choice.disabled === true) return;
    if (!multiple) {
      close();
      if (choice.value !== value[0]) report([choice.value], value);
      return;
    }
    const left = pending.current;
    if (left === null) return;
    const ticked = left.picks.includes(choice.value);
    if (ticked && min > 0 && left.picks.length <= min) return;
    const next = ticked ? left.picks.filter((picked) => picked !== choice.value) : [...left.picks, choice.value];
    pending.current = { picks: next, opened: left.opened };
    setPicks(next);
    setActive(choice.value);
  }
  function move(by) {
    if (!keyboard) {
      setKeyboard(true);
      return;
    }
    if (pickable.length === 0) return;
    const at = pickable.findIndex((choice) => choice.value === marked);
    setActive(pickable[(at + by + pickable.length) % pickable.length]?.value);
  }
  function onKeyDown(event) {
    if (event.nativeEvent.isComposing) return;
    const row = rows.find((choice) => choice.value === marked);
    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      event.preventDefault();
      if (open) move(event.key === "ArrowDown" ? 1 : -1);
      else show(true);
    } else if (event.key === "Enter") {
      event.preventDefault();
      if (!open) show(true);
      else if (!keyboard) setKeyboard(true);
      else if (row !== void 0) pick(row);
    } else if (event.key === " " && (!open || multiple && keyboard && query === "" && row !== void 0)) {
      event.preventDefault();
      if (!open) show(true);
      else if (row !== void 0) pick(row);
    } else if (event.key === "Escape" && open) {
      event.preventDefault();
      close();
    } else if (!open && event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey) {
      event.preventDefault();
      show(true, event.key);
    }
  }
  const note = error ?? hint;
  return /* @__PURE__ */ jsxs2("div", { className: styles.field, children: [
    /* @__PURE__ */ jsx2("label", { htmlFor: id, className: styles.label, children: label }),
    /* @__PURE__ */ jsxs2("div", { ref: box, className: "bc:relative bc:grid", style: { anchorName: anchor }, children: [
      /* @__PURE__ */ jsx2(
        "input",
        {
          ref: input,
          id,
          type: "text",
          role: "combobox",
          autoComplete: "off",
          spellCheck: false,
          "aria-autocomplete": "list",
          "aria-expanded": open,
          "aria-controls": listId,
          "aria-activedescendant": open && marked !== void 0 ? optionId(listId, marked) : void 0,
          "aria-invalid": invalid || error !== void 0 ? true : void 0,
          "aria-describedby": note === void 0 ? void 0 : noteId,
          "data-autofocus": autofocus || void 0,
          disabled,
          className: `${styles.box} ${FIELD} bc:pr-8 bc:text-ellipsis`,
          value: open ? query : shown ?? "",
          placeholder: open ? shown ?? placeholder : placeholder,
          onChange: (event) => {
            if (!open) {
              show(true, event.target.value);
              return;
            }
            setQuery(event.target.value);
            setActive(void 0);
            setKeyboard(true);
          },
          onClick: () => {
            show(false);
          },
          onKeyDown,
          onBlur: close
        }
      ),
      /* @__PURE__ */ jsx2(
        "button",
        {
          type: "button",
          tabIndex: -1,
          disabled,
          "aria-label": open ? `Close the choices of ${label}` : `Show the choices of ${label}`,
          className: "bc:absolute bc:inset-y-0 bc:right-0 bc:flex bc:w-8 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:disabled:cursor-default",
          onMouseDown: (event) => {
            event.preventDefault();
          },
          onClick: () => {
            if (open) close();
            else {
              show(false);
              input.current?.focus();
            }
          },
          children: /* @__PURE__ */ jsx2(Chevron, { open })
        }
      )
    ] }),
    open ? /* @__PURE__ */ jsxs2(List, { ref: list, anchor: box, name: anchor, children: [
      /* @__PURE__ */ jsx2("ul", { id: listId, role: "listbox", "aria-label": label, "aria-multiselectable": multiple || void 0, className: LISTBOX, children: byGroup(rows).map(([group, members]) => /* @__PURE__ */ jsx2(Group, { id: group === void 0 ? void 0 : groupId(listId, group), title: group, children: members.map((choice) => {
        const picked = now.includes(choice.value);
        return /* @__PURE__ */ jsx2(
          Row,
          {
            choice,
            id: optionId(listId, choice.value),
            marked: choice.value === marked,
            picked,
            ticks: multiple,
            locked: picked && locked,
            onPick: pick
          },
          choice.value
        );
      }) }, group ?? "")) }),
      /* @__PURE__ */ jsx2(Note, { choices, empty, found: found.length, rows: rows.length })
    ] }) : null,
    note === void 0 ? null : /* @__PURE__ */ jsx2("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}
function List({
  ref,
  anchor,
  name,
  children
}) {
  useTopLayer(ref, anchor);
  return /* @__PURE__ */ jsx2(
    "div",
    {
      ref,
      popover: "manual",
      className: LIST,
      style: { positionAnchor: name },
      onMouseDown: (event) => {
        event.preventDefault();
      },
      children
    }
  );
}
function useTopLayer(list, anchor) {
  useLayoutEffect(() => {
    const el = list.current;
    const field = anchor.current;
    if (el === null || field === null) return;
    el.showPopover?.();
    const stop = anchored(el) ? void 0 : follow2(el, field);
    return () => {
      stop?.();
      try {
        el.hidePopover?.();
      } catch {
      }
    };
  }, [list, anchor]);
}
function anchored(el) {
  return el.ownerDocument.defaultView?.CSS.supports("position-area", "block-end") === true;
}
function follow2(el, field) {
  const doc = el.ownerDocument;
  const win = doc.defaultView ?? window;
  const place = () => {
    const box = field.getBoundingClientRect();
    el.style.minWidth = `${String(listWidth(box))}px`;
    const spot = listSpot(box, el.getBoundingClientRect(), { width: win.innerWidth, height: win.innerHeight });
    el.style.left = `${String(Math.round(spot.left))}px`;
    el.style.top = `${String(Math.round(spot.top))}px`;
  };
  place();
  const resizes = new win.ResizeObserver(place);
  resizes.observe(el);
  win.addEventListener("resize", place);
  doc.addEventListener("scroll", place, true);
  return () => {
    resizes.disconnect();
    win.removeEventListener("resize", place);
    doc.removeEventListener("scroll", place, true);
  };
}
function Group({ id, title, children }) {
  if (id === void 0 || title === void 0) return /* @__PURE__ */ jsx2(Fragment2, { children });
  return /* @__PURE__ */ jsxs2("li", { role: "presentation", children: [
    /* @__PURE__ */ jsx2("div", { id, className: HEADING, children: title }),
    /* @__PURE__ */ jsx2("ul", { role: "group", "aria-labelledby": id, className: "bc:m-0 bc:list-none bc:p-0", children })
  ] });
}
function Row({
  choice,
  id,
  marked,
  picked,
  ticks = false,
  locked = false,
  onPick
}) {
  const off = choice.disabled === true || locked;
  return /* @__PURE__ */ jsxs2(
    "li",
    {
      id,
      role: "option",
      "data-value": choice.value,
      "aria-selected": picked,
      "aria-disabled": off || void 0,
      className: `${ROW} ${marked ? "bc:bg-bg-subtle bc:outline bc:outline-1 bc:outline-border-accent" : ""} ${choice.disabled === true ? "bc:cursor-default bc:opacity-50" : locked ? "bc:cursor-default" : "bc:cursor-pointer bc:hover:bg-bg-subtle"}`,
      onClick: () => {
        onPick(choice);
      },
      children: [
        ticks ? /* @__PURE__ */ jsx2("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: picked ? /* @__PURE__ */ jsx2(Tick, {}) : null }) : null,
        /* @__PURE__ */ jsx2("span", { "data-label": "", className: `bc:min-w-0 bc:max-w-full bc:shrink-0 bc:truncate ${!ticks && picked ? "bc:font-semibold" : ""}`, children: choice.label }),
        choice.detail === void 0 || choice.detail === "" ? null : /* @__PURE__ */ jsx2("span", { className: "bc:min-w-0 bc:flex-1 bc:truncate bc:text-[13px] bc:text-text-muted", children: choice.detail }),
        choice.badge === void 0 ? null : /* @__PURE__ */ jsx2("span", { className: "bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted", children: choice.badge })
      ]
    }
  );
}
function Note({
  choices,
  empty,
  found,
  rows
}) {
  const text = choices === void 0 ? "Loading\u2026" : choices.length === 0 && found === 0 ? empty : found === 0 ? "No matches" : found > rows ? `${count(found - rows)} more. Type to narrow the list.` : void 0;
  return text === void 0 ? null : /* @__PURE__ */ jsx2("p", { className: NOTE, children: text });
}
function Chevron({ open }) {
  return /* @__PURE__ */ jsx2(
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
      className: `bc:shrink-0 bc:text-text-muted bc:transition-transform ${open ? "bc:rotate-180" : ""}`,
      children: /* @__PURE__ */ jsx2("polyline", { points: "3 4.5 6 7.5 9 4.5" })
    }
  );
}
function Tick() {
  return /* @__PURE__ */ jsx2(
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
      children: /* @__PURE__ */ jsx2("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function matching(choices, query) {
  const needle = query.trim().toLowerCase();
  if (needle === "") return [...choices];
  return choices.filter(
    (choice) => [choice.label, choice.detail ?? "", choice.value, choice.group ?? "", choice.badge ?? "", ...choice.keywords ?? []].some(
      (text) => text.toLowerCase().includes(needle)
    )
  );
}
function byGroup(choices) {
  const groups = /* @__PURE__ */ new Map();
  for (const choice of choices) groups.set(choice.group, [...groups.get(choice.group) ?? [], choice]);
  const loose = groups.get(void 0);
  groups.delete(void 0);
  return [...loose === void 0 ? [] : [[void 0, loose]], ...groups];
}
function summary(names, total) {
  if (names.length === 0) return void 0;
  if (names.length <= 2 || total === void 0) return names.join(", ");
  return `${count(names.length)} of ${count(total)}: ${names.join(", ")}`;
}
function count(n) {
  return n.toLocaleString("en-US");
}
function sameList(a, b) {
  return a.length === b.length && a.every((value, i) => value === b[i]);
}
function anchorName(id) {
  return `--bc-select-${safe(id)}`;
}
function optionId(list, value) {
  return `${list}-option-${safe(value)}`;
}
function groupId(list, key) {
  return `${list}-group-${safe(key)}`;
}
function safe(text) {
  return text.replace(/[^A-Za-z0-9_-]/g, (c) => `_${c.charCodeAt(0).toString(16)}`);
}
var FIELD = "bc:disabled:cursor-default bc:disabled:opacity-50";
var LIST = "bc:select-list bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:p-1 bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:dark:text-gray-100";
var LISTBOX = "bc:m-0 bc:min-h-0 bc:list-none bc:overflow-y-auto bc:p-0";
var HEADING = "bc:sticky bc:top-0 bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:flex bc:min-h-8 bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm";
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/controls/controls.tsx
import { Fragment as Fragment4, jsx as jsx3, jsxs as jsxs3 } from "react/jsx-runtime";
function controls(host, signal) {
  const el = document.createElement("div");
  el.className = styles.stack;
  const root = createRoot(el);
  const items = [];
  const bindings = [];
  const followed = [];
  let error = null;
  let ended = false;
  const redraw = () => {
    if (ended) return;
    const drawn = items.map((item) => /* @__PURE__ */ jsx3(Fragment3, { children: item.draw() }, item.id));
    flushSync(() => {
      root.render(
        /* @__PURE__ */ jsxs3(Fragment4, { children: [
          drawn,
          error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error })
        ] })
      );
    });
  };
  function put(draw) {
    items.push({ id: items.length, draw });
    redraw();
  }
  function add(node) {
    put(() => /* @__PURE__ */ jsx3(Slot, { node }));
  }
  function input(key, draw) {
    let value;
    let refusal = null;
    let made = false;
    const binding = bind(
      host,
      key,
      (shown) => {
        value = shown;
        if (made) redraw();
      },
      {
        settled: (refused) => {
          refusal = refused === null ? null : errorMessage(refused);
          redraw();
        }
      }
    );
    bindings.push(binding);
    made = true;
    put(
      () => draw({
        value,
        error: refusal,
        get: () => binding.get(),
        commit: (next) => binding.commit(next),
        type: (next) => {
          binding.type(next);
        },
        flush: () => {
          binding.flush();
        }
      })
    );
  }
  function alert(next) {
    error = next === null ? null : errorMessage(next);
    redraw();
  }
  function get(key) {
    if (!("option" in host)) return own(host.state(), key);
    if (key !== host.option) {
      throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
    }
    return host.value();
  }
  function bind2(key, show) {
    const binding = bind(host, key, show, { settled: alert });
    bindings.push(binding);
    return binding;
  }
  function follow3(name, options) {
    const request = follow(host, name, options);
    followed.push(request);
    return request;
  }
  function text(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx3(TextInput, { bound, ...options }));
  }
  function number(key, options) {
    let max;
    let outdated = false;
    input(key, (bound) => /* @__PURE__ */ jsx3(NumberInput, { bound, label: options.label, min: options.min, max, outdated }));
    return {
      setMax(next) {
        max = next;
        redraw();
      },
      setOutdated(next) {
        outdated = next;
        redraw();
      }
    };
  }
  function select(key, options) {
    let choices = options.choices;
    let outdated = false;
    input(key, (bound) => /* @__PURE__ */ jsx3(
      Select,
      {
        bound,
        label: options.label,
        placeholder: options.placeholder,
        choices,
        multiple: options.multiple,
        ordered: options.ordered,
        empty: options.empty,
        outdated
      }
    ));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      },
      setOutdated(next) {
        outdated = next;
        redraw();
      }
    };
  }
  function checkbox(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx3(Checkbox, { bound, label: options.label }));
  }
  function cleanup() {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }
  signal.addEventListener("abort", cleanup, { once: true });
  return { el, add, get, bind: bind2, text, number, select, checkbox, follow: follow3, alert, cleanup };
}
function Slot({ node }) {
  const ref = useRef2(null);
  useLayoutEffect2(() => {
    ref.current?.replaceChildren(node);
  }, [node]);
  return /* @__PURE__ */ jsx3("div", { ref, className: "bc:contents" });
}
export {
  PAUSE,
  asChoices,
  controls,
  strings,
  styles
};
