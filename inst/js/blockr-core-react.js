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
    options: host.options(),
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
function useValue(host, key) {
  const name = key ?? ("option" in host ? host.option : "");
  const [value, setValue] = useState(() => held(host, name));
  const [error, setError] = useState(null);
  const binding = useRef(null);
  useLayoutEffect(() => {
    const bound = bind(host, name, setValue, {
      settled: (refused) => {
        setError(refused === null ? null : errorMessage(refused));
      }
    });
    binding.current = bound;
    return () => {
      binding.current = null;
      bound.release();
    };
  }, [host, name]);
  return useMemo(
    () => ({
      value,
      error,
      get: () => binding.current?.get() ?? held(host, name),
      commit: (next) => binding.current?.commit(next) ?? Promise.resolve(false),
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
function held(host, key) {
  return "option" in host ? host.value() : own(host.state(), key);
}
function useBoard(core) {
  const subscribe = useCallback(
    (notify) => core.store.subscribe(() => {
      notify();
    }),
    [core]
  );
  return useSyncExternalStore(subscribe, () => core.store.board());
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
function useNotified(core, block) {
  const subscribe = useCallback(
    (notify) => core.notifications.subscribe(() => {
      notify();
    }),
    [core]
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  return useMemo(
    () => block === null ? all : all.filter((notification) => notification.about !== void 0 && "block" in notification.about && notification.about.block === block),
    [all, block]
  );
}

// srcts/react/module.tsx
import { Component } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { jsx } from "react/jsx-runtime";
function reactModule(Draw, options = {}) {
  return {
    ...options.options === void 0 ? {} : { options: options.options },
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
function outputRenderer(Draw, options = {}) {
  function Renderer({ host }) {
    const now = useOutput(host);
    return /* @__PURE__ */ jsx("div", { className: "bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default", "aria-busy": now.loading, children: /* @__PURE__ */ jsx(Boundary, { host, output: now.output, children: /* @__PURE__ */ jsx("div", { className: `bc:flex bc:min-h-0 bc:flex-col ${now.loading ? "bc:opacity-60" : ""}`, children: /* @__PURE__ */ jsx(
      Draw,
      {
        output: now.output,
        body: now.body,
        params: now.params,
        options: now.options,
        loading: now.loading,
        error: now.error,
        host
      }
    ) }) }) });
  }
  return reactModule(Renderer, options);
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

// srcts/inputs/inputs.tsx
import { useState as useState2 } from "react";

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
import { Fragment, jsx as jsx2, jsxs } from "react/jsx-runtime";
function Stack({ children }) {
  return /* @__PURE__ */ jsx2("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx2("span", { className: styles.label, children: label }),
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
    /* @__PURE__ */ jsx2(Field, { label, children: lines > 1 ? /* @__PURE__ */ jsx2("textarea", { rows: lines, ...props }) : /* @__PURE__ */ jsx2("input", { type: "text", ...props }) }),
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
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
    /* @__PURE__ */ jsx2(Field, { label, children: /* @__PURE__ */ jsx2(
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
    /* @__PURE__ */ jsx2(Refusal, { error: checked.reason ?? bound.error })
  ] });
}
function Checkbox({ bound, label }) {
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsxs("label", { className: styles.check, children: [
      /* @__PURE__ */ jsx2(
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
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
  ] });
}
function missing(value) {
  return `${value} (not available)`;
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx2("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = useState2(null);
  const [seen, setSeen] = useState2(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/inputs/select.tsx
import { useId, useLayoutEffect as useLayoutEffect2, useRef as useRef2, useState as useState3 } from "react";
import { strings } from "@blockr/core/binding";

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
import { Fragment as Fragment2, jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
var MAX_ROWS = 200;
function Select(props) {
  if (props.bound !== void 0) {
    return props.multiple === true ? /* @__PURE__ */ jsx3(BoundSeveral, { ...props }) : /* @__PURE__ */ jsx3(BoundOne, { ...props });
  }
  if (props.multiple === true) {
    const { onChange: onChange2, ...rest2 } = props;
    return /* @__PURE__ */ jsx3(
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
  return /* @__PURE__ */ jsx3(
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
    /* @__PURE__ */ jsx3(
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
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
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
  const held2 = strings(bound.value);
  const known = choices ?? [];
  const marking = choices !== void 0 && !outdated;
  const lacked = held2.filter((value) => !known.some((choice) => choice.value === value));
  const shown = choices === void 0 ? void 0 : [...known, ...lacked.map((value) => ({ value, label: marking ? missing(value) : value }))];
  const report = (picks, opened) => {
    const added = picks.filter((value) => !opened.includes(value));
    const removed = opened.filter((value) => !picks.includes(value));
    const now = strings(bound.get());
    const next = [...now.filter((value) => !removed.includes(value)), ...added.filter((value) => !now.includes(value))];
    void bound.commit(ordered ? inOrder(next, known) : next);
  };
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx3(
      Picker,
      {
        label,
        choices: shown,
        multiple: true,
        value: held2,
        placeholder,
        empty,
        disabled,
        min,
        invalid: marking && lacked.length > 0,
        report
      }
    ),
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
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
  const box = useRef2(null);
  const input = useRef2(null);
  const list = useRef2(null);
  const [open, setOpen] = useState3(false);
  const [query, setQuery] = useState3("");
  const [keyboard, setKeyboard] = useState3(false);
  const [active, setActive] = useState3(void 0);
  const [picks, setPicks] = useState3([]);
  const pending = useRef2(null);
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
  useLayoutEffect2(() => {
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
    /* @__PURE__ */ jsx3("label", { htmlFor: id, className: styles.label, children: label }),
    /* @__PURE__ */ jsxs2("div", { ref: box, className: "bc:relative bc:grid", style: { anchorName: anchor }, children: [
      /* @__PURE__ */ jsx3(
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
      /* @__PURE__ */ jsx3(
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
          children: /* @__PURE__ */ jsx3(Chevron, { open })
        }
      )
    ] }),
    open ? /* @__PURE__ */ jsxs2(List, { ref: list, anchor: box, name: anchor, children: [
      /* @__PURE__ */ jsx3("ul", { id: listId, role: "listbox", "aria-label": label, "aria-multiselectable": multiple || void 0, className: LISTBOX, children: byGroup(rows).map(([group, members]) => /* @__PURE__ */ jsx3(Group, { id: group === void 0 ? void 0 : groupId(listId, group), title: group, children: members.map((choice) => {
        const picked = now.includes(choice.value);
        return /* @__PURE__ */ jsx3(
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
      /* @__PURE__ */ jsx3(Note, { choices, empty, found: found.length, rows: rows.length })
    ] }) : null,
    note === void 0 ? null : /* @__PURE__ */ jsx3("p", { id: noteId, className: error === void 0 ? styles.hint : styles.alert, children: note })
  ] });
}
function List({
  ref,
  anchor,
  name,
  children
}) {
  useTopLayer(ref, anchor);
  return /* @__PURE__ */ jsx3(
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
  useLayoutEffect2(() => {
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
  if (id === void 0 || title === void 0) return /* @__PURE__ */ jsx3(Fragment2, { children });
  return /* @__PURE__ */ jsxs2("li", { role: "presentation", children: [
    /* @__PURE__ */ jsx3("div", { id, className: HEADING, children: title }),
    /* @__PURE__ */ jsx3("ul", { role: "group", "aria-labelledby": id, className: "bc:m-0 bc:list-none bc:p-0", children })
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
        ticks ? /* @__PURE__ */ jsx3("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: picked ? /* @__PURE__ */ jsx3(Tick, {}) : null }) : null,
        /* @__PURE__ */ jsx3("span", { "data-label": "", className: `bc:min-w-0 bc:max-w-full bc:shrink-0 bc:truncate ${!ticks && picked ? "bc:font-semibold" : ""}`, children: choice.label }),
        choice.detail === void 0 || choice.detail === "" ? null : /* @__PURE__ */ jsx3("span", { className: "bc:min-w-0 bc:flex-1 bc:truncate bc:text-[13px] bc:text-text-muted", children: choice.detail }),
        choice.badge === void 0 ? null : /* @__PURE__ */ jsx3("span", { className: "bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted", children: choice.badge })
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
  return text === void 0 ? null : /* @__PURE__ */ jsx3("p", { className: NOTE, children: text });
}
function Chevron({ open }) {
  return /* @__PURE__ */ jsx3(
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
      children: /* @__PURE__ */ jsx3("polyline", { points: "3 4.5 6 7.5 9 4.5" })
    }
  );
}
function Tick() {
  return /* @__PURE__ */ jsx3(
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
      children: /* @__PURE__ */ jsx3("path", { d: "M3.5 8.5l3 3 6-7" })
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
var LISTBOX = "bc:m-0 bc:min-h-0 bc:flex-1 bc:list-none bc:overflow-y-auto bc:p-0";
var HEADING = "bc:sticky bc:top-0 bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:flex bc:min-h-8 bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm";
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
export {
  Checkbox,
  Field,
  NumberInput,
  Select,
  Stack,
  TextInput,
  outputRenderer,
  reactModule,
  useBoard,
  useNotified,
  useOutput,
  useRequest,
  useStatus,
  useValue
};
