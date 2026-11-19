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
      },
      notifier: () => void 0
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

// srcts/inputs/combobox.tsx
import { useContext, useRef as useRef2, useState as useState3 } from "react";
import { strings } from "@blockr/core/binding";
import {
  Button,
  Collection,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  DropIndicator,
  FieldError,
  GridList,
  GridListItem,
  Group,
  Header,
  Input,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  Popover,
  Text,
  UNSAFE_PortalProvider,
  useDragAndDrop
} from "./vendor-230f8b0b.js";
import { Fragment as Fragment2, jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function ComboBox(props) {
  const { bound, ordered = false, reorderable = true, ...rest } = props;
  const drawn = { ...rest, reorderable: reorderable && !ordered };
  if (bound === void 0) return /* @__PURE__ */ jsx3(Drawn, { ...drawn });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx3(BoundSeveral, { ...drawn, bound, ordered }) : /* @__PURE__ */ jsx3(BoundOne, { ...drawn, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx3(
      Drawn,
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
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
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
  const held2 = strings(bound.value);
  const [picks, setPicks] = useState3(null);
  const pending = useRef2(null);
  const submit = (next, opened) => {
    if (sameKeys(next, opened)) return;
    const now = strings(bound.get());
    const removed = opened.filter((value2) => !next.includes(value2));
    const added = next.filter((value2) => !opened.includes(value2) && !now.includes(value2));
    const kept = next.filter((value2) => opened.includes(value2) && now.includes(value2));
    const order = [...kept];
    const held3 = now.filter((value2) => !removed.includes(value2)).map((value2) => kept.includes(value2) ? order.shift() ?? value2 : value2);
    const value = ordered && items !== void 0 ? inOrder([...held3, ...added], ids(items)) : [...held3, ...added];
    if (!sameKeys(value, now)) void bound.commit(value);
  };
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx3(
      Drawn,
      {
        ...props,
        selectionMode: "multiple",
        outdated,
        marking: !outdated && known(props),
        value: picks ?? held2,
        onOpenChange: (isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held2, opened: held2 };
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
            submit(next, held2);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }
      }
    ),
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
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
function inOrder(values, order) {
  return [...order.filter((id) => values.includes(id)), ...values.filter((value) => !order.includes(value))];
}
function Drawn({
  label,
  description,
  errorMessage: errorMessage2,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  reorderable = true,
  className,
  allowsEmptyCollection = true,
  ...props
}) {
  const multiple = props.selectionMode === "multiple";
  const editable = props.isDisabled !== true && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  const unknown = given === void 0 && typeof children === "function";
  const none = given === void 0 ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading\u2026" : none ? renderEmptyState?.() ?? "No options" : "No matches";
  const field = useRef2(null);
  const layer = useRef2(null);
  const value = useSameValue(props.value);
  return /* @__PURE__ */ jsxs2(
    AriaComboBox,
    {
      ...props,
      value,
      defaultItems: unknown ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps(className, (given2) => `bc:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx3(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx3(
          Field2,
          {
            ref: field,
            placeholder,
            multiple,
            marking,
            clearable: props.isRequired !== true && editable,
            reorderable: multiple && reorderable,
            isDisabled: !editable,
            onClear,
            label,
            autoFocus: props.autoFocus
          }
        ),
        /* @__PURE__ */ jsx3(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx3(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx3("div", { ref: layer, className: LAYER }),
        /* @__PURE__ */ jsx3(UNSAFE_PortalProvider, { getContainer: () => container(field.current, layer.current), children: /* @__PURE__ */ jsx3(Popover, { offset: 4, className: LIST, children: /* @__PURE__ */ jsx3(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx3("p", { className: NOTE, children: empty }), className: LISTBOX, children }) }) })
      ]
    }
  );
}
function useSameValue(value) {
  const [drawn, setDrawn] = useState3(value);
  const same = drawn === value || Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value);
  if (!same) setDrawn(value);
  return same ? drawn : value;
}
function sameKeys(a, b) {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}
function container(field, layer) {
  return field?.closest("dialog, [popover]") ? layer : field?.ownerDocument.body ?? null;
}
function hasChildren(children) {
  return Array.isArray(children) ? children.some((child) => hasChildren(child)) : children !== null && children !== void 0 && children !== false;
}
function Field2({
  ref,
  placeholder,
  multiple,
  marking,
  clearable,
  reorderable,
  isDisabled,
  onClear,
  label,
  autoFocus
}) {
  const state = useContext(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? value : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? value : null;
  const item = state?.selectedItems[0] ?? null;
  const lackedOne = one !== null && item === null;
  const known2 = new Map((state?.selectedItems ?? []).map((item2) => [item2.key, item2.textValue]));
  const lacked = keys.some((key) => !known2.has(key));
  const invalid = marking && (lackedOne || lacked);
  return /* @__PURE__ */ jsxs2(Group, { ref, isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs2("div", { className: VALUES, children: [
      keys.length === 0 ? null : /* @__PURE__ */ jsx3(
        Chips,
        {
          keys,
          known: known2,
          marking,
          reorderable,
          isDisabled,
          label,
          onChange: (next) => {
            state?.setValue(next);
          }
        }
      ),
      /* @__PURE__ */ jsx3(
        Input,
        {
          value: multiple || state === null || state.isFocused || state.isOpen ? void 0 : item?.textValue ?? "",
          placeholder: lackedOne ? marking ? missing(String(one)) : String(one) : keys.length > 0 ? void 0 : placeholder,
          "aria-invalid": invalid || void 0,
          "data-autofocus": autoFocus === true || void 0,
          className: INPUT,
          onClick: () => {
            if (state !== null && !state.isOpen) state.open(null, "manual");
          },
          onKeyDown: (event) => {
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }
        }
      )
    ] }),
    clearable && (keys.length > 0 || one !== null) ? (
      // A button of the field's own, not React Aria's, which the select
      // would take for its own and open the list with. It keeps the focus
      // in the text field, as a press in the list does.
      /* @__PURE__ */ jsx3(
        "button",
        {
          type: "button",
          tabIndex: -1,
          "aria-label": label === void 0 ? "Clear" : `Clear ${label}`,
          title: multiple ? "Clear every pick" : "Clear the pick",
          className: CLEAR,
          onMouseDown: (event) => {
            event.preventDefault();
          },
          onClick: () => {
            if (onClear !== void 0) onClear();
            else state?.setValue(multiple ? [] : null);
          },
          children: /* @__PURE__ */ jsx3(Cross, {})
        }
      )
    ) : null,
    /* @__PURE__ */ jsx3(Button, { className: OPENER, children: /* @__PURE__ */ jsx3(Chevron, {}) })
  ] });
}
function Chips({
  keys,
  known: known2,
  marking,
  reorderable,
  isDisabled,
  label,
  onChange
}) {
  const { dragAndDropHooks } = useDragAndDrop({
    isDisabled,
    getItems: (dragged) => [...dragged].map((key) => ({ [PICK]: String(key) })),
    onReorder: (event) => {
      const next = moved(keys, event.keys, event.target);
      if (!sameKeys(next, keys)) onChange(next);
    },
    renderDropIndicator: (target) => /* @__PURE__ */ jsx3(DropIndicator, { target, className: DROP })
  });
  const remove = (gone) => {
    onChange(keys.filter((key) => key !== gone));
  };
  return /* @__PURE__ */ jsx3(
    "div",
    {
      className: "bc:flex bc:min-w-0 bc:max-w-full",
      onKeyDown: (event) => {
        if (event.key !== "Delete" && event.key !== "Backspace") return;
        const chip = event.target instanceof HTMLElement && event.target.getAttribute("role") === "row" ? event.target.dataset.key : void 0;
        const gone = keys.find((key) => String(key) === chip);
        if (gone === void 0) return;
        event.preventDefault();
        remove(gone);
      },
      children: /* @__PURE__ */ jsx3(
        GridList,
        {
          "aria-label": label === void 0 ? "Picks" : `Picks of ${label}`,
          layout: "grid",
          dragAndDropHooks: reorderable ? dragAndDropHooks : void 0,
          className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1",
          children: keys.map((key) => {
            const text = known2.get(key) ?? String(key);
            const gone = marking && !known2.has(key);
            return /* @__PURE__ */ jsxs2(GridListItem, { id: key, textValue: text, "data-missing": gone || void 0, className: CHIP, children: [
              reorderable ? /* @__PURE__ */ jsx3(Button, { slot: "drag", className: GRIP, children: /* @__PURE__ */ jsx3(Grip, {}) }) : null,
              /* @__PURE__ */ jsx3("span", { className: "bc:truncate", children: gone ? missing(text) : text }),
              /* @__PURE__ */ jsx3(
                Button,
                {
                  "aria-label": `Remove ${text}`,
                  className: CHIP_REMOVE,
                  onPress: () => {
                    remove(key);
                  },
                  children: /* @__PURE__ */ jsx3(Cross, {})
                }
              )
            ] }, key);
          })
        }
      )
    }
  );
}
var PICK = "application/x-blockr-pick";
function moved(keys, dragged, target) {
  const rest = keys.filter((key) => !dragged.has(key));
  const at = rest.indexOf(target.key);
  if (at < 0) return [...keys];
  const place = target.dropPosition === "after" ? at + 1 : at;
  return [...rest.slice(0, place), ...keys.filter((key) => dragged.has(key)), ...rest.slice(place)];
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx3(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs2(Fragment2, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx3("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx3(Tick, {}) : null }) : null,
    /* @__PURE__ */ jsx3(
      "span",
      {
        className: `bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${selectionMode === "single" && isSelected ? "bc:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs2(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx3(Header, { className: HEADING, children: title }),
    /* @__PURE__ */ jsx3(Collection, { items, children })
  ] });
}
function Chevron() {
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
      className: "bc:shrink-0 bc:text-text-muted bc:transition-transform bc:group-data-open:rotate-180",
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
function Cross() {
  return /* @__PURE__ */ jsx3(
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
      children: /* @__PURE__ */ jsx3("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" })
    }
  );
}
function Grip() {
  return /* @__PURE__ */ jsxs2("svg", { width: "8", height: "12", viewBox: "0 0 8 12", fill: "currentColor", "aria-hidden": "true", children: [
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "2", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "2", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "10", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "10", r: "1" })
  ] });
}
var FIELD = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var VALUES = "bc:flex bc:min-w-0 bc:flex-1 bc:flex-wrap bc:items-center bc:gap-1";
var INPUT = "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
var CLEAR = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:hover:bg-bg-subtle bc:hover:text-text-default";
var OPENER = "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
var CHIP = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-0.5 bc:pl-1.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-allows-dragging:cursor-grab bc:data-allows-dragging:pl-0.5 bc:data-dragging:opacity-50 bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
var CHIP_REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
var GRIP = "bc:flex bc:h-4 bc:w-3 bc:shrink-0 bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var DROP = "bc:-mx-[3px] bc:w-0.5 bc:shrink-0 bc:rounded-full bc:data-drop-target:bg-border-accent";
var LAYER = "bc:pointer-events-none bc:fixed bc:inset-0 bc:z-[100000]";
var LIST = "bc:pointer-events-auto bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:p-1 bc:outline-none";
var HEADING = "bc:sticky bc:-top-1 bc:z-10 bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:flex bc:min-h-8 bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/index.ts
import { Text as Text2 } from "./vendor-230f8b0b.js";
export {
  Checkbox,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  Field,
  NumberInput,
  Stack,
  Text2 as Text,
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
