// srcts/react/hooks.ts
import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";

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

// srcts/binding/binding.ts
var PAUSE = 500;
function bind(host, key, next, last, given) {
  const [change, show, options] = typeof key === "function" ? [next, last, given ?? {}] : [void 0, next, last ?? {}];
  const source = createSource(host);
  const held3 = source.value(key, change);
  const submitting = options.notifier === void 0 ? {} : { notifier: options.notifier };
  let shown;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || timer !== void 0) return;
    const value = held3.get();
    if (!again && shown !== void 0 && jsonEqual(shown.value, value)) return;
    shown = { value };
    show(value);
  };
  let unbuilt;
  const unbuild = (error) => {
    host.log.error(asError(error));
    unbuilt?.();
    unbuilt = options.notifier === void 0 ? host.notify({
      severity: "error",
      code: "change_failed",
      message: `The control failed to build the change it submits: ${errorMessage(error)}`
    }) : void 0;
  };
  const submit = async (value) => {
    let submitted;
    try {
      submitted = held3.submit(value, submitting);
    } catch (error) {
      unbuild(error);
      options.settled?.(error);
      return false;
    }
    unbuilt?.();
    unbuilt = void 0;
    try {
      if (submitted !== null) await submitted;
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
    get: () => held3.get(),
    async commit(value) {
      const took = await submit(value);
      if (!took) display(true);
      return took;
    },
    async unset() {
      const took = await submit(void 0);
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
    return {
      value(key) {
        if (typeof key === "function") {
          throw new TypeError(`The value of the board option "${host.option}" is bound by its id, not through functions.`);
        }
        if (key !== host.option) {
          throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
        }
        return {
          get: () => host.value(),
          submit(value, opts) {
            if (jsonEqual(host.value(), value)) return null;
            if (value === void 0) {
              return Promise.reject(new Error(`The board option "${host.option}" always holds a value, so none of it is unset.`));
            }
            return host.submit(value, opts);
          }
        };
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => void 0,
      inputs: () => null,
      request: (name, args, opts) => host.request(name, args, opts)
    };
  }
  return {
    value(key, change) {
      const read = typeof key === "function" ? key : (state) => own(state, key);
      const build = typeof key === "function" ? change : (value) => ({ [key]: value });
      if (typeof build !== "function") {
        throw new TypeError("A binding made with a function that reads its value takes a second, which builds the change it submits.");
      }
      return {
        get: () => read(host.state()),
        submit(value, opts) {
          const state = host.state();
          const next = build(value, state);
          return holds(state, next) ? null : host.submit(next, opts);
        }
      };
    },
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input) => ({ input: input.input, revision: input.revision })),
    request: (name, args, opts) => host.request(name, args, opts)
  };
}
function holds(state, change) {
  return Object.entries(change).every(([arg, value]) => jsonEqual(own(state, arg), value));
}
function strings(value) {
  return Array.isArray(value) ? value.filter((x) => typeof x === "string") : [];
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

// srcts/aria/index.ts
import {
  Autocomplete,
  Button,
  CheckboxButton,
  CheckboxField,
  CheckboxGroup,
  Collection,
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorThumb,
  ColorWheel,
  ColorWheelTrack,
  ComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  Dialog,
  DialogTrigger,
  DropZone,
  FieldError,
  FileTrigger,
  Form,
  Group,
  Header,
  Heading,
  Input,
  Keyboard,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Modal,
  ModalOverlay,
  NumberField,
  NumberFieldStateContext,
  parseColor,
  Popover,
  RadioButton,
  RadioField,
  RadioGroup,
  SearchField,
  Select,
  SelectStateContext,
  SelectValue,
  Separator,
  Slider,
  SliderFill,
  SliderOutput,
  SliderStateContext,
  SliderThumb,
  SliderTrack,
  SwitchButton,
  SwitchField,
  Tag,
  TagGroup,
  TagList,
  Text,
  TextArea,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Token,
  TokenField,
  TokenInput,
  useFilter,
  useLocale,
  Virtualizer
} from "react-aria-components";

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
import { jsx as jsx2 } from "react/jsx-runtime";
function Button2({ variant = "primary", className, ...props }) {
  return /* @__PURE__ */ jsx2(Button, { ...props, className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`) });
}
function FieldButton({ className, ...props }) {
  return /* @__PURE__ */ jsx2(Button, { ...props, className: composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`) });
}
var FIELD_BUTTON = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/checkbox.tsx
import { useRef as useRef3 } from "react";

// srcts/inputs/icons.tsx
import { jsx as jsx3, jsxs } from "react/jsx-runtime";
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
function Tick({ size = 14 }) {
  return /* @__PURE__ */ jsx3(
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
      children: /* @__PURE__ */ jsx3("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function Dash() {
  return /* @__PURE__ */ jsx3("svg", { width: "12", height: "12", viewBox: "0 0 16 16", fill: "none", stroke: "currentColor", strokeWidth: "1.5", strokeLinecap: "round", "aria-hidden": "true", children: /* @__PURE__ */ jsx3("path", { d: "M4 8h8" }) });
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
function Magnifier() {
  return /* @__PURE__ */ jsxs(
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
        /* @__PURE__ */ jsx3("circle", { cx: "7", cy: "7", r: "4.5" }),
        /* @__PURE__ */ jsx3("path", { d: "M10.5 10.5L14 14" })
      ]
    }
  );
}

// srcts/inputs/inputs.tsx
import { useState as useState2 } from "react";
import { jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function Stack({ children }) {
  return /* @__PURE__ */ jsx4("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs2("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx4("span", { className: styles.label, children: label }),
    children
  ] });
}
function NotAvailable({
  values,
  onRemove
}) {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return /* @__PURE__ */ jsxs2("div", { className: "bc:flex bc:flex-wrap bc:items-center bc:gap-x-2", children: [
    /* @__PURE__ */ jsx4("p", { role: "alert", className: styles.alert, children: `Not available: ${named}` }),
    onRemove === void 0 ? null : /* @__PURE__ */ jsx4(Button2, { variant: "quiet", "aria-label": `Remove ${named}`, onPress: onRemove, children: "Remove" })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx4("p", { role: "alert", className: styles.alert, children: error });
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

// srcts/inputs/items.ts
import { createContext, useCallback as useCallback2, useContext, useLayoutEffect as useLayoutEffect2, useMemo as useMemo2, useRef as useRef2, useState as useState3 } from "react";
var ItemsContext = createContext(null);
function useItems() {
  const entries = useRef2(/* @__PURE__ */ new Map());
  const [known2, setKnown] = useState3(null);
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
import { Fragment, jsx as jsx5, jsxs as jsxs3 } from "react/jsx-runtime";
function Checkbox({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx5(DrawnCheckbox, { ...props }) : /* @__PURE__ */ jsx5(BoundCheckbox, { ...props, bound });
}
function BoundCheckbox({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs3(Fragment, { children: [
    /* @__PURE__ */ jsx5(
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
    /* @__PURE__ */ jsx5(Refusal, { error: bound.error })
  ] });
}
function DrawnCheckbox({ children, description, errorMessage: errorMessage2, className, ...props }) {
  const ref = useRef3(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs3(CheckboxField, { ...props, ref, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsx5(CheckboxButton, { className: composeRenderProps(className, (given) => `bc:group ${CHECK} ${given ?? ""}`), children: composeRenderProps(children, (children2, { isSelected, isIndeterminate }) => /* @__PURE__ */ jsxs3(Fragment, { children: [
      /* @__PURE__ */ jsx5("span", { className: BOX, children: isIndeterminate ? /* @__PURE__ */ jsx5(Dash, {}) : isSelected ? /* @__PURE__ */ jsx5(Tick, { size: 12 }) : null }),
      children2
    ] })) }),
    /* @__PURE__ */ jsx5(Text, { slot: "description", className: `${styles.hint} bc:ml-6 bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx5(FieldError, { className: `${styles.alert} bc:ml-6`, children: errorMessage2 })
  ] });
}
function CheckboxGroup2({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx5(DrawnGroup, { ...props }) : /* @__PURE__ */ jsx5(BoundGroup, { ...props, bound, outdated, ordered });
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
  return /* @__PURE__ */ jsxs3(Fragment, { children: [
    /* @__PURE__ */ jsx5(
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
        children: /* @__PURE__ */ jsx5(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx5(
      NotAvailable,
      {
        values: gone,
        onRemove: () => {
          void bound.commit(held2(strings(bound.get()), known2));
        }
      }
    ) : null,
    /* @__PURE__ */ jsx5(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup({ label, description, errorMessage: errorMessage2, children, className, ...props }) {
  return /* @__PURE__ */ jsxs3(CheckboxGroup, { ...props, className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx5(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx5("div", { className: "bc:grid bc:gap-1.5", children }),
    /* @__PURE__ */ jsx5(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx5(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
var CHECK = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var BOX = "bc:box-border bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent bc:group-data-selected:text-text-on-accent bc:group-data-indeterminate:border-border-accent bc:group-data-indeterminate:bg-bg-accent bc:group-data-indeterminate:text-text-on-accent bc:group-data-invalid:border-border-danger bc:group-data-selected:group-data-invalid:bg-border-danger";

// srcts/inputs/colorfield.tsx
import { useContext as useContext2, useState as useState5 } from "react";

// srcts/inputs/colors.ts
import { createContext as createContext2, useState as useState4 } from "react";
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
  const [kept, setKept] = useState4(null);
  const [seen, setSeen] = useState4(value);
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
import { Fragment as Fragment2, jsx as jsx6, jsxs as jsxs4 } from "react/jsx-runtime";
function ColorField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx6(Drawn, { ...props }) : /* @__PURE__ */ jsx6(BoundField, { ...props, bound });
}
function BoundField({ bound, onChange, onBlur, ...props }) {
  const value = valueColor(bound.value);
  const [typing, setTyping] = useState5(null);
  const shown = typing === null ? value : typing.value;
  return /* @__PURE__ */ jsxs4(Fragment2, { children: [
    /* @__PURE__ */ jsx6(
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
            const alpha = shown === void 0 ? 1 : parseColor(shown).getChannelValue("alpha");
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
    /* @__PURE__ */ jsx6(Refusal, { error: bound.error })
  ] });
}
function Drawn({ label, description, errorMessage: errorMessage2, className, onChange, onInput, ...props }) {
  const ended = useContext2(Ended);
  return /* @__PURE__ */ jsxs4(
    ColorField,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        if (color !== null) ended?.(color);
      },
      className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx6(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx6(Input, { onInput, className: styles.box }),
        /* @__PURE__ */ jsx6(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx6(FieldError, { className: styles.alert, children: errorMessage2 })
      ]
    }
  );
}

// srcts/inputs/colorparts.tsx
import { useContext as useContext3 } from "react";
import { Fragment as Fragment3, jsx as jsx7, jsxs as jsxs5 } from "react/jsx-runtime";
function ColorThumb2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx7(
    ColorThumb,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Area, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Area, props, bound });
}
function ColorSlider2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Slider2, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Slider2, props, bound });
}
function ColorWheel2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Wheel, { ...props }) : /* @__PURE__ */ jsx7(BoundPart, { Part: Wheel, props, bound });
}
function BoundPart({
  Part,
  props,
  bound
}) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs5(Fragment3, { children: [
    /* @__PURE__ */ jsx7(
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
    /* @__PURE__ */ jsx7(Refusal, { error: bound.error })
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
  return /* @__PURE__ */ jsx7(
    ColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx7(ColorThumb2, {})
    }
  );
}
function Slider2({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs5(
    ColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx7(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx7(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx7(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx7(ColorThumb2, {})
          }
        )
      ]
    }
  );
}
function Wheel({ onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs5(ColorWheel, { ...props, onChangeEnd: useEnd(onChangeEnd), outerRadius: 100, innerRadius: 74, children: [
    /* @__PURE__ */ jsx7(
      ColorWheelTrack,
      {
        className: WHEEL_TRACK,
        style: ({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
        })
      }
    ),
    /* @__PURE__ */ jsx7(ColorThumb2, {})
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

// srcts/overlays/dialog.tsx
import { jsx as jsx8 } from "react/jsx-runtime";
function Modal2({ className, ...props }) {
  return /* @__PURE__ */ jsx8(ModalOverlay, { ...props, className: OVERLAY, children: /* @__PURE__ */ jsx8("div", { className: CENTER, children: /* @__PURE__ */ jsx8(Modal, { ...props, className: composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`) }) }) });
}
function Dialog2({ className, ...props }) {
  return /* @__PURE__ */ jsx8(Dialog, { ...props, className: `${DIALOG} ${className ?? ""}` });
}
var OVERLAY = "bc:absolute bc:top-0 bc:left-0 bc:isolate bc:z-50 bc:h-(--page-height) bc:w-full bc:bg-black/40";
var CENTER = "bc:sticky bc:top-0 bc:left-0 bc:box-border bc:flex bc:h-(--visual-viewport-height) bc:w-full bc:items-center bc:justify-center bc:p-4";
var MODAL = "bc:box-border bc:max-h-[calc(var(--visual-viewport-height)*.9)] bc:w-full bc:max-w-xl bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-xl bc:outline-none bc:dark:text-gray-100";
var DIALOG = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";

// srcts/overlays/menu.tsx
import { Children } from "react";

// srcts/overlays/popover.tsx
import { jsx as jsx9 } from "react/jsx-runtime";
function Popover2({ className, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx9(Popover, { ...props, offset, className: composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`) });
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment4, jsx as jsx10, jsxs as jsxs6 } from "react/jsx-runtime";
function MenuTrigger2({ placement, children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs6(MenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx10(Popover2, { placement, className: "bc:min-w-44", children: menu })
  ] });
}
function Menu2({ className, ...props }) {
  return /* @__PURE__ */ jsx10(Menu, { ...props, className: composeRenderProps(className, (given) => `${MENU} ${given ?? ""}`) });
}
function MenuItem2(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx10(MenuItem, { ...props, textValue, className: composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`), children: composeRenderProps(props.children, (children, { selectionMode, isSelected }) => /* @__PURE__ */ jsxs6(Fragment4, { children: [
    selectionMode === "none" ? null : /* @__PURE__ */ jsx10("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx10(Tick, {}) : null }),
    /* @__PURE__ */ jsx10("span", { className: CONTENT, children })
  ] })) });
}
function MenuSection2({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs6(MenuSection, { ...props, children: [
    title === void 0 ? null : /* @__PURE__ */ jsx10(Header, { className: HEADING, children: title }),
    /* @__PURE__ */ jsx10(Collection, { items, children })
  ] });
}
var MENU = "bc:grid bc:max-h-[inherit] bc:gap-0.5 bc:overflow-auto bc:p-1 bc:outline-none";
var ITEM = "bc:box-border bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var HEADING = "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
var CONTENT = "bc:grid bc:min-w-0 bc:flex-1 bc:grid-cols-[minmax(0,1fr)_auto] bc:items-center bc:gap-x-4 bc:[&>*]:truncate bc:[&>[slot=description]]:col-start-1 bc:[&>[slot=description]]:text-xs bc:[&>[slot=description]]:text-text-muted bc:[&>kbd]:col-start-2 bc:[&>kbd]:row-start-1 bc:[&>kbd]:font-sans bc:[&>kbd]:text-xs bc:[&>kbd]:text-text-muted";

// srcts/overlays/palette.tsx
import { jsx as jsx11, jsxs as jsxs7 } from "react/jsx-runtime";
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
  return /* @__PURE__ */ jsx11(Modal2, { isDismissable: true, isOpen, onOpenChange, children: /* @__PURE__ */ jsx11(Dialog2, { "aria-label": placeholder, className: "bc:flex bc:max-h-[inherit] bc:flex-col bc:gap-2 bc:p-2", children: /* @__PURE__ */ jsxs7(
    Autocomplete,
    {
      filter: filter ?? contains,
      inputValue,
      defaultInputValue,
      onInputChange,
      disableAutoFocusFirst,
      disableVirtualFocus,
      children: [
        /* @__PURE__ */ jsx11(SearchField2, { autoFocus: true, "aria-label": placeholder, placeholder }),
        /* @__PURE__ */ jsx11(
          Menu2,
          {
            renderEmptyState: () => /* @__PURE__ */ jsx11("p", { className: NOTE, children: "No results found." }),
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

// srcts/inputs/colorswatches.tsx
import { useContext as useContext4 } from "react";
import { Fragment as Fragment5, jsx as jsx12, jsxs as jsxs8 } from "react/jsx-runtime";
function ColorSwatch2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx12(
    ColorSwatch,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${SWATCH} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx12(Drawn2, { ...props }) : /* @__PURE__ */ jsx12(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs8(Fragment5, { children: [
    /* @__PURE__ */ jsx12(
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
    /* @__PURE__ */ jsx12(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext4(Ended);
  return /* @__PURE__ */ jsx12(
    ColorSwatchPicker,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        ended?.(color);
      },
      className: composeRenderProps(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem2({ className, ...props }) {
  return /* @__PURE__ */ jsx12(ColorSwatchPickerItem, { ...props, className: composeRenderProps(className, (given) => `${ITEM2} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs8(Fragment5, { children: [
    /* @__PURE__ */ jsx12(ColorSwatch2, {}),
    isSelected ? /* @__PURE__ */ jsx12("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bc:box-border bc:size-8 bc:shrink-0 bc:rounded-md bc:border bc:border-border-default";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bc:flex bc:gap-1";
var ITEM2 = "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var SELECTED = "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";

// srcts/inputs/colorpicker.tsx
import { Fragment as Fragment6, jsx as jsx13, jsxs as jsxs9 } from "react/jsx-runtime";
function ColorPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx13(Drawn3, { ...props }) : /* @__PURE__ */ jsx13(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs9(Fragment6, { children: [
    /* @__PURE__ */ jsx13(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx13(
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
    /* @__PURE__ */ jsx13(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx13(ColorPicker, { ...props, children: /* @__PURE__ */ jsxs9(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs9(Button, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx13(ColorSwatch2, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx13("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx13(Popover2, { placement: "bottom start", children: /* @__PURE__ */ jsx13(Dialog2, { className: "bc:flex bc:flex-col bc:gap-2", children: children ?? /* @__PURE__ */ jsxs9(Fragment6, { children: [
      /* @__PURE__ */ jsx13(ColorArea2, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx13(ColorSlider2, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx13(ColorField2, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded-xs bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/combobox.tsx
import { useContext as useContext5, useRef as useRef4, useState as useState6 } from "react";
import { Fragment as Fragment7, jsx as jsx14, jsxs as jsxs10 } from "react/jsx-runtime";
function ComboBox2(props) {
  const { bound, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx14(Drawn4, { ...rest });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx14(BoundSeveral, { ...rest, bound, ordered }) : /* @__PURE__ */ jsx14(BoundOne, { ...rest, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs10(Fragment7, { children: [
    /* @__PURE__ */ jsx14(
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
    /* @__PURE__ */ jsx14(Refusal, { error: bound.error })
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
  const held3 = strings(bound.value);
  const [picks, setPicks] = useState6(null);
  const pending = useRef4(null);
  const submit = (next, opened) => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && items !== void 0 ? inOrder(value, ids(items)) : value);
  };
  return /* @__PURE__ */ jsxs10(Fragment7, { children: [
    /* @__PURE__ */ jsx14(
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
    /* @__PURE__ */ jsx14(Refusal, { error: bound.error })
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
  return /* @__PURE__ */ jsxs10(
    ComboBox,
    {
      ...props,
      value,
      defaultItems: unknown ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps(className, (given2) => `bc:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx14(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx14(
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
        /* @__PURE__ */ jsx14(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx14(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx14(Lacking, { multiple, marking }),
        /* @__PURE__ */ jsx14(Popover, { offset: 4, className: LIST, children: /* @__PURE__ */ jsx14(Virtualizer, { layout: ListLayout, layoutOptions: LAYOUT, children: /* @__PURE__ */ jsx14(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx14("p", { className: NOTE2, children: empty }), className: LISTBOX, children }) }) })
      ]
    }
  );
}
function useSameValue(value) {
  const [drawn, setDrawn] = useState6(value);
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
  return /* @__PURE__ */ jsxs10(Group, { isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs10("div", { className: VALUES, children: [
      keys.length === 0 ? null : isDisabled ? (
        // The chips of a disabled select take no focus, as its text field
        // takes none.
        // React Aria's TagGroup keeps a tab stop on its list even where
        // every tag is disabled, so a disabled select draws its chips as
        // a plain list.
        /* @__PURE__ */ jsx14("div", { role: "list", "aria-label": picks, className: "bc:flex bc:min-w-0 bc:max-w-full bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx14("div", { role: "listitem", "data-missing": gone || void 0, className: CHIP, children: /* @__PURE__ */ jsx14("span", { className: "bc:truncate", children: text }) }, key)) })
      ) : /* @__PURE__ */ jsx14(TagGroup, { "aria-label": picks, onRemove: editable ? remove : void 0, className: "bc:flex bc:min-w-0 bc:max-w-full", children: /* @__PURE__ */ jsx14(TagList, { className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsxs10(Tag, { id: key, textValue: text, "data-missing": gone || void 0, className: CHIP, children: [
        /* @__PURE__ */ jsx14("span", { className: "bc:truncate", children: text }),
        editable ? /* @__PURE__ */ jsx14(Button, { slot: "remove", "aria-label": `Remove ${text}`, className: CHIP_REMOVE, children: /* @__PURE__ */ jsx14(Cross, {}) }) : null
      ] }, key)) }) }),
      /* @__PURE__ */ jsx14(
        Input,
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
      // React Aria's Button takes no title, so the cross carries it.
      /* @__PURE__ */ jsx14(
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
          children: /* @__PURE__ */ jsx14("span", { title: multiple ? "Clear every pick" : "Clear the pick", className: "bc:flex bc:size-full bc:items-center bc:justify-center", children: /* @__PURE__ */ jsx14(Cross, {}) })
        }
      )
    ) : null,
    /* @__PURE__ */ jsx14(Button, { className: OPENER, children: /* @__PURE__ */ jsx14(Chevron, {}) })
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
  return marking ? /* @__PURE__ */ jsx14(NotAvailable, { values: lacked.map(String) }) : null;
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx14(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs10(Fragment7, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx14("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx14(Tick, {}) : null }) : null,
    /* @__PURE__ */ jsx14(
      "span",
      {
        className: `bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${selectionMode === "single" && isSelected ? "bc:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs10(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx14(Header, { className: HEADING2, children: title }),
    /* @__PURE__ */ jsx14(Collection, { items, children })
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
var HEADING2 = "bc:box-border bc:h-[28px] bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:box-border bc:flex bc:h-[32px] bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
var NOTE2 = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/dropzone.tsx
import { jsx as jsx15 } from "react/jsx-runtime";
function DropZone2({ className, ...props }) {
  return /* @__PURE__ */ jsx15(DropZone, { ...props, className: composeRenderProps(className, (given) => `${ZONE} ${given ?? ""}`) });
}
var ZONE = "bc:box-border bc:flex bc:min-h-20 bc:flex-col bc:items-center bc:justify-center bc:gap-2 bc:rounded-lg bc:border bc:border-dashed bc:border-border-default bc:p-4 bc:text-center bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-drop-target:border-solid bc:data-drop-target:border-border-accent bc:data-drop-target:bg-bg-subtle bc:data-disabled:opacity-50";

// srcts/inputs/form.tsx
import { jsx as jsx16 } from "react/jsx-runtime";
function Form2({ className, ...props }) {
  return /* @__PURE__ */ jsx16(Form, { ...props, className: `bc:grid bc:gap-3 ${className ?? ""}` });
}

// srcts/inputs/listbox.tsx
import { jsx as jsx17 } from "react/jsx-runtime";
function ListBox2({ className, ...props }) {
  return /* @__PURE__ */ jsx17(ListBox, { ...props, className: composeRenderProps(className, (given) => `${LISTBOX2} ${given ?? ""}`) });
}
function ListBoxItem2(props) {
  return /* @__PURE__ */ jsx17(ComboBoxItem, { ...props });
}
var LISTBOX2 = "bc:box-border bc:grid bc:min-w-0 bc:content-start bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-empty:px-2.5 bc:data-empty:py-2 bc:data-empty:italic bc:data-empty:text-text-muted";

// srcts/inputs/numberfield.tsx
import { useContext as useContext6, useEffect, useRef as useRef5, useState as useState7 } from "react";
import { Fragment as Fragment8, jsx as jsx18, jsxs as jsxs11 } from "react/jsx-runtime";
function NumberField2({ bound, outdated = false, maxValue, ...props }) {
  const largest = outdated ? void 0 : maxValue;
  return bound === void 0 ? /* @__PURE__ */ jsx18(Drawn5, { ...props, maxValue: largest }) : /* @__PURE__ */ jsx18(BoundNumber, { ...props, maxValue: largest, bound });
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
  const [kept, setKept] = useState7(null);
  const [typed, setTyped] = useState7(NaN);
  const [keystrokes, setKeystrokes] = useState7(0);
  const [seen, setSeen] = useState7(held3);
  if (!Object.is(seen, held3)) {
    setSeen(held3);
    if (kept !== null && (!kept.typing || !Object.is(held3, typed))) setKept(null);
  }
  const fits = (value) => inRange(value, props.minValue, props.maxValue, props.step);
  return /* @__PURE__ */ jsxs11(Fragment8, { children: [
    /* @__PURE__ */ jsx18(
      Drawn5,
      {
        ...props,
        commitBehavior,
        validationBehavior,
        value: kept?.value ?? held3,
        onInput: (event) => {
          setKept((now) => now?.typing === true ? now : { value: now?.value ?? held3, typing: true });
          setKeystrokes((n) => n + 1);
          onInput?.(event);
        },
        onChange: (value) => {
          if (!fits(value)) {
            bound.type(void 0);
            bound.flush();
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
        },
        watch: /* @__PURE__ */ jsx18(
          Watch,
          {
            keystrokes,
            onType: (value) => {
              setTyped(value);
              bound.type(fits(value) ? value : void 0);
            }
          }
        )
      }
    ),
    /* @__PURE__ */ jsx18(Refusal, { error: bound.error })
  ] });
}
function Watch({ keystrokes, onType }) {
  const state = useContext6(NumberFieldStateContext);
  const value = state?.numberValue ?? NaN;
  const heard = useRef5(0);
  useEffect(() => {
    if (keystrokes === heard.current) return;
    heard.current = keystrokes;
    onType(value);
  }, [keystrokes, value, onType]);
  return null;
}
function inRange(value, min, max, step) {
  if (Number.isNaN(value)) return false;
  if (min !== void 0 && value < min) return false;
  if (max !== void 0 && value > max) return false;
  if (step === void 0 || !(step > 0)) return true;
  const steps = (value - (min ?? 0)) / step;
  return Math.abs(steps - Math.round(steps)) < 1e-9;
}
function Drawn5({ label, description, errorMessage: errorMessage2, placeholder, watch, className, onInput, ...props }) {
  return /* @__PURE__ */ jsxs11(NumberField, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx18(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs11(Group, { className: FIELD2, children: [
      /* @__PURE__ */ jsx18(Input, { placeholder, onInput, className: INPUT2 }),
      /* @__PURE__ */ jsxs11("div", { className: STEPPERS, children: [
        /* @__PURE__ */ jsx18(Button, { slot: "increment", className: STEPPER, children: /* @__PURE__ */ jsx18(Chevron2, { up: true }) }),
        /* @__PURE__ */ jsx18(Button, { slot: "decrement", className: STEPPER, children: /* @__PURE__ */ jsx18(Chevron2, { up: false }) })
      ] })
    ] }),
    /* @__PURE__ */ jsx18(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx18(FieldError, { className: styles.alert, children: errorMessage2 }),
    watch
  ] });
}
function Chevron2({ up }) {
  return /* @__PURE__ */ jsx18(
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
      children: /* @__PURE__ */ jsx18("polyline", { points: up ? "3 7.5 6 4.5 9 7.5" : "3 4.5 6 7.5 9 4.5" })
    }
  );
}
var FIELD2 = "bc:flex bc:min-w-0 bc:items-stretch bc:justify-self-stretch bc:overflow-hidden bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT2 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted";
var STEPPERS = "bc:flex bc:flex-col bc:border-l bc:border-border-default bc:group-data-invalid:border-border-danger";
var STEPPER = "bc:flex bc:flex-1 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:px-1 bc:py-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/radio.tsx
import { useRef as useRef6 } from "react";
import { Fragment as Fragment9, jsx as jsx19, jsxs as jsxs12 } from "react/jsx-runtime";
function RadioGroup2({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx19(DrawnGroup2, { ...props }) : /* @__PURE__ */ jsx19(BoundGroup2, { ...props, bound, outdated });
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
  return /* @__PURE__ */ jsxs12(Fragment9, { children: [
    /* @__PURE__ */ jsx19(
      DrawnGroup2,
      {
        ...props,
        value: held3,
        isInvalid: props.isInvalid === true || marking,
        onChange: (value) => {
          void bound.commit(value);
          onChange?.(value);
        },
        children: /* @__PURE__ */ jsx19(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx19(NotAvailable, { values: gone }) : null,
    /* @__PURE__ */ jsx19(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup2({ label, description, errorMessage: errorMessage2, children, className, ...props }) {
  return /* @__PURE__ */ jsxs12(RadioGroup, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx19(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx19("div", { className: "bc:flex bc:gap-1.5 bc:group-data-[orientation=horizontal]:flex-row bc:group-data-[orientation=horizontal]:flex-wrap bc:group-data-[orientation=horizontal]:gap-4 bc:group-data-[orientation=vertical]:flex-col", children }),
    /* @__PURE__ */ jsx19(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx19(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
function Radio({ children, description, className, ...props }) {
  const ref = useRef6(null);
  useItem(props.value, ref);
  return /* @__PURE__ */ jsxs12(RadioField, { ...props, ref, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsxs12(RadioButton, { className: composeRenderProps(className, (given) => `bc:group ${RADIO} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx19("span", { className: CIRCLE }),
      children
    ] }),
    /* @__PURE__ */ jsx19(Text, { slot: "description", className: `${styles.hint} bc:ml-6 bc:empty:hidden`, children: description })
  ] });
}
var RADIO = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var CIRCLE = "bc:box-border bc:size-4 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:transition-[border-width] bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-[5px] bc:group-data-selected:border-bg-accent bc:group-data-invalid:border-border-danger";

// srcts/inputs/searchfield.tsx
import { Fragment as Fragment10, jsx as jsx20, jsxs as jsxs13 } from "react/jsx-runtime";
function SearchField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx20(Drawn6, { ...props }) : /* @__PURE__ */ jsx20(BoundSearch, { ...props, bound });
}
function BoundSearch({ bound, onChange, onKeyDown, onBlur, onClear, ...props }) {
  const [text, typed] = useTyped(bound);
  return /* @__PURE__ */ jsxs13(Fragment10, { children: [
    /* @__PURE__ */ jsx20(
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
    /* @__PURE__ */ jsx20(Refusal, { error: bound.error })
  ] });
}
function Drawn6({ label, description, errorMessage: errorMessage2, placeholder, className, ...props }) {
  return /* @__PURE__ */ jsxs13(SearchField, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx20(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs13(Group, { className: FIELD3, children: [
      /* @__PURE__ */ jsx20(Magnifier, {}),
      /* @__PURE__ */ jsx20(Input, { placeholder, className: INPUT3 }),
      /* @__PURE__ */ jsx20(FieldButton, { className: "bc:group-data-empty:invisible", children: /* @__PURE__ */ jsx20(Cross, {}) })
    ] }),
    /* @__PURE__ */ jsx20(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx20(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
var FIELD3 = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1.5 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-1 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT3 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:[&::-webkit-search-cancel-button]:hidden";

// srcts/inputs/select.tsx
import { useContext as useContext7, useLayoutEffect as useLayoutEffect3, useMemo as useMemo3, useRef as useRef7, useState as useState8 } from "react";
import { Fragment as Fragment11, jsx as jsx21, jsxs as jsxs14 } from "react/jsx-runtime";
function Select2(props) {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx21(Drawn7, { ...rest });
  return /* @__PURE__ */ jsx21(BoundSelect, { ...rest, bound, outdated, ordered });
}
function SelectItem(props) {
  return /* @__PURE__ */ jsx21(ComboBoxItem, { ...props });
}
function SelectSection(props) {
  return /* @__PURE__ */ jsx21(ComboBoxSection, { ...props });
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
  const values = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const [known2, setKnown] = useState8(null);
  const unknown = props.items === void 0 && typeof props.children === "function";
  const ids2 = unknown ? null : known2;
  const [picks, setPicks] = useState8(null);
  const pending = useRef7(null);
  const shown = picks ?? values;
  const asIs = ids2 === null ? values : values.filter((value) => !ids2.includes(value));
  const marking = !outdated && ids2 !== null && asIs.length > 0;
  const submit = (next, opened) => {
    const value = merged(strings(bound.get()), opened, next);
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
  const line = marking ? /* @__PURE__ */ jsx21(
    NotAvailable,
    {
      values: asIs,
      onRemove: several ? () => {
        void bound.commit(held2(strings(bound.get()), ids2));
      } : void 0
    }
  ) : null;
  if (!several) {
    return /* @__PURE__ */ jsxs14(Fragment11, { children: [
      /* @__PURE__ */ jsx21(
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
      /* @__PURE__ */ jsx21(Refusal, { error: bound.error })
    ] });
  }
  return /* @__PURE__ */ jsxs14(Fragment11, { children: [
    /* @__PURE__ */ jsx21(
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
    /* @__PURE__ */ jsx21(Refusal, { error: bound.error })
  ] });
}
function sameIds(a, b) {
  return a.length === b.length && a.every((id, i) => id === b[i]);
}
function Drawn7({ label, description, errorMessage: errorMessage2, items, children, asIs = [], onKnown, className, ...props }) {
  const value = useSameValue(props.value);
  const { locale } = useLocale();
  const list = useMemo3(() => new Intl.ListFormat(locale), [locale]);
  return /* @__PURE__ */ jsxs14(
    Select,
    {
      ...props,
      value,
      className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx21(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsxs14(Button, { className: TRIGGER, children: [
          /* @__PURE__ */ jsx21(SelectValue, { className: ({ isPlaceholder }) => isPlaceholder && asIs.length === 0 ? `${VALUE} ${PLACEHOLDER}` : VALUE, children: ({ state, selectedText, defaultChildren, isPlaceholder }) => asIs.length > 0 ? list.format([...state.selectedItems.map((item) => item.textValue), ...asIs]) : isPlaceholder ? defaultChildren : selectedText || defaultChildren }),
          /* @__PURE__ */ jsx21(Chevron, {})
        ] }),
        /* @__PURE__ */ jsx21(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx21(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx21(Popover, { offset: 4, className: LIST2, children: /* @__PURE__ */ jsx21(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx21("p", { className: NOTE3, children: "No options" }), className: LISTBOX3, children: items === void 0 ? typeof children === "function" ? null : children : /* @__PURE__ */ jsx21(Collection, { items, children }) }) }),
        onKnown === void 0 ? null : /* @__PURE__ */ jsx21(Known, { onKnown })
      ]
    }
  );
}
function Known({ onKnown }) {
  const state = useContext7(SelectStateContext);
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
import { useContext as useContext8, useLayoutEffect as useLayoutEffect4, useRef as useRef8, useState as useState9 } from "react";
import { Fragment as Fragment12, jsx as jsx22, jsxs as jsxs15 } from "react/jsx-runtime";
function Slider3({ bound, outdated = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx22(Drawn8, { ...props }) : /* @__PURE__ */ jsx22(BoundSlider, { ...props, bound, outdated });
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
  const [moving, setMoving] = useState9(null);
  const [seen, setSeen] = useState9(bound.value);
  if (!jsonEqual(seen, bound.value)) {
    setSeen(bound.value);
    setMoving(null);
  }
  const shown = useRef8([]);
  const start2 = useRef8(null);
  const min = props.minValue ?? 0;
  const fits = (value2) => inRange(value2, min, props.maxValue ?? 100, props.step ?? 1);
  const lacked = moving === null ? held3.filter((value2) => !fits(value2)) : [];
  const marking = !outdated && lacked.length > 0;
  const drawn = moving ?? (held3.length > 0 ? held3 : [min]);
  const value = several ? [...drawn] : drawn[0] ?? min;
  const { locale } = useLocale();
  return /* @__PURE__ */ jsxs15(Fragment12, { children: [
    /* @__PURE__ */ jsx22(
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
        watch: /* @__PURE__ */ jsx22(
          Watch2,
          {
            onShown: (values) => {
              shown.current = values;
            }
          }
        )
      }
    ),
    marking ? /* @__PURE__ */ jsx22(NotAvailable, { values: lacked.map((x) => new Intl.NumberFormat(locale, props.formatOptions).format(x)) }) : null,
    /* @__PURE__ */ jsx22(Refusal, { error: bound.error })
  ] });
}
function Watch2({ onShown }) {
  const state = useContext8(SliderStateContext);
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
  return /* @__PURE__ */ jsxs15(
    Slider,
    {
      ...props,
      "data-invalid": invalid || void 0,
      className: composeRenderProps(className, (given) => `bc:group ${SLIDER2} ${given ?? ""}`),
      children: [
        label === void 0 ? /* @__PURE__ */ jsx22("span", {}) : /* @__PURE__ */ jsx22(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx22(SliderOutput, { className: OUTPUT2, children: ({ state }) => said === void 0 ? state.getFormattedValue() : said((value) => state.getFormattedValue(value)) }),
        /* @__PURE__ */ jsx22(SliderTrack, { className: TRACK2, children: ({ state }) => /* @__PURE__ */ jsxs15(Fragment12, { children: [
          /* @__PURE__ */ jsx22("div", { className: BAR, children: /* @__PURE__ */ jsx22(SliderFill, { offset: fillOffset, className: FILL }) }),
          state.values.map((_, i) => /* @__PURE__ */ jsx22(SliderThumb, { index: i, "aria-label": thumbLabels?.[i], className: THUMB2 }, i))
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
import { Fragment as Fragment13, jsx as jsx23, jsxs as jsxs16 } from "react/jsx-runtime";
function Switch({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx23(Drawn9, { ...props, onChange });
  return /* @__PURE__ */ jsxs16(Fragment13, { children: [
    /* @__PURE__ */ jsx23(
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
    /* @__PURE__ */ jsx23(Refusal, { error: bound.error })
  ] });
}
function Drawn9({ children, description, errorMessage: errorMessage2, className, ...props }) {
  return /* @__PURE__ */ jsxs16(SwitchField, { ...props, className: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default", children: [
    /* @__PURE__ */ jsxs16(SwitchButton, { className: composeRenderProps(className, (given) => `bc:group ${SWITCH} ${given ?? ""}`), children: [
      /* @__PURE__ */ jsx23("span", { className: TRACK3, children: /* @__PURE__ */ jsx23("span", { className: HANDLE }) }),
      children
    ] }),
    /* @__PURE__ */ jsx23(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx23(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}
var SWITCH = "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var TRACK3 = "bc:box-border bc:flex bc:h-5 bc:w-9 bc:shrink-0 bc:items-center bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:px-px bc:outline-offset-2 bc:outline-border-accent bc:transition-colors bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent";
var HANDLE = "bc:size-4 bc:rounded-full bc:bg-text-muted bc:shadow-xs bc:transition-transform bc:group-data-selected:translate-x-full bc:group-data-selected:bg-text-on-accent";

// srcts/inputs/tags.tsx
import { createContext as createContext3, useContext as useContext9 } from "react";
import { Fragment as Fragment14, jsx as jsx24, jsxs as jsxs17 } from "react/jsx-runtime";
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
  return /* @__PURE__ */ jsxs17(TagGroup, { ...props, className: `${styles.field} ${className ?? ""}`, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx24(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx24(ColorContext.Provider, { value: color, children: /* @__PURE__ */ jsx24(TagList, { items, renderEmptyState, className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children }) }),
    description === void 0 ? null : /* @__PURE__ */ jsx24(Text, { slot: "description", className: styles.hint, children: description }),
    errorMessage2 === void 0 ? null : /* @__PURE__ */ jsx24(Text, { slot: "errorMessage", className: styles.alert, children: errorMessage2 })
  ] });
}
function Tag2({ children, color, className, ...props }) {
  const textValue = typeof children === "string" ? children : void 0;
  const groupColor = useContext9(ColorContext);
  return /* @__PURE__ */ jsx24(
    Tag,
    {
      textValue,
      ...props,
      className: composeRenderProps(className, (given) => `${TAG} ${colors[color ?? groupColor]} ${given ?? ""}`),
      children: composeRenderProps(children, (children2, { allowsRemoving }) => /* @__PURE__ */ jsxs17(Fragment14, { children: [
        /* @__PURE__ */ jsx24("span", { className: "bc:truncate", children: children2 }),
        allowsRemoving ? /* @__PURE__ */ jsx24(Button, { slot: "remove", className: REMOVE, children: /* @__PURE__ */ jsx24(Cross, {}) }) : null
      ] }))
    }
  );
}
var TAG = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:cursor-default bc:items-center bc:gap-0.5 bc:rounded bc:border bc:px-1.5 bc:py-0 bc:font-sans bc:text-xs bc:leading-5 bc:outline-none bc:data-allows-removing:pr-0.5 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-selected:border-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:opacity-50";
var REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:data-hovered:bg-bg-subtle";

// srcts/inputs/textfield.tsx
import { Fragment as Fragment15, jsx as jsx25, jsxs as jsxs18 } from "react/jsx-runtime";
function TextField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx25(Drawn10, { ...props }) : /* @__PURE__ */ jsx25(BoundText, { ...props, bound });
}
function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }) {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
  return /* @__PURE__ */ jsxs18(Fragment15, { children: [
    /* @__PURE__ */ jsx25(
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
    /* @__PURE__ */ jsx25(Refusal, { error: bound.error })
  ] });
}
function Drawn10({ label, description, errorMessage: errorMessage2, placeholder, lines = 1, className, ...props }) {
  return /* @__PURE__ */ jsxs18(TextField, { ...props, className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx25(Label, { className: styles.label, children: label }),
    lines > 1 ? /* @__PURE__ */ jsx25(TextArea, { rows: lines, placeholder, className: styles.box }) : /* @__PURE__ */ jsx25(Input, { placeholder, className: styles.box }),
    /* @__PURE__ */ jsx25(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx25(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}

// srcts/inputs/toggle.tsx
import { createContext as createContext4, useContext as useContext10, useId, useRef as useRef9 } from "react";
import { Fragment as Fragment16, jsx as jsx26, jsxs as jsxs19 } from "react/jsx-runtime";
function ToggleButton2({ bound, onChange, ...props }) {
  if (bound === void 0) return /* @__PURE__ */ jsx26(DrawnButton, { ...props, onChange });
  return /* @__PURE__ */ jsxs19(Fragment16, { children: [
    /* @__PURE__ */ jsx26(
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
    /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
  ] });
}
var InGroup = createContext4(false);
function DrawnButton({ className, ...props }) {
  const ref = useRef9(null);
  const inGroup = useContext10(InGroup);
  useItem(props.id === void 0 ? void 0 : String(props.id), ref);
  return /* @__PURE__ */ jsx26(
    ToggleButton,
    {
      ...props,
      ref,
      className: composeRenderProps(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)
    }
  );
}
function ToggleButtonGroup2({ bound, outdated = false, ordered = false, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx26(DrawnGroup3, { ...props }) : /* @__PURE__ */ jsx26(BoundGroup3, { ...props, bound, outdated, ordered });
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
  const shown = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(shown, known2);
  const marking = !outdated && gone.length > 0;
  return /* @__PURE__ */ jsxs19(Fragment16, { children: [
    /* @__PURE__ */ jsx26(
      DrawnGroup3,
      {
        ...props,
        selectedKeys: shown,
        invalid: marking,
        onSelectionChange: (keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value = merged(strings(bound.get()), shown, picks);
            if (value !== null) void bound.commit(ordered && known2 !== null ? inOrder(value, known2) : value);
          } else {
            const [pick] = picks;
            void (pick === void 0 ? bound.unset() : bound.commit(pick));
          }
          onSelectionChange?.(keys);
        },
        children: /* @__PURE__ */ jsx26(ItemsContext.Provider, { value: items, children })
      }
    ),
    marking ? /* @__PURE__ */ jsx26(
      NotAvailable,
      {
        values: gone,
        onRemove: several ? () => {
          void bound.commit(held2(strings(bound.get()), known2));
        } : void 0
      }
    ) : null,
    /* @__PURE__ */ jsx26(Refusal, { error: bound.error })
  ] });
}
function DrawnGroup3({ label, description, invalid = false, children, className, ...props }) {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== void 0 && description !== "";
  const group = /* @__PURE__ */ jsx26(
    ToggleButtonGroup,
    {
      ...props,
      "aria-labelledby": props["aria-labelledby"] ?? (label === void 0 ? void 0 : labelId),
      "aria-describedby": described ? descriptionId : void 0,
      "data-invalid": invalid || void 0,
      className: composeRenderProps(className, (given) => `${GROUP} ${given ?? ""}`),
      children: /* @__PURE__ */ jsx26(InGroup.Provider, { value: true, children })
    }
  );
  if (label === void 0 && !described) return group;
  return /* @__PURE__ */ jsxs19("div", { className: styles.field, children: [
    label === void 0 ? null : /* @__PURE__ */ jsx26("span", { id: labelId, className: styles.label, children: label }),
    group,
    described ? /* @__PURE__ */ jsx26("p", { id: descriptionId, className: styles.hint, children: description }) : null
  ] });
}
var GROUP = "bc:flex bc:w-fit bc:max-w-full bc:flex-wrap bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-0.5 bc:font-sans bc:data-[orientation=vertical]:flex-col bc:data-invalid:border-border-danger";
var TOGGLE = "bc:inline-flex bc:min-h-6 bc:cursor-pointer bc:items-center bc:justify-center bc:gap-1 bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var ALONE = "bc:rounded-md bc:border bc:border-solid bc:border-border-default bc:data-selected:border-border-accent";

// srcts/inputs/tokenfield.tsx
import { jsx as jsx27, jsxs as jsxs20 } from "react/jsx-runtime";
function TokenField2({
  label,
  description,
  className,
  inputRef,
  children,
  ...props
}) {
  return /* @__PURE__ */ jsxs20(TokenField, { ...props, className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx27(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsx27(TokenInput, { ref: inputRef, className: INPUT4, children }),
    description === void 0 ? null : /* @__PURE__ */ jsx27(Text, { slot: "description", className: styles.hint, children: description })
  ] });
}
function Token2({ className, ...props }) {
  return /* @__PURE__ */ jsx27(Token, { ...props, className: composeRenderProps(className, (given) => `${TOKEN} ${given ?? ""}`) });
}
var INPUT4 = "bc:box-border bc:min-h-[30px] bc:w-full bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focused:outline-2 bc:data-focused:outline-offset-1 bc:data-focused:outline-border-accent bc:data-disabled:opacity-50";
var TOKEN = "bc:mx-0.5 bc:inline-flex bc:h-5 bc:cursor-default bc:items-center bc:whitespace-nowrap bc:rounded bc:border bc:border-border-accent bc:bg-bg-subtle bc:px-1.5 bc:text-xs bc:text-text-default bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent";

// srcts/react/notifications.tsx
import { jsx as jsx28 } from "react/jsx-runtime";
function NotificationList({ notifications, "aria-label": label }) {
  if (notifications.length === 0) return null;
  return /* @__PURE__ */ jsx28("ul", { "aria-label": label, className: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0", children: notifications.map(({ id, severity, message }) => /* @__PURE__ */ jsx28("li", { "data-severity": severity, className: `${ITEM3} ${SETS[severity]}`, children: message }, id)) });
}
var ITEM3 = "bc:rounded-md bc:border bc:px-3 bc:py-2 bc:font-sans bc:text-[0.8125rem] bc:leading-normal bc:whitespace-pre-wrap bc:wrap-anywhere";
var SETS = {
  error: "bc:border-border-danger-subtle bc:bg-bg-danger bc:text-text-danger",
  warning: "bc:border-border-warning-subtle bc:bg-bg-warning bc:text-text-warning",
  message: "bc:border-border-default bc:bg-bg-subtle bc:text-text-default"
};
export {
  Autocomplete,
  Button2 as Button,
  Checkbox,
  CheckboxGroup2 as CheckboxGroup,
  ColorArea2 as ColorArea,
  ColorField2 as ColorField,
  ColorPicker2 as ColorPicker,
  ColorSlider2 as ColorSlider,
  ColorSwatch2 as ColorSwatch,
  ColorSwatchPicker2 as ColorSwatchPicker,
  ColorSwatchPickerItem2 as ColorSwatchPickerItem,
  ColorThumb2 as ColorThumb,
  ColorWheel2 as ColorWheel,
  ComboBox2 as ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  CommandPalette,
  Dialog2 as Dialog,
  DialogTrigger,
  DropZone2 as DropZone,
  Field,
  FieldButton,
  FileTrigger,
  Form2 as Form,
  Heading,
  Keyboard,
  ListBox2 as ListBox,
  ListBoxItem2 as ListBoxItem,
  Menu2 as Menu,
  MenuItem2 as MenuItem,
  MenuSection2 as MenuSection,
  MenuTrigger2 as MenuTrigger,
  Modal2 as Modal,
  NotificationList,
  NumberField2 as NumberField,
  Popover2 as Popover,
  Radio,
  RadioGroup2 as RadioGroup,
  SearchField2 as SearchField,
  Select2 as Select,
  SelectItem,
  SelectSection,
  Slider3 as Slider,
  Stack,
  Switch,
  Tag2 as Tag,
  TagGroup2 as TagGroup,
  Text,
  TextField2 as TextField,
  ToggleButton2 as ToggleButton,
  ToggleButtonGroup2 as ToggleButtonGroup,
  Token2 as Token,
  TokenField2 as TokenField,
  outputRenderer,
  reactModule,
  useBoard,
  useFilter,
  useNotified,
  useOutput,
  useRequest,
  useStatus,
  useValue
};
