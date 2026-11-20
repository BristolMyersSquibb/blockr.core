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
function bind(host, key, show, options = {}) {
  const source = createSource(host);
  source.check(key);
  const submitting = options.notifier === void 0 ? {} : { notifier: options.notifier };
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
      await (value === void 0 ? source.unset(key, submitting) : source.submit(key, value, submitting));
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
      submit(key, value, opts) {
        check(key);
        return host.submit(value, opts);
      },
      unset(key) {
        check(key);
        return Promise.reject(new Error(`The board option "${host.option}" always holds a value, so none of it is unset.`));
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
    submit: (key, value, opts) => host.submit({ [key]: value }, opts),
    unset: (key, opts) => host.submit({ [key]: void 0 }, opts),
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input) => ({ input: input.input, revision: input.revision })),
    request: (name, args, opts) => host.request(name, args, opts)
  };
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
  Breadcrumb,
  Breadcrumbs,
  Button,
  Collection,
  ComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  Dialog,
  DialogTrigger,
  Disclosure,
  DisclosureGroup,
  DisclosurePanel,
  DisclosureStateContext,
  FieldError,
  Focusable,
  Group,
  Header,
  Heading,
  Input,
  Keyboard,
  Label,
  Link,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Meter,
  Modal,
  ModalOverlay,
  NumberField,
  NumberFieldStateContext,
  OverlayArrow,
  Popover,
  ProgressBar,
  SelectionIndicator,
  Separator,
  Sheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay,
  SubmenuTrigger,
  Tab,
  TabList,
  TabPanel,
  Tabs,
  Tag,
  TagGroup,
  TagList,
  Text,
  TextArea,
  TextField,
  Toolbar,
  Tooltip,
  TooltipTrigger,
  TooltipTriggerStateContext,
  UNSTABLE_Toast,
  UNSTABLE_ToastContent,
  UNSTABLE_ToastQueue,
  UNSTABLE_ToastRegion,
  useFilter,
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
  destructive: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-red-700 bc:bg-red-700 bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-white bc:cursor-pointer bc:hover:bg-red-800 bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet: "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle"
};

// srcts/inputs/button.tsx
import { jsx as jsx2 } from "react/jsx-runtime";
function Button2({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }) {
  return /* @__PURE__ */ jsx2(
    Button,
    {
      ...props,
      render: shortcuts === void 0 || props.render !== void 0 ? props.render : (dom) => /* @__PURE__ */ jsx2("button", { ...dom, "aria-keyshortcuts": shortcuts }),
      className: composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)
    }
  );
}

// srcts/inputs/inputs.tsx
import { useState as useState2 } from "react";
import { Fragment, jsx as jsx3, jsxs } from "react/jsx-runtime";
function Stack({ children }) {
  return /* @__PURE__ */ jsx3("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs("label", { className: styles.field, children: [
    /* @__PURE__ */ jsx3("span", { className: styles.label, children: label }),
    children
  ] });
}
function Checkbox({ bound, label }) {
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsxs("label", { className: styles.check, children: [
      /* @__PURE__ */ jsx3(
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
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
  ] });
}
function missing(value) {
  return `${value} (not available)`;
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx3("p", { role: "alert", className: styles.alert, children: error });
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
import { useContext, useEffect, useEffectEvent, useRef as useRef2, useState as useState3 } from "react";

// srcts/overlays/dialog.tsx
import { Fragment as Fragment2, jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function Modal2({ className, ...props }) {
  return /* @__PURE__ */ jsx4(ModalOverlay, { ...props, className: OVERLAY, children: /* @__PURE__ */ jsx4("div", { className: CENTER, children: /* @__PURE__ */ jsx4(Modal, { ...props, className: composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`) }) }) });
}
function Dialog2({ className, ...props }) {
  return /* @__PURE__ */ jsx4(Dialog, { ...props, className: `${DIALOG} ${className ?? ""}` });
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
  return /* @__PURE__ */ jsx4(Dialog2, { role: "alertdialog", ...props, children: ({ close }) => /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx4(Heading, { slot: "title", className: TITLE, children: title }),
    /* @__PURE__ */ jsx4("span", { className: `bc:absolute bc:top-4 bc:right-4 ${destructive ? "bc:text-text-danger" : "bc:text-blue-700 bc:dark:text-blue-400"}`, children: /* @__PURE__ */ jsx4(Mark, { destructive }) }),
    /* @__PURE__ */ jsx4("p", { className: "bc:mt-3 bc:mb-0 bc:font-sans bc:text-sm bc:text-text-muted", children }),
    /* @__PURE__ */ jsxs2("div", { className: "bc:mt-6 bc:flex bc:justify-end bc:gap-2", children: [
      /* @__PURE__ */ jsx4(Button, { className: buttons.secondary, onPress: close, children: cancelLabel }),
      /* @__PURE__ */ jsx4(
        Button,
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
  return /* @__PURE__ */ jsxs2(
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
        /* @__PURE__ */ jsx4("circle", { cx: "8", cy: "8", r: "6.4" }),
        /* @__PURE__ */ jsx4("path", { d: destructive ? "M8 4.6v4.2M8 11.2v.1" : "M8 7.4v3.6M8 4.9v.1" })
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

// srcts/overlays/stacking.ts
var OVERLAY_Z_INDEX = "var(--bc-z-index-overlay)";
function overlayStyle(style) {
  return composeRenderProps(style, (given) => ({ ...given, zIndex: OVERLAY_Z_INDEX }));
}

// srcts/overlays/popover.tsx
import { jsx as jsx5 } from "react/jsx-runtime";
function Popover2({ className, style, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx5(
    Popover,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`)
    }
  );
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment3, jsx as jsx6, jsxs as jsxs3 } from "react/jsx-runtime";
function MenuTrigger2({ placement, children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs3(MenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx6(Popover2, { placement, className: "bc:min-w-44", children: menu })
  ] });
}
function SubmenuTrigger2({ children, ...props }) {
  const [trigger, menu] = Children.toArray(children);
  return /* @__PURE__ */ jsxs3(SubmenuTrigger, { ...props, children: [
    trigger,
    /* @__PURE__ */ jsx6(Popover2, { offset: -2, crossOffset: -4, className: "bc:min-w-44", children: menu })
  ] });
}
function Menu2({ className, ...props }) {
  return /* @__PURE__ */ jsx6(Menu, { ...props, className: composeRenderProps(className, (given) => `${MENU} ${given ?? ""}`) });
}
function MenuItem2(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx6(MenuItem, { ...props, textValue, className: composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`), children: composeRenderProps(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => /* @__PURE__ */ jsxs3(Fragment3, { children: [
    selectionMode === "none" ? null : /* @__PURE__ */ jsx6("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx6(Tick, {}) : null }),
    /* @__PURE__ */ jsx6("span", { className: CONTENT, children }),
    hasSubmenu ? /* @__PURE__ */ jsx6(Chevron, {}) : null
  ] })) });
}
function MenuSection2({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs3(MenuSection, { ...props, children: [
    title === void 0 ? null : /* @__PURE__ */ jsx6(Header, { className: HEADING, children: title }),
    /* @__PURE__ */ jsx6(Collection, { items, children })
  ] });
}
function Tick() {
  return /* @__PURE__ */ jsx6(
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
      children: /* @__PURE__ */ jsx6("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function Chevron() {
  return /* @__PURE__ */ jsx6(
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
      className: "bc:shrink-0 bc:text-text-muted",
      children: /* @__PURE__ */ jsx6("path", { d: "M6 3.5l4.5 4.5L6 12.5" })
    }
  );
}
var MENU = "bc:grid bc:max-h-[inherit] bc:gap-0.5 bc:overflow-auto bc:p-1 bc:outline-none";
var ITEM = "bc:box-border bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var HEADING = "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
var CONTENT = "bc:grid bc:min-w-0 bc:flex-1 bc:grid-cols-[minmax(0,1fr)_auto] bc:items-center bc:gap-x-4 bc:[&>*]:truncate bc:[&>[slot=description]]:col-start-1 bc:[&>[slot=description]]:text-xs bc:[&>[slot=description]]:text-text-muted bc:[&>kbd]:col-start-2 bc:[&>kbd]:row-start-1 bc:[&>kbd]:font-sans bc:[&>kbd]:text-xs bc:[&>kbd]:text-text-muted";

// srcts/overlays/sheet.tsx
import { jsx as jsx7, jsxs as jsxs4 } from "react/jsx-runtime";
function Sheet2({ children, className, overscrollPadding = true, ...props }) {
  const { snapPoints } = props;
  return /* @__PURE__ */ jsxs4(SheetOverlay, { ...props, className: composeRenderProps(className, (given) => `bc:group/sheet bc:z-overlay ${given ?? ""}`), children: [
    /* @__PURE__ */ jsx7(
      SheetBackdrop,
      {
        className: BACKDROP,
        swipeAnimation: "bc-sheet-backdrop",
        swipeAnimationRange: snapPoints === void 0 ? void 0 : { start: snapPoints.length - 1 }
      }
    ),
    /* @__PURE__ */ jsx7(
      Sheet,
      {
        overscrollPadding,
        stackAnimation: "bc-sheet-scale-back",
        className: ({ position }) => `${SHEET} ${POSITIONS[position]}`,
        children: /* @__PURE__ */ jsx7(SheetContent, { className: CONTENT2, children })
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
import { jsx as jsx8 } from "react/jsx-runtime";
var FADES_FOR = 300;
function ToastRegion({ className, ...props }) {
  return /* @__PURE__ */ jsx8(UNSTABLE_ToastRegion, { ...props, className: composeRenderProps(className, (given) => `${REGION} ${given ?? ""}`) });
}
function Toast({ className, style, ...props }) {
  const { timeout } = props.toast;
  const fade = timeout === void 0 ? void 0 : { "--bc-toast-fade-at": `${String(Math.max(0, timeout - FADES_FOR))}ms` };
  return /* @__PURE__ */ jsx8(
    UNSTABLE_Toast,
    {
      ...props,
      style: composeRenderProps(style, (given) => ({ ...fade, ...given })),
      className: composeRenderProps(className, (given) => `${TOAST} ${timeout === void 0 ? "" : FADE} ${given ?? ""}`)
    }
  );
}
var REGION = "bc:group/toasts bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-[calc(var(--bc-z-index-overlay)+1)] bc:m-0 bc:box-border bc:flex bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:flex-col-reverse bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
var TOAST = "bc:pointer-events-auto bc:box-border bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-3 bc:py-2 bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:dark:text-gray-100";
var FADE = "bc:toast-fade bc:group-data-hovered/toasts:[animation-play-state:paused] bc:group-focus-within/toasts:[animation-play-state:paused]";

// srcts/overlays/tooltip.tsx
import { jsx as jsx9, jsxs as jsxs5 } from "react/jsx-runtime";
function Tooltip2({ children, className, style, offset = 8, ...props }) {
  return /* @__PURE__ */ jsxs5(
    Tooltip,
    {
      ...props,
      offset,
      style: overlayStyle(style),
      className: composeRenderProps(className, (given) => `${TOOLTIP} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx9(OverlayArrow, { children: /* @__PURE__ */ jsx9("svg", { width: 8, height: 8, viewBox: "0 0 8 8", className: ARROW, "aria-hidden": "true", children: /* @__PURE__ */ jsx9("path", { d: "M0 0 L4 4 L8 0" }) }) }),
        children
      ]
    }
  );
}
var TOOLTIP = "bc:group bc:box-border bc:max-w-xs bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-gray-900 bc:shadow-md bc:outline-none bc:will-change-transform bc:dark:text-gray-100";
var ARROW = "bc:block bc:fill-bg-raised bc:stroke-border-default bc:forced-colors:fill-[Canvas] bc:forced-colors:stroke-[ButtonBorder] bc:group-data-[placement=bottom]:rotate-180 bc:group-data-[placement=left]:-rotate-90 bc:group-data-[placement=right]:rotate-90";

// srcts/inputs/combobox.tsx
import { Fragment as Fragment4, jsx as jsx10, jsxs as jsxs6 } from "react/jsx-runtime";
function ComboBox2(props) {
  const { bound, ordered = false, ...rest } = props;
  if (bound === void 0) return /* @__PURE__ */ jsx10(Drawn, { ...rest });
  return props.selectionMode === "multiple" ? /* @__PURE__ */ jsx10(BoundSeveral, { ...rest, bound, ordered }) : /* @__PURE__ */ jsx10(BoundOne, { ...rest, bound });
}
function BoundOne({ bound, outdated = false, ...props }) {
  const current = typeof bound.value === "string" ? bound.value : null;
  return /* @__PURE__ */ jsxs6(Fragment4, { children: [
    /* @__PURE__ */ jsx10(
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
    /* @__PURE__ */ jsx10(Refusal, { error: bound.error })
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
    const added = next.filter((value2) => !opened.includes(value2));
    const removed = opened.filter((value2) => !next.includes(value2));
    if (added.length === 0 && removed.length === 0) return;
    const now = strings(bound.get());
    const value = [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
    void bound.commit(ordered && items !== void 0 ? inOrder(value, ids(items)) : value);
  };
  return /* @__PURE__ */ jsxs6(Fragment4, { children: [
    /* @__PURE__ */ jsx10(
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
    /* @__PURE__ */ jsx10(Refusal, { error: bound.error })
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
  return /* @__PURE__ */ jsxs6(
    ComboBox,
    {
      ...props,
      value,
      defaultItems: unknown ? [] : props.defaultItems,
      allowsEmptyCollection,
      className: composeRenderProps(className, (given2) => `bc:group ${styles.field} ${given2 ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx10(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx10(
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
        /* @__PURE__ */ jsx10(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx10(FieldError, { className: styles.alert, children: errorMessage2 }),
        /* @__PURE__ */ jsx10(Popover, { offset: 4, className: LIST, style: { zIndex: OVERLAY_Z_INDEX }, children: /* @__PURE__ */ jsx10(Virtualizer, { layout: ListLayout, layoutOptions: LAYOUT, children: /* @__PURE__ */ jsx10(ListBox, { renderEmptyState: () => /* @__PURE__ */ jsx10("p", { className: NOTE, children: empty }), className: LISTBOX, children }) }) })
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
  const state = useContext(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? value : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? value : null;
  const item = state?.selectedItems[0] ?? null;
  const lackedOne = one !== null && item === null;
  const known2 = new Map((state?.selectedItems ?? []).map((item2) => [item2.key, item2.textValue]));
  const lacked = keys.some((key) => !known2.has(key));
  const invalid = marking && (lackedOne || lacked);
  const remove = (gone) => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  const chips = keys.map((key) => ({ key, text: known2.get(key) ?? String(key), gone: marking && !known2.has(key) }));
  const picks = label === void 0 ? "Picks" : `Picks of ${label}`;
  return /* @__PURE__ */ jsxs6(Group, { isInvalid: invalid, className: FIELD, children: [
    /* @__PURE__ */ jsxs6("div", { className: VALUES, children: [
      keys.length === 0 ? null : isDisabled ? (
        // The chips of a disabled select take no focus, as its text field
        // takes none.
        // React Aria's TagGroup keeps a tab stop on its list even where
        // every tag is disabled, so a disabled select draws its chips as
        // a plain list.
        /* @__PURE__ */ jsx10("div", { role: "list", "aria-label": picks, className: "bc:flex bc:min-w-0 bc:max-w-full bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx10(PlainChip, { text: gone ? missing(text) : text, gone }, key)) })
      ) : /* @__PURE__ */ jsx10(TagGroup, { "aria-label": picks, onRemove: editable ? remove : void 0, className: "bc:flex bc:min-w-0 bc:max-w-full", children: /* @__PURE__ */ jsx10(TagList, { className: "bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1", children: chips.map(({ key, text, gone }) => /* @__PURE__ */ jsx10(Tag, { id: key, textValue: text, "data-missing": gone || void 0, className: CHIP, children: ({ isFocusVisible }) => /* @__PURE__ */ jsxs6(Fragment4, { children: [
        /* @__PURE__ */ jsx10(ChipText, { text: gone ? missing(text) : text, focused: isFocusVisible }),
        editable ? /* @__PURE__ */ jsx10(Button, { slot: "remove", "aria-label": `Remove ${text}`, className: CHIP_REMOVE, children: /* @__PURE__ */ jsx10(Cross, {}) }) : null
      ] }) }, key)) }) }),
      /* @__PURE__ */ jsx10(
        Input,
        {
          value: multiple || state === null || state.isFocused || state.isOpen ? void 0 : item?.textValue ?? "",
          placeholder: lackedOne ? marking ? missing(String(one)) : String(one) : keys.length > 0 ? void 0 : placeholder,
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
      // A button of the field's own, not React Aria's, which the select
      // would take for its own and open the list with. It keeps the focus
      // in the text field, as a press in the list does. React Aria's
      // Focusable makes it the trigger of its tooltip.
      /* @__PURE__ */ jsxs6(TooltipTrigger, { children: [
        /* @__PURE__ */ jsx10(Focusable, { children: /* @__PURE__ */ jsx10(
          "button",
          {
            type: "button",
            tabIndex: -1,
            "aria-label": label === void 0 ? "Clear" : `Clear ${label}`,
            className: CLEAR,
            onMouseDown: (event) => {
              event.preventDefault();
            },
            onClick: () => {
              if (onClear !== void 0) onClear();
              else state?.setValue(multiple ? [] : null);
            },
            children: /* @__PURE__ */ jsx10(Cross, {})
          }
        ) }),
        /* @__PURE__ */ jsx10(Tooltip2, { children: multiple ? "Clear every pick" : "Clear the pick" })
      ] })
    ) : null,
    /* @__PURE__ */ jsx10(Button, { className: OPENER, children: /* @__PURE__ */ jsx10(Chevron2, {}) })
  ] });
}
function PlainChip({ text, gone }) {
  return /* @__PURE__ */ jsx10("div", { role: "listitem", "data-missing": gone || void 0, className: CHIP, children: /* @__PURE__ */ jsx10(ChipText, { text, focused: false }) });
}
function ChipText({ text, focused }) {
  return /* @__PURE__ */ jsx10(TooltipTrigger, { children: /* @__PURE__ */ jsx10(CutText, { text, focused }) });
}
function CutText({ text, focused }) {
  const tip = useContext(TooltipTriggerStateContext);
  const ref = useRef2(null);
  const [pointed, setPointed] = useState3(false);
  const shown = useRef2({ pointed: false, focused: false });
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
  return /* @__PURE__ */ jsxs6(Fragment4, { children: [
    /* @__PURE__ */ jsx10(
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
    /* @__PURE__ */ jsx10(Tooltip2, { triggerRef: ref, children: text })
  ] });
}
function ComboBoxItem(props) {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : void 0);
  return /* @__PURE__ */ jsx10(ListBoxItem, { ...props, textValue, className: ROW, children: composeRenderProps(props.children, (children, { isSelected, selectionMode }) => /* @__PURE__ */ jsxs6(Fragment4, { children: [
    selectionMode === "multiple" ? /* @__PURE__ */ jsx10("span", { className: "bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent", children: isSelected ? /* @__PURE__ */ jsx10(Tick2, {}) : null }) : null,
    /* @__PURE__ */ jsx10(
      "span",
      {
        className: `bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${selectionMode === "single" && isSelected ? "bc:font-semibold" : ""}`,
        children
      }
    )
  ] })) });
}
function ComboBoxSection({ title, items, children, ...props }) {
  return /* @__PURE__ */ jsxs6(ListBoxSection, { ...props, children: [
    /* @__PURE__ */ jsx10(Header, { className: HEADING2, children: title }),
    /* @__PURE__ */ jsx10(Collection, { items, children })
  ] });
}
function Chevron2() {
  return /* @__PURE__ */ jsx10(
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
      children: /* @__PURE__ */ jsx10("polyline", { points: "3 4.5 6 7.5 9 4.5" })
    }
  );
}
function Tick2() {
  return /* @__PURE__ */ jsx10(
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
      children: /* @__PURE__ */ jsx10("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function Cross() {
  return /* @__PURE__ */ jsx10(
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
      children: /* @__PURE__ */ jsx10("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" })
    }
  );
}
var FIELD = "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var VALUES = "bc:flex bc:min-w-0 bc:flex-1 bc:flex-wrap bc:items-center bc:gap-1";
var INPUT = "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
var CLEAR = "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:hover:bg-bg-subtle bc:hover:text-text-default";
var OPENER = "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
var CHIP = "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-1.5 bc:pl-1.5 bc:data-allows-removing:pr-0.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
var CHIP_REMOVE = "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
var LAYOUT = { rowHeight: 32, headingHeight: 28, padding: 4 };
var LIST = "bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
var LISTBOX = "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:outline-none";
var HEADING2 = "bc:box-border bc:h-[28px] bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
var ROW = "bc:box-border bc:flex bc:h-[32px] bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
var NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";

// srcts/inputs/numberfield.tsx
import { useContext as useContext2, useEffect as useEffect2, useRef as useRef3, useState as useState4 } from "react";
import { Fragment as Fragment5, jsx as jsx11, jsxs as jsxs7 } from "react/jsx-runtime";
function NumberField2({ bound, outdated = false, maxValue, ...props }) {
  const largest = outdated ? void 0 : maxValue;
  return bound === void 0 ? /* @__PURE__ */ jsx11(Drawn2, { ...props, maxValue: largest }) : /* @__PURE__ */ jsx11(BoundNumber, { ...props, maxValue: largest, bound });
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
  const held2 = typeof bound.value === "number" ? bound.value : NaN;
  const [kept, setKept] = useState4(null);
  const [typed, setTyped] = useState4(NaN);
  const [keystrokes, setKeystrokes] = useState4(0);
  const [seen, setSeen] = useState4(held2);
  if (!Object.is(seen, held2)) {
    setSeen(held2);
    if (kept !== null && (!kept.typing || !Object.is(held2, typed))) setKept(null);
  }
  const fits = (value) => inRange(value, props.minValue, props.maxValue, props.step);
  return /* @__PURE__ */ jsxs7(Fragment5, { children: [
    /* @__PURE__ */ jsx11(
      Drawn2,
      {
        ...props,
        commitBehavior,
        validationBehavior,
        value: kept?.value ?? held2,
        onInput: (event) => {
          setKept((now) => now?.typing === true ? now : { value: now?.value ?? held2, typing: true });
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
            setKept(Object.is(value, held2) ? null : { value, typing: false });
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
        watch: /* @__PURE__ */ jsx11(
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
    /* @__PURE__ */ jsx11(Refusal, { error: bound.error })
  ] });
}
function Watch({ keystrokes, onType }) {
  const state = useContext2(NumberFieldStateContext);
  const value = state?.numberValue ?? NaN;
  const heard = useRef3(0);
  useEffect2(() => {
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
function Drawn2({ label, description, errorMessage: errorMessage2, placeholder, watch, className, onInput, ...props }) {
  return /* @__PURE__ */ jsxs7(NumberField, { ...props, className: composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx11(Label, { className: styles.label, children: label }),
    /* @__PURE__ */ jsxs7(Group, { className: FIELD2, children: [
      /* @__PURE__ */ jsx11(Input, { placeholder, onInput, className: INPUT2 }),
      /* @__PURE__ */ jsxs7("div", { className: STEPPERS, children: [
        /* @__PURE__ */ jsx11(Button, { slot: "increment", className: STEPPER, children: /* @__PURE__ */ jsx11(Chevron3, { up: true }) }),
        /* @__PURE__ */ jsx11(Button, { slot: "decrement", className: STEPPER, children: /* @__PURE__ */ jsx11(Chevron3, { up: false }) })
      ] })
    ] }),
    /* @__PURE__ */ jsx11(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx11(FieldError, { className: styles.alert, children: errorMessage2 }),
    watch
  ] });
}
function Chevron3({ up }) {
  return /* @__PURE__ */ jsx11(
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
      children: /* @__PURE__ */ jsx11("polyline", { points: up ? "3 7.5 6 4.5 9 7.5" : "3 4.5 6 7.5 9 4.5" })
    }
  );
}
var FIELD2 = "bc:flex bc:min-w-0 bc:items-stretch bc:justify-self-stretch bc:overflow-hidden bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
var INPUT2 = "bc:min-w-0 bc:flex-1 bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted";
var STEPPERS = "bc:flex bc:flex-col bc:border-l bc:border-border-default bc:group-data-invalid:border-border-danger";
var STEPPER = "bc:flex bc:flex-1 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:px-1 bc:py-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-pressed:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";

// srcts/inputs/textfield.tsx
import { Fragment as Fragment6, jsx as jsx12, jsxs as jsxs8 } from "react/jsx-runtime";
function TextField2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx12(Drawn3, { ...props }) : /* @__PURE__ */ jsx12(BoundText, { ...props, bound });
}
function BoundText({ bound, onChange, onKeyDown, onBlur, ...props }) {
  const [text, typed] = useTyped(bound);
  const lines = props.lines ?? 1;
  return /* @__PURE__ */ jsxs8(Fragment6, { children: [
    /* @__PURE__ */ jsx12(
      Drawn3,
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
    /* @__PURE__ */ jsx12(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, description, errorMessage: errorMessage2, placeholder, lines = 1, className, ...props }) {
  return /* @__PURE__ */ jsxs8(TextField, { ...props, className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`), children: [
    label === void 0 ? null : /* @__PURE__ */ jsx12(Label, { className: styles.label, children: label }),
    lines > 1 ? /* @__PURE__ */ jsx12(TextArea, { rows: lines, placeholder, className: styles.box }) : /* @__PURE__ */ jsx12(Input, { placeholder, className: styles.box }),
    /* @__PURE__ */ jsx12(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
    /* @__PURE__ */ jsx12(FieldError, { className: styles.alert, children: errorMessage2 })
  ] });
}

// srcts/layout/link.tsx
import { jsx as jsx13 } from "react/jsx-runtime";
function Link2({ variant = "primary", className, ...props }) {
  return /* @__PURE__ */ jsx13(Link, { ...props, className: composeRenderProps(className, (given) => `${LINK} ${VARIANTS[variant]} ${given ?? ""}`) });
}
var LINK = "bc:cursor-pointer bc:rounded-xs bc:underline bc:outline-none bc:transition bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:no-underline bc:forced-colors:data-disabled:text-[GrayText]";
var VARIANTS = {
  primary: "bc:text-blue-700 bc:decoration-blue-700/60 bc:data-hovered:decoration-blue-700 bc:dark:text-blue-400 bc:dark:decoration-blue-400/60 bc:dark:data-hovered:decoration-blue-400",
  secondary: "bc:text-text-default bc:decoration-current/50 bc:data-hovered:decoration-current"
};

// srcts/layout/breadcrumbs.tsx
import { Fragment as Fragment7, jsx as jsx14, jsxs as jsxs9 } from "react/jsx-runtime";
function Breadcrumbs2({ className, ...props }) {
  return /* @__PURE__ */ jsx14(Breadcrumbs, { ...props, className: `${BREADCRUMBS} ${className ?? ""}` });
}
function Breadcrumb2({ className, id, ...props }) {
  return /* @__PURE__ */ jsx14(Breadcrumb, { ...props, id, className: composeRenderProps(className, (given) => `${BREADCRUMB} ${given ?? ""}`), children: ({ isCurrent }) => /* @__PURE__ */ jsxs9(Fragment7, { children: [
    /* @__PURE__ */ jsx14(Link2, { variant: "secondary", ...props }),
    isCurrent ? null : /* @__PURE__ */ jsx14(Chevron4, {})
  ] }) });
}
function Chevron4() {
  return /* @__PURE__ */ jsx14(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      className: "bc:size-3 bc:shrink-0 bc:text-text-muted",
      children: /* @__PURE__ */ jsx14("path", { d: "M6 3.5l4.5 4.5L6 12.5" })
    }
  );
}
var BREADCRUMBS = "bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:items-center bc:gap-1 bc:p-0 bc:font-sans bc:text-sm";
var BREADCRUMB = "bc:flex bc:items-center bc:gap-1";

// srcts/layout/disclosure.tsx
import { useContext as useContext3 } from "react";
import { jsx as jsx15, jsxs as jsxs10 } from "react/jsx-runtime";
function Disclosure2({ className, ...props }) {
  return /* @__PURE__ */ jsx15(Disclosure, { ...props, className: composeRenderProps(className, (given) => `${DISCLOSURE} ${given ?? ""}`) });
}
function DisclosureHeader({ children, level = 3, className }) {
  const expanded = useContext3(DisclosureStateContext)?.isExpanded ?? false;
  return /* @__PURE__ */ jsx15(Heading, { level, className: `${HEADING3} ${className ?? FACE}`, children: /* @__PURE__ */ jsxs10(Button, { slot: "trigger", className: TRIGGER, children: [
    /* @__PURE__ */ jsx15(Marker, { expanded }),
    children
  ] }) });
}
function DisclosurePanel2({ className, ...props }) {
  return /* @__PURE__ */ jsx15(DisclosurePanel, { ...props, className: composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`) });
}
function Marker({ expanded }) {
  return /* @__PURE__ */ jsx15(
    "svg",
    {
      "aria-hidden": "true",
      viewBox: "0 0 16 16",
      fill: "currentColor",
      className: `bc:size-3 bc:shrink-0 bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`,
      children: /* @__PURE__ */ jsx15("path", { d: "M5.5 3.5 11 8l-5.5 4.5Z" })
    }
  );
}
var DISCLOSURE = "bc:font-sans bc:text-text-default";
var HEADING3 = "bc:m-0 bc:font-sans";
var FACE = "bc:text-base bc:font-semibold";
var TRIGGER = "bc:flex bc:w-full bc:cursor-pointer bc:items-center bc:gap-1.5 bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-left bc:text-inherit bc:[font:inherit] bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var PANEL = "bc:h-(--disclosure-panel-height) bc:overflow-clip bc:motion-safe:transition-[height]";

// srcts/layout/meter.tsx
import { Fragment as Fragment8, jsx as jsx16, jsxs as jsxs11 } from "react/jsx-runtime";
function Meter2({ label, className, ...props }) {
  return /* @__PURE__ */ jsx16(Meter, { ...props, className: composeRenderProps(className, (given) => `${METER} ${given ?? ""}`), children: ({ percentage, valueText }) => /* @__PURE__ */ jsxs11(Fragment8, { children: [
    /* @__PURE__ */ jsxs11("div", { className: "bc:flex bc:justify-between bc:gap-2", children: [
      /* @__PURE__ */ jsx16(Label, { className: styles.label, children: label }),
      /* @__PURE__ */ jsxs11("span", { className: `bc:text-sm ${percentage >= 80 ? "bc:text-text-danger" : "bc:text-text-muted"}`, children: [
        percentage >= 80 ? /* @__PURE__ */ jsx16(Alert, {}) : null,
        ` ${valueText ?? ""}`
      ] })
    ] }),
    /* @__PURE__ */ jsx16("div", { className: `${TRACK} bc:w-64`, children: /* @__PURE__ */ jsx16("div", { className: `${FILL} bc:left-0 ${fill(percentage)}`, style: { width: `${String(percentage)}%` } }) })
  ] }) });
}
function ProgressBar2({ label, className, ...props }) {
  return /* @__PURE__ */ jsx16(ProgressBar, { ...props, className: composeRenderProps(className, (given) => `${PROGRESS} ${given ?? ""}`), children: ({ percentage, valueText, isIndeterminate }) => /* @__PURE__ */ jsxs11(Fragment8, { children: [
    /* @__PURE__ */ jsxs11("div", { className: "bc:flex bc:justify-between bc:gap-2", children: [
      /* @__PURE__ */ jsx16(Label, { className: styles.label, children: label }),
      /* @__PURE__ */ jsx16("span", { className: "bc:text-sm bc:text-text-muted", children: valueText })
    ] }),
    /* @__PURE__ */ jsx16("div", { className: `${TRACK} bc:overflow-hidden`, children: /* @__PURE__ */ jsx16(
      "div",
      {
        className: `${FILL} bc:left-0 bc:bg-bg-accent ${isIndeterminate ? "bc:motion-safe:animate-[bc-progress-slide_1s_ease-out_infinite]" : ""}`,
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
  return /* @__PURE__ */ jsxs11(
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
        /* @__PURE__ */ jsx16("path", { d: "M8 1.8 14.6 13.6H1.4Z" }),
        /* @__PURE__ */ jsx16("path", { d: "M8 6.2v3.2M8 11.6v.1" })
      ]
    }
  );
}
var METER = "bc:flex bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
var PROGRESS = "bc:flex bc:w-64 bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
var TRACK = "bc:relative bc:h-2 bc:max-w-full bc:rounded-full bc:bg-border-default bc:outline bc:outline-1 bc:-outline-offset-1 bc:outline-transparent";
var FILL = "bc:absolute bc:top-0 bc:h-full bc:rounded-full bc:forced-colors:bg-[Highlight]";

// srcts/layout/tabs.tsx
import { Fragment as Fragment9, jsx as jsx17, jsxs as jsxs12 } from "react/jsx-runtime";
function Tabs2({ className, ...props }) {
  return /* @__PURE__ */ jsx17(
    Tabs,
    {
      ...props,
      className: composeRenderProps(className, (given, { orientation }) => `${TABS} ${ORIENTED[orientation]} ${given ?? ""}`)
    }
  );
}
function TabList2({ className, ...props }) {
  return /* @__PURE__ */ jsx17(
    TabList,
    {
      ...props,
      className: composeRenderProps(className, (given, { orientation }) => `${LIST2} ${LISTED[orientation]} ${given ?? ""}`)
    }
  );
}
function Tab2({ className, children, ...props }) {
  return /* @__PURE__ */ jsx17(Tab, { ...props, className: composeRenderProps(className, (given) => `${TAB} ${given ?? ""}`), children: composeRenderProps(children, (children2) => /* @__PURE__ */ jsxs12(Fragment9, { children: [
    children2,
    /* @__PURE__ */ jsx17(SelectionIndicator, { className: PILL })
  ] })) });
}
function TabPanel2({ className, ...props }) {
  return /* @__PURE__ */ jsx17(TabPanel, { ...props, className: composeRenderProps(className, (given) => `${PANEL2} ${given ?? ""}`) });
}
var TABS = "bc:flex bc:max-w-full bc:gap-4 bc:font-sans bc:text-text-default";
var ORIENTED = { horizontal: "bc:flex-col", vertical: "bc:flex-row" };
var LIST2 = "bc:-m-1 bc:flex bc:max-w-full bc:overflow-x-auto bc:overflow-y-clip bc:p-1 bc:[scrollbar-width:none]";
var LISTED = { horizontal: "bc:flex-row", vertical: "bc:flex-col bc:items-start" };
var TAB = "bc:relative bc:isolate bc:flex bc:cursor-default bc:items-center bc:rounded-full bc:px-3 bc:py-1.5 bc:font-sans bc:text-sm bc:font-medium bc:text-text-muted bc:outline-none bc:transition-colors bc:forced-color-adjust-none bc:[-webkit-tap-highlight-color:transparent] bc:data-hovered:text-text-default bc:data-selected:text-text-on-accent bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:forced-colors:data-selected:text-[HighlightText]";
var PILL = "bc:absolute bc:inset-0 bc:-z-1 bc:rounded-full bc:bg-bg-accent bc:motion-safe:transition-[translate,width,height] bc:forced-colors:bg-[Highlight]";
var PANEL2 = "bc:box-border bc:flex-1 bc:rounded bc:p-4 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";

// srcts/layout/toolbar.tsx
import { jsx as jsx18 } from "react/jsx-runtime";
function Toolbar2({ className, ...props }) {
  return /* @__PURE__ */ jsx18(
    Toolbar,
    {
      ...props,
      className: composeRenderProps(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED2[orientation]} ${given ?? ""}`)
    }
  );
}
function Separator2({ className, ...props }) {
  return /* @__PURE__ */ jsx18(Separator, { ...props, className: `${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}` });
}
var TOOLBAR = "bc:flex bc:flex-wrap bc:gap-2";
var ORIENTED2 = { horizontal: "bc:flex-row bc:items-center", vertical: "bc:flex-col bc:items-start" };
var SEPARATOR = "bc:m-0 bc:border-none bc:bg-border-default bc:forced-colors:bg-[ButtonBorder]";
var ACROSS = { horizontal: "bc:h-px bc:w-full", vertical: "bc:w-px bc:self-stretch" };
export {
  AlertDialog,
  Breadcrumb2 as Breadcrumb,
  Breadcrumbs2 as Breadcrumbs,
  Button2 as Button,
  Checkbox,
  ComboBox2 as ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  Dialog2 as Dialog,
  DialogTrigger,
  Disclosure2 as Disclosure,
  DisclosureGroup,
  DisclosureHeader,
  DisclosurePanel2 as DisclosurePanel,
  Field,
  Focusable,
  Heading,
  Keyboard,
  Link2 as Link,
  Menu2 as Menu,
  MenuItem2 as MenuItem,
  MenuSection2 as MenuSection,
  MenuTrigger2 as MenuTrigger,
  Meter2 as Meter,
  Modal2 as Modal,
  NumberField2 as NumberField,
  Popover2 as Popover,
  ProgressBar2 as ProgressBar,
  Separator2 as Separator,
  Sheet2 as Sheet,
  Stack,
  SubmenuTrigger2 as SubmenuTrigger,
  Tab2 as Tab,
  TabList2 as TabList,
  TabPanel2 as TabPanel,
  Tabs2 as Tabs,
  Text,
  TextField2 as TextField,
  Toast,
  UNSTABLE_ToastContent as ToastContent,
  UNSTABLE_ToastQueue as ToastQueue,
  ToastRegion,
  Toolbar2 as Toolbar,
  Tooltip2 as Tooltip,
  TooltipTrigger,
  outputRenderer,
  reactModule,
  useBoard,
  useNotified,
  useOutput,
  useRequest,
  useStatus,
  useValue
};
