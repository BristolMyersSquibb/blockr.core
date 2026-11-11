// srcts/react/hooks.ts
import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import { bind, follow } from "@blockr/core/binding";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}
function report(error) {
  queueMicrotask(() => {
    throw error;
  });
}

// srcts/react/hooks.ts
var snapshots = /* @__PURE__ */ new WeakMap();
function useOutput(host) {
  const subscribe = useCallback((notify) => host.on("output", notify), [host]);
  const read = useCallback(() => snapshotOf(host), [host]);
  return useSyncExternalStore(subscribe, read);
}
function snapshotOf(host) {
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
        setError(refused === null ? null : messageOf(refused));
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
        setAnswered((last) => ({ answer: last.answer, error: messageOf(failure), loading: false }));
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

// srcts/react/module.tsx
import { Component } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

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

// srcts/react/module.tsx
import { jsx, jsxs } from "react/jsx-runtime";
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
    return /* @__PURE__ */ jsxs("div", { className: "bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default", "aria-busy": now.loading, children: [
      now.error === null ? null : /* @__PURE__ */ jsx(Alert, { message: now.error.message }),
      /* @__PURE__ */ jsx(Boundary, { output: now.output, children: /* @__PURE__ */ jsx("div", { className: `bc:flex bc:min-h-0 bc:flex-col ${now.loading ? "bc:opacity-60" : ""}`, children: /* @__PURE__ */ jsx(
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
      ) }) })
    ] });
  }
  return reactModule(Renderer, options);
}
function Alert({ message }) {
  return /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: message });
}
var Boundary = class extends Component {
  state = { failed: null };
  static getDerivedStateFromError(error) {
    return { failed: messageOf(error) };
  }
  componentDidCatch(error) {
    report(error);
  }
  componentDidUpdate(before) {
    if (this.state.failed !== null && before.output !== this.props.output) {
      this.setState({ failed: null });
    }
  }
  render() {
    const { failed } = this.state;
    return failed === null ? this.props.children : /* @__PURE__ */ jsx(Alert, { message: `The renderer failed to draw the output: ${failed}` });
  }
};

// srcts/inputs/inputs.tsx
import { useState as useState2 } from "react";
import { strings } from "@blockr/core/binding";
import { Fragment, jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
function Stack({ children }) {
  return /* @__PURE__ */ jsx2("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ jsxs2("label", { className: styles.field, children: [
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
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
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
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
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
function Select({
  bound,
  label,
  placeholder,
  choices,
  outdated = false
}) {
  const current = typeof bound.value === "string" ? bound.value : void 0;
  const known = choices ?? [];
  const lacked = current !== void 0 && !known.some((choice) => choice.value === current);
  const invalid = lacked && choices !== void 0 && !outdated;
  const shown = lacked ? [...known, { value: current, label: invalid ? missing(current) : current }] : known;
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsx2(Field, { label, children: /* @__PURE__ */ jsxs2(
      "select",
      {
        className: styles.box,
        value: current ?? "",
        "aria-invalid": invalid,
        onChange: (event) => {
          void bound.commit(event.target.value);
        },
        children: [
          current === void 0 && /* @__PURE__ */ jsx2("option", { value: "", disabled: true, children: placeholder }),
          shown.map((choice) => /* @__PURE__ */ jsx2("option", { value: choice.value, children: choice.label }, choice.value))
        ]
      }
    ) }),
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
  ] });
}
function Checkbox({ bound, label }) {
  return /* @__PURE__ */ jsxs2(Fragment, { children: [
    /* @__PURE__ */ jsxs2("label", { className: styles.check, children: [
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
function Checklist({
  bound,
  legend,
  choices,
  empty,
  ordered = false,
  outdated = false
}) {
  const held2 = strings(bound.value);
  const known = choices ?? [];
  const shown = [...known, ...held2.filter((value) => !known.includes(value))];
  const toggle = (value, checked) => {
    const now = strings(bound.get());
    const next = checked ? now.includes(value) ? now : [...now, value] : now.filter((other) => other !== value);
    void bound.commit(
      ordered ? [...known.filter((choice) => next.includes(choice)), ...next.filter((other) => !known.includes(other))] : next
    );
  };
  return /* @__PURE__ */ jsxs2("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ jsx2("legend", { className: styles.legend, children: legend }),
    /* @__PURE__ */ jsxs2("div", { className: styles.list, children: [
      shown.map((value) => {
        const invalid = choices !== void 0 && !outdated && !known.includes(value);
        return /* @__PURE__ */ jsxs2("label", { className: styles.check, children: [
          /* @__PURE__ */ jsx2(
            "input",
            {
              type: "checkbox",
              className: styles.checkbox,
              value,
              checked: held2.includes(value),
              "aria-invalid": invalid,
              onChange: (event) => {
                toggle(value, event.target.checked);
              }
            }
          ),
          invalid ? missing(value) : value
        ] }, value);
      }),
      choices !== void 0 && empty !== void 0 && shown.length === 0 && /* @__PURE__ */ jsx2("p", { className: styles.hint, children: empty })
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
export {
  Checkbox,
  Checklist,
  Field,
  NumberInput,
  Select,
  Stack,
  TextInput,
  outputRenderer,
  reactModule,
  useOutput,
  useRequest,
  useValue
};
