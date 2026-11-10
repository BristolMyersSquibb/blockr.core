// srcts/controls/controls.tsx
import { Fragment as Fragment2, useLayoutEffect, useRef } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function messageOf(error) {
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
  const source = sourceOf(host);
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
  const source = sourceOf(host);
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
function sourceOf(host) {
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
  max
}) {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "number" ? String(bound.value) : "");
  const read = (input) => {
    const value = input.trim() === "" ? Number.NaN : Number(input);
    if (!Number.isInteger(value)) return void 0;
    if (min !== void 0 && value < min) return void 0;
    if (max !== void 0 && value > max) return void 0;
    return value;
  };
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(Field, { label, children: /* @__PURE__ */ jsx(
      "input",
      {
        type: "number",
        step: 1,
        min,
        max,
        required: true,
        className: styles.box,
        value: shown,
        "aria-invalid": read(shown) === void 0,
        onChange: (event) => {
          typed(event.target.value);
          bound.type(read(event.target.value));
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
        },
        onBlur: () => {
          bound.flush();
        }
      }
    ) }),
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
  ] });
}
function Select({
  bound,
  label,
  placeholder,
  choices
}) {
  const current = typeof bound.value === "string" ? bound.value : void 0;
  const known = choices ?? [];
  const lacked = current !== void 0 && !known.some((choice) => choice.value === current);
  const invalid = lacked && choices !== void 0;
  const shown = lacked ? [...known, { value: current, label: invalid ? missing(current) : current }] : known;
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx(Field, { label, children: /* @__PURE__ */ jsxs(
      "select",
      {
        className: styles.box,
        value: current ?? "",
        "aria-invalid": invalid,
        onChange: (event) => {
          void bound.commit(event.target.value);
        },
        children: [
          current === void 0 && /* @__PURE__ */ jsx("option", { value: "", disabled: true, children: placeholder }),
          shown.map((choice) => /* @__PURE__ */ jsx("option", { value: choice.value, children: choice.label }, choice.value))
        ]
      }
    ) }),
    /* @__PURE__ */ jsx(Refusal, { error: bound.error })
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
function Checklist({
  bound,
  legend,
  choices,
  empty,
  ordered = false
}) {
  const held = strings(bound.value);
  const known = choices ?? [];
  const shown = [...known, ...held.filter((value) => !known.includes(value))];
  const toggle = (value, checked) => {
    const now = strings(bound.get());
    const next = checked ? now.includes(value) ? now : [...now, value] : now.filter((other) => other !== value);
    void bound.commit(
      ordered ? [...known.filter((choice) => next.includes(choice)), ...next.filter((other) => !known.includes(other))] : next
    );
  };
  return /* @__PURE__ */ jsxs("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ jsx("legend", { className: styles.legend, children: legend }),
    /* @__PURE__ */ jsxs("div", { className: styles.list, children: [
      shown.map((value) => {
        const invalid = choices !== void 0 && !known.includes(value);
        return /* @__PURE__ */ jsxs("label", { className: styles.check, children: [
          /* @__PURE__ */ jsx(
            "input",
            {
              type: "checkbox",
              className: styles.checkbox,
              value,
              checked: held.includes(value),
              "aria-invalid": invalid,
              onChange: (event) => {
                toggle(value, event.target.checked);
              }
            }
          ),
          invalid ? missing(value) : value
        ] }, value);
      }),
      choices !== void 0 && empty !== void 0 && shown.length === 0 && /* @__PURE__ */ jsx("p", { className: styles.hint, children: empty })
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

// srcts/controls/controls.tsx
import { Fragment as Fragment3, jsx as jsx2, jsxs as jsxs2 } from "react/jsx-runtime";
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
    const drawn = items.map((item) => /* @__PURE__ */ jsx2(Fragment2, { children: item.draw() }, item.id));
    flushSync(() => {
      root.render(
        /* @__PURE__ */ jsxs2(Fragment3, { children: [
          drawn,
          error === null ? null : /* @__PURE__ */ jsx2("p", { role: "alert", className: styles.alert, children: error })
        ] })
      );
    });
  };
  function put(draw) {
    items.push({ id: items.length, draw });
    redraw();
  }
  function add(node) {
    put(() => /* @__PURE__ */ jsx2(Slot, { node }));
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
          refusal = refused === null ? null : messageOf(refused);
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
    error = next === null ? null : messageOf(next);
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
  function follow2(name, options) {
    const request = follow(host, name, options);
    followed.push(request);
    return request;
  }
  function text(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx2(TextInput, { bound, ...options }));
  }
  function number(key, options) {
    let max;
    input(key, (bound) => /* @__PURE__ */ jsx2(NumberInput, { bound, label: options.label, min: options.min, max }));
    return {
      setMax(next) {
        max = next;
        redraw();
      }
    };
  }
  function select(key, options) {
    let choices = options.choices;
    input(key, (bound) => /* @__PURE__ */ jsx2(Select, { bound, label: options.label, placeholder: options.placeholder, choices }));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      }
    };
  }
  function checkbox(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx2(Checkbox, { bound, label: options.label }));
  }
  function checklist(key, options) {
    let choices = options.choices;
    input(key, (bound) => /* @__PURE__ */ jsx2(
      Checklist,
      {
        bound,
        legend: options.legend,
        choices,
        empty: options.empty,
        ordered: options.ordered
      }
    ));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      }
    };
  }
  function cleanup() {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }
  signal.addEventListener("abort", cleanup, { once: true });
  return { el, add, get, bind: bind2, text, number, select, checkbox, checklist, follow: follow2, alert, cleanup };
}
function Slot({ node }) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    ref.current?.replaceChildren(node);
  }, [node]);
  return /* @__PURE__ */ jsx2("div", { ref, className: "bc:contents" });
}
export {
  PAUSE,
  asChoices,
  controls,
  strings,
  styles
};
