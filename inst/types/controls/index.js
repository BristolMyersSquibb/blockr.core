// srcts/controls/controls.tsx
import { Fragment, useLayoutEffect, useRef } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import * as inputs from "@blockr/ui";

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
  const held = source.value(key, change);
  const submitting = options.notifier === void 0 ? {} : { notifier: options.notifier };
  let shown;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || timer !== void 0) return;
    const value = held.get();
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
      submitted = held.submit(value, submitting);
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
    get: () => held.get(),
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

// srcts/controls/controls.tsx
import { jsx, jsxs } from "react/jsx-runtime";
var ALERT = "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap";
function controls(host, signal) {
  const el = document.createElement("div");
  const root = createRoot(el);
  const entries = [];
  const bindings = [];
  const followed = [];
  let error = null;
  let ended = false;
  const redraw = () => {
    if (ended) return;
    const drawn = entries.map((entry) => /* @__PURE__ */ jsx(Fragment, { children: entry.draw() }, entry.id));
    flushSync(() => {
      root.render(
        /* @__PURE__ */ jsxs(inputs.FormLayout, { children: [
          drawn,
          error === null ? null : /* @__PURE__ */ jsx("p", { role: "alert", className: ALERT, children: error })
        ] })
      );
    });
  };
  function put(draw) {
    entries.push({ id: entries.length, draw });
    redraw();
  }
  function add(node) {
    put(() => /* @__PURE__ */ jsx(Slot, { node }));
  }
  function bindTo(target, show, options) {
    const binding = typeof target === "string" ? bind(host, target, show, options) : bind(host, target.read, target.change, show, options);
    bindings.push(binding);
    return binding;
  }
  function input(target, draw) {
    let value;
    let refusal = null;
    let made = false;
    const binding = bindTo(
      target,
      (shown) => {
        value = shown;
        if (made) redraw();
      },
      {
        settled: (refused) => {
          refusal = refused === null ? null : errorMessage(refused);
          redraw();
        },
        // The input shows why the host refused what it submitted, so core
        // raises no notification of it.
        notifier: () => void 0
      }
    );
    made = true;
    put(
      () => draw({
        value,
        error: refusal,
        get: () => binding.get(),
        commit: (next) => binding.commit(next),
        unset: () => binding.unset(),
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
  function bind2(key, next, last) {
    const [target, show] = after(key, next, last);
    return bindTo(target, show, { settled: alert, notifier: () => void 0 });
  }
  function follow2(name, options) {
    const request = follow(host, name, options);
    followed.push(request);
    return request;
  }
  function text(key, next, last) {
    const [target, options] = after(key, next, last);
    input(target, (bound) => /* @__PURE__ */ jsx(inputs.TextField, { bound, spellCheck: "false", ...options }));
  }
  function number(key, next, last) {
    const [target, options] = after(key, next, last);
    let maxValue = options.maxValue;
    let outdated = false;
    input(target, (bound) => /* @__PURE__ */ jsx(inputs.NumberField, { bound, ...options, maxValue, outdated }));
    return {
      setMaxValue(next2) {
        maxValue = next2;
        redraw();
      },
      setOutdated(next2) {
        outdated = next2;
        redraw();
      }
    };
  }
  function comboBox(key, next, last) {
    const [target, options] = after(key, next, last);
    let items = options.items;
    let outdated = false;
    input(target, (bound) => /* @__PURE__ */ jsx(
      inputs.ComboBox,
      {
        bound,
        label: options.label,
        placeholder: options.placeholder,
        defaultItems: items,
        selectionMode: options.selectionMode ?? "single",
        ordered: options.ordered,
        isRequired: options.isRequired,
        renderEmptyState: options.renderEmptyState,
        outdated,
        children: (item) => /* @__PURE__ */ jsx(inputs.ComboBoxItem, { id: item.id, children: item.name ?? item.id })
      }
    ));
    return {
      setItems(next2) {
        items = next2;
        redraw();
      },
      setOutdated(next2) {
        outdated = next2;
        redraw();
      }
    };
  }
  function checkbox(key, next, last) {
    const [target, options] = after(key, next, last);
    input(target, (bound) => /* @__PURE__ */ jsx(inputs.Checkbox, { bound, children: options.label }));
  }
  function cleanup() {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }
  signal.addEventListener("abort", cleanup, { once: true });
  return { el, add, get, bind: bind2, text, number, comboBox, checkbox, follow: follow2, alert, cleanup };
}
function after(key, next, last) {
  return typeof key === "function" ? [{ read: key, change: next }, last] : [key, next];
}
function Slot({ node }) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    ref.current?.replaceChildren(node);
  }, [node]);
  return /* @__PURE__ */ jsx("div", { ref, className: "bc:contents" });
}
export {
  PAUSE,
  controls,
  strings
};
