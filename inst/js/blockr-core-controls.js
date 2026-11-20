// srcts/controls/controls.tsx
import { Fragment, useLayoutEffect, useRef } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { bind as bindValue, follow as followRequest } from "@blockr/core/binding";
import * as inputs from "@blockr/core/react";

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

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}

// srcts/controls/controls.tsx
import { Fragment as Fragment2, jsx, jsxs } from "react/jsx-runtime";
function controls(host, signal) {
  const el = document.createElement("div");
  el.className = styles.stack;
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
        /* @__PURE__ */ jsxs(Fragment2, { children: [
          drawn,
          error === null ? null : /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: error })
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
    const binding = typeof target === "string" ? bindValue(host, target, show, options) : bindValue(host, target.read, target.change, show, options);
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
  function bind(key, next, last) {
    const [target, show] = after(key, next, last);
    return bindTo(target, show, { settled: alert, notifier: () => void 0 });
  }
  function follow(name, options) {
    const request = followRequest(host, name, options);
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
    input(target, (bound) => /* @__PURE__ */ jsx(inputs.Checkbox, { bound, label: options.label }));
  }
  function cleanup() {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }
  signal.addEventListener("abort", cleanup, { once: true });
  return { el, add, get, bind, text, number, comboBox, checkbox, follow, alert, cleanup };
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

// srcts/controls/index.ts
import { PAUSE, strings } from "@blockr/core/binding";
export {
  PAUSE,
  controls,
  strings,
  styles
};
