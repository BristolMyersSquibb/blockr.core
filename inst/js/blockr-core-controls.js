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
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}

// srcts/controls/controls.tsx
import { Fragment as Fragment2, jsx, jsxs } from "react/jsx-runtime";
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
    const drawn = items.map((item) => /* @__PURE__ */ jsx(Fragment, { children: item.draw() }, item.id));
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
    items.push({ id: items.length, draw });
    redraw();
  }
  function add(node) {
    put(() => /* @__PURE__ */ jsx(Slot, { node }));
  }
  function input(key, draw) {
    let value;
    let refusal = null;
    let made = false;
    const binding = bindValue(
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
  function bind(key, show) {
    const binding = bindValue(host, key, show, { settled: alert });
    bindings.push(binding);
    return binding;
  }
  function follow(name, options) {
    const request = followRequest(host, name, options);
    followed.push(request);
    return request;
  }
  function text(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx(inputs.TextInput, { bound, ...options }));
  }
  function number(key, options) {
    let max;
    input(key, (bound) => /* @__PURE__ */ jsx(inputs.NumberInput, { bound, label: options.label, min: options.min, max }));
    return {
      setMax(next) {
        max = next;
        redraw();
      }
    };
  }
  function select(key, options) {
    let choices = options.choices;
    input(key, (bound) => /* @__PURE__ */ jsx(inputs.Select, { bound, label: options.label, placeholder: options.placeholder, choices }));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      }
    };
  }
  function checkbox(key, options) {
    input(key, (bound) => /* @__PURE__ */ jsx(inputs.Checkbox, { bound, label: options.label }));
  }
  function checklist(key, options) {
    let choices = options.choices;
    input(key, (bound) => /* @__PURE__ */ jsx(
      inputs.Checklist,
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
  return { el, add, get, bind, text, number, select, checkbox, checklist, follow, alert, cleanup };
}
function Slot({ node }) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    ref.current?.replaceChildren(node);
  }, [node]);
  return /* @__PURE__ */ jsx("div", { ref, className: "bc:contents" });
}

// srcts/controls/index.ts
import { asChoices, PAUSE, strings } from "@blockr/core/binding";
export {
  PAUSE,
  asChoices,
  controls,
  strings,
  styles
};
