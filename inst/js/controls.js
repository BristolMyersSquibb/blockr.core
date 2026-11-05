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
function choicesOf(values) {
  return values.map((value) => ({ value, label: value }));
}
function strings(value) {
  return Array.isArray(value) ? value.filter((x) => typeof x === "string") : [];
}

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:text-sm bc:text-text-default bc:[font-family:inherit] bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/controls/controls.ts
function controls(host, signal) {
  const el = document.createElement("div");
  el.className = styles.stack;
  const line = document.createElement("p");
  line.setAttribute("role", "alert");
  line.className = styles.alert;
  line.hidden = true;
  el.append(line);
  const bindings = [];
  const followed = [];
  signal.addEventListener("abort", () => {
    for (const request of followed) request.release();
  }, { once: true });
  function add(node) {
    line.before(node);
  }
  function alert(error) {
    line.textContent = error === null ? "" : messageOf(error);
    line.hidden = error === null;
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
    const lines = options.lines ?? 1;
    const input = lines > 1 ? document.createElement("textarea") : document.createElement("input");
    if (input instanceof HTMLTextAreaElement) input.rows = lines;
    else input.type = "text";
    input.className = styles.box;
    input.spellcheck = false;
    if (options.placeholder !== void 0) input.placeholder = options.placeholder;
    const binding = bind2(key, (value) => {
      const shown = typeof value === "string" ? value : "";
      if (input.value !== shown) input.value = shown;
    });
    typing(input, binding, lines > 1, () => input.value);
    add(field(options.label, input));
  }
  function number(key, options) {
    const input = document.createElement("input");
    input.type = "number";
    input.step = "1";
    input.required = true;
    input.className = styles.box;
    if (options.min !== void 0) input.min = String(options.min);
    let max;
    const read = () => {
      const text2 = input.value.trim();
      const value = text2 === "" ? Number.NaN : Number(text2);
      if (!Number.isInteger(value)) return void 0;
      if (options.min !== void 0 && value < options.min) return void 0;
      if (max !== void 0 && value > max) return void 0;
      return value;
    };
    const mark = () => {
      input.setAttribute("aria-invalid", String(read() === void 0));
    };
    const binding = bind2(key, (value) => {
      const shown = typeof value === "number" ? String(value) : "";
      if (input.value !== shown) input.value = shown;
      mark();
    });
    typing(input, binding, false, read);
    input.addEventListener("input", mark);
    add(field(options.label, input));
    return {
      setMax(next) {
        max = next;
        if (next === void 0) input.removeAttribute("max");
        else input.max = String(next);
        mark();
      }
    };
  }
  function select(key, options) {
    const select2 = document.createElement("select");
    select2.className = styles.box;
    let choices = options.choices ?? [];
    let current;
    let built = "";
    const draw = () => {
      const shown = current === void 0 || choices.some((choice) => choice.value === current) ? choices : [...choices, { value: current, label: current }];
      const drawn = JSON.stringify([current === void 0, shown]);
      if (drawn !== built) {
        built = drawn;
        select2.replaceChildren(
          ...current === void 0 ? [option("", options.placeholder, true)] : [],
          ...shown.map((choice) => option(choice.value, choice.label))
        );
      }
      select2.value = current ?? "";
    };
    const binding = bind2(key, (value) => {
      current = typeof value === "string" ? value : void 0;
      draw();
    });
    select2.addEventListener("change", () => {
      void binding.commit(select2.value);
    });
    add(field(options.label, select2));
    return {
      setChoices(next) {
        choices = next;
        draw();
      }
    };
  }
  function checkbox(key, options) {
    const input = document.createElement("input");
    input.type = "checkbox";
    input.className = styles.checkbox;
    const binding = bind2(key, (value) => {
      input.checked = value === true;
    });
    input.addEventListener("change", () => {
      void binding.commit(input.checked);
    });
    const label = document.createElement("label");
    label.className = styles.check;
    label.append(input, options.label);
    add(label);
  }
  function checklist(key, options) {
    const fieldset = document.createElement("fieldset");
    fieldset.className = styles.fieldset;
    const legend = document.createElement("legend");
    legend.className = styles.legend;
    legend.textContent = options.legend;
    const list = document.createElement("div");
    list.className = styles.list;
    fieldset.append(legend, list);
    let choices = options.choices ?? [];
    let known = options.choices !== void 0;
    let held = [];
    let built;
    const boxes = /* @__PURE__ */ new Map();
    const draw = () => {
      const shown = [...choices, ...held.filter((value) => !choices.includes(value))];
      if (built === void 0 || !jsonEqual(built, shown)) {
        built = shown;
        boxes.clear();
        list.replaceChildren(...shown.map((value) => box(value)));
      }
      if (known && options.empty !== void 0 && shown.length === 0 && list.childElementCount === 0) {
        list.append(hint(options.empty));
      }
      for (const [value, input] of boxes) input.checked = held.includes(value);
    };
    function box(value) {
      const input = document.createElement("input");
      input.type = "checkbox";
      input.value = value;
      input.className = styles.checkbox;
      input.addEventListener("change", () => {
        const now = strings(binding.get());
        const next = input.checked ? now.includes(value) ? now : [...now, value] : now.filter((other) => other !== value);
        void binding.commit(
          options.ordered === true ? [...choices.filter((choice) => next.includes(choice)), ...next.filter((other) => !choices.includes(other))] : next
        );
      });
      boxes.set(value, input);
      const label = document.createElement("label");
      label.className = styles.check;
      label.append(input, value);
      return label;
    }
    const binding = bind2(key, (value) => {
      held = strings(value);
      draw();
    });
    add(fieldset);
    return {
      setChoices(next) {
        choices = next;
        known = true;
        built = void 0;
        draw();
      }
    };
  }
  function cleanup() {
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
  }
  return { el, add, get, bind: bind2, text, number, select, checkbox, checklist, follow: follow2, alert, cleanup };
}
function field(text, control) {
  const label = document.createElement("label");
  label.className = styles.field;
  const span = document.createElement("span");
  span.className = styles.label;
  span.textContent = text;
  label.append(span, control);
  return label;
}
function hint(text) {
  const p = document.createElement("p");
  p.className = styles.hint;
  p.textContent = text;
  return p;
}
function typing(input, binding, multiline, read) {
  input.addEventListener("input", () => {
    binding.type(read());
  });
  input.addEventListener("keydown", (event) => {
    if (event.key !== "Enter" || event.isComposing) return;
    if (multiline && !(event.ctrlKey || event.metaKey)) return;
    binding.flush();
  });
  input.addEventListener("blur", () => {
    binding.flush();
  });
}
function option(value, label, placeholder = false) {
  const option2 = document.createElement("option");
  option2.value = value;
  option2.textContent = label;
  if (placeholder) option2.disabled = true;
  return option2;
}
export {
  PAUSE,
  choicesOf,
  controls,
  field,
  hint,
  strings,
  styles
};
