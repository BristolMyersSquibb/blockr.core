var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __commonJS = (cb, mod) => function __require() {
  try {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  } catch (e) {
    throw mod = 0, e;
  }
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
  mod
));

// shared-react:react
var require_react = __commonJS({
  "shared-react:react"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react"];
  }
});

// shared-react:react/jsx-runtime
var require_jsx_runtime = __commonJS({
  "shared-react:react/jsx-runtime"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react/jsx-runtime"];
  }
});

// shared-react:react-dom
var require_react_dom = __commonJS({
  "shared-react:react-dom"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react-dom"];
  }
});

// shared-react:react-dom/client
var require_client = __commonJS({
  "shared-react:react-dom/client"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react-dom/client"];
  }
});

// srcts/modules/filebrowser-block/index.tsx
var import_react4 = __toESM(require_react(), 1);

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
  let typing = false;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || typing) return;
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
    typing = false;
    typed = void 0;
    if (value !== void 0) void submit(value);
  };
  const flush = () => {
    if (!typing) return;
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
    type(value, pause = true) {
      if (released) return;
      typing = true;
      typed = value;
      clearTimeout(timer);
      timer = pause ? setTimeout(() => {
        timer = void 0;
        send();
      }, PAUSE) : void 0;
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
function strings(value) {
  return Array.isArray(value) ? value.filter((x) => typeof x === "string") : [];
}

// srcts/inputs/inputs.tsx
var import_react = __toESM(require_react(), 1);

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

// srcts/inputs/inputs.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
function Stack({ children }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)("div", { className: styles.stack, children });
}
function Field({ label, children }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("label", { className: styles.field, children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: styles.label, children: label }),
    children
  ] });
}

// srcts/react/hooks.ts
var import_react2 = __toESM(require_react(), 1);
function useValue(host, key) {
  const name = key ?? ("option" in host ? host.option : "");
  const [value, setValue] = (0, import_react2.useState)(() => held(host, name));
  const [error, setError] = (0, import_react2.useState)(null);
  const binding = (0, import_react2.useRef)(null);
  (0, import_react2.useLayoutEffect)(() => {
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
  return (0, import_react2.useMemo)(
    () => ({
      value,
      error,
      get: () => binding.current?.get() ?? held(host, name),
      commit: (next) => binding.current?.commit(next) ?? Promise.resolve(false),
      type: (next, pause) => {
        binding.current?.type(next, pause);
      },
      flush: () => {
        binding.current?.flush();
      }
    }),
    [value, error, host, name]
  );
}
function useRequest(host, name, options = {}) {
  const [answered, setAnswered] = (0, import_react2.useState)({ answer: void 0, error: null });
  const reads = JSON.stringify([options.args ?? {}, options.reads ?? null]);
  (0, import_react2.useEffect)(() => {
    const [args, extra] = JSON.parse(reads);
    const followed = follow(host, name, {
      args: () => args,
      reads: () => extra,
      answer: (answer) => {
        setAnswered({ answer, error: null });
      },
      failed: (failure) => {
        setAnswered((last) => ({ answer: last.answer, error: messageOf(failure) }));
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
var import_react3 = __toESM(require_react(), 1);
var import_react_dom = __toESM(require_react_dom(), 1);
var import_client = __toESM(require_client(), 1);
var import_jsx_runtime2 = __toESM(require_jsx_runtime(), 1);
function reactModule(Draw, options = {}) {
  return {
    ...options.options === void 0 ? {} : { options: options.options },
    render({ el, host, signal }) {
      const root = (0, import_client.createRoot)(el);
      (0, import_react_dom.flushSync)(() => {
        root.render(/* @__PURE__ */ (0, import_jsx_runtime2.jsx)(Draw, { host, signal }));
      });
      return () => {
        root.unmount();
      };
    }
  };
}

// srcts/modules/filebrowser-block/index.tsx
var import_jsx_runtime3 = __toESM(require_jsx_runtime(), 1);
var api = 1;
var link = "bc:cursor-pointer bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-left bc:text-sm bc:text-inherit bc:[font-family:inherit] bc:hover:bg-current/5 bc:aria-current:font-semibold";
var entry = `${link} bc:justify-self-stretch`;
function FileBrowserBlock({ host }) {
  const shared = useValue(host, "volumes").value;
  const volumes = volumesOf(shared);
  const file = useValue(host, "file_path");
  const held2 = typeof file.value === "string" && file.value !== "" ? file.value : void 0;
  const [place, setPlace] = (0, import_react4.useState)(() => locate(file.value, volumes));
  const volume = place.volume !== void 0 && own(volumes, place.volume) !== void 0 ? place.volume : Object.keys(volumes)[0];
  const dirs = volume === place.volume ? place.dirs : [];
  const root = volume === void 0 ? void 0 : own(volumes, volume);
  const listed = useRequest(host, "list_dir", {
    args: { ...volume === void 0 ? {} : { volume }, path: dirs.join("/") },
    reads: shared ?? null
  });
  const [left, setLeft] = (0, import_react4.useState)(null);
  const loading = listed === left || listed.answer === void 0 && listed.error === null;
  const listing2 = listed.error === null ? listingOf(listed.answer) : { dirs: [], files: [] };
  const error = (loading ? null : listed.error) ?? file.error;
  const go = (next) => {
    if (next.volume === volume && jsonEqual(next.dirs, dirs)) return;
    setLeft(listed);
    setPlace(next);
  };
  const names = Object.keys(volumes);
  return /* @__PURE__ */ (0, import_jsx_runtime3.jsxs)(Stack, { children: [
    names.length > 1 && /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(Field, { label: "Volume", children: /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(
      "select",
      {
        className: styles.box,
        value: volume ?? "",
        onChange: (event) => {
          go({ volume: event.target.value, dirs: [] });
        },
        children: names.map((name) => /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("option", { value: name, children: name }, name))
      }
    ) }),
    /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("nav", { "aria-label": "Directory", className: "bc:flex bc:flex-wrap bc:items-center bc:gap-1 bc:text-sm", children: [volume ?? "", ...dirs].map((name, i) => /* @__PURE__ */ (0, import_jsx_runtime3.jsxs)(import_react4.Fragment, { children: [
      i > 0 && /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("span", { className: "bc:opacity-50", "aria-hidden": "true", children: "/" }),
      /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(
        "button",
        {
          type: "button",
          className: link,
          "aria-current": i === dirs.length ? "true" : void 0,
          onClick: () => {
            go({ volume, dirs: dirs.slice(0, i) });
          },
          children: name
        }
      )
    ] }, i)) }),
    /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("ul", { className: "bc:m-0 bc:grid bc:max-h-64 bc:list-none bc:gap-0.5 bc:overflow-y-auto bc:p-0", children: loading ? /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(Note, { text: "Loading\u2026" }) : /* @__PURE__ */ (0, import_jsx_runtime3.jsxs)(import_jsx_runtime3.Fragment, { children: [
      dirs.length > 0 && /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(
        Entry,
        {
          text: "..",
          onClick: () => {
            go({ volume, dirs: dirs.slice(0, -1) });
          }
        }
      ),
      listing2.dirs.map((dir) => /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(
        Entry,
        {
          text: `${dir}/`,
          onClick: () => {
            go({ volume, dirs: [...dirs, dir] });
          }
        },
        `dir ${dir}`
      )),
      listing2.files.map((name) => {
        const at = root === void 0 ? void 0 : join(root, [...dirs, name]);
        return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(
          Entry,
          {
            text: name,
            current: at !== void 0 && at === held2,
            onClick: () => {
              if (at !== void 0) void file.commit(at);
            }
          },
          `file ${name}`
        );
      }),
      listing2.dirs.length + listing2.files.length === 0 && /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(Note, { text: "This directory is empty." })
    ] }) }),
    /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("p", { className: styles.hint, children: held2 === void 0 ? "No file chosen yet." : `Chosen: ${held2}` }),
    error !== null && /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("p", { role: "alert", className: styles.alert, children: error })
  ] });
}
function Entry({ text, current = false, onClick }) {
  return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("li", { className: "bc:grid", children: /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("button", { type: "button", className: entry, "aria-current": current ? "true" : void 0, onClick, children: text }) });
}
function Note({ text }) {
  return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("li", { className: "bc:grid", children: /* @__PURE__ */ (0, import_jsx_runtime3.jsx)("p", { className: styles.hint, children: text }) });
}
var filebrowser_block_default = reactModule(FileBrowserBlock);
function locate(file, roots) {
  if (typeof file === "string") {
    for (const [name, root] of Object.entries(roots)) {
      const prefix = `${join(root, [])}/`;
      if (file.startsWith(prefix)) {
        return { volume: name, dirs: file.slice(prefix.length).split("/").slice(0, -1) };
      }
    }
  }
  return { volume: Object.keys(roots)[0], dirs: [] };
}
function volumesOf(volumes) {
  if (!isObject(volumes)) return {};
  return Object.fromEntries(
    Object.entries(volumes).filter((pair) => typeof pair[1] === "string")
  );
}
function join(root, names) {
  return [root.replace(/\/+$/, ""), ...names].join("/");
}
function listingOf(answer) {
  return isObject(answer) ? { dirs: strings(answer.dirs), files: strings(answer.files) } : { dirs: [], files: [] };
}
export {
  api,
  filebrowser_block_default as default
};
