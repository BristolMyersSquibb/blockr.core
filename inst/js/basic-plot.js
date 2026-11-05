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

// shared-react:react/jsx-runtime
var require_jsx_runtime = __commonJS({
  "shared-react:react/jsx-runtime"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react/jsx-runtime"];
  }
});

// srcts/modules/basic-plot/index.tsx
var import_react2 = __toESM(require_react(), 1);
var import_react_dom = __toESM(require_react_dom(), 1);
var import_client = __toESM(require_client(), 1);

// srcts/react/hooks.ts
var import_react = __toESM(require_react(), 1);

// srcts/util/util.ts
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

// srcts/react/hooks.ts
var snapshots = /* @__PURE__ */ new WeakMap();
function useOutput(host) {
  const subscribe = (0, import_react.useCallback)((notify) => host.on("output", notify), [host]);
  const read = (0, import_react.useCallback)(() => snapshotOf(host), [host]);
  return (0, import_react.useSyncExternalStore)(subscribe, read);
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

// srcts/modules/basic-plot/index.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
var renderer = {
  render({ el, host }) {
    const root = (0, import_client.createRoot)(el);
    (0, import_react_dom.flushSync)(() => {
      root.render(/* @__PURE__ */ (0, import_jsx_runtime.jsx)(PlotOutput, { host }));
    });
    return () => {
      root.unmount();
    };
  }
};
var basic_plot_default = renderer;
function PlotOutput({ host }) {
  const { output, body, loading, error } = useOutput(host);
  const { width, height } = isObject(output) ? output : {};
  const url = (0, import_react2.useMemo)(() => body === null ? null : URL.createObjectURL(body), [body]);
  (0, import_react2.useEffect)(
    () => () => {
      if (url !== null) URL.revokeObjectURL(url);
    },
    [url]
  );
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("div", { className: "bc:flex bc:flex-col bc:gap-2", "aria-busy": loading, children: [
    error !== null && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { role: "alert", className: "bc:m-0 bc:text-sm bc:text-red-600 bc:whitespace-pre-wrap", children: error.message }),
    url !== null && /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
      "img",
      {
        src: url,
        alt: "Plot",
        width: typeof width === "number" ? width : void 0,
        height: typeof height === "number" ? height : void 0,
        className: `bc:block bc:max-w-none ${loading ? "bc:opacity-60" : ""}`
      }
    )
  ] });
}
export {
  api,
  basic_plot_default as default
};
