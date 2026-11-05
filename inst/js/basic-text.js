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

// srcts/react/hooks.ts
var import_react = __toESM(require_react(), 1);

// srcts/util/util.ts
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

// srcts/react/module.ts
var import_react2 = __toESM(require_react(), 1);
var import_react_dom = __toESM(require_react_dom(), 1);
var import_client = __toESM(require_client(), 1);
function reactModule(Draw, options = {}) {
  return {
    ...options.options === void 0 ? {} : { options: options.options },
    render({ el, host, signal }) {
      const root = (0, import_client.createRoot)(el);
      (0, import_react_dom.flushSync)(() => {
        root.render((0, import_react2.createElement)(Draw, { host, signal }));
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
    const drawn = (0, import_react2.createElement)(Draw, {
      output: now.output,
      body: now.body,
      params: now.params,
      options: now.options,
      loading: now.loading,
      error: now.error,
      host
    });
    return (0, import_react2.createElement)(
      "div",
      { className: "bc:flex bc:max-h-full bc:flex-col bc:gap-2", "aria-busy": now.loading },
      now.error === null ? null : (0, import_react2.createElement)(Alert, { message: now.error.message }),
      (0, import_react2.createElement)(
        Boundary,
        { output: now.output },
        (0, import_react2.createElement)("div", { className: `bc:flex bc:min-h-0 bc:flex-col ${now.loading ? "bc:opacity-60" : ""}` }, drawn)
      )
    );
  }
  return reactModule(Renderer, options);
}
function Alert({ message }) {
  return (0, import_react2.createElement)("p", { role: "alert", className: "bc:m-0 bc:text-sm bc:text-red-600 bc:whitespace-pre-wrap" }, message);
}
var Boundary = class extends import_react2.Component {
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
    return failed === null ? this.props.children : (0, import_react2.createElement)(Alert, { message: `The renderer failed to draw the output: ${failed}` });
  }
};

// srcts/modules/basic-text/index.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
var basic_text_default = outputRenderer(({ output }) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
  "pre",
  {
    tabIndex: 0,
    className: "bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-current/5 bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre",
    children: output.text
  }
));
export {
  api,
  basic_text_default as default
};
