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

// blockr.core-shared:react
var require_react = __commonJS({
  "blockr.core-shared:react"(exports, module) {
    var shared = globalThis["blockr.core-shared"];
    if (shared === void 0) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");
    module.exports = shared["react"];
  }
});

// blockr.core-shared:@blockr/core/binding
var require_binding = __commonJS({
  "blockr.core-shared:@blockr/core/binding"(exports, module) {
    var shared = globalThis["blockr.core-shared"];
    if (shared === void 0) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");
    module.exports = shared["@blockr/core/binding"];
  }
});

// blockr.core-shared:@blockr/core/react
var require_react2 = __commonJS({
  "blockr.core-shared:@blockr/core/react"(exports, module) {
    var shared = globalThis["blockr.core-shared"];
    if (shared === void 0) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");
    module.exports = shared["@blockr/core/react"];
  }
});

// blockr.core-shared:react/jsx-runtime
var require_jsx_runtime = __commonJS({
  "blockr.core-shared:react/jsx-runtime"(exports, module) {
    var shared = globalThis["blockr.core-shared"];
    if (shared === void 0) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");
    module.exports = shared["react/jsx-runtime"];
  }
});

// srcts/modules/upload-block/index.tsx
var import_react = __toESM(require_react(), 1);
var import_binding = __toESM(require_binding(), 1);
var import_react2 = __toESM(require_react2(), 1);

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/util/util.ts
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}

// srcts/modules/upload-block/index.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
function UploadBlock({ host }) {
  const path = (0, import_react2.useValue)(host, "file_path");
  const [uploading, setUploading] = (0, import_react.useState)(null);
  const [failed, setFailed] = (0, import_react.useState)(null);
  const sending = (0, import_react.useRef)(null);
  (0, import_react.useEffect)(
    () => () => {
      sending.current?.abort();
    },
    []
  );
  async function upload(file) {
    const controller = new AbortController();
    sending.current = controller;
    setUploading(file.name);
    setFailed(null);
    try {
      const [kept] = (0, import_binding.strings)(await host.request("upload", {}, { file, signal: controller.signal }));
      if (kept === void 0) throw new Error(`R kept ${file.name} but gave no path for it.`);
      await path.commit(kept);
    } catch (error2) {
      if (!controller.signal.aborted) setFailed(messageOf(error2));
    } finally {
      setUploading(null);
    }
  }
  const held = typeof path.value === "string" && path.value !== "" ? path.value : void 0;
  const error = failed ?? path.error;
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_react2.Stack, { children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(import_react2.Field, { label: "Upload data", children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
      "input",
      {
        type: "file",
        className: styles.box,
        disabled: uploading !== null,
        onChange: (event) => {
          const file = event.target.files?.[0];
          event.target.value = "";
          if (file !== void 0) void upload(file);
        }
      }
    ) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { className: styles.hint, children: uploading !== null ? `Uploading ${uploading}\u2026` : held !== void 0 ? `Uploaded ${basename(held)}` : "No file uploaded yet." }),
    error !== null && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { role: "alert", className: styles.alert, children: error })
  ] });
}
var upload_block_default = (0, import_react2.reactModule)(UploadBlock);
function basename(path) {
  return path.replace(/[\\/]+$/, "").split(/[\\/]/).pop() ?? path;
}
export {
  api,
  upload_block_default as default
};
