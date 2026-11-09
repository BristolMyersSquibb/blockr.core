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

// blockr.core-shared:@blockr/core/binding
var require_binding = __commonJS({
  "blockr.core-shared:@blockr/core/binding"(exports, module) {
    var shared = globalThis["blockr.core-shared"];
    if (shared === void 0) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");
    module.exports = shared["@blockr/core/binding"];
  }
});

// blockr.core-shared:@blockr/core/react
var require_react = __commonJS({
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

// srcts/modules/dataset-block/index.tsx
var import_binding = __toESM(require_binding(), 1);
var import_react = __toESM(require_react(), 1);
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
function DatasetBlock({ host }) {
  const pkg = (0, import_react.useValue)(host, "package").value;
  const datasets = (0, import_react.useRequest)(host, "datasets", { reads: pkg ?? null }).answer;
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)(import_react.Stack, { children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
    import_react.Select,
    {
      bound: (0, import_react.useValue)(host, "dataset"),
      label: "Dataset",
      placeholder: "Choose a dataset",
      choices: (0, import_binding.choicesOf)((0, import_binding.strings)(datasets))
    }
  ) });
}
var dataset_block_default = (0, import_react.reactModule)(DatasetBlock);
export {
  api,
  dataset_block_default as default
};
