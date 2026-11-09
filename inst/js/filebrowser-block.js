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

// srcts/modules/filebrowser-block/index.tsx
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
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
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

// srcts/modules/filebrowser-block/index.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
var link = "bc:cursor-pointer bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-left bc:text-sm bc:text-inherit bc:[font-family:inherit] bc:hover:bg-bg-subtle bc:aria-current:font-semibold";
var entry = `${link} bc:justify-self-stretch`;
function FileBrowserBlock({ host }) {
  const shared = (0, import_react2.useValue)(host, "volumes").value;
  const volumes = volumesOf(shared);
  const file = (0, import_react2.useValue)(host, "file_path");
  const held = typeof file.value === "string" && file.value !== "" ? file.value : void 0;
  const [place, setPlace] = (0, import_react.useState)(() => locate(file.value, volumes));
  const volume = place.volume !== void 0 && own(volumes, place.volume) !== void 0 ? place.volume : Object.keys(volumes)[0];
  const dirs = volume === place.volume ? place.dirs : [];
  const root = volume === void 0 ? void 0 : own(volumes, volume);
  const listed = (0, import_react2.useRequest)(host, "list_dir", {
    args: { ...volume === void 0 ? {} : { volume }, path: dirs.join("/") },
    reads: shared ?? null
  });
  const [left, setLeft] = (0, import_react.useState)(null);
  const loading = listed === left || listed.answer === void 0 && listed.error === null;
  const listing2 = listed.error === null ? listingOf(listed.answer) : { dirs: [], files: [] };
  const error = (loading ? null : listed.error) ?? file.error;
  const go = (next) => {
    if (next.volume === volume && jsonEqual(next.dirs, dirs)) return;
    setLeft(listed);
    setPlace(next);
  };
  const names = Object.keys(volumes);
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_react2.Stack, { children: [
    names.length > 1 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)(import_react2.Field, { label: "Volume", children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
      "select",
      {
        className: styles.box,
        value: volume ?? "",
        onChange: (event) => {
          go({ volume: event.target.value, dirs: [] });
        },
        children: names.map((name) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)("option", { value: name, children: name }, name))
      }
    ) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("nav", { "aria-label": "Directory", className: "bc:flex bc:flex-wrap bc:items-center bc:gap-1 bc:text-sm", children: [volume ?? "", ...dirs].map((name, i) => /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_react.Fragment, { children: [
      i > 0 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: "bc:text-text-muted", "aria-hidden": "true", children: "/" }),
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
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
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("ul", { className: "bc:m-0 bc:grid bc:max-h-64 bc:list-none bc:gap-0.5 bc:overflow-y-auto bc:p-0", children: loading ? /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Note, { text: "Loading\u2026" }) : /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_jsx_runtime.Fragment, { children: [
      dirs.length > 0 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
        Entry,
        {
          text: "..",
          onClick: () => {
            go({ volume, dirs: dirs.slice(0, -1) });
          }
        }
      ),
      listing2.dirs.map((dir) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
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
        return /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
          Entry,
          {
            text: name,
            current: at !== void 0 && at === held,
            onClick: () => {
              if (at !== void 0) void file.commit(at);
            }
          },
          `file ${name}`
        );
      }),
      listing2.dirs.length + listing2.files.length === 0 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Note, { text: "This directory is empty." })
    ] }) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { className: styles.hint, children: held === void 0 ? "No file chosen yet." : `Chosen: ${held}` }),
    error !== null && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { role: "alert", className: styles.alert, children: error })
  ] });
}
function Entry({ text, current = false, onClick }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)("li", { className: "bc:grid", children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)("button", { type: "button", className: entry, "aria-current": current ? "true" : void 0, onClick, children: text }) });
}
function Note({ text }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)("li", { className: "bc:grid", children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { className: styles.hint, children: text }) });
}
var filebrowser_block_default = (0, import_react2.reactModule)(FileBrowserBlock);
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
  return isObject(answer) ? { dirs: (0, import_binding.strings)(answer.dirs), files: (0, import_binding.strings)(answer.files) } : { dirs: [], files: [] };
}
export {
  api,
  filebrowser_block_default as default
};
