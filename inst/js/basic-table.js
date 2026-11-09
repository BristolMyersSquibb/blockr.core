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

// srcts/modules/basic-table/index.tsx
var import_react = __toESM(require_react(), 1);
var import_react2 = __toESM(require_react2(), 1);

// srcts/modules/basic-table/table.ts
var NUMBERS = /* @__PURE__ */ new Set(["numeric", "integer"]);
function isNumber(column) {
  return NUMBERS.has(column.class);
}
function pageOf(table, params, nRows) {
  const rows = table.rows.length;
  const size = params.page_size ?? countOf(nRows) ?? Math.max(rows, 1);
  const page = params.page ?? 1;
  const last = table.total === null ? null : Math.max(1, Math.ceil(table.total / size));
  return {
    page,
    first: (page - 1) * size + 1,
    rows,
    last,
    more: last === null ? rows > 0 && rows >= size : page < last
  };
}
function rowsText(table, page) {
  if (page.rows === 0) return "No rows";
  if (table.total !== null && page.last === 1) {
    return `${count(table.total)} ${table.total === 1 ? "row" : "rows"}`;
  }
  const of = table.total === null ? "" : ` of ${count(table.total)}`;
  return `Rows ${count(page.first)}\u2013${count(page.first + page.rows - 1)}${of}`;
}
function pageText(page) {
  return page.last === null ? `Page ${String(page.page)}` : `Page ${String(page.page)} of ${String(page.last)}`;
}
function paramsFor(page, sort, params) {
  const asked = {};
  if (page > 1) asked.page = page;
  if (params.page_size !== void 0) asked.page_size = params.page_size;
  if (sort !== void 0) asked.sort = sortOf(sort.column, sort.desc === true);
  return asked;
}
function nextSort(sort, column) {
  if (sort?.column !== column) return sortOf(column, false);
  return sort.desc === true ? void 0 : sortOf(column, true);
}
function sortedColumn(table, sort) {
  return sort === void 0 ? -1 : table.columns.findIndex((column) => column.name === sort.column);
}
function retry(params, tried) {
  if (tried < 1 && (params.page ?? 1) > 1) return { tried: 1, sort: params.sort };
  if (tried < 2 && params.sort !== void 0) return { tried: 2, sort: void 0 };
  return void 0;
}
function cellText(value) {
  if (value === null) return null;
  if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
  return typeof value === "object" ? JSON.stringify(value) : String(value);
}
function sortOf(column, desc) {
  return desc ? { column, desc: true } : { column };
}
function count(n) {
  return n.toLocaleString("en-US");
}
function countOf(value) {
  return typeof value === "number" && Number.isInteger(value) && value >= 1 ? value : void 0;
}

// srcts/modules/basic-table/index.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
var api = 1;
var basic_table_default = (0, import_react2.outputRenderer)(Table, { options: ["n_rows"] });
function Table({ output, params, options, error, host }) {
  const page = pageOf(output, params, options.n_rows);
  const sorted = sortedColumn(output, params.sort);
  const ask = (to, sort) => {
    host.setParams(paramsFor(to, sort, params));
  };
  const tried = (0, import_react.useRef)(0);
  (0, import_react.useEffect)(() => {
    if (error === null) {
      tried.current = 0;
      return;
    }
    const again = retry(params, tried.current);
    if (again === void 0) return;
    tried.current = again.tried;
    host.setParams(paramsFor(1, again.sort, params));
  }, [host, error, params]);
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("div", { className: "bc:flex bc:min-h-0 bc:flex-col bc:gap-2", children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("div", { className: "bc:min-h-0 bc:overflow-auto", children: /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("table", { className: "bc:w-full bc:border-collapse bc:text-left bc:text-sm", children: [
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)("thead", { children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)("tr", { children: output.columns.map((column, j) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
        Header,
        {
          column,
          sort: j === sorted ? params.sort : void 0,
          onSort: (sort) => {
            ask(1, sort);
          }
        },
        j
      )) }) }),
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)("tbody", { children: output.rows.map((row, i) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)("tr", { children: output.columns.map((column, j) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Cell, { column, value: row[j] ?? null }, j)) }, i)) })
    ] }) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
      Pager,
      {
        table: output,
        page,
        onPage: (to) => {
          ask(to, params.sort);
        }
      }
    )
  ] });
}
function Header(props) {
  const { column, sort, onSort } = props;
  const number = isNumber(column);
  const order = sort === void 0 ? "none" : sort.desc === true ? "descending" : "ascending";
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)("th", { scope: "col", "aria-sort": order, className: "bc:border-b bc:border-border-default bc:px-2 bc:py-1 bc:align-bottom", children: /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(
    "button",
    {
      type: "button",
      className: `bc:flex bc:w-full bc:cursor-pointer bc:items-end bc:gap-1 bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:[font:inherit] ${number ? "bc:justify-end bc:text-right" : "bc:text-left"}`,
      onClick: () => {
        onSort(nextSort(sort, column.name));
      },
      children: [
        /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("span", { className: `bc:flex bc:flex-col ${number ? "bc:items-end" : ""}`, children: [
          /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: "bc:font-medium", children: column.name }),
          /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: "bc:text-xs bc:font-normal bc:text-text-muted", children: column.class })
        ] }),
        /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { "aria-hidden": "true", children: order === "none" ? "" : order === "descending" ? "\u2193" : "\u2191" })
      ]
    }
  ) });
}
function Cell({ column, value }) {
  const text = cellText(value);
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)("td", { className: `bc:border-b bc:border-border-default/40 bc:px-2 bc:py-1 bc:whitespace-nowrap ${isNumber(column) ? "bc:text-right bc:tabular-nums" : ""}`, children: text ?? /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: "bc:italic bc:text-text-muted", children: "NA" }) });
}
function Pager({ table, page, onPage }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted", children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { children: rowsText(table, page) }),
    (page.page > 1 || page.more) && /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("div", { className: "bc:flex bc:items-center bc:gap-2", children: [
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
        PageButton,
        {
          text: "Previous",
          disabled: page.page <= 1,
          onClick: () => {
            onPage(page.page - 1);
          }
        }
      ),
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { children: pageText(page) }),
      /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
        PageButton,
        {
          text: "Next",
          disabled: !page.more,
          onClick: () => {
            onPage(page.page + 1);
          }
        }
      )
    ] })
  ] });
}
function PageButton({ text, disabled, onClick }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
    "button",
    {
      type: "button",
      className: "bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-inherit bc:[font:inherit] bc:disabled:cursor-default bc:disabled:opacity-40",
      disabled,
      onClick,
      children: text
    }
  );
}
export {
  api,
  basic_table_default as default
};
