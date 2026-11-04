// srcts/util/util.ts
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

// srcts/modules/basic-table/index.ts
var api = 1;
var NUMBERS = /* @__PURE__ */ new Set(["numeric", "integer"]);
var SORTER = "bc:flex bc:w-full bc:cursor-pointer bc:items-end bc:gap-1 bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:[font:inherit]";
var CELL = "bc:border-b bc:border-gray-100 bc:px-2 bc:py-1 bc:whitespace-nowrap bc:dark:border-gray-800";
var renderer = {
  options: ["n_rows"],
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className = "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const table = document.createElement("table");
    table.className = "bc:w-full bc:border-collapse bc:text-left bc:text-sm";
    const headRow = table.createTHead().insertRow();
    const body = table.createTBody();
    const scroller = document.createElement("div");
    scroller.className = "bc:min-h-0 bc:overflow-auto";
    scroller.append(table);
    const counts = document.createElement("span");
    const page = document.createElement("span");
    const previous = pagerButton("Previous");
    const next = pagerButton("Next");
    const controls = document.createElement("div");
    controls.className = "bc:flex bc:items-center bc:gap-2";
    controls.append(previous, page, next);
    const pages = document.createElement("div");
    pages.append(controls);
    const pager = document.createElement("div");
    pager.className = "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-gray-600 bc:dark:text-gray-400";
    pager.append(counts, pages);
    const root = document.createElement("div");
    root.className = "bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:text-gray-900 bc:dark:text-gray-100";
    root.append(alert, scroller, pager);
    el.replaceChildren(root);
    let headed;
    let headers = [];
    let recovery = 0;
    let params = {};
    let at = { page: 1, last: 1 };
    const ask = (to, sort) => {
      const asked = {};
      if (to > 1) asked.page = to;
      if (params.page_size !== void 0) asked.page_size = params.page_size;
      if (sort !== void 0) {
        asked.sort = sort.desc === true ? { column: sort.column, desc: true } : { column: sort.column };
      }
      host.setParams(asked);
    };
    previous.addEventListener("click", () => {
      ask(at.page - 1, params.sort);
    });
    next.addEventListener("click", () => {
      ask(at.page + 1, params.sort);
    });
    const head = (columns) => {
      headers = columns.map((column) => {
        const number = NUMBERS.has(column.class);
        const name = document.createElement("span");
        name.className = "bc:font-medium";
        name.textContent = column.name;
        const type = document.createElement("span");
        type.className = "bc:text-xs bc:font-normal bc:text-gray-500 bc:dark:text-gray-400";
        type.textContent = column.class;
        const arrow = document.createElement("span");
        arrow.setAttribute("aria-hidden", "true");
        const label = document.createElement("span");
        label.className = number ? "bc:flex bc:flex-col bc:items-end" : "bc:flex bc:flex-col";
        label.append(name, type);
        const button = document.createElement("button");
        button.type = "button";
        button.className = number ? `${SORTER} bc:justify-end bc:text-right` : `${SORTER} bc:text-left`;
        button.append(label, arrow);
        button.addEventListener("click", () => {
          ask(1, nextSort(params.sort, column.name));
        });
        const th = document.createElement("th");
        th.scope = "col";
        th.className = "bc:sticky bc:top-0 bc:border-b bc:border-gray-200 bc:bg-white bc:px-2 bc:py-1 bc:align-bottom bc:dark:border-gray-700 bc:dark:bg-gray-900";
        th.append(button);
        return { th, arrow, name: column.name };
      });
      headRow.replaceChildren(...headers.map(({ th }) => th));
    };
    const draw = () => {
      const output = tableOf(host.output());
      params = paramsOf(host.params());
      if (headed === void 0 || !jsonEqual(output.columns, headed)) {
        headed = output.columns;
        head(output.columns);
      }
      const sorted = headers.find(({ name }) => name === params.sort?.column);
      for (const header of headers) {
        const by = header === sorted ? params.sort : void 0;
        header.th.setAttribute("aria-sort", by === void 0 ? "none" : by.desc === true ? "descending" : "ascending");
        header.arrow.textContent = by === void 0 ? "" : by.desc === true ? "\u2193" : "\u2191";
      }
      body.replaceChildren(
        ...output.rows.map((row) => {
          const tr = document.createElement("tr");
          tr.append(...output.columns.map((column, j) => cell(row[j] ?? null, NUMBERS.has(column.class))));
          return tr;
        })
      );
      const rows = output.rows.length;
      const size = params.page_size ?? countOf(host.options().n_rows) ?? Math.max(rows, 1);
      const shown = params.page ?? 1;
      const last = output.total === null ? rows < size ? shown : shown + 1 : Math.max(1, Math.ceil(output.total / size));
      at = { page: shown, last };
      const first = (shown - 1) * size + 1;
      counts.textContent = rows === 0 ? "No rows" : last === 1 && output.total !== null ? `${count(output.total)} ${output.total === 1 ? "row" : "rows"}` : `Rows ${count(first)}\u2013${count(first + rows - 1)}${output.total === null ? "" : ` of ${count(output.total)}`}`;
      pages.hidden = last === 1;
      page.textContent = output.total === null ? `Page ${String(shown)}` : `Page ${String(shown)} of ${String(last)}`;
      previous.disabled = shown <= 1;
      next.disabled = shown >= last;
      const loading = host.loading();
      root.setAttribute("aria-busy", String(loading));
      table.classList.toggle("bc:opacity-60", loading);
      const error = host.error();
      alert.hidden = error === null;
      alert.textContent = error?.message ?? "";
      if (error === null) {
        recovery = 0;
      } else if (recovery === 0 && shown > 1) {
        recovery = 1;
        ask(1, params.sort);
      } else if (recovery < 2 && params.sort !== void 0) {
        recovery = 2;
        ask(1, void 0);
      }
    };
    draw();
    return host.on("output", draw);
  }
};
var basic_table_default = renderer;
function nextSort(sort, column) {
  if (sort?.column !== column) return { column };
  return sort.desc === true ? void 0 : { column, desc: true };
}
function cell(value, number) {
  const td = document.createElement("td");
  td.className = number ? `${CELL} bc:text-right bc:tabular-nums` : CELL;
  if (value === null) {
    const na = document.createElement("span");
    na.className = "bc:italic bc:text-gray-400 bc:dark:text-gray-500";
    na.textContent = "NA";
    td.append(na);
  } else {
    td.textContent = typeof value === "boolean" ? value ? "TRUE" : "FALSE" : typeof value === "object" ? JSON.stringify(value) : String(value);
  }
  return td;
}
function pagerButton(text) {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "bc:cursor-pointer bc:rounded bc:border bc:border-gray-300 bc:bg-white bc:px-2 bc:py-0.5 bc:text-xs bc:text-inherit bc:disabled:cursor-default bc:disabled:opacity-40 bc:dark:border-gray-600 bc:dark:bg-gray-800";
  button.textContent = text;
  return button;
}
function count(n) {
  return n.toLocaleString("en-US");
}
function tableOf(output) {
  const fields = recordOf(output);
  return {
    columns: listOf(fields.columns).map((column) => {
      const { name, class: type } = recordOf(column);
      return { name: typeof name === "string" ? name : "", class: typeof type === "string" ? type : "" };
    }),
    rows: listOf(fields.rows).map(listOf),
    total: countOf(fields.total, 0) ?? null
  };
}
function paramsOf(params) {
  const fields = recordOf(params);
  const table = {};
  const page = countOf(fields.page);
  const size = countOf(fields.page_size);
  const sort = recordOf(fields.sort);
  if (page !== void 0) table.page = page;
  if (size !== void 0) table.page_size = size;
  if (typeof sort.column === "string") {
    table.sort = sort.desc === true ? { column: sort.column, desc: true } : { column: sort.column };
  }
  return table;
}
function countOf(value, min = 1) {
  return typeof value === "number" && Number.isInteger(value) && value >= min ? value : void 0;
}
function recordOf(value) {
  return isObject(value) ? value : {};
}
function listOf(value) {
  return isList(value) ? value : [];
}
function isList(value) {
  return Array.isArray(value);
}
export {
  api,
  basic_table_default as default
};
