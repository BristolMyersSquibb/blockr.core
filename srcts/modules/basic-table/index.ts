// Core's plain renderer of table outputs, blockr.core-table_output, which draws
// every table output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It draws the page of rows that the output holds under the table's columns,
// and pages and sorts through the output's parameters, TableParams, which it
// sets through its host, so that R makes the page asked for:
// - A click on a column's name sorts the table by it, from the smallest value,
//   then from the largest, then not at all, and shows the first page.
// - The pager asks for the page before or after, by the rows of a page that
//   the parameters give, or else the board option n_rows, by which the
//   coercion pages and which the renderer names as the board option it
//   requires.
// - Where the ask for a page fails, as for a page that no longer exists once
//   the table holds fewer rows, it asks for the first page, and then for the
//   first page unsorted, while it shows the error over the page it holds.

import type { OutputModule } from "../../types/hosts.ts";
import type { Json } from "../../types/json.ts";
import type { TableColumn, TableParams } from "../../types/outputs.ts";
import { isObject, jsonEqual } from "../../util/index.ts";

export const api = 1;

type Sort = NonNullable<TableParams["sort"]>;

// The column classes whose values are numbers, which line up on the right,
// under a name that does too.
const NUMBERS = new Set(["numeric", "integer"]);

const SORTER =
  "bc:flex bc:w-full bc:cursor-pointer bc:items-end bc:gap-1 bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:[font:inherit]";

const CELL = "bc:border-b bc:border-gray-100 bc:px-2 bc:py-1 bc:whitespace-nowrap bc:dark:border-gray-800";

const renderer: OutputModule = {
  options: ["n_rows"],
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className =
      "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const table = document.createElement("table");
    table.className = "bc:w-full bc:border-collapse bc:text-left bc:text-sm";
    const headRow = table.createTHead().insertRow();
    const body = table.createTBody();
    // The table scrolls within the element where a front-end gives that a
    // height, while the pager stays in view.
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
    // What `hidden` hides carries no class that sets its display, which
    // would show it all the same.
    const pages = document.createElement("div");
    pages.append(controls);
    const pager = document.createElement("div");
    pager.className =
      "bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-gray-600 bc:dark:text-gray-400";
    pager.append(counts, pages);
    const root = document.createElement("div");
    root.className = "bc:flex bc:max-h-full bc:flex-col bc:gap-2 bc:text-gray-900 bc:dark:text-gray-100";
    root.append(alert, scroller, pager);
    el.replaceChildren(root);

    // The columns that the head shows, which it draws again only once they
    // change, so that a column's button keeps the focus as the user sorts.
    let headed: readonly TableColumn[] | undefined;
    let headers: { th: HTMLTableCellElement; arrow: HTMLElement; name: string }[] = [];
    // How far the renderer went in asking again since an ask last failed: not
    // at all, for the first page, or for the first page unsorted.
    let recovery = 0;
    let params: TableParams = {};
    let at = { page: 1, last: 1 };

    // Asks for a page, sorted as given, by the rows of a page that the
    // parameters give, if any. It leaves out the first page and no sort, which
    // the coercion takes by default.
    const ask = (to: number, sort: Sort | undefined): void => {
      const asked: Record<string, Json> = {};
      if (to > 1) asked.page = to;
      if (params.page_size !== undefined) asked.page_size = params.page_size;
      if (sort !== undefined) {
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

    const head = (columns: readonly TableColumn[]): void => {
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
        th.className =
          "bc:sticky bc:top-0 bc:border-b bc:border-gray-200 bc:bg-white bc:px-2 bc:py-1 bc:align-bottom bc:dark:border-gray-700 bc:dark:bg-gray-900";
        th.append(button);
        return { th, arrow, name: column.name };
      });
      headRow.replaceChildren(...headers.map(({ th }) => th));
    };

    const draw = (): void => {
      const output = tableOf(host.output());
      params = paramsOf(host.params());

      if (headed === undefined || !jsonEqual(output.columns, headed)) {
        headed = output.columns;
        head(output.columns);
      }
      // Sorted by a column that repeats, R sorts by the first of that name.
      const sorted = headers.find(({ name }) => name === params.sort?.column);
      for (const header of headers) {
        const by = header === sorted ? params.sort : undefined;
        header.th.setAttribute("aria-sort", by === undefined ? "none" : by.desc === true ? "descending" : "ascending");
        header.arrow.textContent = by === undefined ? "" : by.desc === true ? "↓" : "↑";
      }

      body.replaceChildren(
        ...output.rows.map((row) => {
          const tr = document.createElement("tr");
          tr.append(...output.columns.map((column, j) => cell(row[j] ?? null, NUMBERS.has(column.class))));
          return tr;
        }),
      );

      const rows = output.rows.length;
      const size = params.page_size ?? countOf(host.options().n_rows) ?? Math.max(rows, 1);
      const shown = params.page ?? 1;
      const last = output.total === null ? (rows < size ? shown : shown + 1) : Math.max(1, Math.ceil(output.total / size));
      at = { page: shown, last };
      const first = (shown - 1) * size + 1;
      counts.textContent =
        rows === 0
          ? "No rows"
          : last === 1 && output.total !== null
            ? `${count(output.total)} ${output.total === 1 ? "row" : "rows"}`
            : `Rows ${count(first)}–${count(first + rows - 1)}${output.total === null ? "" : ` of ${count(output.total)}`}`;
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
      } else if (recovery < 2 && params.sort !== undefined) {
        recovery = 2;
        ask(1, undefined);
      }
    };

    draw();
    return host.on("output", draw);
  },
};

export default renderer;

// The sort that a click on a column's name asks for: by that column from the
// smallest value, then from the largest, then none.
function nextSort(sort: Sort | undefined, column: string): Sort | undefined {
  if (sort?.column !== column) return { column };
  return sort.desc === true ? undefined : { column, desc: true };
}

// A cell, which shows a missing value as R prints one. R sends an infinite
// number and NaN as null too, which JSON cannot hold.
function cell(value: Json, number: boolean): HTMLTableCellElement {
  const td = document.createElement("td");
  td.className = number ? `${CELL} bc:text-right bc:tabular-nums` : CELL;
  if (value === null) {
    const na = document.createElement("span");
    na.className = "bc:italic bc:text-gray-400 bc:dark:text-gray-500";
    na.textContent = "NA";
    td.append(na);
  } else {
    td.textContent =
      typeof value === "boolean" ? (value ? "TRUE" : "FALSE") : typeof value === "object" ? JSON.stringify(value) : String(value);
  }
  return td;
}

function pagerButton(text: string): HTMLButtonElement {
  const button = document.createElement("button");
  button.type = "button";
  button.className =
    "bc:cursor-pointer bc:rounded bc:border bc:border-gray-300 bc:bg-white bc:px-2 bc:py-0.5 bc:text-xs bc:text-inherit bc:disabled:cursor-default bc:disabled:opacity-40 bc:dark:border-gray-600 bc:dark:bg-gray-800";
  button.textContent = text;
  return button;
}

function count(n: number): string {
  return n.toLocaleString("en-US");
}

interface Table {
  readonly columns: readonly TableColumn[];
  readonly rows: readonly (readonly Json[])[];
  readonly total: number | null;
}

// The table output the host holds, as far as it has the fields of one.
function tableOf(output: Json): Table {
  const fields = recordOf(output);
  return {
    columns: listOf(fields.columns).map((column) => {
      const { name, class: type } = recordOf(column);
      return { name: typeof name === "string" ? name : "", class: typeof type === "string" ? type : "" };
    }),
    rows: listOf(fields.rows).map(listOf),
    total: countOf(fields.total, 0) ?? null,
  };
}

// The parameters the output was asked with, as far as they are those of a
// table.
function paramsOf(params: Json): TableParams {
  const fields = recordOf(params);
  const table: TableParams = {};
  const page = countOf(fields.page);
  const size = countOf(fields.page_size);
  const sort = recordOf(fields.sort);
  if (page !== undefined) table.page = page;
  if (size !== undefined) table.page_size = size;
  if (typeof sort.column === "string") {
    table.sort = sort.desc === true ? { column: sort.column, desc: true } : { column: sort.column };
  }
  return table;
}

// A whole number of at least `min`, or undefined for anything else.
function countOf(value: Json | undefined, min = 1): number | undefined {
  return typeof value === "number" && Number.isInteger(value) && value >= min ? value : undefined;
}

function recordOf(value: Json | undefined): Readonly<Record<string, Json>> {
  return isObject(value) ? value : {};
}

function listOf(value: Json | undefined): readonly Json[] {
  return isList(value) ? value : [];
}

function isList(value: Json | undefined): value is readonly Json[] {
  return Array.isArray(value);
}
