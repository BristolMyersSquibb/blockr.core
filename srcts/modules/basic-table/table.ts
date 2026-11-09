// What the plain table renderer computes from what its host holds, as pure
// functions: where the page sits in the table, which parameters to ask for,
// and what to ask for again once an ask failed.

import type { Json } from "../../types/json.ts";
import type { TableColumn, TableOutput, TableParams } from "../../types/outputs.ts";

export type Sort = NonNullable<TableParams["sort"]>;

// Where a page sits in its table.
export interface Page {
  // The page, counted from 1.
  readonly page: number;
  // The number of the page's first row, counted from 1.
  readonly first: number;
  // How many rows the page holds.
  readonly rows: number;
  // The last page, or null where the total row count is unknown.
  readonly last: number | null;
  // Whether a page follows.
  readonly more: boolean;
}

// The column classes whose values are numbers, which line up on the right.
const NUMBERS = new Set(["numeric", "integer"]);

export function isNumber(column: TableColumn): boolean {
  return NUMBERS.has(column.class);
}

// Where the page that the output holds sits. A page holds the rows that the
// parameters give, or else as many as the board option n_rows gives, by which
// the coercion pages, or else every row, where nothing gives n_rows. Where the
// total is unknown, a page with fewer rows than a page holds is the last.
export function pageOf(table: TableOutput, params: TableParams, nRows: Json | undefined): Page {
  const rows = table.rows.length;
  const size = params.page_size ?? (nRows === undefined ? Math.max(rows, 1) : asCount(nRows));
  const page = params.page ?? 1;
  const last = table.total === null ? null : Math.max(1, Math.ceil(table.total / size));
  return {
    page,
    first: (page - 1) * size + 1,
    rows,
    last,
    more: last === null ? rows > 0 && rows >= size : page < last,
  };
}

// What the pager says of the rows on the page, as "Rows 4–6 of 7".
export function rowsText(table: TableOutput, page: Page): string {
  if (page.rows === 0) return "No rows";
  if (table.total !== null && page.last === 1) {
    return `${count(table.total)} ${table.total === 1 ? "row" : "rows"}`;
  }
  const of = table.total === null ? "" : ` of ${count(table.total)}`;
  return `Rows ${count(page.first)}–${count(page.first + page.rows - 1)}${of}`;
}

export function pageText(page: Page): string {
  return page.last === null ? `Page ${String(page.page)}` : `Page ${String(page.page)} of ${String(page.last)}`;
}

// The parameters that ask for a page, sorted as given, by the rows of a page
// that the parameters give, if any. They leave out the first page and no sort,
// which the coercion takes by default, so that one page is asked for alike.
export function paramsFor(page: number, sort: Sort | undefined, params: TableParams): Record<string, Json> {
  const asked: Record<string, Json> = {};
  if (page > 1) asked.page = page;
  if (params.page_size !== undefined) asked.page_size = params.page_size;
  if (sort !== undefined) asked.sort = sortOf(sort.column, sort.desc === true);
  return asked;
}

// The sort that a click on a column asks for: by the column from the smallest
// value, then from the largest, then none.
export function nextSort(sort: Sort | undefined, column: string): Sort | undefined {
  if (sort?.column !== column) return sortOf(column, false);
  return sort.desc === true ? undefined : sortOf(column, true);
}

// The index of the column that the table is sorted by, or -1. R sorts by the
// first column of a name that repeats.
export function sortedColumn(table: TableOutput, sort: Sort | undefined): number {
  return sort === undefined ? -1 : table.columns.findIndex((column) => column.name === sort.column);
}

// What the renderer asks for again once an ask failed, as for a page that no
// longer exists once the table holds fewer rows: the first page with its
// sort, and then the first page unsorted. `tried` counts the asks it made
// since the last output came without an error, so that it asks each once.
export function retry(params: TableParams, tried: number): { tried: number; sort: Sort | undefined } | undefined {
  if (tried < 1 && (params.page ?? 1) > 1) return { tried: 1, sort: params.sort };
  if (tried < 2 && params.sort !== undefined) return { tried: 2, sort: undefined };
  return undefined;
}

// A cell as text, as R prints it, with a missing value as null. R sends an
// infinite number and NaN as null too, which JSON cannot hold.
export function cellText(value: Json): string | null {
  if (value === null) return null;
  if (typeof value === "boolean") return value ? "TRUE" : "FALSE";
  return typeof value === "object" ? JSON.stringify(value) : String(value);
}

function sortOf(column: string, desc: boolean): Sort {
  return desc ? { column, desc: true } : { column };
}

function count(n: number): string {
  return n.toLocaleString("en-US");
}

// The board option n_rows as the rows that a page holds, a whole number of at
// least 1. Any other value fails, rather than page by another.
function asCount(value: Json): number {
  if (typeof value === "number" && Number.isInteger(value) && value >= 1) return value;
  throw new TypeError(`The board option n_rows is ${JSON.stringify(value)}, where a table pages by a whole number of at least 1.`);
}
