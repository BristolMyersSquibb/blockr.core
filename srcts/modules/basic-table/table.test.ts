// What the plain table renderer computes: where a page sits, what the pager
// says, which parameters it asks for and what it asks for again.

import { describe, expect, test } from "vitest";
import type { TableOutput } from "../../types/outputs.ts";
import { cellText, nextSort, pageOf, pageText, paramsFor, retry, rowsText, sortedColumn } from "./table.ts";

// A table of `total` rows, holding a page of `rows` rows.
function table(rows: number, total: number | null): TableOutput {
  return {
    renderer: { id: "blockr.core-table_output", module: "blockr.core/js/basic-table.js" },
    columns: [{ name: "x", class: "integer" }],
    rows: Array.from({ length: rows }, (_, i) => [i]),
    total,
  };
}

describe("where a page sits", () => {
  test("takes the rows of a page from the parameters, or else from n_rows", () => {
    expect(pageOf(table(3, 7), { page: 2 }, 3)).toEqual({ page: 2, first: 4, rows: 3, last: 3, more: true });
    expect(pageOf(table(2, 7), { page: 2, page_size: 2 }, 3)).toEqual({ page: 2, first: 3, rows: 2, last: 4, more: true });
    expect(pageOf(table(1, 7), { page: 3 }, 3)).toEqual({ page: 3, first: 7, rows: 1, last: 3, more: false });
  });

  test("takes a page with fewer rows than a page holds for the last where the total is unknown", () => {
    expect(pageOf(table(3, null), {}, 3)).toMatchObject({ last: null, more: true });
    expect(pageOf(table(2, null), { page: 2 }, 3)).toMatchObject({ first: 4, last: null, more: false });
    expect(pageOf(table(0, null), { page: 2 }, 3)).toMatchObject({ rows: 0, more: false });
  });

  test("says how many rows the page holds and which page it is", () => {
    expect(rowsText(table(3, 7), pageOf(table(3, 7), { page: 2 }, 3))).toBe("Rows 4–6 of 7");
    expect(rowsText(table(5, 5), pageOf(table(5, 5), {}, 50))).toBe("5 rows");
    expect(rowsText(table(1, 1), pageOf(table(1, 1), {}, 50))).toBe("1 row");
    expect(rowsText(table(0, 0), pageOf(table(0, 0), {}, 50))).toBe("No rows");
    expect(rowsText(table(3, null), pageOf(table(3, null), {}, 3))).toBe("Rows 1–3");
    expect(rowsText(table(50, 1532000), pageOf(table(50, 1532000), { page: 3 }, 50))).toBe("Rows 101–150 of 1,532,000");
    expect(pageText(pageOf(table(3, 7), { page: 2 }, 3))).toBe("Page 2 of 3");
    expect(pageText(pageOf(table(3, null), { page: 2 }, 3))).toBe("Page 2");
  });
});

describe("what it asks for", () => {
  test("asks for a page, sorted as given, keeping the rows of a page that the parameters give", () => {
    expect(paramsFor(3, undefined, {})).toEqual({ page: 3 });
    expect(paramsFor(1, { column: "x" }, { page: 4 })).toEqual({ sort: { column: "x" } });
    expect(paramsFor(2, { column: "x", desc: true }, { page_size: 2 })).toEqual({
      page: 2,
      page_size: 2,
      sort: { column: "x", desc: true },
    });
  });

  test("sorts by a column from the smallest value, then from the largest, then not at all", () => {
    expect(nextSort(undefined, "x")).toEqual({ column: "x" });
    expect(nextSort({ column: "x" }, "x")).toEqual({ column: "x", desc: true });
    expect(nextSort({ column: "x", desc: true }, "x")).toBeUndefined();
    expect(nextSort({ column: "y", desc: true }, "x")).toEqual({ column: "x" });
  });

  test("marks the first column of the name that the table is sorted by, as R sorts by it", () => {
    const repeated: TableOutput = {
      ...table(0, 0),
      columns: [{ name: "a", class: "integer" }, { name: "b", class: "character" }, { name: "b", class: "integer" }],
    };
    expect(sortedColumn(repeated, { column: "b" })).toBe(1);
    expect(sortedColumn(repeated, { column: "c" })).toBe(-1);
    expect(sortedColumn(repeated, undefined)).toBe(-1);
  });

  test("asks again once an ask failed, for the first page with its sort and then unsorted, each once", () => {
    const sorted = { page: 4, sort: { column: "x", desc: true } };
    expect(retry(sorted, 0)).toEqual({ tried: 1, sort: { column: "x", desc: true } });
    expect(retry(sorted, 1)).toEqual({ tried: 2, sort: undefined });
    expect(retry(sorted, 2)).toBeUndefined();
    expect(retry({ page: 2 }, 0)).toEqual({ tried: 1, sort: undefined });
    expect(retry({ page: 2 }, 1)).toBeUndefined();
    expect(retry({ sort: { column: "x" } }, 0)).toEqual({ tried: 2, sort: undefined });
    expect(retry({}, 0)).toBeUndefined();
  });
});

test("shows a cell as R prints it, and a missing value as none", () => {
  expect([1.5, "a", true, false, null].map(cellText)).toEqual(["1.5", "a", "TRUE", "FALSE", null]);
});
