// Core's plain renderer of table outputs, blockr.core-table_output, which draws
// every table output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It draws the page of rows that the output holds under the table's columns,
// and pages and sorts through the output's parameters, which it sets through
// its host, so that R makes the page asked for. It names n_rows, by which the
// coercion pages, as the board option it requires. What it computes is in
// ./table.ts, and this file only draws. The table takes its colours from the
// element it draws in, so that it suits a light and a dark front-end alike.

import { useEffect, useRef } from "react";
import type { ReactElement } from "react";
import { outputRenderer } from "../../react/index.ts";
import type { OutputProps } from "../../react/index.ts";
import type { Json } from "../../types/json.ts";
import type { TableColumn, TableOutput, TableParams } from "../../types/outputs.ts";
import {
  cellText,
  isNumber,
  nextSort,
  pageOf,
  pageText,
  paramsFor,
  retry,
  rowsText,
  sortedColumn,
} from "./table.ts";
import type { Page, Sort } from "./table.ts";

export const api = 1;

export default outputRenderer<TableOutput, TableParams>(Table, { options: ["n_rows"] });

function Table({ output, params, options, error, host }: OutputProps<TableOutput, TableParams>): ReactElement {
  const page = pageOf(output, params, options.n_rows);
  const sorted = sortedColumn(output, params.sort);
  const ask = (to: number, sort: Sort | undefined): void => {
    host.setParams(paramsFor(to, sort, params));
  };

  // Where an ask failed, as for a page that no longer exists once the table
  // holds fewer rows, it asks for the first page with its sort, and then for
  // the first page unsorted, each once until an output comes without an error.
  const tried = useRef(0);
  useEffect(() => {
    if (error === null) {
      tried.current = 0;
      return;
    }
    const again = retry(params, tried.current);
    if (again === undefined) return;
    tried.current = again.tried;
    host.setParams(paramsFor(1, again.sort, params));
  }, [host, error, params]);

  return (
    <div className="bc:flex bc:min-h-0 bc:flex-col bc:gap-2">
      <div className="bc:min-h-0 bc:overflow-auto">
        <table className="bc:w-full bc:border-collapse bc:text-left bc:text-sm">
          <thead>
            <tr>
              {/* Columns and rows go by place, since a name may repeat and a
                  page holds other rows as it turns. */}
              {output.columns.map((column, j) => (
                <Header
                  key={j}
                  column={column}
                  sort={j === sorted ? params.sort : undefined}
                  onSort={(sort) => {
                    ask(1, sort);
                  }}
                />
              ))}
            </tr>
          </thead>
          <tbody>
            {output.rows.map((row, i) => (
              <tr key={i}>
                {output.columns.map((column, j) => (
                  <Cell key={j} column={column} value={row[j] ?? null} />
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <Pager
        table={output}
        page={page}
        onPage={(to) => {
          ask(to, params.sort);
        }}
      />
    </div>
  );
}

// A column's name and class, as a button that sorts by it.
function Header(props: { column: TableColumn; sort: Sort | undefined; onSort: (sort: Sort | undefined) => void }): ReactElement {
  const { column, sort, onSort } = props;
  const number = isNumber(column);
  const order = sort === undefined ? "none" : sort.desc === true ? "descending" : "ascending";
  return (
    <th scope="col" aria-sort={order} className="bc:border-b bc:border-current/25 bc:px-2 bc:py-1 bc:align-bottom">
      <button
        type="button"
        className={`bc:flex bc:w-full bc:cursor-pointer bc:items-end bc:gap-1 bc:border-0 bc:bg-transparent bc:p-0 bc:text-inherit bc:[font:inherit] ${number ? "bc:justify-end bc:text-right" : "bc:text-left"}`}
        onClick={() => {
          onSort(nextSort(sort, column.name));
        }}
      >
        <span className={`bc:flex bc:flex-col ${number ? "bc:items-end" : ""}`}>
          <span className="bc:font-medium">{column.name}</span>
          <span className="bc:text-xs bc:font-normal bc:opacity-60">{column.class}</span>
        </span>
        <span aria-hidden="true">{order === "none" ? "" : order === "descending" ? "↓" : "↑"}</span>
      </button>
    </th>
  );
}

function Cell({ column, value }: { column: TableColumn; value: Json }): ReactElement {
  const text = cellText(value);
  return (
    <td className={`bc:border-b bc:border-current/10 bc:px-2 bc:py-1 bc:whitespace-nowrap ${isNumber(column) ? "bc:text-right bc:tabular-nums" : ""}`}>
      {text ?? <span className="bc:italic bc:opacity-50">NA</span>}
    </td>
  );
}

// The rows that the page holds, and the buttons to the page before and after,
// where the table has more than one page.
function Pager({ table, page, onPage }: { table: TableOutput; page: Page; onPage: (page: number) => void }): ReactElement {
  return (
    <div className="bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:opacity-75">
      <span>{rowsText(table, page)}</span>
      {(page.page > 1 || page.more) && (
        <div className="bc:flex bc:items-center bc:gap-2">
          <PageButton
            text="Previous"
            disabled={page.page <= 1}
            onClick={() => {
              onPage(page.page - 1);
            }}
          />
          <span>{pageText(page)}</span>
          <PageButton
            text="Next"
            disabled={!page.more}
            onClick={() => {
              onPage(page.page + 1);
            }}
          />
        </div>
      )}
    </div>
  );
}

function PageButton({ text, disabled, onClick }: { text: string; disabled: boolean; onClick: () => void }): ReactElement {
  return (
    <button
      type="button"
      className="bc:cursor-pointer bc:rounded bc:border bc:border-current/25 bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-inherit bc:[font:inherit] bc:disabled:cursor-default bc:disabled:opacity-40"
      disabled={disabled}
      onClick={onClick}
    >
      {text}
    </button>
  );
}
