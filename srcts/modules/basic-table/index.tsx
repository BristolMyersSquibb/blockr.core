// Core's plain renderer of table outputs, blockr.core-table_output, which draws
// every table output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It draws the page of rows that the output holds under the table's columns,
// in the Table of core's React SDK, and pages and sorts through the output's
// parameters, which it sets through its host, so that R makes the page asked
// for. It reads no board option:
// the output holds the page and the page size, by which it draws its pager,
// while the table family reads n_rows, by which R pages the table. What it
// computes is in ./table.ts, and this file only draws. The table takes its colours from the
// element it draws in, so that it suits a light and a dark frontend alike.
// It draws only the rows of the page, which are all that R sends.
// So it needs no Virtualizer of React Aria's.

import { useEffect, useRef } from "react";
import type { ReactElement } from "react";
import type { SortDescriptor } from "../../aria/index.ts";
import { Cell, Column, outputRenderer, Row, Table as SdkTable, TableBody, TableHeader } from "../../react/index.ts";
import type { OutputProps } from "../../react/index.ts";
import type { TableColumn, TableOutput, TableParams } from "../../types/outputs.ts";
import {
  cellText,
  isNumber,
  nextSort,
  pageText,
  paramsFor,
  retry,
  rowsText,
  sortedColumn,
  tablePage,
} from "./table.ts";
import type { Page, Sort } from "./table.ts";

export const api = 1;

export default outputRenderer<TableOutput, TableParams>(Table);

function Table({ output, params, error, host }: OutputProps<TableOutput, TableParams>): ReactElement {
  const page = tablePage(output);
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

  // React Aria names the column that the user pressed by its id.
  // The id holds the column's place, since a name may repeat.
  // R sorts by the first column of a name.
  // So a press on a later column of that name asks for the sort from the
  // smallest value again.
  const sortBy = (next: SortDescriptor): void => {
    const j = Number(String(next.column).slice("column ".length));
    const column = output.columns[j];
    if (column !== undefined) ask(1, nextSort(j === sorted ? params.sort : undefined, column.name));
  };

  return (
    <div className="bc:flex bc:min-h-0 bc:flex-col bc:gap-2">
      <SdkTable aria-label="Table" className="bc:min-h-0" sortDescriptor={descriptor(sorted, params.sort)} onSortChange={sortBy}>
        <TableHeader>
          {/* Columns and rows go by place, since a name may repeat and a
              page holds other rows as it turns. React Aria keeps the
              columns and the rows in one collection, so their ids differ. */}
          {output.columns.map((column, j) => (
            <Column key={j} id={columnId(j)} textValue={column.name} align={isNumber(column) ? "end" : "start"} allowsSorting isRowHeader={j === 0}>
              <Heading column={column} />
            </Column>
          ))}
        </TableHeader>
        <TableBody>
          {output.rows.map((row, i) => (
            <Row key={i} id={`row ${String(i)}`}>
              {output.columns.map((column, j) => {
                const text = cellText(row[j] ?? null);
                return (
                  <Cell key={j} textValue={text ?? "NA"} className={isNumber(column) ? "bc:text-right bc:tabular-nums" : ""}>
                    {text ?? <span className="bc:italic bc:text-text-muted">NA</span>}
                  </Cell>
                );
              })}
            </Row>
          ))}
        </TableBody>
      </SdkTable>
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

// The id of the column at a place.
function columnId(j: number): string {
  return `column ${String(j)}`;
}

// The sort that React Aria shows, on the column that the table is sorted by.
function descriptor(sorted: number, sort: Sort | undefined): SortDescriptor | undefined {
  if (sort === undefined || sorted < 0) return undefined;
  return { column: columnId(sorted), direction: sort.desc === true ? "descending" : "ascending" };
}

// A column's name and class, which line up on the right above numbers.
function Heading({ column }: { column: TableColumn }): ReactElement {
  return (
    <span className={`bc:flex bc:flex-col ${isNumber(column) ? "bc:items-end" : ""}`}>
      <span className="bc:max-w-full bc:truncate bc:font-medium">{column.name}</span>
      <span className="bc:max-w-full bc:truncate bc:text-xs bc:font-normal bc:text-text-muted">{column.class}</span>
    </span>
  );
}

// The rows that the page holds, and the buttons to the page before and after,
// where the table has more than one page.
function Pager({ table, page, onPage }: { table: TableOutput; page: Page; onPage: (page: number) => void }): ReactElement {
  return (
    <div className="bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-text-muted">
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
      className="bc:cursor-pointer bc:rounded bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-inherit bc:[font:inherit] bc:disabled:cursor-default bc:disabled:opacity-40"
      disabled={disabled}
      onClick={onClick}
    >
      {text}
    </button>
  );
}
