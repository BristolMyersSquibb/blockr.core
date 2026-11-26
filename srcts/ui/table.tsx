// What a table of the board's blocks, links or stacks needs to stay usable as
// the board grows: a search that narrows its rows, and pages of a fixed number
// of rows, which the user moves between.

import { useState } from "react";
import type { ReactElement } from "react";
import { SearchField } from "@blockr/ui";
import { classes } from "./ui.tsx";

// How many rows a table shows on a page.
export const PAGE_SIZE = 10;

export interface TableView<T> {
  // The rows of the page shown.
  readonly rows: readonly T[];
  readonly query: string;
  readonly setQuery: (query: string) => void;
  // The page shown, from 0, and how many pages the rows that the search
  // finds fill.
  readonly page: number;
  readonly pages: number;
  readonly setPage: (page: number) => void;
  // How many rows the search finds, and the place of the first row shown
  // among them, from 0.
  readonly found: number;
  readonly first: number;
}

// The page of the rows that the search finds by the text of each row, whatever
// its case. A new search shows the first page again, and a page past the last,
// as where rows have gone, gives way to the last.
export function useTableView<T>(rows: readonly T[], text: (row: T) => string, size = PAGE_SIZE): TableView<T> {
  const [query, setQueryState] = useState("");
  const [page, setPage] = useState(0);
  const needle = query.trim().toLowerCase();
  const matching = needle === "" ? rows : rows.filter((row) => text(row).toLowerCase().includes(needle));
  const pages = Math.max(1, Math.ceil(matching.length / size));
  const shown = Math.min(page, pages - 1);
  const first = shown * size;
  return {
    rows: matching.slice(first, first + size),
    query,
    setQuery: (next) => {
      setQueryState(next);
      setPage(0);
    },
    page: shown,
    pages,
    setPage,
    found: matching.length,
    first,
  };
}

// The search over a table's rows, blockr.ui's SearchField, in a search
// landmark.
// Its button clears the search, and so does Escape, which goes on to the
// elements around the field where the search is empty already.
export function TableSearch({ label, view }: { label: string; view: TableView<unknown> }): ReactElement {
  return (
    <search>
      <SearchField label={label} spellCheck="false" autoComplete="off" value={view.query} onChange={view.setQuery} />
    </search>
  );
}

// Which rows the page shows of how many, and the buttons to the page before
// and the page after. It draws nothing while one page holds every row.
export function Pager({ label, view }: { label: string; view: TableView<unknown> }): ReactElement | null {
  const { page, pages, found, first, rows } = view;
  if (pages < 2) return null;
  return (
    <nav aria-label={label} className="bc:flex bc:items-center bc:justify-between bc:gap-2 bc:text-xs bc:text-muted-foreground">
      <span aria-live="polite">
        Rows {first + 1}–{first + rows.length} of {found}
      </span>
      <span className="bc:flex bc:gap-1">
        <button
          type="button"
          className={classes.button}
          disabled={page === 0}
          onClick={() => {
            view.setPage(page - 1);
          }}
        >
          Previous
        </button>
        <button
          type="button"
          className={classes.button}
          disabled={page >= pages - 1}
          onClick={() => {
            view.setPage(page + 1);
          }}
        >
          Next
        </button>
      </span>
    </nav>
  );
}
