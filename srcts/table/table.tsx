// The table of core's React SDK: React Aria's Table, TableHeader, Column,
// TableBody, Row and Cell (https://react-aria.adobe.com/Table), in core's
// theme, after the Table of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Table.tsx).
// Each takes the props of its part of React Aria.
// It looks as core's tables looked before React Aria drew them.
// The table scrolls in a box that draws no frame, so that a table adds no
// frame inside the card or the panel that holds it.
// A line divides the header from the rows, and a fainter line divides each
// row from the next.
// Each column is as wide as its widest value, and the box scrolls sideways
// where the columns are wider than the box.
// The user resizes no column, since the board would not keep the widths.
// A column that `allowsSorting` sorts the table as the user presses its
// header.
// While the table is sorted by the column, the column shows an arrow after
// its heading.
// The arrow points up for a sort from the smallest value, and down for one
// from the largest.
// A column takes `align` too, as the Column of React Spectrum's TableView does
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/packages/%40react-spectrum/s2/src/TableView.tsx#L764-L768).
// With "end", the column lines its heading up at its right edge, and draws
// the arrow before the heading.
// So the heading of a column of numbers sits above the numbers.
// A cell aligns as its caller's classes say.
// Where the table's `selectionMode` is "single" or "multiple", each row
// starts with a checkbox that selects it.
// Where it is "multiple", the header starts with one that selects every row.
// Where the table's `dragAndDropHooks` let its rows drag, each row starts with
// a button that drags it.
// A keyboard user drags the row from that button too.

import type { ReactElement } from "react";
import {
  Cell as AriaCell,
  Collection,
  Column as AriaColumn,
  composeRenderProps,
  Row as AriaRow,
  Table as AriaTable,
  TableBody as AriaTableBody,
  TableHeader as AriaTableHeader,
  useTableOptions,
} from "../aria/index.ts";
import type {
  CellProps,
  ColumnProps as AriaColumnProps,
  RowProps,
  TableBodyProps,
  TableHeaderProps,
  TableProps as AriaTableProps,
} from "../aria/index.ts";
import { DragButton } from "../dnd/index.ts";
import { Checkbox } from "../inputs/index.ts";

// The table's `className` styles the box that the table scrolls in.
// React Aria's Table would give the class to the table itself.
export interface TableProps extends Omit<AriaTableProps, "className"> {
  readonly className?: string;
}

export function Table({ className, ...props }: TableProps): ReactElement {
  return (
    <div onScroll={props.onScroll} className={`${BOX} ${className ?? ""}`}>
      <AriaTable {...props} className={TABLE} />
    </div>
  );
}

export interface ColumnProps extends AriaColumnProps {
  readonly align?: "start" | "end";
}

export function Column({ align = "start", ...props }: ColumnProps): ReactElement {
  return (
    <AriaColumn
      {...props}
      className={composeRenderProps(props.className, (given) => `${COLUMN} ${align === "end" ? "bc:text-end" : "bc:text-start"} ${given ?? ""}`)}
    >
      {composeRenderProps(props.children, (children, { sortDirection }) => (
        <span className={`${HEADING} ${align === "end" ? "bc:flex-row-reverse" : ""}`}>
          {children}
          {sortDirection === undefined ? null : <Arrow down={sortDirection === "descending"} />}
        </span>
      ))}
    </AriaColumn>
  );
}

export function TableHeader<T extends object>({ columns, children, ...props }: TableHeaderProps<T>): ReactElement {
  const { selectionBehavior, selectionMode, allowsDragging } = useTableOptions();
  return (
    <AriaTableHeader {...props}>
      {/* The columns of the buttons that drag a row and of the checkboxes
          that select one. */}
      {allowsDragging && <AriaColumn className={PART} />}
      {selectionBehavior === "toggle" && <AriaColumn className={PART}>{selectionMode === "multiple" && <Checkbox slot="selection" />}</AriaColumn>}
      <Collection items={columns}>{children}</Collection>
    </AriaTableHeader>
  );
}

export function TableBody<T extends object>(props: TableBodyProps<T>): ReactElement {
  return <AriaTableBody {...props} className={composeRenderProps(props.className, (given) => `${BODY} ${given ?? ""}`)} />;
}

export function Row<T extends object>({ id, columns, children, ...props }: RowProps<T>): ReactElement {
  const { selectionBehavior, allowsDragging } = useTableOptions();
  return (
    <AriaRow id={id} {...props} className={composeRenderProps(props.className, (given) => `${ROW} ${given ?? ""}`)}>
      {allowsDragging && (
        <Cell className="bc:w-px">
          <DragButton />
        </Cell>
      )}
      {selectionBehavior === "toggle" && (
        <Cell className="bc:w-px">
          <Checkbox slot="selection" />
        </Cell>
      )}
      <Collection items={columns}>{children}</Collection>
    </AriaRow>
  );
}

export function Cell(props: CellProps): ReactElement {
  return <AriaCell {...props} className={composeRenderProps(props.className, (given) => `${CELL} ${given ?? ""}`)} />;
}

// The arrow of the column that the table is sorted by, which points up, or
// down where `down`.
function Arrow({ down }: { down: boolean }): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className={`bc:mb-0.5 bc:shrink-0 bc:forced-colors:text-[ButtonText] ${down ? "bc:rotate-180" : ""}`}
    >
      <path d="M8 13V3M4 7l4-4 4 4" />
    </svg>
  );
}

// The classes of the table and its parts.
const BOX = "bc:min-w-0 bc:overflow-auto bc:font-sans bc:text-text-default";
const TABLE = "bc:w-full bc:border-collapse bc:text-left bc:text-sm";
// A column's header holds its heading, at the bottom, and the arrow of its
// sort.
const COLUMN =
  "bc:border-b bc:border-border-default bc:px-2 bc:py-1 bc:align-bottom bc:font-semibold bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-allows-sorting:cursor-pointer";
// The heading is a box of its own, rather than a line of text, which would
// add the space below a line to the header's height.
// Turned round, it starts at the right edge.
const HEADING = "bc:flex bc:items-end bc:gap-1";
// The columns of a row's button and checkbox, as narrow as these.
const PART = "bc:w-px bc:border-b bc:border-border-default bc:px-2 bc:py-1";
const BODY = "bc:data-empty:text-center bc:data-empty:text-sm bc:data-empty:italic bc:data-empty:text-text-muted";
// A row that the user selects shows in a tint of the accent colour.
// Where a press selects a row, the row's text cannot be selected.
const ROW =
  "bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50 bc:data-selection-mode:select-none";
const CELL =
  "bc:border-b bc:border-border-default/40 bc:px-2 bc:py-1 bc:whitespace-nowrap bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
