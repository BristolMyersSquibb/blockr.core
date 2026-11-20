// The table of core's React SDK: React Aria's Table, TableHeader, Column,
// TableBody, Row and Cell (https://react-aria.adobe.com/Table), in core's
// theme, after the Table of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Table.tsx).
// Each takes the props of its part of React Aria.
// The table scrolls inside a border of its own.
// Its header stays in view as the rows scroll.
// The user resizes a column that has no `width` by the bar at its right edge.
// The pointer drags the bar, and the arrow keys move it, through React Aria's
// ResizableTableContainer and ColumnResizer.
// A column that `allowsSorting` sorts the table as the user presses its
// header.
// While the table is sorted by the column, the column shows an arrow.
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
  ColumnResizer,
  composeRenderProps,
  Group,
  ResizableTableContainer,
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
import { SelectionCheckbox } from "../inputs/index.ts";

// The table's `className` styles the box that the table scrolls in.
// React Aria's Table would give the class to the table itself.
export interface TableProps extends Omit<AriaTableProps, "className"> {
  readonly className?: string;
}

export function Table({ className, ...props }: TableProps): ReactElement {
  return (
    <ResizableTableContainer onScroll={props.onScroll} className={`${BOX} ${className ?? ""}`}>
      <AriaTable {...props} className={TABLE} />
    </ResizableTableContainer>
  );
}

export interface ColumnProps extends AriaColumnProps {
  readonly align?: "start" | "end";
}

export function Column({ align = "start", ...props }: ColumnProps): ReactElement {
  return (
    <AriaColumn {...props} className={composeRenderProps(props.className, (given) => `${COLUMN} ${given ?? ""}`)}>
      {composeRenderProps(props.children, (children, { allowsSorting, sortDirection }) => (
        <div className="bc:flex bc:items-center">
          <Group role="presentation" tabIndex={-1} className={`${HEADING} ${align === "end" ? "bc:flex-row-reverse bc:text-end" : ""}`}>
            <span className="bc:min-w-0 bc:flex-1 bc:truncate">{children}</span>
            {allowsSorting && (
              <span className={`bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center ${sortDirection === "descending" ? "bc:rotate-180" : ""}`}>
                {sortDirection === undefined ? null : <Arrow />}
              </span>
            )}
          </Group>
          {props.width === undefined && <ColumnResizer className={RESIZER} />}
        </div>
      ))}
    </AriaColumn>
  );
}

export function TableHeader<T extends object>({ columns, children, ...props }: TableHeaderProps<T>): ReactElement {
  const { selectionBehavior, selectionMode, allowsDragging } = useTableOptions();
  return (
    <AriaTableHeader {...props} className={composeRenderProps(props.className, (given) => `${HEADER} ${given ?? ""}`)}>
      {/* The columns of the buttons that drag a row and of the checkboxes
          that select one. */}
      {allowsDragging && <AriaColumn width={28} minWidth={28} className={PART} />}
      {selectionBehavior === "toggle" && (
        <AriaColumn width={36} minWidth={36} className={PART}>
          {selectionMode === "multiple" && <SelectionCheckbox />}
        </AriaColumn>
      )}
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
        <Cell>
          <DragButton />
        </Cell>
      )}
      {selectionBehavior === "toggle" && (
        <Cell>
          <SelectionCheckbox />
        </Cell>
      )}
      <Collection items={columns}>{children}</Collection>
    </AriaRow>
  );
}

export function Cell(props: CellProps): ReactElement {
  return <AriaCell {...props} className={composeRenderProps(props.className, (given) => `${CELL} ${given ?? ""}`)} />;
}

// The arrow of the column that the table is sorted by.
// It points up, and its box turns it to point down.
function Arrow(): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className="bc:text-text-muted bc:forced-colors:text-[ButtonText]"
    >
      <path d="M8 13V3M4 7l4-4 4 4" />
    </svg>
  );
}

// The classes of the table and its parts.
// The box scrolls the table.
// Where the focus scrolls a row into view, the box leaves room above the row
// for the header, which stays in view.
const BOX =
  "bc:relative bc:box-border bc:w-full bc:scroll-pt-[2.281rem] bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:font-sans bc:text-text-default";
const TABLE = "bc:box-border bc:border-separate bc:border-spacing-0 bc:has-[>[data-empty]]:h-full";
// A column's header holds its heading and the bar that resizes the column.
// The bar straddles the header's right edge, so it takes no width from the
// heading.
// It overlaps the header of the next column, and lies above that header while
// the pointer or the focus is on the bar.
// So the heading is as wide as the column's cells, and the heading of a
// column that aligns at its end sits above them.
const COLUMN =
  "bc:relative bc:box-border bc:h-1 bc:cursor-default bc:text-start bc:align-bottom bc:text-sm bc:font-semibold bc:text-text-default bc:outline-none bc:hover:z-20 bc:focus-within:z-20 bc:data-allows-sorting:cursor-pointer";
const HEADING =
  "bc:box-border bc:flex bc:min-h-5 bc:min-w-0 bc:flex-1 bc:items-center bc:gap-1 bc:overflow-hidden bc:px-2 bc:py-1 bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
const RESIZER =
  "bc:absolute bc:top-1/2 bc:-right-2 bc:z-10 bc:box-content bc:h-5 bc:w-px bc:-translate-y-1/2 bc:cursor-col-resize bc:rounded-xs bc:bg-border-default bc:bg-clip-content bc:px-2 bc:py-1 bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-resizing:w-0.5 bc:data-resizing:bg-bg-accent bc:data-resizing:pl-[7px] bc:forced-colors:bg-[ButtonBorder] bc:forced-colors:data-resizing:bg-[Highlight]";
// The header covers the rows that scroll below it.
// A line divides the header from the rows.
// The table draws that line below each cell of the header, since a table with
// separate borders draws no border of the header itself.
const HEADER =
  "bc:sticky bc:top-0 bc:z-10 bc:rounded-t-lg bc:bg-bg-raised bc:forced-colors:bg-[Canvas] bc:[&>tr>th]:border-b bc:[&>tr>th]:border-border-default";
// The columns of a row's button and checkbox.
const PART = "bc:box-border bc:px-2 bc:py-1 bc:text-start";
const BODY = "bc:data-empty:text-center bc:data-empty:text-sm bc:data-empty:italic bc:data-empty:text-text-muted";
// A row that the user selects shows in a tint of the accent colour.
// Where a press selects a row, the row's text cannot be selected.
const ROW =
  "bc:group/row bc:relative bc:cursor-default bc:text-sm bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50 bc:data-selection-mode:select-none bc:last:rounded-b-lg";
const CELL =
  "bc:box-border bc:truncate bc:border-b bc:border-border-default/40 bc:px-2 bc:py-1 bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:group-last/row:border-b-0 bc:group-last/row:first:rounded-bl-lg bc:group-last/row:last:rounded-br-lg";
