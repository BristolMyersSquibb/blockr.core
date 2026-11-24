// What the Edit board plugin draws: three sections, for the board's blocks,
// its links and its stacks, as What it shows has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows).
// Each section holds a form that adds one, and a table of those that the board
// holds. A table shows a page of rows at a time, which a search narrows, so
// that it stays usable on a board of many blocks.
// The forms that add a block, a link and a stack are the advanced actions of
// the board actions that play core's roles add_block_role, add_link_role and
// add_stack_role on the board. Those are core's Add block, Create link and
// Create stack, unless the app picks another board action for a role. The
// plugin mounts each form through mountAdvanced() by the role's id, so that
// the plugin and the menus share it, and so that the form of a board action
// that the app picks shows here too
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#roles).
// The plugin mounts a form only where core.boardActions.on() offers the board
// action that plays the form's role on the board.
// Each row of a table ends in a "…" button, which opens the menu of the row's
// block, link or stack, and starts with a checkbox, which adds the row's object
// to core.selection or takes it out. While the selection holds a board
// object, a bar above the sections says how many it holds, with a "…" button
// that opens the menu of the selection and a button that clears the
// selection (./menus.tsx, ./selection.ts).
// Each section is a Disclosure of the SDK, which starts open and collapses by
// its header, which counts what the board holds.
// It draws nothing of a block's state, which the block's UI sets, nor the
// board's name or its board options, which the frontend draws.
// It follows the board through the store, so the next edit reads the board as
// the last one left it, a block that the user just added among it.
// Where the store or the command registry refuses an edit, the plugin says why
// close to the control that made it, in a banner in the colour of an error,
// under a sentence that names the edit: below the buttons of a form, and below
// the table or the bar whose menu ran the board action.
// Under the board lock, the store refuses what every board action of core's
// but Copy block ID runs, so they fail their offers. The plugin then mounts no
// form, and its rows and its bar show no "…" unless another package's board
// action applies. It lists only the board's links and stacks, as it reads
// from core.lock.board, since a frontend draws the blocks on their cards.

import { useCallback, useMemo } from "react";
import type { ReactElement, ReactNode } from "react";
import type { Target } from "../../actions/index.ts";
import { Mounted } from "../../card/index.ts";
import {
  Button,
  Cell,
  Column,
  Disclosure,
  DisclosureHeader,
  DisclosurePanel,
  Row,
  Table,
  TableBody,
  TableHeader,
  useBoard,
} from "../../react/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { Refusal } from "../../ui/index.ts";
import { BlocksSection } from "./blocks.tsx";
import { LinksSection } from "./links.tsx";
import { MenuButton, PlaceForm, useMenus } from "./menus.tsx";
import type { Menus } from "./menus.tsx";
import { objectOf, selectedIds, tickRows, useSelected } from "./selection.ts";
import type { Kind } from "./selection.ts";
import { StacksSection } from "./stacks.tsx";

export function EditBoard({ core }: { core: CoreApi }): ReactElement {
  const board = useBoard(core);
  const locked = core.lock.board;
  return (
    <div className="bc:grid bc:gap-3 bc:font-sans bc:text-text-default">
      <SelectionBar core={core} />
      {locked ? null : <BlocksSection core={core} board={board} />}
      <LinksSection core={core} board={board} locked={locked} />
      <StacksSection core={core} board={board} locked={locked} />
    </div>
  );
}

// The key of the bar's place for the advanced actions of the selection's menu.
const SELECTION = "selection";

// The bar of the selection, while the selection holds a board object, of
// whatever kind and whatever selected it: how many it holds, a "…" button that
// opens the menu of the selection, which offers Remove, Create stack and Add
// to stack among core's board actions, and a button that clears the
// selection. An advanced action that the menu opens shows below the bar, as
// Create stack's form with the selected blocks given, until it ends, even
// where the selection empties meanwhile. Why the store refused a board action
// that the menu ran shows below the bar too.
function SelectionBar({ core }: { core: CoreApi }): ReactElement | null {
  const selected = useSelected(core);
  const menus = useMenus(core);
  const form = menus.shown.get(SELECTION);
  if (selected.length === 0 && form === undefined) return null;
  const target: Target = { selection: selected };
  return (
    <div role="region" aria-label="Selection" className="bc:grid bc:gap-2">
      {selected.length === 0 ? null : (
        <div className="bc:flex bc:items-center bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:px-3 bc:py-1.5 bc:text-sm">
          <span aria-live="polite">{selected.length} selected</span>
          <span className="bc:ml-auto bc:flex bc:items-center bc:gap-1">
            {core.boardActions.on(target).length > 0 ? (
              <MenuButton menus={menus} target={target} place={SELECTION} label="Actions of the selection" />
            ) : null}
            <Button
              variant="quiet"
              onPress={() => {
                core.selection.set([]);
              }}
            >
              Clear selection
            </Button>
          </span>
        </div>
      )}
      {form === undefined ? null : (
        <PlaceForm
          args={form}
          onClose={() => {
            menus.close(SELECTION, form);
          }}
        />
      )}
      <Refusal refusal={menus.refusal} />
    </div>
  );
}

// One of the plugin's sections, a region named by its title, which the user
// collapses by its header, which counts what the board holds of it.
export function Section({ title, count, children }: { title: string; count: number; children: ReactNode }): ReactElement {
  return (
    <section aria-label={title} className="bc:rounded-md bc:border bc:border-border-default">
      <Disclosure defaultExpanded>
        <DisclosureHeader className="bc:text-sm bc:font-semibold bc:[&>button]:gap-2 bc:[&>button]:px-3 bc:[&>button]:py-2">
          {title}
          <span className="bc:rounded-full bc:border bc:border-border-default bc:px-1.5 bc:text-xs bc:font-normal bc:text-text-muted">
            {count}
          </span>
        </DisclosureHeader>
        <DisclosurePanel>
          <div className="bc:grid bc:gap-4 bc:border-t bc:border-border-default bc:p-3">{children}</div>
        </DisclosurePanel>
      </Disclosure>
    </section>
  );
}

// A part of a section, such as its form or its table, under a heading of its
// own.
export function Part({ title, children }: { title: string; children: ReactNode }): ReactElement {
  return (
    <div role="group" aria-label={title} className="bc:grid bc:gap-2">
      <h4 className="bc:m-0 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted">{title}</h4>
      {children}
    </div>
  );
}

// A column of a table of the board's objects.
export interface ObjectColumn {
  readonly id: string;
  readonly name: string;
  // The column whose cell names the row, which a screen reader reads as the
  // row's name.
  readonly rowHeader?: boolean;
}

// A row of a table of the board's objects: the object's id, the row's text,
// the label of its "…" button, as "Actions of the block Head", and the
// content of each cell, with the classes of the cell.
export interface ObjectRow {
  readonly id: string;
  readonly text: string;
  readonly menuLabel: string;
  readonly cells: readonly { readonly content: ReactNode; readonly className?: string }[];
}

// What a cell of the plugin's tables takes beside the SDK's classes: text that
// wraps where the column is narrow, rather than a box that scrolls sideways
// in a narrow panel.
export const WRAP = "bc:whitespace-normal! bc:break-words";

// A table of the board's blocks, links or stacks, which the SDK's Table draws:
// a checkbox that selects each row's object, ticked where core.selection holds
// it, then the columns given, then a column with the "…" button of each row
// whose object a board action applies to, where any row has one. The menus
// are those of the table's section, which shows the banner of a board action
// that the store refused below the table.
// A row whose menu opened an advanced action turns into the form, in a cell
// across the row, which keeps the row's checkbox.
// The checkbox in the header ticks every row of the page, or unticks them.
export function ObjectsTable({
  core,
  menus,
  kind,
  label,
  columns,
  rows,
}: {
  core: CoreApi;
  menus: Menus;
  kind: Kind;
  label: string;
  columns: readonly ObjectColumn[];
  rows: readonly ObjectRow[];
}): ReactElement {
  const selected = useSelected(core);
  const keys = useMemo(() => selectedIds(selected, kind), [selected, kind]);
  const offered = new Set(rows.flatMap(({ id }) => (core.boardActions.on(objectOf(kind, id)).length > 0 ? [id] : [])));
  const actions = offered.size > 0;
  const span = columns.length + (actions ? 1 : 0);
  return (
    <Table
      aria-label={label}
      selectionMode="multiple"
      selectedKeys={keys}
      onSelectionChange={(picked) => {
        tickRows(
          core,
          kind,
          rows.map(({ id }) => id),
          picked,
        );
      }}
    >
      <TableHeader>
        {columns.map((column) => (
          <Column key={column.id} id={column.id} isRowHeader={column.rowHeader === true}>
            {column.name}
          </Column>
        ))}
        {actions ? (
          <Column id="actions" textValue="Actions" className="bc:w-px">
            <span className="bc:sr-only">Actions</span>
          </Column>
        ) : null}
      </TableHeader>
      <TableBody>
        {rows.map((row) => {
          const form = menus.shown.get(row.id);
          return (
            <Row key={row.id} id={row.id} textValue={row.text}>
              {form === undefined ? (
                [
                  ...row.cells.map(({ content, className }, i) => (
                    <Cell key={i} className={className}>
                      {content}
                    </Cell>
                  )),
                  ...(actions
                    ? [
                        <Cell key="actions" className="bc:text-right">
                          {offered.has(row.id) ? (
                            <MenuButton menus={menus} target={objectOf(kind, row.id)} place={row.id} label={row.menuLabel} />
                          ) : null}
                        </Cell>,
                      ]
                    : []),
                ]
              ) : (
                <Cell colSpan={span} className={WRAP}>
                  <PlaceForm
                    args={form}
                    onClose={() => {
                      menus.close(row.id, form);
                    }}
                  />
                </Cell>
              )}
            </Row>
          );
        })}
      </TableBody>
    </Table>
  );
}

// The board, as the target of a board action that acts on it.
export const BOARD = { board: true } as const;

// Whether a menu of the target offers the board action that plays the role
// with the id given, as on() gives it. Of the board actions that name the
// role, on() gives only the one that plays the role in the app.
export function offers(core: CoreApi, role: string, target: Target): boolean {
  return core.boardActions.on(target).some((action) => action.role === role);
}

// The advanced action of the board action that plays the role with the id
// given, on the target. The plugin mounts it in room of its own through
// mountAdvanced() by the role's id, until the component goes or its target
// changes. Core renders it again once its done() has run a batch, so that a
// form that adds is ready for the next edit, and leaves the element empty once
// the board no longer holds what the target names.
export function Advanced({ core, role, target }: { core: CoreApi; role: string; target: Target }): ReactElement {
  const key = JSON.stringify(target);
  const mount = useCallback((el: HTMLElement) => core.boardActions.mountAdvanced(el, role, JSON.parse(key) as Target), [core, role, key]);
  return <Mounted mount={mount} className="bc:min-w-0" />;
}
