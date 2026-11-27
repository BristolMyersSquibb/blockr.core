// What core's basic frontend, its Edit board plugin and the advanced actions
// of core's board actions, such as the block picker, draw with: the classes
// of their surfaces, lists, hints and alerts, what the text fields of their forms
// take where they hold an identifier, the search and the pages of their
// tables, and the hook through which the frontend follows the store's undo
// history.
// It also holds how each runs an edit, and the banner of the Edit board plugin
// and of the advanced actions that says why one was refused, which marks an
// error as a card's banners do.
// They draw their text fields with blockr.ui's TextField, and the search of a
// table with its SearchField.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util and srcts/card, and so does the
// file of each advanced action of core's board actions, which core's runtime
// loads as it first renders the advanced action (../actions).
// They follow the board itself through useBoard() of core's React SDK, and
// draw their selects with blockr.ui's ComboBox, those that find their items by
// more than their names through useSearch(), and a pick of a few values with
// its Select.

export { blockItem, blockRow, blockTexts } from "./blocks.tsx";
export type { BlockItem } from "./blocks.tsx";
export { notifiedReasons, runCommand, useEdits } from "./edits.ts";
export type { Done, Edits, Refused } from "./edits.ts";
export { useHistory } from "./hooks.ts";
export { useSearch } from "./search.ts";
export type { Search } from "./search.ts";
export { Pager, TableSearch, useTableView } from "./table.tsx";
export type { TableView } from "./table.tsx";
export { ActionsButton, classes, drawForm, identifier, NameField, Refusal } from "./ui.tsx";
