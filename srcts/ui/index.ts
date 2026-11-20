// What core's basic frontend and its Edit board plugin draw with: the classes
// of their buttons, lists and surfaces, what the text fields of their forms
// take where they hold an identifier, the search and the pages of their
// tables, and the hook through which the frontend follows the store's undo
// history.
// It also holds how each runs an edit, and the Edit board plugin's banner that
// says why one was refused, which marks an error as a card's banners do.
// They draw their text fields with the SDK's TextField.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util and srcts/card.
// Both follow the board itself through useBoard() of core's React SDK, and
// draw their selects with the SDK's ComboBox, those that find their items by
// more than their names through useSearch().

export { runCommand, useEdits } from "./edits.ts";
export type { Edits, Refused } from "./edits.ts";
export { useHistory } from "./hooks.ts";
export { useSearch } from "./search.ts";
export type { Search } from "./search.ts";
export { Pager, TableSearch, useTableView } from "./table.tsx";
export type { TableView } from "./table.tsx";
export { classes, identifier, NameField, Refusal } from "./ui.tsx";
