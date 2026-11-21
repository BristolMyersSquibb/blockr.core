// What core's basic frontend, its Edit board plugin and the advanced actions
// of core's board actions draw with: the classes of their buttons, lists and
// surfaces, what the text fields of their forms take where they hold an
// identifier, a block as their selects offer it, the search and the pages of
// their tables, and the hook through which the frontend follows the store's
// undo history.
// It also holds how each runs an edit, and the banner that says why one was
// refused, which marks an error as a card's banners do.
// They draw their text fields with the SDK's TextField, and the search of a
// table with its SearchField.
// The bundles of the basic frontend, of the Edit board plugin and of core's
// runtime each hold a copy of it, as each holds one of srcts/util and
// srcts/card.
// Each follows the board itself through useBoard() of core's React SDK, and
// draws its selects with the SDK's ComboBox, those that find their items by
// more than their names through useSearch(), and a pick of a few values with
// its Select.

export { blockItem, blockName, blockRow, blockTexts } from "./blocks.tsx";
export type { BlockItem } from "./blocks.tsx";
export { runCommand, useEdits } from "./edits.ts";
export type { Edits, Refused } from "./edits.ts";
export { useHistory } from "./hooks.ts";
export { useSearch } from "./search.ts";
export type { Search } from "./search.ts";
export { Pager, TableSearch, useTableView } from "./table.tsx";
export type { TableView } from "./table.tsx";
export { classes, identifier, NameField, Refusal } from "./ui.tsx";
