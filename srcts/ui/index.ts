// What core's basic frontend and its Edit board plugin draw with: the classes
// of their buttons, lists and surfaces, the fields of their forms, the search
// and the pages of their tables, and the hook through which the frontend
// follows the store's undo history.
// It also holds how each runs an edit.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util.
// Both follow the board itself through useBoard() of core's React SDK, and
// draw their selects with the SDK's Select.

export { runCommand } from "./edits.ts";
export { TextField } from "./fields.tsx";
export { useHistory } from "./hooks.ts";
export { Pager, TableSearch, useTableView } from "./table.tsx";
export type { TableView } from "./table.tsx";
export { classes, NameField } from "./ui.tsx";
