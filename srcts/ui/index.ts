// What core's basic frontend and its Edit board plugin draw with: the classes
// of their buttons, lists and surfaces, an inline field that renames, and the
// hooks through which they follow the store.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util.

export { useBoard, useHistory } from "./hooks.ts";
export { classes, NameField } from "./ui.tsx";
