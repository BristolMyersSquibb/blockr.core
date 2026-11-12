// The picker of the block registry's entries, which core's Edit board plugin
// shows and the menu of a card of core's basic frontend opens, so that both
// add a block the same way.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util.

export { BlockPicker } from "./picker.tsx";
export type { AddMode, PickerProps } from "./picker.tsx";
