// The form that adds a block of a class from the block registry, which core's
// Edit board plugin shows and the menu of a card of core's basic frontend
// opens, so that both add a block the same way, and the checks of what a user
// names a block's id or a variadic block's input, which other forms share.
// The bundles of the basic frontend and of the Edit board plugin each hold a
// copy of it, as each holds one of srcts/util.

export { blockIdError, BlockPicker, focusFirstInvalid, inputNameError, isVariadic, namesInto, without } from "./picker.tsx";
export type { AddMode } from "./picker.tsx";
