// The block picker, the form that adds a block of a class from the block
// registry, which is the advanced action of core's four board actions that add
// a block (../actions/add.ts), and what the other forms of core's board
// actions share with it: the check of what a user names a block's id, whether
// a block's free inputs are those of a variadic block, and the helpers for the
// errors of a form's fields.
// Core's runtime imports the picker as it first renders one of those advanced
// actions, so that the page loads the components of blockr.ui that the picker
// draws with only then. The file of each of core's forms holds a copy of what
// it uses of the module, as it holds one of srcts/util.
// The block classes as the picker and the quick actions that add a block list
// them, and the check of what a user names a variadic block's input, which
// core's runtime runs itself, are in ../util.

export { BlockPicker, blockIdError, drawPicker, focusFirstInvalid, isVariadic, without } from "./picker.tsx";
export type { AddMode, PickerProps } from "./picker.tsx";
