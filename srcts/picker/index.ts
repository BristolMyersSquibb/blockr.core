// The block picker, the form that adds a block of a class from the block
// registry, which is the advanced action of core's four board actions that add
// a block (../actions/add.tsx), the block classes as it and their quick
// actions list them, and the checks of what a user names a block's id or a
// variadic block's input, which other forms share.
// Core's runtime holds the picker. The bundle of the Edit board plugin holds
// a copy of the checks, as it holds one of srcts/util.

export { blockClasses, blockIdError, BlockPicker, focusFirstInvalid, inputNameError, isVariadic, namesInto, without } from "./picker.tsx";
export type { AddMode, ClassItem } from "./picker.tsx";
