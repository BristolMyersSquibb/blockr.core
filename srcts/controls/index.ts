// Inputs for packages that write their modules without React,
// each bound to one value, a state arg of a block, a value that a function
// reads from a block's state, or the value of a board option,
// which controls() draws with blockr.ui's inputs, on the one copy of React on
// the page.

export { controls } from "./controls.tsx";
export type { BlockControls, ComboBox, ComboBoxOptions, Controls, Item, NumberField, NumberFieldOptions, TextFieldOptions } from "./controls.tsx";
export { PAUSE, strings } from "../binding/index.ts";
export type { Binding, Followed, FollowOptions } from "../binding/index.ts";
