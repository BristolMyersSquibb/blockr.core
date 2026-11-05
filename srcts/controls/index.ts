// Core's inputs for packages that write their modules without React,
// each bound to one value, a state arg of a block or the value of a board
// option, which controls() draws with core's copy of React.

export { controls } from "./controls.tsx";
export type { Checklist, Controls, NumberInput, Select } from "./controls.tsx";
export { choicesOf, PAUSE, strings } from "../binding/index.ts";
export type { Binding, Choice, Followed, FollowOptions } from "../binding/index.ts";
export { styles } from "../styles/index.ts";
