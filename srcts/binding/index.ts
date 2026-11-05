// The binding of a control to one value, a block's state arg or a board
// option's value, and requests that follow what they read, needing neither
// the DOM nor React.

export { bind, choicesOf, follow, PAUSE, strings } from "./binding.ts";
export type { BindOptions, Binding, Choice, Followed, FollowOptions } from "./binding.ts";
