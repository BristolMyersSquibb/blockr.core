// The binding of a control to one value, a block's state arg or a board
// option's value, and requests that follow what they read, needing neither
// the DOM nor React.

export { bind, follow, PAUSE, SEARCH_PAUSE, strings } from "./binding.ts";
export type { BindOptions, Binding, Followed, FollowOptions } from "./binding.ts";
