// The state of a merge block: the columns it joins by, unset in a new block,
// and whether it keeps the rows of each input that the other does not match.
// The build writes its JSON Schema beside the module,
// as merge-block.schema.json.

export type State = Readonly<{
  /** Shared key column(s) to join on. */
  by?: readonly string[];
  /** Keep unmatched rows from the first input. */
  all_x: boolean;
  /** Keep unmatched rows from the second input. */
  all_y: boolean;
}>;
