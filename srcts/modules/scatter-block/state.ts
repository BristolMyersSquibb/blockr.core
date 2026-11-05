// The state of a scatter block: the columns on its two axes,
// each unset in a new block.
// The build writes its JSON Schema beside the module,
// as scatter-block.schema.json.

export type State = Readonly<{
  x?: string;
  y?: string;
}>;
