// The state of a subset block: the R expressions, as strings,
// that select its rows and its columns, each empty where it selects nothing.
// The build writes its JSON Schema beside the module,
// as subset-block.schema.json.

export type State = Readonly<{
  subset: string;
  select: string;
}>;
