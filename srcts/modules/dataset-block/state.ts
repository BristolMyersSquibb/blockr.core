// The state of a dataset block: the data frame it loads, unset in a new block,
// and the package that ships it.
// The build writes its JSON Schema beside the module,
// as dataset-block.schema.json.

export type State = Readonly<{
  dataset?: string;
  package: string;
}>;
