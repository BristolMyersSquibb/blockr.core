// The state of a fixed block: the R code it applies to its data input,
// as a string, which an app script gives, since no UI sets it.
// The build writes its JSON Schema beside the module,
// as fixed-block.schema.json.

export type State = Readonly<{
  expr?: string;
}>;
