// The state of a static block: the R expression it evaluates, as a string,
// which an app script gives, since no UI sets it.
// The build writes its JSON Schema beside the module,
// as static-block.schema.json.

export type State = Readonly<{
  expression?: string;
}>;
