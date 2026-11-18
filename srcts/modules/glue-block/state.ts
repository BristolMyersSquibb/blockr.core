// The state of a glue block: the text that glue::glue() fills in.
// The build writes its JSON Schema beside the module,
// as glue-block.schema.json.

export type State = Readonly<{
  /** Template string evaluated with glue::glue(). */
  text: string;
}>;
