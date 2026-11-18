// The state of a CSV block:
// the field separator and the quoting characters,
// which utils::read.table() takes.
// The build writes its JSON Schema beside the module, as csv-block.schema.json.

export type State = Readonly<{
  /** Field separator character. */
  sep: string;
  /** Quoting character. */
  quote: string;
}>;
