// The state of a head block: the number of rows it keeps, and from which end.
// The build writes its JSON Schema beside the module,
// as head-block.schema.json.

export type State = Readonly<{
  /**
   * Number of rows to keep.
   * @asType integer
   * @minimum 0
   */
  n: number;
  /** Row end to keep, either head (first) or tail (last). */
  direction: "head" | "tail";
}>;
