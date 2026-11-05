// The state of a head block: the number of rows it keeps, and from which end.
// The build writes its JSON Schema beside the module,
// as head-block.schema.json.

export type State = Readonly<{
  /**
   * @asType integer
   * @minimum 0
   */
  n: number;
  direction: "head" | "tail";
}>;
