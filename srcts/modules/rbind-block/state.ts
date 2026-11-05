// The state of a row-bind block, which has no state args.
// The build writes its JSON Schema beside the module,
// as rbind-block.schema.json.

export type State = Readonly<Record<string, never>>;
