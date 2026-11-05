// The state of an upload block: the path at which R keeps the uploaded file,
// unset until a file is uploaded.
// The build writes its JSON Schema beside the module,
// as upload-block.schema.json.

export type State = Readonly<{
  file_path?: string;
}>;
