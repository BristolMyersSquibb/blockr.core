// The state of a file browser block: the path of the file it gives,
// unset in a new block, and the directories the browser starts from,
// each by its name.
// The build writes its JSON Schema beside the module,
// as filebrowser-block.schema.json.

export type State = Readonly<{
  file_path?: string;
  volumes: Readonly<Record<string, string>>;
}>;
