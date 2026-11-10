// What the basic frontend tells the user of what the store dropped as a board
// opened, as Board objects has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#objects-that-cannot-be-rebuilt):
// each package that the board was saved with and the app does not load, with
// the version that the board records, so that the user learns what to
// install, each block and each plugin with its class and the package of its
// class, and each board option by its id.

import type { Dropped } from "../../store/index.ts";
import type { Note } from "../../types/core-api.ts";

// The note that names what the store dropped, one line for each, or undefined
// where it dropped nothing.
export function droppedNote(dropped: readonly Dropped[]): Note | undefined {
  if (dropped.length === 0) return undefined;
  const lines = dropped.map((item) => `- ${named(item)}`);
  return {
    message: ["The board opened without what the app cannot rebuild:", ...lines].join("\n"),
    severity: "warning",
  };
}

function named(item: Dropped): string {
  if ("option" in item) return `the value of the board option "${item.option}"`;
  if ("block" in item) return `the block "${item.block}" (${ofClass(item.registry_id, item.package)})`;
  if ("plugin" in item) return `the plugin "${item.plugin}" (${ofClass(item.class, item.package)})`;
  return `the package ${item.package} ${item.version}, which the board was saved with and the app does not load`;
}

// A class by its id, with the package that defines it, where one does.
function ofClass(id: string, pkg: string | null): string {
  return pkg === null ? id : `${id}, from ${pkg}`;
}
