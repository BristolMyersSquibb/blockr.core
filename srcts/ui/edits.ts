// How core's basic frontend and its Edit board plugin make an edit: each runs
// one of the commands that the command registry holds, or a list of them, as
// one step of the user, as the user makes the edit, so that undo takes back
// each one.
// Where the store or the command registry refuses an edit, core tells the user
// why through a notification, an error of the code `board_edit_failed`, which
// concerns the board (../core/core.ts). So the plugin that made the edit only
// learns whether it ran.

import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";

// Runs the command with the args, or the commands of the list in order, as an
// edit of the user, through CoreApi, and resolves whether it ran.
export function runCommand(core: Pick<CoreApi, "commands">, id: string, args?: Json): Promise<boolean>;
export function runCommand(
  core: Pick<CoreApi, "commands">,
  list: readonly { readonly id: string; readonly args?: Json }[],
): Promise<boolean>;
export function runCommand(
  core: Pick<CoreApi, "commands">,
  target: string | readonly { readonly id: string; readonly args?: Json }[],
  args?: Json,
): Promise<boolean> {
  const running = typeof target === "string" ? core.commands.run(target, args) : core.commands.run(target);
  return running.then(
    () => true,
    () => false,
  );
}
