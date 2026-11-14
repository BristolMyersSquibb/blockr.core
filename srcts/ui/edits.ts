// How core's basic frontend and its Edit board plugin make an edit: each runs
// one of the commands that the command registry holds, as one step of the
// user, as the user makes the edit, so that undo takes back each one.
// Where the store or the command registry refuses an edit, the plugin that
// made it tells the user why through a notification, an error of the code
// `edit_refused`, which concerns the plugin, whatever block the edit names, so
// that a frontend shows it apart from the block's card, as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).

import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// Tells the user why the store or the command registry refused an edit that
// the plugin of the class `plugin` made, through its CoreApi, `core`.
export function refused(
  by: { readonly core: Pick<CoreApi, "notifications">; readonly plugin: PluginClassId },
  error: unknown,
): void {
  by.core.notifications.notify(by.plugin, { severity: "error", code: "edit_refused", message: errorMessage(error) });
}

// Runs the command with the args as an edit of the plugin of the class
// `plugin`, through its CoreApi, `core`, and resolves whether it ran.
export function runCommand(
  by: { readonly core: Pick<CoreApi, "commands" | "notifications">; readonly plugin: PluginClassId },
  id: string,
  args?: Json,
): Promise<boolean> {
  return by.core.commands.run(id, args).then(
    () => true,
    (error: unknown) => {
      refused(by, error);
      return false;
    },
  );
}
