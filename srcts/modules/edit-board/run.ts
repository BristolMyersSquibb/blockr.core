// How the Edit board plugin makes an edit: it runs one of the commands that
// the command registry holds, as one step of the user, as the user makes the
// edit, so that undo takes back each one.

import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { errorMessage } from "../../util/index.ts";

// The id of the plugin's class, which is the origin of its notifications.
const EDIT_BOARD = "blockr.core-edit_board_plugin";

// Runs the command with the args, and resolves whether it ran. Where the
// command registry or the store refuses it, the plugin tells the user why
// through a notification, an error of the class `edit_refused` that concerns
// no block, whatever block the edit names, which the frontend draws, as The
// Edit board plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#each-edit-at-once).
export function runCommand(core: Pick<CoreApi, "commands" | "notifications">, id: string, args?: Json): Promise<boolean> {
  return core.commands.run(id, args).then(
    () => true,
    (error: unknown) => {
      core.notifications.notify({
        severity: "error",
        class: ["edit_refused", "blockr_error", "error", "condition"],
        message: errorMessage(error),
        origin: EDIT_BOARD,
      });
      return false;
    },
  );
}
