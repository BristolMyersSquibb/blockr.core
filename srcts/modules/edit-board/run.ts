// How the Edit board plugin makes an edit: it runs one of the commands that
// the command registry holds, as one step of the user, as the user makes the
// edit, so that undo takes back each one.

import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import { errorMessage } from "../../util/index.ts";

// Runs the command with the args, and resolves whether it ran. Where the
// command registry or the store refuses it, the plugin tells the user why
// through a notification, an error, which the frontend draws.
export function runCommand(core: Pick<CoreApi, "commands" | "notifications">, id: string, args?: Json): Promise<boolean> {
  return core.commands.run(id, args).then(
    () => true,
    (error: unknown) => {
      core.notifications.notify({ message: errorMessage(error), severity: "error" });
      return false;
    },
  );
}
