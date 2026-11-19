// How core's basic frontend and its Edit board plugin make an edit: each runs
// one of the commands that the command registry holds, or a list of them, as
// one step of the user, as the user makes the edit, so that undo takes back
// each one.
// Where the store or the command registry refuses an edit, the two tell the
// user why in different places:
// - The basic frontend runs its own edits through runCommand(), which leaves
//   the refusal to core. Core raises it as a notification, an error of the
//   code `board_edit_failed`, which concerns the board (../core/core.ts), and
//   the frontend draws it as it draws every notification.
// - The Edit board plugin runs its edits through useEdits(), which takes the
//   notification of a refusal through the edit's notifier, so that core raises
//   none. The plugin shows why close to the control that made the edit, until
//   the user's next edit there: below the buttons of a form that adds to the
//   board, and below a table for an edit in its rows, as The Edit board plugin
//   has it
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#each-edit-at-once).

import { useMemo, useState } from "react";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// A command of a list that an edit runs, with its args.
interface Listed {
  readonly id: string;
  readonly args?: Json;
}

// Runs the command with the args, or the commands of the list in order, as an
// edit of the user, through CoreApi, and resolves whether it ran.
export function runCommand(core: Pick<CoreApi, "commands">, id: string, args?: Json): Promise<boolean>;
export function runCommand(core: Pick<CoreApi, "commands">, list: readonly Listed[]): Promise<boolean>;
export function runCommand(core: Pick<CoreApi, "commands">, target: string | readonly Listed[], args?: Json): Promise<boolean> {
  const running = typeof target === "string" ? core.commands.run(target, args) : core.commands.run(target);
  return running.then(
    () => true,
    () => false,
  );
}

// The edits of one control, such as a form or a table with the buttons and
// the forms of its rows, and why the store or the command registry refused
// the last of them, which the control shows close to itself.
export interface Edits {
  // Why the last edit that run() made was refused, or null where it ran. It
  // is the reason with which the command registry rejected the edit, after
  // the sentence `which` where run() was given one. It holds until the next
  // edit that run() makes, or until clear().
  readonly refusal: string | null;
  // Runs the command with the args, or the commands of the list in order, as
  // runCommand() does, and resolves whether it ran.
  // A control that shows the reason away from where the user made the edit,
  // as a table does below itself for an edit in its rows, gives the sentence
  // `which`, such as `Could not remove the block "Head".`. The reason names
  // the objects by their ids, which the table may not show.
  run(id: string, args?: Json, which?: string): Promise<boolean>;
  run(list: readonly Listed[], which?: string): Promise<boolean>;
  // Takes the refusal down where the user's next edit there does not reach
  // run(): where a form's own checks stop it, or where the user opens the
  // form that changes a row of the table.
  clear(): void;
}

// The edits of one control, which run() runs with a notifier that takes the
// notification of a refusal, so that core raises none, since the control
// shows why itself.
// Where `shows` is false, run() passes no notifier, so that core raises the
// notification, and `refusal` stays null, as for a control that leaves the
// refusal to core, such as the form for a block in a dialog of core's basic
// frontend.
export function useEdits(core: Pick<CoreApi, "commands">, shows = true): Edits {
  const [refusal, setRefusal] = useState<string | null>(null);
  return useMemo(() => {
    const opts = { notifier: () => undefined };
    // The second argument is the args for a command, and `which` for a list.
    function run(target: string | readonly Listed[], second?: Json, third?: string): Promise<boolean> {
      const args = typeof target === "string" ? second : undefined;
      const which = typeof target === "string" ? third : typeof second === "string" ? second : undefined;
      if (!shows) return typeof target === "string" ? runCommand(core, target, args) : runCommand(core, target);
      setRefusal(null);
      const running =
        typeof target === "string" ? core.commands.run(target, args, "user", opts) : core.commands.run(target, "user", opts);
      return running.then(
        () => true,
        (error: unknown) => {
          setRefusal(which === undefined ? errorMessage(error) : `${which}\n${errorMessage(error)}`);
          return false;
        },
      );
    }
    return {
      refusal,
      run,
      clear: () => {
        setRefusal(null);
      },
    };
  }, [core, shows, refusal]);
}
