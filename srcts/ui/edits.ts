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
//   The advanced action of Edit inputs, one of core's board actions, runs its
//   edits so too, and shows why below its list of inputs
//   (../actions/inputs.tsx).
//   The block picker, the advanced action of core's board actions that add a
//   block, runs its edit through useEdits() as well, and hands it to its
//   done(), which runs it as one step of the user and ends the advanced action
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).

import { useMemo, useState } from "react";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// A command of a list that an edit runs, with its args.
interface Listed {
  readonly id: string;
  readonly args?: Json;
}

// What runs the list of an edit in place of the command registry, as the
// done() of an advanced action does: as one step of the user, rejecting with
// the refusal, whose notifications the notifier takes.
export type Done = (list: readonly Listed[], opts: { notifier(): void }) => Promise<void>;

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
  // holds until the next edit that run() makes, or until clear().
  readonly refusal: Refused | null;
  // Runs the command with the args, or the commands of the list in order, as
  // runCommand() does, and resolves whether it ran. A list runs through
  // `done` where it is given.
  // The title is the sentence that names the edit, such as `Could not remove
  // the block "Head".`, which heads the reasons where the edit is refused.
  // The reasons name the board's objects by their ids, which a table may not
  // show.
  run(title: string, id: string, args?: Json): Promise<boolean>;
  run(title: string, list: readonly Listed[], done?: Done): Promise<boolean>;
  // Takes the refusal down where the user's next edit there does not reach
  // run(): where a form's own checks stop it, or where the user opens the
  // form that changes a row of the table.
  clear(): void;
}

// Why the store or the command registry refused an edit: the sentence that
// names the edit, and each reason in a sentence of its own.
export interface Refused {
  readonly title: string;
  readonly reasons: readonly string[];
}

// The edits of one control, which run() runs with a notifier that takes the
// notification of a refusal, so that core raises none, since the control
// shows why itself.
export function useEdits(core: Pick<CoreApi, "commands">): Edits {
  const [refusal, setRefusal] = useState<Refused | null>(null);
  return useMemo(() => {
    const opts = { notifier: () => undefined };
    function run(title: string, target: string | readonly Listed[], given?: Json | Done): Promise<boolean> {
      setRefusal(null);
      const running =
        typeof target === "string"
          ? core.commands.run(target, given as Json | undefined, "user", opts)
          : typeof given === "function"
            ? given(target, opts)
            : core.commands.run(target, "user", opts);
      return running.then(
        () => true,
        (error: unknown) => {
          setRefusal({ title, reasons: reasons(error) });
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
  }, [core, refusal]);
}

// Each reason why the command registry or the store refused an edit: the
// failures of the CommandError or the StoreError that the edit rejects with,
// without the line that heads its message, which names the command or the
// delta rather than the edit, or else what the error says.
// It reads the failures off the error, since the bundle of a plugin holds its
// own copy of core's sources, whose classes are not those of the runtime that
// threw.
function reasons(error: unknown): readonly string[] {
  const failures = error instanceof Error && "failures" in error ? error.failures : undefined;
  return isSentences(failures) ? failures : [errorMessage(error)];
}

function isSentences(x: unknown): x is readonly string[] {
  return Array.isArray(x) && x.length > 0 && x.every((item) => typeof item === "string");
}
