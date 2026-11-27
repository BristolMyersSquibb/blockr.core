// How core's basic frontend and the advanced actions of core's board actions
// make an edit: each runs one of the commands that the command registry
// holds, or a batch of them, as one step of the user, as the user makes the
// edit, so that undo takes back each one.
// Where the store or the command registry refuses an edit, they tell the user
// why in different places:
// - The basic frontend runs its own edits through runCommand(), which leaves
//   the refusal to core. Core raises it as a notification, an error of the
//   code `board_edit_failed`, which concerns the board (../core/core.ts), and
//   the frontend draws it as it draws every notification.
// - The advanced actions run their edits through useEdits(), which takes the
//   notification of a refusal through the edit's notifier, so that core raises
//   none. Each shows why close to the control that made the edit, until the
//   user's next edit there. The block picker, the advanced action of core's
//   board actions that add a block, the forms of a link and of a stack, and
//   the inputs of a block that Edit inputs lists (../actions/inputs.tsx) hand
//   their edit to their done(), which runs it as one step of the user and ends
//   the advanced action. They show why below their buttons
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
// The Edit board plugin makes no edit itself. Its forms are those advanced
// actions, and its rows open the menus of board actions. It takes the
// notifications of a board action that one of its menus ran through the
// notifier of menu(), and reads the reasons off them (notifiedReasons(),
// below), as The Edit board plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#each-edit-at-once).

import { useMemo, useState } from "react";
import type { CommandCall } from "../actions/index.ts";
import type { CommandId } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// What runs the batch of an edit in place of the command registry, as the
// done() of an advanced action does: as one step of the user, rejecting with
// the refusal, whose notifications the notifier takes.
export type Done = (batch: readonly CommandCall[], opts: { notifier(): void }) => Promise<void>;

// Runs the command with the args, or the commands of the batch in order, as
// an edit of the user, through CoreApi, and resolves whether it ran.
export function runCommand(core: Pick<CoreApi, "commands">, id: CommandId, args?: Json): Promise<boolean>;
export function runCommand(core: Pick<CoreApi, "commands">, batch: readonly CommandCall[]): Promise<boolean>;
export function runCommand(core: Pick<CoreApi, "commands">, target: CommandId | readonly CommandCall[], args?: Json): Promise<boolean> {
  const running = isCommandName(target) ? core.commands.run(target, args) : core.commands.run(target);
  return running.then(
    () => true,
    () => false,
  );
}

// The edits of one control, such as a form, and why the store or the command
// registry refused the last of them, which the control shows close to itself.
export interface Edits {
  // Why the last edit that run() made was refused, or null where it ran. It
  // holds until the next edit that run() makes, or until clear().
  readonly refusal: Refused | null;
  // Runs the command with the args, or the commands of the batch in order, as
  // runCommand() does, and resolves whether it ran. A batch runs through
  // `done` where it is given.
  // The title is the sentence that names the edit, such as `Could not add the
  // link.`, which heads the reasons where the edit is refused. The reasons
  // name the board's objects by their ids, which a form may not show.
  run(title: string, id: CommandId, args?: Json): Promise<boolean>;
  run(title: string, batch: readonly CommandCall[], done?: Done): Promise<boolean>;
  // Takes the refusal down where the user's next edit there does not reach
  // run(), as where a form's own checks stop it.
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
    function run(title: string, target: CommandId | readonly CommandCall[], given?: Json | Done): Promise<boolean> {
      setRefusal(null);
      const running =
        isCommandName(target)
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

// Each reason why the command registry or the store refused an edit, as the
// notifications of the refusal say it, for a control that takes the
// notifications through a notifier rather than the error, as the Edit board
// plugin takes those of a menu.
// The message of a CommandError or a StoreError heads its failures with a
// line that names the command or the delta, and then gives each failure on a
// line of its own, after "- ". The reasons are those failures, without the
// line that heads them. A message of another shape, such as that a board
// action's run failed, is one reason as it stands.
export function notifiedReasons(notifications: readonly Pick<Notification, "message">[]): readonly string[] {
  return notifications.flatMap(({ message }) => {
    const [, ...lines] = message.split("\n");
    const failures = lines.filter((line) => line.startsWith("- "));
    return failures.length > 0 && failures.length === lines.length ? failures.map((line) => line.slice(2)) : [message];
  });
}

function isSentences(x: unknown): x is readonly string[] {
  return Array.isArray(x) && x.length > 0 && x.every((item) => typeof item === "string");
}

// A command alone, by its id, as runCommand() and an edit's run() take it
// beside a batch.
function isCommandName(target: CommandId | readonly CommandCall[]): target is CommandId {
  return !Array.isArray(target);
}
